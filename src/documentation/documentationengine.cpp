/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <openspace/documentation/documentationengine.h>

#include <openspace/openspace.h>
#include <openspace/documentation/core_registration.h>
#include <openspace/documentation/verifier.h>
#include <openspace/json.h>
#include <openspace/util/json_helper.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/profiling.h>

#include <fstream>

namespace openspace::documentation {

DocumentationEngine* DocumentationEngine::_instance = nullptr;

DocumentationEngine::DuplicateDocumentationException::DuplicateDocumentationException(
                                                                        Documentation doc)
    : ghoul::RuntimeError(fmt::format(
        "Duplicate Documentation with name '{}' and id '{}'", doc.name, doc.id
    ))
    , documentation(std::move(doc))
{}

DocumentationEngine::DocumentationEngine() {}

void DocumentationEngine::initialize() {
    ghoul_assert(!isInitialized(), "DocumentationEngine is already initialized");
    _instance = new DocumentationEngine;
}

void DocumentationEngine::deinitialize() {
    ghoul_assert(isInitialized(), "DocumentationEngine is not initialized");
    delete _instance;
    _instance = nullptr;
}

bool DocumentationEngine::isInitialized() {
    return _instance != nullptr;
}

DocumentationEngine& DocumentationEngine::ref() {
    if (_instance == nullptr) {
        _instance = new DocumentationEngine;
        registerCoreClasses(*_instance);
    }
    return *_instance;
}

nlohmann::json generateJsonDocumentation(const Documentation& d) {
    nlohmann::json json;

    json["name"] = d.name;
    json["id"] = d.id;
    json["description"] = d.description;
    json["properties"] = nlohmann::json::array();

    for (const DocumentationEntry& p : d.entries) {
        nlohmann::json entry;
        entry["key"] = p.key;
        entry["optional"] = static_cast<bool>(p.optional);
        entry["type"] = p.verifier->type();
        entry["documentation"] = p.documentation;

        TableVerifier* tv = dynamic_cast<TableVerifier*>(p.verifier.get());
        ReferencingVerifier* rv = dynamic_cast<ReferencingVerifier*>(p.verifier.get());

        if (rv) {
            const std::vector<Documentation>& documentations = DocEng.documentations();
            auto it = std::find_if(
                documentations.begin(),
                documentations.end(),
                [rv](const Documentation& doc) { return doc.id == rv->identifier; }
            );

            if (it == documentations.end()) {
                entry["reference"]["found"] = false;
            }
            else {
                nlohmann::json reference;
                reference["found"] = true;
                reference["name"] = it->name;
                reference["identifier"] = rv->identifier;

                entry["reference"] = reference;
            }
        }
        else if (tv) {
            Documentation doc = { .entries = tv->documentations };
            nlohmann::json restrictions = generateJsonDocumentation(doc);
            // We have a TableVerifier, so we need to recurse
            entry["restrictions"] = restrictions;
        }
        else {
            entry["description"] = p.verifier->documentation();
        }
        json["properties"].push_back(entry);
    }

    return json;
}

std::string DocumentationEngine::generateJson() const {
    nlohmann::json json;

    for (const Documentation& d : _documentations) {
        json["data"].push_back(generateJsonDocumentation(d));
    }

    return json.dump();
}

nlohmann::json DocumentationEngine::generateJsonJson() const {
    nlohmann::json json;

    for (const Documentation& d : _documentations) {
        json.push_back(generateJsonDocumentation(d));
    }

    return json;
}


void DocumentationEngine::addDocumentation(Documentation documentation) {
    if (documentation.id.empty()) {
        _documentations.push_back(std::move(documentation));
    }
    else {
        auto it = std::find_if(
            _documentations.begin(),
            _documentations.end(),
            [documentation](const Documentation& d) { return documentation.id == d.id; }
        );

        if (it != _documentations.end()) {
            throw DuplicateDocumentationException(std::move(documentation));
        }
        else {
            _documentations.push_back(std::move(documentation));
        }
    }
}

std::vector<Documentation> DocumentationEngine::documentations() const {
    return _documentations;
}
} // namespace openspace::documentation
