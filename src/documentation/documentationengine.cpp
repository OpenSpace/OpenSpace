/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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
#include <ghoul/fmt.h>

namespace {
    constexpr const char* MainTemplateFilename = "${WEB}/documentation/main.hbs";
    constexpr const char* DocumentationTemplateFilename =
                                                 "${WEB}/documentation/documentation.hbs";
    constexpr const char* JsFilename = "${WEB}/documentation/script.js";
} // namespace

namespace openspace::documentation {

DocumentationEngine* DocumentationEngine::_instance = nullptr;

DocumentationEngine::DuplicateDocumentationException::DuplicateDocumentationException(
                    Documentation doc)
    : ghoul::RuntimeError(fmt::format(
        "Duplicate Documentation with name '{}' and id '{}'",
        doc.name,
        doc.id
    ))
    , documentation(std::move(doc))
{}

DocumentationEngine::DocumentationEngine()
    : DocumentationGenerator(
        "Documentation",
        "documentation",
        {
            { "mainTemplate", MainTemplateFilename },
            { "documentationTemplate", DocumentationTemplateFilename }
        },
        JsFilename
    )
{}

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

std::string generateTextDocumentation(const Documentation& d, int& indentLevel) {
    auto indentMessage = [&indentLevel](const std::string& prefix, const std::string& msg)
    {
        if (msg.empty()) {
            return std::string();
        }
        else {
            return std::string(indentLevel, '\t') + prefix + ": " + msg + '\n';
        }
    };
    std::string result;

    result += indentMessage("Name", d.name);
    if (!d.name.empty()) {
        ++indentLevel;
    }
    for (const DocumentationEntry& p : d.entries) {
        result += indentMessage("Key", (p.key == "*") ? p.key : "\"" + p.key + "\"");
        result += indentMessage("Optional", (p.optional ? "true" : "false"));
        result += indentMessage("Type", p.verifier->type());
        result += indentMessage("Documentation", p.documentation);
        TableVerifier* tv = dynamic_cast<TableVerifier*>(p.verifier.get());
        ReferencingVerifier* rv = dynamic_cast<ReferencingVerifier*>(p.verifier.get());

        // We have to check ReferencingVerifier first as a ReferencingVerifier is also a
        // TableVerifier
        if (rv) {
            const std::vector<Documentation>& documentations = DocEng.documentations();
            auto it = std::find_if(
                documentations.begin(),
                documentations.end(),
                [rv](const Documentation& doc) { return doc.id == rv->identifier; }
            );

            if (it == documentations.end()) {
                result += indentMessage("Referencing", rv->identifier + "(NOT FOUND)");
            }
            else {
                result += indentMessage("Referencing", it->name);
            }
        } else if (tv) {
            // We have a TableVerifier, so we need to recurse
            ++indentLevel;
            result += generateTextDocumentation(
                { "", "", tv->documentations },
                indentLevel
            );
            result = result.substr(0, result.size() - 2);
            --indentLevel;
        }
        else {
            result += indentMessage("Restrictions", p.verifier->documentation());
        }
        result += indentMessage("Documentation", p.documentation);
        result += "\n\n";
    }
    if (!d.name.empty()) {
        --indentLevel;
    }

    return result;
}

std::string generateJsonDocumentation(const Documentation& d) {
    std::stringstream result;
    result << "{";

    result << R"("name": ")" << d.name << "\",";
    result << R"("id": ")" << d.id << "\",";
    result << R"("entries": [)";
    for (const DocumentationEntry& p : d.entries) {
        result << '{';
        result << R"("key": ")" << p.key << "\",";
        result << R"("optional": )" << (p.optional ? "true" : "false") << ',';
        result << R"("type": ")" << p.verifier->type() << "\",";
        result << R"("documentation": ")" << escapedJson(p.documentation) << "\",";
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
                result << R"("reference": { "found": false })";
            } else {
                result << R"("reference": {)"
                    << R"("found": true,)"
                    << R"("name": ")" << it->name << "\","
                    << R"("identifier": ")" << rv->identifier << '\"'
                    << '}';
            }
        }
        else if (tv) {
            std::string json = generateJsonDocumentation({ "", "", tv->documentations });
            // We have a TableVerifier, so we need to recurse
            result << R"("restrictions": )" << json;
        }
        else {
            result << R"("description": ")" << p.verifier->documentation() << '\"';
        }
        result << '}';
        if (&p != &d.entries.back()) {
            result << ", ";
        }

    }

    result << ']';
    result << '}';

    return result.str();
}

std::string generateHtmlDocumentation(const Documentation& d) {
    std::stringstream html;

    html << "\t<tr>\n"
         << "\t\t<td colspan=6>" << d.name << "<a name=\"" << d.id << "\"></a></td>\n";

    for (const DocumentationEntry& p : d.entries) {
        html << "\t<tr>\n"
             << "\t\t<td></td>\n"
             << "\t\t<td>" << p.key << "</td>\n"
             << "\t\t<td>" << (p.optional ? "Optional" : "Required") << "</td>\n"
             << "\t\t<td>" << escapedJson(p.documentation) << "</td>\n"
             << "\t\t<td>" << p.verifier->type() << "</td>\n";

        TableVerifier* tv = dynamic_cast<TableVerifier*>(p.verifier.get());
        ReferencingVerifier* rv = dynamic_cast<ReferencingVerifier*>(p.verifier.get());

        // We have to check ReferencingVerifier first as a ReferencingVerifier is also a
        // TableVerifier
        if (rv) {
            const std::vector<Documentation>& documentations = DocEng.documentations();
            auto it = std::find_if(
                documentations.begin(),
                documentations.end(),
                [rv](const Documentation& doc) { return doc.id == rv->identifier; }
            );

            if (it == documentations.end()) {
                html << "\t\t<td>"
                     << "<font color=\"red\">"
                     << "Could not find identifier: " << rv->identifier
                     << "</font>"
                     << "</td>";
            }
            else {
                html << "\t\t<td>"
                     << "\t\t\tReferencing: "
                     << "<a href=\"#" << rv->identifier << "\">" << it->name << "</a>"
                     << "\t\t</td>";
            }
        }
        else if (tv) {
            // We have a TableVerifier, so we need to recurse
            html << "<td><table>\n"
                 << "\t<thead>\n"
                 << "\t\t<tr>\n"
                 << "\t\t\t<th></th>\n"
                 << "\t\t\t<th>Key</th>\n"
                 << "\t\t\t<th>Optional</th>\n"
                 << "\t\t\t<th>Type</th>\n"
                 << "\t\t\t<th>Restrictions</th>\n"
                 << "\t\t\t<th>Documentation</th>\n"
                 << "\t\t</tr>\n"
                 << "\t</thead>\n"
                 << "\t<tbody>\n"
                 << generateHtmlDocumentation({ "", "", tv->documentations })
                 << "\t</tbody>\n"
                 << "</table>\n"
                 << "</td>\n";
        }
        else {
            html << "\t\t<td>" << p.verifier->documentation() << "</td>\n";
        }
        html << "\t\t<td>" << p.documentation << "</td>\n"
             << "\t</tr>\n";

    }

    return html.str();
}

std::string DocumentationEngine::generateJson() const {
    std::stringstream json;
    json << "[";

    for (const Documentation& d : _documentations) {
        json << generateJsonDocumentation(d);
        if (&d != &_documentations.back()) {
            json << ", ";
        }
    }

    json << "]";

    return json.str();
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
