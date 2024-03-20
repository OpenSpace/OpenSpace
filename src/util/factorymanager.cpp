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

#include <openspace/util/factorymanager.h>

#include <openspace/documentation/documentationengine.h>
#include <openspace/documentation/documentation.h>
#include <openspace/json.h>
#include <openspace/rendering/dashboarditem.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/lightsource.h>
#include <openspace/scene/rotation.h>
#include <openspace/scene/scale.h>
#include <openspace/scene/timeframe.h>
#include <openspace/scene/translation.h>
#include <openspace/util/resourcesynchronization.h>
#include <openspace/util/task.h>
#include <sstream>

namespace {
using namespace openspace;
using namespace openspace::documentation;

nlohmann::json generateJsonDocumentation(const Documentation& d) {
    nlohmann::json json;

    json["name"] = d.name;
    json["identifier"] = d.id;
    json["description"] = d.description;
    json["members"] = nlohmann::json::array();

    for (const DocumentationEntry& p : d.entries) {
        nlohmann::json entry;
        entry["name"] = p.key;
        entry["optional"] = p.optional.value;
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
            const Documentation doc = { .entries = tv->documentations };
            const nlohmann::json restrictions = generateJsonDocumentation(doc);
            // We have a TableVerifier, so we need to recurse
            entry["restrictions"] = restrictions;
        }
        else {
            entry["description"] = p.verifier->documentation();
        }
        json["members"].push_back(entry);
    }
    sortJson(json["members"], "name");

    return json;
}

} // namespace

namespace openspace {

FactoryManager* FactoryManager::_manager = nullptr;

FactoryManager::FactoryNotFoundError::FactoryNotFoundError(std::string t)
    : ghoul::RuntimeError("Could not find TemplateFactory for type '" + t + "'")
    , type(std::move(t))
{
    ghoul_assert(!type.empty(), "Type must not be empty");
}

FactoryManager::FactoryManager() {}

void FactoryManager::initialize() {
    ghoul_assert(!_manager, "Factory Manager must not have been initialized");

    _manager = new FactoryManager;
    _manager->addFactory<Renderable>("Renderable");
    _manager->addFactory<Translation>("Translation");
    _manager->addFactory<Rotation>("Rotation");
    _manager->addFactory<Scale>("Scale");
    _manager->addFactory<TimeFrame>("TimeFrame");
    _manager->addFactory<LightSource>("LightSource");
    _manager->addFactory<Task>("Task");
    _manager->addFactory<ResourceSynchronization>("ResourceSynchronization");
    _manager->addFactory<DashboardItem>("DashboardItem");
}

void FactoryManager::deinitialize() {
    ghoul_assert(_manager, "Factory Manager must have been initialized");

    delete _manager;
    _manager = nullptr;
}

bool FactoryManager::isInitialized() {
    return _manager != nullptr;
}

FactoryManager& FactoryManager::ref() {
    ghoul_assert(_manager, "Factory Manager must have been initialized");

    return *_manager;
}

nlohmann::json FactoryManager::generateJson() const {
    nlohmann::json json;
    std::vector<Documentation> docs = DocEng.documentations();

    for (const FactoryInfo& factoryInfo : _factories) {
        nlohmann::json factory;
        factory["name"] = factoryInfo.name;
        factory["identifier"] = "category" + factoryInfo.name;

        ghoul::TemplateFactoryBase* f = factoryInfo.factory.get();
        // Add documentation about base class
        auto factoryDoc = std::find_if(
            docs.begin(),
            docs.end(),
            [&factoryInfo](const Documentation& d) {
                return d.name == factoryInfo.name;
            });
        if (factoryDoc != docs.end()) {
            const nlohmann::json documentation = generateJsonDocumentation(*factoryDoc);
            factory["classes"].push_back(documentation);
            // Remove documentation from list check at the end if all docs got put in
            docs.erase(factoryDoc);
        }
        else {
            nlohmann::json documentation;
            documentation["name"] = factoryInfo.name;
            documentation["identifier"] = factoryInfo.name;
            factory["classes"].push_back(documentation);
        }

        // Add documentation about derived classes
        const std::vector<std::string>& registeredClasses = f->registeredClasses();
        for (const std::string& c : registeredClasses) {
            auto found = std::find_if(
                docs.begin(),
                docs.end(),
                [&c](const Documentation& d) {
                    return d.name == c;
                });
            if (found != docs.end()) {
                const nlohmann::json documentation = generateJsonDocumentation(*found);
                factory["classes"].push_back(documentation);
                docs.erase(found);
            }
            else {
                nlohmann::json documentation;
                documentation["name"] = c;
                documentation["identifier"] = c;
                factory["classes"].push_back(documentation);
            }
        }
        sortJson(factory["classes"], "name");
        json.push_back(factory);
    }
    // Add all leftover docs
    nlohmann::json leftovers;
    leftovers["name"] = "Other";
    leftovers["identifier"] = "other";

    for (const Documentation& doc : docs) {
        leftovers["classes"].push_back(generateJsonDocumentation(doc));
    }
    sortJson(leftovers["classes"], "name");
    json.push_back(leftovers);
    sortJson(json, "name");

    // I did not check the output of this for correctness ---abock
    nlohmann::json result;
    result["name"] = "Asset Types";
    result["data"] = json;

    return result;
}

}  // namespace openspace
