/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

void sortJson(nlohmann::json& json) {
    std::sort(json.begin(), json.end(), []
    (const nlohmann::json& lhs, const nlohmann::json& rhs) {
            std::string lhsString = lhs["Name"];
            std::string rhsString = rhs["Name"];
            std::transform(lhsString.begin(), lhsString.end(), lhsString.begin(),
                [](unsigned char c) { return std::tolower(c); });
            std::transform(rhsString.begin(), rhsString.end(), rhsString.begin(),
                [](unsigned char c) { return std::tolower(c); });
            return rhsString > lhsString;
        });
}

nlohmann::json generateJsonDocumentation(const Documentation& d) {
    nlohmann::json json;

    json["Name"] = d.name;
    json["Identifier"] = d.id;
    json["Members"] = nlohmann::json::array();

    for (const DocumentationEntry& p : d.entries) {
        nlohmann::json entry;
        entry["Name"] = p.key;
        entry["Optional"] = p.optional ? true : false;
        entry["Type"] = p.verifier->type();
        entry["Documentation"] = p.documentation;

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
                entry["Reference"]["Found"] = false;
            }
            else {
                nlohmann::json reference;
                reference["Found"] = true;
                reference["Name"] = it->name;
                reference["Identifier"] = rv->identifier;

                entry["Reference"] = reference;
            }
        }
        else if (tv) {
            nlohmann::json json = generateJsonDocumentation(tv->documentations);
            // We have a TableVerifier, so we need to recurse
            entry["Restrictions"] = json;
        }
        else {
            entry["Description"] = p.verifier->documentation();
        }
        json["Members"].push_back(entry);
    }
    sortJson(json["Members"]);

    return json;
}
}

namespace openspace {

FactoryManager* FactoryManager::_manager = nullptr;

FactoryManager::FactoryNotFoundError::FactoryNotFoundError(std::string t)
    : ghoul::RuntimeError("Could not find TemplateFactory for type '" + t + "'")
    , type(std::move(t))
{
    ghoul_assert(!type.empty(), "Type must not be empty");
}

FactoryManager::FactoryManager()
    : DocumentationGenerator(
        "Factory Documentation",
        "factory",
        {
            { "factoryTemplate", "${WEB}/documentation/factory.hbs" }
        }
    )
{}

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

std::string FactoryManager::generateJson() const {
    nlohmann::json json;

    for (const FactoryInfo& factoryInfo : _factories) {
        nlohmann::json factory;
        factory["Name"] = factoryInfo.name;

        ghoul::TemplateFactoryBase* f = factoryInfo.factory.get();
        const std::vector<std::string>& registeredClasses = f->registeredClasses();
        for (const std::string& c : registeredClasses) {
            json["Classes"].push_back(c);
        }
        json["Data"].push_back(factory);
    }

    // I did not check the output of this for correctness ---abock
    return json.dump();
}

nlohmann::json FactoryManager::generateJsonJson() const {
    nlohmann::json json;
    std::vector<Documentation> docs = DocEng.documentations(); 

    for (const FactoryInfo& factoryInfo : _factories) {
        nlohmann::json factory;
        factory["Name"] = factoryInfo.name;
        factory["Identifier"] = "category" + factoryInfo.name;

        ghoul::TemplateFactoryBase* f = factoryInfo.factory.get();
        // Add documentation about base class
        auto factoryDoc = std::find_if(
            docs.begin(),
            docs.end(),
            [&factoryInfo](const Documentation& d) {
                return d.name == factoryInfo.name;
            });
        if (factoryDoc != docs.end()) {
            nlohmann::json documentation = generateJsonDocumentation(*factoryDoc);
            factory["Classes"].push_back(documentation);
            // Remove documentation from list check at the end if all docs got put in
            docs.erase(factoryDoc);          }
        else {
            nlohmann::json documentation;
            documentation["Name"] = factoryInfo.name;
            documentation["Identifier"] = factoryInfo.name;
            factory["Classes"].push_back(documentation);
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
                nlohmann::json documentation = generateJsonDocumentation(*found);
                factory["Classes"].push_back(documentation);
                docs.erase(found);
            }
            else {
                nlohmann::json documentation;
                documentation["Name"] = c;
                documentation["Identifier"] = c;
                factory["Classes"].push_back(documentation);
            }
        }
        sortJson(factory["Classes"]);
        json.push_back(factory);
    }
    // Add all leftover docs
    nlohmann::json leftovers;
    leftovers["Name"] = "Other";
    leftovers["Identifier"] = "other";

    for (const Documentation& doc : docs) {
        leftovers["Classes"].push_back(generateJsonDocumentation(doc));
    }
    sortJson(leftovers["Classes"]);
    json.push_back(leftovers);
    sortJson(json);
    // I did not check the output of this for correctness ---abock
    return json;
}

}  // namespace openspace
