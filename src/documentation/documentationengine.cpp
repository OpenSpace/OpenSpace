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
#include <openspace/engine/globals.h>
#include <openspace/engine/configuration.h>
#include <openspace/events/eventengine.h>
#include <openspace/interaction/actionmanager.h>
#include <openspace/interaction/keybindingmanager.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/json.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenelicensewriter.h>
#include <openspace/scripting/scriptscheduler.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/json_helper.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/profiling.h>

#include <fstream>
#include <future>

namespace openspace::documentation {

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
            Documentation doc = { .entries = tv->documentations };
            nlohmann::json restrictions = generateJsonDocumentation(doc);
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

nlohmann::json createPropertyJson(openspace::properties::PropertyOwner* owner) {
    ZoneScoped;

    using namespace openspace;
    nlohmann::json json;
    json["name"] = !owner->guiName().empty() ? owner->guiName() : owner->identifier();

    json["description"] = owner->description();
    json["properties"] = nlohmann::json::array();
    json["propertyOwners"] = nlohmann::json::array();
    json["type"] = owner->type();
    json["tags"] = owner->tags();

    const std::vector<properties::Property*>& properties = owner->properties();
    for (properties::Property* p : properties) {
        nlohmann::json propertyJson;
        std::string name = !p->guiName().empty() ? p->guiName() : p->identifier();
        propertyJson["name"] = name;
        propertyJson["type"] = p->className();
        propertyJson["uri"] = p->fullyQualifiedIdentifier();
        propertyJson["identifier"] = p->identifier();
        propertyJson["description"] = p->description();

        json["properties"].push_back(propertyJson);
    }
    sortJson(json["properties"], "name");

    auto propertyOwners = owner->propertySubOwners();
    for (properties::PropertyOwner* o : propertyOwners) {
        nlohmann::json propertyOwner;
        json["propertyOwners"].push_back(createPropertyJson(o));
    }
    sortJson(json["propertyOwners"], "name");

    return json;
}

nlohmann::json LuaFunctionToJson(const openspace::scripting::LuaLibrary::Function& f,
    bool includeSourceLocation)
{
    using namespace openspace;
    using namespace openspace::scripting;
    nlohmann::json function;
    function["name"] = f.name;
    nlohmann::json arguments = nlohmann::json::array();

    for (const LuaLibrary::Function::Argument& arg : f.arguments) {
        nlohmann::json argument;
        argument["name"] = arg.name;
        argument["type"] = arg.type;
        argument["defaultValue"] = arg.defaultValue.value_or("");
        arguments.push_back(argument);
    }

    function["arguments"] = arguments;
    function["returnType"] = f.returnType;
    function["help"] = f.helpText;

    if (includeSourceLocation) {
        nlohmann::json sourceLocation;
        sourceLocation["file"] = f.sourceLocation.file;
        sourceLocation["line"] = f.sourceLocation.line;
        function["sourceLocation"] = sourceLocation;
    }

    return function;
}

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

nlohmann::json DocumentationEngine::generateScriptEngineJson() const {
    ZoneScoped;

    using namespace openspace;
    using namespace scripting;
    const std::vector<LuaLibrary> libraries = global::scriptEngine->allLuaLibraries();
    nlohmann::json json;

    for (const LuaLibrary& l : libraries) {

        nlohmann::json library;
        std::string libraryName = l.name;
        // Keep the library key for backwards compatability
        library["library"] = libraryName;
        library["name"] = libraryName;
        std::string os = "openspace";
        library["fullName"] = libraryName.empty() ? os : os + "." + libraryName;

        for (const LuaLibrary::Function& f : l.functions) {
            bool hasSourceLocation = true;
            library["functions"].push_back(LuaFunctionToJson(f, hasSourceLocation));
        }

        for (const LuaLibrary::Function& f : l.documentations) {
            bool hasSourceLocation = false;
            library["functions"].push_back(LuaFunctionToJson(f, hasSourceLocation));
        }
        sortJson(library["functions"], "name");
        json.push_back(library);

        sortJson(json, "library");
    }
    return json;
}

nlohmann::json DocumentationEngine::generateFactoryManagerJson() const {                 
    nlohmann::json json;

    std::vector<Documentation> docs = _documentations; // Copy the documentations
    const std::vector<FactoryManager::FactoryInfo>& factories =
        FactoryManager::ref().factories();

    for (const FactoryManager::FactoryInfo& factoryInfo : factories) {
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
            nlohmann::json documentation = generateJsonDocumentation(*factoryDoc);
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
                nlohmann::json documentation = generateJsonDocumentation(*found);
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

nlohmann::json DocumentationEngine::generateKeybindingsJson() const {
    ZoneScoped;

    nlohmann::json json;
    const std::multimap<KeyWithModifier, std::string>& luaKeys =
        global::keybindingManager->keyBindings();

    for (const std::pair<const KeyWithModifier, std::string>& p : luaKeys) {
        nlohmann::json keybind;
        keybind["name"] = ghoul::to_string(p.first);
        keybind["action"] = p.second;
        json.push_back(std::move(keybind));
    }
    sortJson(json, "name");

    nlohmann::json result;
    result["name"] = "Keybindings";
    result["keybindings"] = json;
    return result;
}

nlohmann::json DocumentationEngine::generatePropertyOwnerJson(
    properties::PropertyOwner* owner) const {
    ZoneScoped;

    nlohmann::json json;
    std::vector<properties::PropertyOwner*> subOwners = owner->propertySubOwners();
    for (properties::PropertyOwner* o : subOwners) {
        if (o->identifier() != "Scene") {
            nlohmann::json jsonOwner = createPropertyJson(o);

            json.push_back(jsonOwner);
        }
    }
    sortJson(json, "name");

    nlohmann::json result;
    result["name"] = "propertyOwner";
    result["data"] = json;

    return result;
}

void DocumentationEngine::writeDocumentation() const {
    ZoneScoped;

    // Write documentation to json files if config file supplies path for doc files
    std::string path = global::configuration->documentation.path;
    if (path.empty()) {
        // if path was empty, that means that no documentation is requested
        return;
    }
    path = absPath(path).string() + '/';

    // Start the async requests as soon as possible so they are finished when we need them
    std::future<nlohmann::json> settings = std::async(
        &DocumentationEngine::generatePropertyOwnerJson,
        this,
        global::rootPropertyOwner
    );

    std::future<nlohmann::json> sceneJson = std::async(
        &DocumentationEngine::generatePropertyOwnerJson,
        this,
        global::renderEngine->scene()
    );

    SceneLicenseWriter writer;

    nlohmann::json scripting = generateScriptEngineJson();
    nlohmann::json factory = generateFactoryManagerJson();
    nlohmann::json keybindings = generateKeybindingsJson();
    nlohmann::json license = writer.generateJsonGroupedByLicense();
    nlohmann::json sceneProperties = settings.get();
    nlohmann::json sceneGraph = sceneJson.get();
    nlohmann::json actions = global::actionManager->generateJson();
    nlohmann::json events = global::eventEngine->generateJson();

    sceneProperties["name"] = "Settings";
    sceneGraph["name"] = "Scene";

    // Add this here so that the generateJson function is the same as before to ensure
    // backwards compatibility
    nlohmann::json scriptingResult;
    scriptingResult["name"] = "Scripting API";
    scriptingResult["data"] = scripting;

    nlohmann::json documentation = {
        sceneGraph, sceneProperties, actions, events, keybindings, license,
        scriptingResult, factory
    };

    nlohmann::json result;
    result["documentation"] = documentation;

    std::ofstream out(absPath("${DOCUMENTATION}/documentationData.js"));
    out << "var data = " << result.dump();
    out.close();
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
