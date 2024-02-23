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
        &properties::PropertyOwner::generateJson,
        global::rootPropertyOwner
    );

    std::future<nlohmann::json> sceneJson = std::async(
        &properties::PropertyOwner::generateJson,
        global::renderEngine->scene()
    );

    SceneLicenseWriter writer;

    nlohmann::json scripting = generateScriptEngineJson();
    nlohmann::json factory = FactoryManager::ref().generateJson();
    nlohmann::json keybindings = global::keybindingManager->generateJson();
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
        sceneGraph, sceneProperties, actions, events, keybindings, license, scriptingResult, factory
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
