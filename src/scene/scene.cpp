/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/scene/scene.h>

#include <openspace/openspace.h>
#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scripting/script_helper.h>
#include <openspace/util/time.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/exception.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/onscopeexit.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

#include <chrono>
#include <iostream>
#include <iterator>
#include <numeric>
#include <fstream>
#include <string>

#include "scene_doc.inl"
#include "scene_lua.inl"

namespace {
    const char* _loggerCat = "Scene";
    const char* _moduleExtension = ".mod";
    const char* _commonModuleToken = "${COMMON_MODULE}";

    const char* KeyCamera = "Camera";
    const char* KeyFocusObject = "Focus";
    const char* KeyPositionObject = "Position";
    const char* KeyViewOffset = "Offset";

    const char* MainTemplateFilename = "${OPENSPACE_DATA}/web/properties/main.hbs";
    const char* PropertyOwnerTemplateFilename = "${OPENSPACE_DATA}/web/properties/propertyowner.hbs";
    const char* PropertyTemplateFilename = "${OPENSPACE_DATA}/web/properties/property.hbs";
    const char* HandlebarsFilename = "${OPENSPACE_DATA}/web/common/handlebars-v4.0.5.js";
    const char* JsFilename = "${OPENSPACE_DATA}/web/properties/script.js";
    const char* BootstrapFilename = "${OPENSPACE_DATA}/web/common/bootstrap.min.css";
    const char* CssFilename = "${OPENSPACE_DATA}/web/common/style.css";
} // namespace

namespace openspace {

Scene::Scene()
    : _focus(SceneGraphNode::RootNodeName)
{}

Scene::~Scene() {
    deinitialize();
}

bool Scene::initialize() {
    LDEBUG("Initializing SceneGraph");   
    return true;
}

bool Scene::deinitialize() {
    clearSceneGraph();
    return true;
}

void Scene::update(const UpdateData& data) {
    if (!_sceneGraphToLoad.empty()) {
        OsEng.renderEngine().scene()->clearSceneGraph();
        try {          
            loadSceneInternal(_sceneGraphToLoad);

            // Reset the InteractionManager to Orbital/default mode
            // TODO: Decide if it belongs in the scene and/or how it gets reloaded
            //OsEng.interactionHandler().setInteractionMode("Orbital");

            // After loading the scene, the keyboard bindings have been set
            const std::string KeyboardShortcutsType =
                ConfigurationManager::KeyKeyboardShortcuts + "." +
                ConfigurationManager::PartType;

            const std::string KeyboardShortcutsFile =
                ConfigurationManager::KeyKeyboardShortcuts + "." +
                ConfigurationManager::PartFile;


            std::string type;
            std::string file;
            bool hasType = OsEng.configurationManager().getValue(
                KeyboardShortcutsType, type
            );
            
            bool hasFile = OsEng.configurationManager().getValue(
                KeyboardShortcutsFile, file
            );
            
            if (hasType && hasFile) {
                OsEng.interactionHandler().writeKeyboardDocumentation(type, file);
            }

            LINFO("Loaded " << _sceneGraphToLoad);
            _sceneGraphToLoad = "";
        }
        catch (const ghoul::RuntimeError& e) {
            LERROR(e.what());
            _sceneGraphToLoad = "";
            return;
        }
    }

    for (SceneGraphNode* node : _graph.nodes()) {
        try {
            LTRACE("Scene::update(begin '" + node->name() + "')");
            node->update(data);
            LTRACE("Scene::update(end '" + node->name() + "')");
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.what());
        }
    }
}

void Scene::evaluate(Camera* camera) {
    for (SceneGraphNode* node : _graph.nodes())
        node->evaluate(camera);
    //_root->evaluate(camera);
}

void Scene::render(const RenderData& data, RendererTasks& tasks) {
    for (SceneGraphNode* node : _graph.nodes()) {
        LTRACE("Scene::render(begin '" + node->name() + "')");
        node->render(data, tasks);
        LTRACE("Scene::render(end '" + node->name() + "')");
    }
}

void Scene::scheduleLoadSceneFile(const std::string& sceneDescriptionFilePath) {
    _sceneGraphToLoad = sceneDescriptionFilePath;
}

void Scene::clearSceneGraph() {
    LINFO("Clearing current scene graph");
    // deallocate the scene graph. Recursive deallocation will occur
    _graph.clear();
    //if (_root) {
    //    _root->deinitialize();
    //    delete _root;
    //    _root = nullptr;
    //}

 //   _nodes.erase(_nodes.begin(), _nodes.end());
 //   _allNodes.erase(_allNodes.begin(), _allNodes.end());

    _focus.clear();
}

bool Scene::loadSceneInternal(const std::string& sceneDescriptionFilePath) {
    ghoul::Dictionary dictionary;
    
    OsEng.windowWrapper().setSynchronization(false);
    OnExit(
        [](){ OsEng.windowWrapper().setSynchronization(true); }
    );
    
    lua_State* state = ghoul::lua::createNewLuaState();
    OnExit(
           // Delete the Lua state at the end of the scope, no matter what
           [state](){ ghoul::lua::destroyLuaState(state); }
           );
    
    OsEng.scriptEngine().initializeLuaState(state);

    ghoul::lua::loadDictionaryFromFile(
        sceneDescriptionFilePath,
        dictionary,
        state
    );

    // Perform testing against the documentation/specification
    openspace::documentation::testSpecificationAndThrow(
        Scene::Documentation(),
        dictionary,
        "Scene"
    );


    _graph.loadFromFile(sceneDescriptionFilePath);

    // Initialize all nodes
    for (SceneGraphNode* node : _graph.nodes()) {
        try {
            bool success = node->initialize();
            if (success)
                LDEBUG(node->name() << " initialized successfully!");
            else
                LWARNING(node->name() << " not initialized.");
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(std::string(_loggerCat) + "(" + e.component + ")", e.what());
        }
    }

    // update the position of all nodes
    // TODO need to check this; unnecessary? (ab)
    for (SceneGraphNode* node : _graph.nodes()) {
        try {
            node->update({
                glm::dvec3(0),
                glm::dmat3(1),
                1,
                Time::ref().j2000Seconds() });
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);
        }
    }

    for (auto it = _graph.nodes().rbegin(); it != _graph.nodes().rend(); ++it)
        (*it)->calculateBoundingSphere();

    // Read the camera dictionary and set the camera state
    ghoul::Dictionary cameraDictionary;
    if (dictionary.getValue(KeyCamera, cameraDictionary)) {
        OsEng.interactionHandler().setCameraStateFromDictionary(cameraDictionary);
    }

    // If a PropertyDocumentationFile was specified, generate it now
    const std::string KeyPropertyDocumentationType =
        ConfigurationManager::KeyPropertyDocumentation + '.' +
        ConfigurationManager::PartType;

    const std::string KeyPropertyDocumentationFile =
        ConfigurationManager::KeyPropertyDocumentation + '.' +
        ConfigurationManager::PartFile;

    const bool hasType = OsEng.configurationManager().hasKey(KeyPropertyDocumentationType);
    const bool hasFile = OsEng.configurationManager().hasKey(KeyPropertyDocumentationFile);
    if (hasType && hasFile) {
        std::string propertyDocumentationType;
        OsEng.configurationManager().getValue(KeyPropertyDocumentationType, propertyDocumentationType);
        std::string propertyDocumentationFile;
        OsEng.configurationManager().getValue(KeyPropertyDocumentationFile, propertyDocumentationFile);

        propertyDocumentationFile = absPath(propertyDocumentationFile);
        writePropertyDocumentation(propertyDocumentationFile, propertyDocumentationType, sceneDescriptionFilePath);
    }


    OsEng.runPostInitializationScripts(sceneDescriptionFilePath);

    OsEng.enableBarrier();

    return true;
}

//void Scene::loadModules(
//    const std::string& directory, 
//    const ghoul::Dictionary& dictionary) 
//{
//    // Struct containing dependencies and nodes
//    LoadMaps m;
//
//    // Get the common directory
//    std::string commonDirectory(_defaultCommonDirectory);
//    dictionary.getValue(constants::scenegraph::keyCommonFolder, commonDirectory);
//    FileSys.registerPathToken(_commonModuleToken, commonDirectory);
//
//    lua_State* state = ghoul::lua::createNewLuaState();
//    OsEng.scriptEngine()->initializeLuaState(state);
//
//    LDEBUG("Loading common module folder '" << commonDirectory << "'");
//    // Load common modules into LoadMaps struct
//    loadModule(m, FileSys.pathByAppendingComponent(directory, commonDirectory), state);
//
//    // Load the rest of the modules into LoadMaps struct
//    ghoul::Dictionary moduleDictionary;
//    if (dictionary.getValue(constants::scenegraph::keyModules, moduleDictionary)) {
//        std::vector<std::string> keys = moduleDictionary.keys();
//        std::sort(keys.begin(), keys.end());
//        for (const std::string& key : keys) {
//            std::string moduleFolder;
//            if (moduleDictionary.getValue(key, moduleFolder)) {
//                loadModule(m, FileSys.pathByAppendingComponent(directory, moduleFolder), state);
//            }
//        }
//    }
//
//    // Load and construct scenegraphnodes from LoadMaps struct
//    loadNodes(SceneGraphNode::RootNodeName, m);
//
//    // Remove loaded nodes from dependency list
//    for(const auto& name: m.loadedNodes) {
//        m.dependencies.erase(name);
//    }
//
//    // Check to see what dependencies are not resolved.
//    for(auto& node: m.dependencies) {
//        LWARNING(
//            "'" << node.second << "'' not loaded, parent '" 
//            << node.first << "' not defined!");
//    }
//}

//void Scene::loadModule(LoadMaps& m,const std::string& modulePath, lua_State* state) {
//    auto pos = modulePath.find_last_of(ghoul::filesystem::FileSystem::PathSeparator);
//    if (pos == modulePath.npos) {
//        LERROR("Bad format for module path: " << modulePath);
//        return;
//    }
//
//    std::string fullModule = modulePath + modulePath.substr(pos) + _moduleExtension;
//    LDEBUG("Loading nodes from: " << fullModule);
//
//    ghoul::filesystem::Directory oldDirectory = FileSys.currentDirectory();
//    FileSys.setCurrentDirectory(modulePath);
//
//    ghoul::Dictionary moduleDictionary;
//    ghoul::lua::loadDictionaryFromFile(fullModule, moduleDictionary, state);
//    std::vector<std::string> keys = moduleDictionary.keys();
//    for (const std::string& key : keys) {
//        if (!moduleDictionary.hasValue<ghoul::Dictionary>(key)) {
//            LERROR("SceneGraphElement '" << key << "' is not a table in module '"
//                                         << fullModule << "'");
//            continue;
//        }
//        
//        ghoul::Dictionary element;
//        std::string nodeName;
//        std::string parentName;
//
//        moduleDictionary.getValue(key, element);
//        element.setValue(constants::scenegraph::keyPathModule, modulePath);
//
//        element.getValue(constants::scenegraphnode::keyName, nodeName);
//        element.getValue(constants::scenegraphnode::keyParentName, parentName);
//
//        m.nodes[nodeName] = element;
//        m.dependencies.emplace(parentName,nodeName);
//    }
//
//    FileSys.setCurrentDirectory(oldDirectory);
//}

//void Scene::loadNodes(const std::string& parentName, LoadMaps& m) {
//    auto eqRange = m.dependencies.equal_range(parentName);
//    for (auto it = eqRange.first; it != eqRange.second; ++it) {
//        auto node = m.nodes.find((*it).second);
//        loadNode(node->second);
//        loadNodes((*it).second, m);
//    }
//    m.loadedNodes.emplace_back(parentName);
//}
//
//void Scene::loadNode(const ghoul::Dictionary& dictionary) {
//    SceneGraphNode* node = SceneGraphNode::createFromDictionary(dictionary);
//    if(node) {
//        _allNodes.emplace(node->name(), node);
//        _nodes.push_back(node);
//    }
//}

//void SceneGraph::loadModule(const std::string& modulePath) {
//    auto pos = modulePath.find_last_of(ghoul::filesystem::FileSystem::PathSeparator);
//    if (pos == modulePath.npos) {
//        LERROR("Bad format for module path: " << modulePath);
//        return;
//    }
//
//    std::string fullModule = modulePath + modulePath.substr(pos) + _moduleExtension;
//    LDEBUG("Loading modules from: " << fullModule);
//
//    ghoul::filesystem::Directory oldDirectory = FileSys.currentDirectory();
//    FileSys.setCurrentDirectory(modulePath);
//
//    ghoul::Dictionary moduleDictionary;
//    ghoul::lua::loadDictionaryFromFile(fullModule, moduleDictionary);
//    std::vector<std::string> keys = moduleDictionary.keys();
//    for (const std::string& key : keys) {
//        if (!moduleDictionary.hasValue<ghoul::Dictionary>(key)) {
//            LERROR("SceneGraphElement '" << key << "' is not a table in module '"
//                                         << fullModule << "'");
//            continue;
//        }
//        
//        ghoul::Dictionary element;
//        moduleDictionary.getValue(key, element);
//
//        element.setValue(constants::scenegraph::keyPathModule, modulePath);
//
//        //each element in this new dictionary becomes a scenegraph node. 
//        SceneGraphNode* node = SceneGraphNode::createFromDictionary(element);
//
//        _allNodes.emplace(node->name(), node);
//        _nodes.push_back(node);
//    }
//
//    FileSys.setCurrentDirectory(oldDirectory);
//
//    // Print the tree
//    //printTree(_root);
//}

SceneGraphNode* Scene::root() const {
    return _graph.rootNode();
}
    
SceneGraphNode* Scene::sceneGraphNode(const std::string& name) const {
    return _graph.sceneGraphNode(name);
}

std::vector<SceneGraphNode*> Scene::allSceneGraphNodes() const {
    return _graph.nodes();
}

SceneGraph& Scene::sceneGraph() {
    return _graph;
}

void Scene::writePropertyDocumentation(const std::string& filename, const std::string& type, const std::string& sceneFilename) {
    LDEBUG("Writing documentation for properties");
    if (type == "text") {
        std::ofstream file;
        file.exceptions(~std::ofstream::goodbit);
        file.open(filename);

        using properties::Property;
        for (SceneGraphNode* node : _graph.nodes()) {
            std::vector<Property*> properties = node->propertiesRecursive();
            if (!properties.empty()) {
                file << node->name() << std::endl;

                for (Property* p : properties) {
                    file << p->fullyQualifiedIdentifier() << ":   " <<
                        p->guiName() << std::endl;
                }

                file << std::endl;
            }
        }
    }
    else if (type == "html") {
        std::ofstream file;
        file.exceptions(~std::ofstream::goodbit);
        file.open(filename);


        std::ifstream handlebarsInput(absPath(HandlebarsFilename));
        std::ifstream jsInput(absPath(JsFilename));

        std::string jsContent;
        std::back_insert_iterator<std::string> jsInserter(jsContent);

        std::copy(std::istreambuf_iterator<char>{handlebarsInput}, std::istreambuf_iterator<char>(), jsInserter);
        std::copy(std::istreambuf_iterator<char>{jsInput}, std::istreambuf_iterator<char>(), jsInserter);

        std::ifstream bootstrapInput(absPath(BootstrapFilename));
        std::ifstream cssInput(absPath(CssFilename));

        std::string cssContent;
        std::back_insert_iterator<std::string> cssInserter(cssContent);

        std::copy(std::istreambuf_iterator<char>{bootstrapInput}, std::istreambuf_iterator<char>(), cssInserter);
        std::copy(std::istreambuf_iterator<char>{cssInput}, std::istreambuf_iterator<char>(), cssInserter);

        std::ifstream mainTemplateInput(absPath(MainTemplateFilename));
        std::string mainTemplateContent{ std::istreambuf_iterator<char>{mainTemplateInput},
            std::istreambuf_iterator<char>{} };

        std::ifstream propertyOwnerTemplateInput(absPath(PropertyOwnerTemplateFilename));
        std::string propertyOwnerTemplateContent{ std::istreambuf_iterator<char>{propertyOwnerTemplateInput},
            std::istreambuf_iterator<char>{} };

        std::ifstream propertyTemplateInput(absPath(PropertyTemplateFilename));
        std::string propertyTemplateContent{ std::istreambuf_iterator<char>{propertyTemplateInput},
            std::istreambuf_iterator<char>{} };

        // Create JSON
        std::function<std::string(properties::PropertyOwner*)> createJson =
            [&createJson](properties::PropertyOwner* owner) -> std::string 
        {
            std::stringstream json;
            json << "{";
            json << "\"name\": \"" << owner->name() << "\",";

            json << "\"properties\": [";
            auto properties = owner->properties();
            for (properties::Property* p : properties) {
                json << "{";
                json << "\"id\": \"" << p->identifier() << "\",";
                json << "\"type\": \"" << p->className() << "\",";
                json << "\"fullyQualifiedId\": \"" << p->fullyQualifiedIdentifier() << "\",";
                json << "\"guiName\": \"" << p->guiName() << "\"";
                json << "}";
                if (p != properties.back()) {
                    json << ",";
                }
            }
            json << "],";

            json << "\"propertyOwners\": [";
            auto propertyOwners = owner->propertySubOwners();
            for (properties::PropertyOwner* o : propertyOwners) {
                json << createJson(o);
                if (o != propertyOwners.back()) {
                    json << ",";
                }
            }
            json << "]";
            json << "}";

            return json.str();
        };


        std::stringstream json;
        json << "[";
        std::vector<SceneGraphNode*> nodes = _graph.nodes();
        if (!nodes.empty()) {
            json << std::accumulate(
                std::next(nodes.begin()),
                nodes.end(),
                createJson(*nodes.begin()),
                [createJson](std::string a, SceneGraphNode* n) {
                    return a + "," + createJson(n);
                }
            );
        }

        json << "]";

        std::string jsonString = "";
        for (const char& c : json.str()) {
            if (c == '\'') {
                jsonString += "\\'";
            } else {
                jsonString += c;
            }
        }

        std::stringstream html;
        html << "<!DOCTYPE html>\n"
            << "<html>\n"
            << "\t<head>\n"
            << "\t\t<script id=\"mainTemplate\" type=\"text/x-handlebars-template\">\n"
            << mainTemplateContent << "\n"
            << "\t\t</script>\n"
            << "\t\t<script id=\"propertyOwnerTemplate\" type=\"text/x-handlebars-template\">\n"
            << propertyOwnerTemplateContent << "\n"
            << "\t\t</script>\n"
            << "\t\t<script id=\"propertyTemplate\" type=\"text/x-handlebars-template\">\n"
            << propertyTemplateContent << "\n"
            << "\t\t</script>\n"
            << "\t<script>\n"
            << "var propertyOwners = JSON.parse('" << jsonString << "');\n"
            << "var version = [" << OPENSPACE_VERSION_MAJOR << ", " << OPENSPACE_VERSION_MINOR << ", " << OPENSPACE_VERSION_PATCH << "];\n"
            << "var sceneFilename = '" << sceneFilename << "';\n"
            << "var generationTime = '" << Time::now().ISO8601() << "';\n"
            << jsContent << "\n"
            << "\t</script>\n"
            << "\t<style type=\"text/css\">\n"
            << cssContent << "\n"
            << "\t</style>\n"
            << "\t\t<title>Documentation</title>\n"
            << "\t</head>\n"
            << "\t<body>\n"
            << "\t<body>\n"
            << "</html>\n";

        /*

        html << "<html>\n"
             << "\t<head>\n"
             << "\t\t<title>Properties</title>\n"
             << "\t</head>\n"
             << "<body>\n"
             << "<table cellpadding=3 cellspacing=0 border=1>\n"
             << "\t<caption>Properties</caption>\n\n"
             << "\t<thead>\n"
             << "\t\t<tr>\n"
             << "\t\t\t<th>ID</th>\n"
             << "\t\t\t<th>Type</th>\n"
             << "\t\t\t<th>Description</th>\n"
             << "\t\t</tr>\n"
             << "\t</thead>\n"
             << "\t<tbody>\n";

        for (SceneGraphNode* node : _graph.nodes()) {
            for (properties::Property* p : node->propertiesRecursive()) {
                html << "\t\t<tr>\n"
                     << "\t\t\t<td>" << p->fullyQualifiedIdentifier() << "</td>\n"
                     << "\t\t\t<td>" << p->className() << "</td>\n"
                     << "\t\t\t<td>" << p->guiName() << "</td>\n"
                     << "\t\t</tr>\n";
            }

            if (!node->propertiesRecursive().empty()) {
                html << "\t<tr><td style=\"line-height: 10px;\" colspan=3></td></tr>\n";
            }

        }

        html << "\t</tbody>\n"
             << "</table>\n"
             << "</html>;";

        */
        file << html.str();
    }
    else
        LERROR("Undefined type '" << type << "' for Property documentation");
}

scripting::LuaLibrary Scene::luaLibrary() {
    return {
        "",
        {
            {
                "setPropertyValue",
                &luascriptfunctions::property_setValue,
                "string, *",
                "Sets all properties identified by the URI (with potential wildcards) in "
                "the first argument. The second argument can be any type, but it has to "
                "match the type that the property (or properties) expect."
            },
            {
                "setPropertyValueRegex",
                &luascriptfunctions::property_setValueRegex,
                "string, *",
                "Sets all properties that pass the regular expression in the first "
                "argument. The second argument can be any type, but it has to match the "
                "type of the properties that matched the regular expression. The regular "
                "expression has to be of the ECMAScript grammar."
            },
            {
                "setPropertyValueSingle",
                &luascriptfunctions::property_setValueSingle,
                "string, *",
                "Sets a property identified by the URI in "
                "the first argument. The second argument can be any type, but it has to "
                "match the type that the property expects.",
            },
			{
                "setPropertyGroup",
				&luascriptfunctions::property_setGroup,
				"string, string, *",
				"Sets all properties that belong to a tagged group AND match the "
				"URI (with optional wildcards) in the first argument. Second argument is "
				"the tag name to match. The third argument can be any type, but it has to "
				"match tye type that the property expects.",
			},
			{
                "setPropertyGroupSingle",
				&luascriptfunctions::property_setGroupSingle,
				"string, string, *",
				"Sets all properties that belong to a tagged group AND match the "
				"URI (requires exact match) in the first argument. Second argument is "
				"the tag name to match. The third argument can be any type, but it has to "
				"match tye type that the property expects.",
			},
			{
                "setPropertyGroupRegex",
				&luascriptfunctions::property_setGroupRegex,
				"string, string, *",
				"Sets all properties that belong to a tagged group AND match the "
				"URI regex in the first argument. Second argument is the tag name to"
				"match. The third argument can be any type, but it has to match the "
				"type that the property expects.",
			},
            {
                "getPropertyValue",
                &luascriptfunctions::property_getValue,
                "string",
                "Returns the value the property, identified by "
                "the provided URI."
            },
            {
                "loadScene",
                &luascriptfunctions::loadScene,
                "string",
                "Loads the scene found at the file passed as an "
                "argument. If a scene is already loaded, it is unloaded first"
            },
            {
                "addSceneGraphNode",
                &luascriptfunctions::addSceneGraphNode,
                "table",
                "Loads the SceneGraphNode described in the table and adds it to the "
                "SceneGraph"
            },
            {
                "removeSceneGraphNode",
                &luascriptfunctions::removeSceneGraphNode,
                "string",
                "Removes the SceneGraphNode identified by name"
            }
        }
    };
}

}  // namespace openspace
