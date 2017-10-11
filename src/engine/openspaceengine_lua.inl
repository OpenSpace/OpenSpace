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

#include <openspace/scene/scenegraphnode.h>

namespace openspace {
namespace luascriptfunctions {

/**
 * \ingroup LuaScripts
 * toggleShutdown():
 * Toggles the shutdown mode that will close the application after the countdown timer is
 * reached
 */
int toggleShutdown(lua_State* L) {
    const int nArguments = lua_gettop(L);
    if (nArguments != 0) {
        return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);
    }

    OsEng.toggleShutdownMode();

    return 0;
}

/**
* \ingroup LuaScripts
* writeDocumentation():
* Writes out documentation files
*/
int writeDocumentation(lua_State* L) {
    const int nArguments = lua_gettop(L);
    if (nArguments != 0) {
        return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);
    }

    OsEng.writeDocumentation();

    return 0;
}

/**
 * \ingroup LuaScripts
 * addVirtualProperty():
 * Adds a virtual property that will set a group of properties
 */
int addVirtualProperty(lua_State* L) {
    const int nArguments = lua_gettop(L);
    if (nArguments != 7) {
        return luaL_error(L, "Expected %i arguments, got %i", 7, nArguments);
    }

    const std::string type = lua_tostring(L, -7);
    const std::string name = lua_tostring(L, -6);
    const std::string identifier = lua_tostring(L, -5);
    const std::string description = lua_tostring(L, -4);

    std::unique_ptr<properties::Property> prop;
    if (type == "BoolProperty") {
        bool v = lua_toboolean(L, -3);
        prop = std::make_unique<properties::BoolProperty>(
            properties::Property::PropertyInfo{ identifier, name, description },
            v
        );
    }
    else if (type == "IntProperty") {
        int v = static_cast<int>(lua_tonumber(L, -3));
        int min = static_cast<int>(lua_tonumber(L, -2));
        int max = static_cast<int>(lua_tonumber(L, -1));

        prop = std::make_unique<properties::IntProperty>(
            properties::Property::PropertyInfo{ identifier, name, description },
            v,
            min,
            max
        );
    }
    else if (type == "FloatProperty") {
        float v = static_cast<float>(lua_tonumber(L, -3));
        float min = static_cast<float>(lua_tonumber(L, -2));
        float max = static_cast<float>(lua_tonumber(L, -1));

        prop = std::make_unique<properties::FloatProperty>(
            properties::Property::PropertyInfo{ identifier, name, description },
            v,
            min,
            max
        );
    }
    else if (type == "TriggerProperty") {
        prop = std::make_unique<properties::TriggerProperty>(
            properties::Property::PropertyInfo{ identifier, name, description }
        );
    }
    else {
        return luaL_error(L, "Unknown property type '%s'", type.c_str());
    }

    OsEng.virtualPropertyManager().addProperty(std::move(prop));
    return 0;
}

/**
* \ingroup LuaScripts
* removeVirtualProperty():
* Removes a previously added virtual property
*/
int removeVirtualProperty(lua_State* L) {
    const int nArguments = lua_gettop(L);
    if (nArguments != 1) {
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);
    }

    const std::string name = lua_tostring(L, -1);
    properties::Property* p = OsEng.virtualPropertyManager().property(name);
    OsEng.virtualPropertyManager().removeProperty(p);
    return 0;
}

/**
* \ingroup LuaScripts
* removeAllVirtualProperties():
* Remove all registered virtual properties
*/
int removeAllVirtualProperties(lua_State* L) {
    const int nArguments = lua_gettop(L);
    if (nArguments != 1) {
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);
    }
    
    std::vector<properties::Property*> ps = OsEng.virtualPropertyManager().properties();
    for (properties::Property* p : ps) {
        OsEng.virtualPropertyManager().removeProperty(p);
        delete p;
    }
    return 0;
}

/**
 * \ingroup LuaScripts
 * addTag()
 * Adds a Tag to a SceneGraphNode
 */
int addTag(lua_State* L) {
    const int nArguments = lua_gettop(L);
    if (nArguments != 2) {
        return luaL_error(L, "Expected %i arguments, got %i", 2, nArguments);
    }

    const std::string uri = lua_tostring(L, -2);
    const std::string tag = lua_tostring(L, -1);

    SceneGraphNode* node = OsEng.renderEngine().scene()->sceneGraphNode(uri);
    if (!node) {
        return luaL_error(L, "Unknown scene graph node type '%s'", uri.c_str());
    }

    node->addTag(tag);

    return 0;
}

/**
* \ingroup LuaScripts
* downloadFile():
* Downloads a file from Lua interpreter
*/
int downloadFile(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 2)
        return luaL_error(L, "Expected %i arguments, got %i", 2, nArguments);
    std::string uri = luaL_checkstring(L, -2);
    std::string savePath = luaL_checkstring(L, -1);

    const std::string _loggerCat = "OpenSpaceEngine";
    LINFO("Downloading file from " << uri);
    DownloadManager dm = openspace::DownloadManager("", 1, false);
    std::shared_ptr<openspace::DownloadManager::FileFuture> future =
        dm.downloadFile(uri, absPath("${SCENE}/" + savePath), true, true, 5);
    if (!future || (future && !future->isFinished)) {
        std::string errorMsg = "Download failed";
        if (future)
            errorMsg += ": " + future->errorMessage;
        return luaL_error(L, errorMsg.c_str());
    }
    return 1;
}

} // namespace luascriptfunctions
} // namespace openspace
