/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>

namespace openspace::luascriptfunctions {

int iswa_addCygnet(lua_State* L) {
    int nArguments = lua_gettop(L);

    int id = -1;
    std::string type = "Texture";
    std::string group = "";

    if (nArguments > 0) {
        id = static_cast<int>(lua_tonumber(L, 1));
    }

    if (nArguments > 1) {
        type = luaL_checkstring(L, 2);
    }

    if (nArguments > 2) {
        group = luaL_checkstring(L, 3);
    }

    IswaManager::ref().addIswaCygnet(id, type, group);

    return 0;
}

int iswa_addScreenSpaceCygnet(lua_State* L) {
    static const std::string _loggerCat = "addScreenSpaceCygnet";
    using ghoul::lua::errorLocation;

    int nArguments = lua_gettop(L);
    if (nArguments != 1) {
        return ghoul::lua::luaError(L, fmt::format(
            "Expected {} argumemts, got {}", 1, nArguments
        ));
    }

    ghoul::Dictionary d;
    try {
        ghoul::lua::luaDictionaryFromState(L, d);
    }
    catch (const ghoul::lua::LuaFormatException& e) {
        LERROR(e.what());
        return 0;
    }

    int id = static_cast<int>(d.value<double>("CygnetId"));

    auto cygnetInformation = IswaManager::ref().cygnetInformation();
    if (cygnetInformation.find(id) == cygnetInformation.end()) {
        LWARNING("Could not find Cygnet with id = " + std::to_string(id));
        return 0;
    }

    auto info = cygnetInformation[id];
    std::string name = info->name;
    int updateInterval = info->updateInterval;
    info->selected = true;

    if (global::renderEngine.screenSpaceRenderable(name)) {
        LERROR("A cygnet with the name \"" + name +"\" already exist");
        return 0;
    } else {
        d.setValue("Name", name);
        d.setValue("Type", "ScreenSpaceCygnet");
        d.setValue("UpdateInterval", static_cast<float>(updateInterval));

        std::unique_ptr<ScreenSpaceRenderable> s(
            ScreenSpaceRenderable::createFromDictionary(d)
        );
        global::renderEngine.addScreenSpaceRenderable(std::move(s));
    }
    return 0;
}

// int iswa_addKameleonPlane(lua_State* L){
//     int nArguments = lua_gettop(L);

//     std::string kwPath = "";
//     std::string type = "x";
//     std::string group = "";

//     if(nArguments > 0)
//         kwPath = luaL_checkstring(L, 1);

//     if(nArguments > 1)
//         type = luaL_checkstring(L, 2);

//     if(nArguments > 2)
//         group = luaL_checkstring(L, 3);

//     IswaManager::ref().createKameleonPlane(kwPath, type, group);
//     return 0;
// }

int iswa_removeCygnet(lua_State* L) {
    std::string name = luaL_checkstring(L, -1);
    global::scriptEngine.queueScript(
        "openspace.removeSceneGraphNode('" + name + "')",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
    // IswaManager::ref().deleteIswaCygnet(s);
    return 0;
}

int iswa_removeScrenSpaceCygnet(lua_State* L) {
    static const std::string _loggerCat = "removeScreenSpaceCygnet";

    int id = static_cast<int>(lua_tonumber(L, 1));

    auto cygnetInformation = IswaManager::ref().cygnetInformation();
    if (cygnetInformation.find(id) == cygnetInformation.end()) {
        LWARNING("Could not find Cygnet with id = " + std::to_string(id));
        return 0;
    }

    auto info = cygnetInformation[id];
    info->selected = false;

    std::string script =
        "openspace.unregisterScreenSpaceRenderable('" +
        cygnetInformation[id]->name + "');";

    global::scriptEngine.queueScript(
        script,
        scripting::ScriptEngine::RemoteScripting::Yes
    );
    return 0;
}

int iswa_removeGroup(lua_State* L) {
    std::string name = luaL_checkstring(L, -1);
    // IswaManager::ref().unregisterGroup(id);

    auto groups = IswaManager::ref().groups();
    if (groups.find(name) != groups.end()) {
        groups[name]->clearGroup();
    }

    return 0;
}

int iswa_addCdfFiles(lua_State* L) {
    std::string path = luaL_checkstring(L, 1);
    IswaManager::ref().addCdfFiles(path);

    return 0;
}

int iswa_addKameleonPlanes(lua_State* L) {
    std::string group = luaL_checkstring(L, 1);
    int pos = static_cast<int>(lua_tonumber(L, 2));
    IswaManager::ref().addKameleonCdf(group, pos);
    // auto cdfInfo =
    return 0;
}

int iswa_setBaseUrl(lua_State* L) {
    std::string url = luaL_checkstring(L, 1);
    IswaManager::ref().setBaseUrl(url);
    return 0;
}

} // namespace openspace::luascriptfunctions
