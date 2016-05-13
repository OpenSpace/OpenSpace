/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

namespace openspace {

namespace luascriptfunctions {

int iswa_addCygnet(lua_State* L) {
    std::string s = luaL_checkstring(L, -1);
    IswaManager::ref().addIswaCygnet(s);
    return 0;
}

int iswa_addScreenSpaceCygnet(lua_State* L){
    static const std::string _loggerCat = "addScreenSpaceCygnet";
    using ghoul::lua::errorLocation;

    int nArguments = lua_gettop(L);
    if (nArguments != 1)
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

    ghoul::Dictionary d;
    try {
        ghoul::lua::luaDictionaryFromState(L, d);
    }
    catch (const ghoul::lua::LuaFormatException& e) {
        LERROR(e.what());
        return 0;
    }

    float id;
    d.getValue("CygnetId", id);
    
    auto cygnetInformation = IswaManager::ref().cygnetInformation(); 
    if(cygnetInformation.find((int)id) == cygnetInformation.end()){
        LWARNING("Could not find Cygnet with id = " + std::to_string(id));
        return 0;
    }

    auto info = cygnetInformation[(int)id];
    std::string name = info->name;
    int updateInterval = info->updateInterval;
    info->selected = true;
 
    if(OsEng.renderEngine().screenSpaceRenderable(name)){
        LERROR("A cygnet with the name \"" + name +"\" already exist");
        return 0;
    }else{
        d.setValue("Name", name);
        d.setValue("Type", "ScreenSpaceCygnet");
        d.setValue("UpdateInterval", (float) updateInterval);

        std::shared_ptr<ScreenSpaceRenderable> s( ScreenSpaceRenderable::createFromDictionary(d) );
        OsEng.renderEngine().registerScreenSpaceRenderable(s);
    }
    return 0;
}

int iswa_removeCygnet(lua_State* L){
    std::string s = luaL_checkstring(L, -1);
    IswaManager::ref().deleteIswaCygnet(s);
    return 0;
}

int iswa_removeScrenSpaceCygnet(lua_State* L){
    static const std::string _loggerCat = "removeScreenSpaceCygnet";

    int id = lua_tonumber(L, 1);

    auto cygnetInformation = IswaManager::ref().cygnetInformation(); 
    if(cygnetInformation.find(id) == cygnetInformation.end()){
        LWARNING("Could not find Cygnet with id = " + std::to_string(id));
        return 0;
    }

    std::string script = "openspace.unregisterScreenSpaceRenderable('" + cygnetInformation[id]->name + "');";
    OsEng.scriptEngine().queueScript(script);
    return 0;
}

int iswa_removeGroup(lua_State* L){
	int id = lua_tonumber(L, 1);
	// IswaManager::ref().unregisterGroup(id);

    auto groups = IswaManager::ref().groups(); 
    if(groups.find(id) != groups.end())
        groups[id]->clearGroup();

	return 0;
}


}// namespace luascriptfunctions

}// namespace openspace