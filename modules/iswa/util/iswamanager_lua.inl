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
	int id = lua_tonumber(L, 1);
    std::string name = luaL_checkstring(L, 2);

    std::cout << id << " " << name << std::endl;

	IswaManager::ref().createScreenSpace(id,name);
}

int iswa_removeCygnet(lua_State* L){
    std::string s = luaL_checkstring(L, -1);
    IswaManager::ref().deleteIswaCygnet(s);
    return 0;
}

int iswa_removeScrenSpaceCygnet(lua_State* L){
    std::string s = luaL_checkstring(L, -1);
    IswaManager::ref().deleteScreenSpaceCygnet(s);
    return 0;
}

int iswa_removeGroup(lua_State* L){
	int id = lua_tonumber(L, 1);
	IswaManager::ref().unregisterGroup(id);
	return 0;
}


}// namespace luascriptfunctions

}// namespace openspace