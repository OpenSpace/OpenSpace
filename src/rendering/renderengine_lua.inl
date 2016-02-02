/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

/**
int changeCoordinateSystem(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 1)
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

    std::string newCenter = std::string(lua_tostring(L, -1));
    OsEng.renderEngine()->changeViewPoint(newCenter);
    return 1;
}
*/

/**
	* \ingroup LuaScripts
	* takeScreenshot():
	* Save the rendering to an image file
	*/
int takeScreenshot(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 0)
		return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);
	OsEng.renderEngine()->takeScreenshot();
	return 0;
}

/**
* \ingroup LuaScripts
* setRenderer(string):
* Set renderer
*/
int setRenderer(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 1)
		return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

	const int type = lua_type(L, -1);
    if (type != LUA_TSTRING)
        return luaL_error(L, "Expected argument of type 'bool'");
    std::string r = lua_tostring(L, -1);
    OsEng.renderEngine()->setRendererFromString(r);
	return 0;
}

/**
* \ingroup LuaScripts
* visualizeABuffer(bool):
* Toggle the visualization of the ABuffer
*/
int showRenderInformation(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 1)
		return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

	const int type = lua_type(L, -1);
    if (type != LUA_TBOOLEAN)
        return luaL_error(L, "Expected argument of type 'bool'");
	bool b = lua_toboolean(L, -1) != 0;
	OsEng.renderEngine()->toggleInfoText(b);
	return 0;
}

/**
* \ingroup LuaScripts
* visualizeABuffer(bool):
* Toggle the visualization of the ABuffer
*/
int setPerformanceMeasurement(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 1)
		return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

	bool b = lua_toboolean(L, -1) != 0;
	OsEng.renderEngine()->setPerformanceMeasurements(b);
	return 0;
}

/**
* \ingroup LuaScripts
* fadeIn(float):
* start a global fadein over (float) seconds
*/
int fadeIn(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 1)
		return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

	double t = luaL_checknumber(L, -1);
			
	OsEng.renderEngine()->startFading(1, static_cast<float>(t));
	return 0;
}
/**
* \ingroup LuaScripts
* fadeIn(float):
* start a global fadeout over (float) seconds
*/
int fadeOut(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 1)
		return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

	double t = luaL_checknumber(L, -1);

	OsEng.renderEngine()->startFading(-1, static_cast<float>(t));
	return 0;
}

} // namespace luascriptfunctions

}// namespace openspace
