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
 * \ingroup LuaScripts
 * setOrigin():
 * Set the origin of the camera
 */
int setOrigin(lua_State* L) {
	using ghoul::lua::luaTypeToString;
	const std::string _loggerCat = "lua.setOrigin";

	int nArguments = lua_gettop(L);
	if (nArguments != 1)
		return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

	const int type = lua_type(L, -1);
	if (type != LUA_TSTRING)
		return luaL_error(L, "Expected string, got %i", type);

	std::string s = luaL_checkstring(L, -1);

	SceneGraphNode* node = sceneGraphNode(s);
	if (!node) {
		LWARNING("Could not find a node in scenegraph called '" << s <<"'");
		return 0;
	}

	OsEng.interactionHandler().setFocusNode(node);

	return 0;
}

/**
* \ingroup LuaScripts
* bindKey():
* Binds a key to Lua command
*/
int bindKey(lua_State* L) {
	using ghoul::lua::luaTypeToString;
	const std::string _loggerCat = "lua.bindKey";

	int nArguments = lua_gettop(L);
	if (nArguments != 2)
		return luaL_error(L, "Expected %i arguments, got %i", 2, nArguments);


	std::string key = luaL_checkstring(L, -2);
	std::string command = luaL_checkstring(L, -1);

	if (command.empty())
		return luaL_error(L, "Command string is empty");

    openspace::Key iKey = stringToKey(key);

    if (iKey == openspace::Key::Unknown) {
		LERROR("Could not find key '"<< key <<"'");
		return 0;
	}


	OsEng.interactionHandler().bindKey(iKey, command);

	return 0;
}

/**
* \ingroup LuaScripts
* clearKeys():
* Clears all key bindings
*/
int clearKeys(lua_State* L) {
	using ghoul::lua::luaTypeToString;
	const std::string _loggerCat = "lua.clearKeys";

	int nArguments = lua_gettop(L);
	if (nArguments != 0)
		return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);

	OsEng.interactionHandler().resetKeyBindings();

	return 0;
}

/**
* \ingroup LuaScripts
* dt(bool):
* Get current frame time
*/
int dt(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 0)
		return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);

	lua_pushnumber(L,OsEng.interactionHandler().deltaTime());
	return 1;
}

/**
* \ingroup LuaScripts
* distance(double, double):
* Change distance to origin
*/
int distance(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 2)
		return luaL_error(L, "Expected %i arguments, got %i", 2, nArguments);

	double d1 = luaL_checknumber(L, -2);
	double d2 = luaL_checknumber(L, -1);
	PowerScaledScalar dist(static_cast<float>(d1), static_cast<float>(d2));
	OsEng.interactionHandler().distanceDelta(dist);
	return 0;
}

/**
 * \ingroup LuaScripts
 * setInteractionSensitivity(double):
 * Changes the global interaction sensitivity to the passed value
 */
int setInteractionSensitivity(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 1)
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

    float sensitivity = static_cast<float>(luaL_checknumber(L, -1));
    OsEng.interactionHandler().setInteractionSensitivity(sensitivity);
    return 0;
}

/**
 * \ingroup LuaScripts
 * interactionSensitivity():
 * Returns the current, global interaction sensitivity
 */
int interactionSensitivity(lua_State* L) {
    float sensitivity = OsEng.interactionHandler().interactionSensitivity();
    lua_pushnumber(L, sensitivity);
    return 1;
}

/**
 * \ingroup LuaScripts
 * setInvertRoll(bool):
 * Determines if the roll movement is inverted
 */
int setInvertRoll(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 1)
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

    bool invert = lua_toboolean(L, -1) == 1;
    OsEng.interactionHandler().setInvertRoll(invert);
    return 0;
}

/**
 * \ingroup LuaScripts
 * invertRoll():
 * Returns the current setting for inversion of roll movement
 */
int invertRoll(lua_State* L) {
    bool invert = OsEng.interactionHandler().invertRoll();
    lua_pushboolean(L, invert);
    return 1;
}

/**
 * \ingroup LuaScripts
 * setInvertRotation(bool):
 * Determines if the rotation movement is inverted
 */
int setInvertRotation(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 1)
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

    bool invert = lua_toboolean(L, -1) == 1;
    OsEng.interactionHandler().setInvertRotation(invert);
    return 0;
}

/**
 * \ingroup LuaScripts
 * invertRotation():
 * Returns the current setting for inversion of rotation movement
 */
int invertRotation(lua_State* L) {
    bool invert = OsEng.interactionHandler().invertRotation();
    lua_pushboolean(L, invert);
    return 1;
}

} // namespace luascriptfunctions

} // namespace openspace
