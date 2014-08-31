/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#include <openspace/scripting/scriptfunctions.h>

#include <openspace/scenegraph/scenegraphnode.h>
#include <openspace/query/query.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>

namespace openspace {
namespace scripting {
    
using namespace openspace::properties;
    
void printInternal(ghoul::logging::LogManager::LogLevel level, lua_State* L) {
    using ghoul::lua::luaTypeToString;
    const std::string _loggerCat = "print";
    
    const int type = lua_type(L, -1);
    switch (type) {
        case LUA_TNONE:
        case LUA_TLIGHTUSERDATA:
        case LUA_TTABLE:
        case LUA_TFUNCTION:
        case LUA_TUSERDATA:
        case LUA_TTHREAD:
            LOG(level, "Function parameter was of type '" <<
                 luaTypeToString(type) << "'");
        case LUA_TNIL:
            break;
        case LUA_TBOOLEAN:
            LOG(level, lua_toboolean(L, -1));
            break;
        case LUA_TNUMBER:
            LOG(level, lua_tonumber(L, -1));
            break;
        case LUA_TSTRING:
            LOG(level, lua_tostring(L, -1));
            break;
    }
}
    
int printDebug(lua_State* L) {
    printInternal(ghoul::logging::LogManager::LogLevel::Debug, L);
    return 0;
}

int printInfo(lua_State* L) {
    printInternal(ghoul::logging::LogManager::LogLevel::Info, L);
    return 0;
}

int printWarning(lua_State* L) {
    printInternal(ghoul::logging::LogManager::LogLevel::Warning, L);
    return 0;
}

int printError(lua_State* L) {
    printInternal(ghoul::logging::LogManager::LogLevel::Error, L);
    return 0;
}
    
int printFatal(lua_State* L) {
    printInternal(ghoul::logging::LogManager::LogLevel::Fatal, L);
    return 0;
}

    
int property_setValue(lua_State* L)
{
    using ghoul::lua::luaTypeToString;
    const std::string _loggerCat = "property_setValue";

    // TODO Check for argument number (ab)
    std::string uri = luaL_checkstring(L, -2);
    const int type = lua_type(L, -1);
  //  boost::any propertyValue;
  //  switch (type) {
  //      case LUA_TNONE:
  //      case LUA_TLIGHTUSERDATA:
  //      case LUA_TFUNCTION:
  //      case LUA_TUSERDATA:
  //      case LUA_TTHREAD:
  //          LERROR("Function parameter was of type '" << luaTypeToString(type) << "'");
  //          return 0;
  //      case LUA_TNIL:
  //          propertyValue = 0;
  //          break;
  //      case LUA_TBOOLEAN:
  //          propertyValue = lua_toboolean(L, -1);
  //          break;
  //      case LUA_TNUMBER:
  //          propertyValue = lua_tonumber(L, -1);
  //          break;
  //      case LUA_TSTRING:
  //          propertyValue = std::string(lua_tostring(L, -1));
  //          break;
		//case LUA_TTABLE: {
		//	ghoul::Dictionary d;
		//	ghoul::lua::populateDictionary(L, d);
		//	propertyValue = d;
		//	break;
		//}
  //  }
    
    Property* prop = property(uri);
    if (!prop) {
        LERROR("Property with uri '" << uri << "' could not be found");
        return 0;
    }

	//if (propertyValue.type() != prop->type()) {
	if (type != prop->typeLua())
		LERROR("Property '" << uri << "' does not accept input of type '"
			<< luaTypeToString(type) << "'. Requested type: '"
			<< luaTypeToString(prop->typeLua() << "'");
	}
	else
		prop->setLua(L);
		//prop->set(propertyValue);
    
    return 0;
}

int property_getValue(lua_State* L) {
    const std::string _loggerCat = "property_getValue";

    // TODO Check for argument number (ab)
    std::string uri = luaL_checkstring(L, -1);

	Property* prop = property(uri);
	if (!prop) {
		LERROR("Property with uri '" << uri << "' could not be found");
		lua_pushnil(L);
	}
	else {
		prop->getLua(L);

        //switch (type) {
        //    case LUA_TNONE:
        //    case LUA_TLIGHTUSERDATA:
        //    case LUA_TFUNCTION:
        //    case LUA_TUSERDATA:
        //    case LUA_TTHREAD:
        //        LERROR("Function parameter was of type '" << luaTypeToString(type)
        //                                                    << "'");
        //        return 0;
        //    case LUA_TNIL:
        //        propertyValue = 0;
        //        break;
        //    case LUA_TBOOLEAN:
        //        propertyValue = lua_toboolean(L, -1);
        //        break;
        //    case LUA_TNUMBER:
        //        propertyValue = lua_tonumber(L, -1);
        //        break;
        //    case LUA_TSTRING:
        //        propertyValue = std::string(lua_tostring(L, -1));
        //        break;
        //    case LUA_TTABLE: {
        //        ghoul::Dictionary d;
        //        ghoul::lua::populateDictionary(L, d);
        //        propertyValue = d;
        //        break;
        //    }
        //}
}
    return 1;
}
    
} // namespace scripting
} // namespace openspace
