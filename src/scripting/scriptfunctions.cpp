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

#include <ghoul/lua/ghoul_lua.h>

using namespace openspace;
using namespace openspace::properties;

int property_setValue(lua_State* L)
{
    const std::string _loggerCat = "property_setValue";
    using ghoul::lua::luaTypeToString;

    // TODO Check for argument number (ab)
    std::string nodeName = luaL_checkstring(L, -3);
    std::string propertyName = luaL_checkstring(L, -2);
    const int type = lua_type(L, -1);
    boost::any propertyValue;
    switch (type) {
        case LUA_TNONE:
        case LUA_TLIGHTUSERDATA:
        case LUA_TTABLE:
        case LUA_TFUNCTION:
        case LUA_TUSERDATA:
        case LUA_TTHREAD:
            LERROR("Function parameter was of type '" << luaTypeToString(type) << "'");
            return 0;
        case LUA_TNIL:
            propertyValue = 0;
            break;
        case LUA_TBOOLEAN:
            propertyValue = lua_toboolean(L, -1);
            break;
        case LUA_TNUMBER:
            propertyValue = lua_tonumber(L, -1);
            break;
        case LUA_TSTRING:
            propertyValue = std::string(lua_tostring(L, -1));
            break;
    }
    
    Property* prop = property(nodeName, propertyName);
    if (!prop) {
        LERROR("Property at " << nodeName << "." << propertyName <<
               " could not be found");
        return 0;
    }

    prop->set(propertyValue);
    
    return 0;
}

//int property_getValue(lua_State* L)
//{
//    
//}
