/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#include <ghoul/lua/lua_helper.h>

#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/stringhelper.h>
#include <cstring>
#include <stdexcept>
#include <vector>

namespace {
    using namespace ghoul::lua;

    lua_State* _state = nullptr;
    constexpr int KeyTableIndex = -2;
    constexpr int ValueTableIndex = -1;

    int luaAbsoluteLocation(lua_State* state, int relativeLocation) {
        if (relativeLocation >= 0) {
            return relativeLocation;
        }
        // Negative indices implies indexing the stack from top
        // -1 is the topmost item. +1 is the first item
        const int nItems = lua_gettop(state);
        return nItems + relativeLocation + 1;
    }

    lua_State* staticLuaState() {
        if (!_state) {
            _state = createNewLuaState();
        }
        return _state;
    }

    // Code snippet that causes the Lua State to strict by making it that the lookup in
    // the meta table for an unknown key causes a panic.
    // Code taken originally from the Lua webpage and used under the Lua license
    constexpr std::string_view StrictStateSource = R"(
    --[[ strict.lua
    Checks uses of undeclared global variables
    All global variables must be 'declared' through a regular assignment (even assigning
    nil will do) in a main chunk before being used anywhere or assigned to inside a
    function.

    Distributed under the Lua license: http://www.lua.org/license.html
    ]]--

    local getinfo, error, rawset, rawget = debug.getinfo, error, rawset, rawget

    local mt = getmetatable(_G)
    if mt == nil then
      mt = {}
      setmetatable(_G, mt)
    end

    mt.__declared = {}

    local function what ()
      local d = getinfo(3, "S")
      return d and d.what or "C"
    end

    mt.__newindex = function (t, n, v)
      if not mt.__declared[n] then
        local w = what()
        if w ~= "main" and w ~= "C" then
          error("assign to undeclared variable '" .. n .. "'", 2)
        end
        mt.__declared[n] = true
      end
      rawset(t, n, v)
    end

    mt.__index = function (t, n)
      if not mt.__declared[n] and what() ~= "C" then
        error("variable '" .. n .. "' is not declared", 2)
      end
      return rawget(t, n)
    end

    is_declared = function(a)
      return mt.__declared[a] and what() ~= "C";
    end)";

    // Code snippet that sandboxes the state by removing ways to directly affect the
    // outside world
    constexpr std::string_view SandboxedStateSourceBase = R"(
    -- Remove the function that can be used to load additional modules
    require = nil
    )";

    constexpr std::string_view SandboxedStateSourceLibrary = R"(
    io = nil
    os = nil
    package = nil
    )";
} // namespace

namespace ghoul::lua {

LuaError::LuaError(std::string msg)
    : RuntimeError(std::move(msg))
{}

LuaRuntimeException::LuaRuntimeException(std::string msg)
    : RuntimeError(std::move(msg), "Lua")
{}

LuaFormatException::LuaFormatException(std::string msg, std::filesystem::path file)
    : LuaRuntimeException(std::move(msg))
    , filename(std::move(file))
{}

LuaLoadingException::LuaLoadingException(std::string error, std::filesystem::path file)
    : LuaRuntimeException(std::format("Error loading script '{}': {}", file, error))
    , errorMessage(std::move(error))
    , filename(std::move(file))
{}

LuaExecutionException::LuaExecutionException(std::string error,
                                             std::filesystem::path file)
    : LuaRuntimeException(std::format("Error executing script '{}': {}", file, error))
    , errorMessage(std::move(error))
    , filename(std::move(file))
{}

const char* errorLocation(lua_State* state) {
    ghoul_assert(state, "State must not be empty");

    luaL_where(state, 1);
    const char* result = lua_tostring(state, -1);
    lua_pop(state, 1);
    return result;
}

int luaError(lua_State* state, const std::string& message) {
    ghoul_assert(state, "State must not be empty");
    return luaL_error(state, message.c_str());
}

std::string luaValueToString(lua_State* state, int location) {
    ghoul_assert(state, "State must not be nullptr");

    const int type = lua_type(state, location);
    switch (type) {
        case LUA_TBOOLEAN: return lua_toboolean(state, location) == 1 ? "true" : "false";
        case LUA_TNUMBER:  return std::format("{}", lua_tonumber(state, location));
        case LUA_TSTRING:  return std::format("\"{}\"", lua_tostring(state, location));
        case LUA_TTABLE:   return luaTableToString(state, location);
        default:           return std::string(luaTypeToString(type));
    }
}

std::string luaTableToString(lua_State* state, int tableLocation) {
    ghoul_assert(state, "State must not be nullptr");
    ghoul_assert(lua_istable(state, tableLocation), "Lua object is not a table");

    lua_pushvalue(state, tableLocation);
    lua_pushnil(state);

    auto key = [](lua_State* L) {
        const int keyType = lua_type(L, KeyTableIndex);
        switch (keyType) {
            case LUA_TNUMBER: return std::format("[{}]", lua_tonumber(L, KeyTableIndex));
            case LUA_TSTRING: return std::string(lua_tostring(L, KeyTableIndex));
            default:          return std::string(luaTypeToString(keyType));
        }
    };

    std::vector<std::string> values;
    while (lua_next(state, -2) != 0) {
        values.push_back(std::format(
            "{} = {}", key(state), luaValueToString(state, ValueTableIndex)
        ));
        lua_pop(state, 1);
    }
    lua_pop(state, 1);

    if (values.empty()) {
        return "{}";
    }

    return std::format("{{ {} }}", join(values, ", "));
}

std::string stackInformation(lua_State* state) {
    ghoul_assert(state, "State must not be nullptr");

    const int top = lua_gettop(state);
    if (top == 0) {
        return "Lua Stack (empty)";
    }

    std::string result = "Lua Stack\n";
    for (int i = 1; i <= top; i++) {
        result += std::format("{}: ", i);
        const int t = lua_type(state, i);
        switch (t) {
            case LUA_TSTRING:
                result += lua_tostring(state, i);
                break;
            case LUA_TBOOLEAN:
                result += (lua_toboolean(state, i) ? "true" : "false");
                break;
            case LUA_TNUMBER:
                result += std::to_string(lua_tonumber(state, i));
                break;
            case LUA_TTABLE:
                result += luaTableToString(state, i);
                break;
            case LUA_TTHREAD:
            case LUA_TUSERDATA:
            case LUA_TLIGHTUSERDATA:
            case LUA_TFUNCTION:
                result += luaTypeToString(t);
                break;
            default:
                result += lua_typename(state, t);
                break;
        }
        result += '\n';
    }
    return result;
}

void loadDictionaryFromFile(const std::filesystem::path& filename, Dictionary& dictionary,
                            lua_State* state)
{
    ghoul_assert(!filename.empty(), "filename must not be empty");
    ghoul_assert(
        std::filesystem::is_regular_file(filename),
        "filename must be an existing file"
    );

    if (!state) {
        state = staticLuaState();
    }

    const std::string f = filename.string();
    const int loadStatus = luaL_loadfile(state, f.c_str());
    if (loadStatus != LUA_OK) {
        throw LuaLoadingException(lua_tostring(state, -1), filename);
    }

    const int callStatus = lua_pcall(state, 0, LUA_MULTRET, 0);
    if (callStatus != LUA_OK) {
        throw LuaExecutionException(lua_tostring(state, -1), filename);
    }

    if (lua_isnil(state, -1)) {
        throw LuaFormatException("Script did not return anything", filename);
    }

    if (!lua_istable(state, -1)) {
        throw LuaFormatException("Script did not return a table", filename);
    }

    luaDictionaryFromState(state, dictionary);

    // Clean up after ourselves by cleaning the stack
    lua_settop(state, 0);
}

Dictionary loadDictionaryFromFile(const std::filesystem::path& filename, lua_State* state)
{
    Dictionary result;
    loadDictionaryFromFile(filename, result, state);
    return result;
}

void loadDictionaryFromString(const std::string& script, Dictionary& dictionary,
                              lua_State* state)
{
    ghoul_assert(!script.empty(), "Script must not be empty");

    if (!state) {
        state = staticLuaState();
    }

    const int loadStatus = luaL_loadstring(state, script.c_str());
    if (loadStatus != LUA_OK) {
        throw LuaLoadingException(lua_tostring(state, -1));
    }

    const int callStatus = lua_pcall(state, 0, LUA_MULTRET, 0);
    if (callStatus != LUA_OK) {
        throw LuaExecutionException(lua_tostring(state, -1));
    }

    if (lua_isnil(state, -1)) {
        throw LuaFormatException("Script did not return anything");
    }

    if (!lua_istable(state, -1)) {
        throw LuaFormatException("Script did not return a table");
    }

    luaDictionaryFromState(state, dictionary);

    // Clean up after ourselves by cleaning the stack
    lua_settop(state, 0);
}

void loadArrayDictionaryFromString(const std::string& script, Dictionary& dictionary,
                                   lua_State* state)
{
    ghoul_assert(!script.empty(), "Script must not be empty");

    if (!state) {
        state = staticLuaState();
    }

    // Clear the stack. This should not be necessary, but if the stack was left unclean,
    // we want to avoid previous values to leak into the dictionary
    lua_settop(state, 0);

    const int loadStatus = luaL_loadstring(state, script.c_str());
    if (loadStatus != LUA_OK) {
        throw LuaLoadingException(lua_tostring(state, -1));
    }

    const int callStatus = lua_pcall(state, 0, LUA_MULTRET, 0);
    if (callStatus != LUA_OK) {
        throw LuaExecutionException(lua_tostring(state, -1));
    }

    luaArrayDictionaryFromState(state, dictionary);

    // Clean up after ourselves by cleaning the stack
    lua_settop(state, 0);
}

Dictionary loadDictionaryFromString(const std::string& script, lua_State* state) {
    Dictionary result;
    loadDictionaryFromString(script, result, state);
    return result;
}

Dictionary loadArrayDictionaryFromString(const std::string& script, lua_State* state) {
    Dictionary result;
    loadArrayDictionaryFromString(script, result, state);
    return result;
}

void luaDictionaryFromState(lua_State* state, Dictionary& dictionary,
                            int relativeLocation)
{
    enum class TableType {
        Undefined = 1,  // 001
        Map = 3,        // 010
        Array = 5       // 101
    };

    ghoul_assert(state, "State must not be nullptr");

    TableType type = TableType::Undefined;

    const int location = luaAbsoluteLocation(state, relativeLocation);

    const int size = lua_gettop(state);
    if (size == 0) {
        throw LuaFormatException("Tried to load Dictionary from empty state");
    }
    const int topType = lua_type(state, size);
    if (topType == LUA_TNIL) {
        // There was no table specified, so we can return an empty Dictionary
        dictionary = Dictionary();
        return;
    }
    if (topType != LUA_TTABLE) {
        throw LuaFormatException("Tried to load Dictionary from wrong parameter type");
    }

    lua_pushnil(state);
    while (lua_next(state, location) != 0) {
        // Get the key
        std::string key;
        const int keyType = lua_type(state, KeyTableIndex);
        switch (keyType) {
            case LUA_TNUMBER:
                key = std::to_string(lua_tointeger(state, KeyTableIndex));

                if (type == TableType::Map) {
                    throw LuaFormatException(std::format(
                        "Dictionary can only contain a pure map or a pure array. Error "
                        "at key '{}'", key
                    ));
                }

                type = TableType::Array;
                key = std::to_string(lua_tointeger(state, KeyTableIndex));
                break;
            case LUA_TSTRING:
                key = lua_tostring(state, KeyTableIndex);

                if (type == TableType::Array) {
                    throw LuaFormatException(std::format(
                        "Dictionary can only contain a pure map or a pure array. Error "
                        "at key '{}'", key
                    ));
                }

                type = TableType::Map;
                key = lua_tostring(state, KeyTableIndex);
                break;
            default:
                throw LuaFormatException("Table index type is not a number or a string");
        }

        // Get the value
        const int valueType = lua_type(state, ValueTableIndex);
        switch (valueType) {
            case LUA_TNUMBER: {
                const double value = lua_tonumber(state, ValueTableIndex);
                dictionary.setValue(key, value);
                break;
            }
            case LUA_TBOOLEAN: {
                const bool value = (lua_toboolean(state, ValueTableIndex) == 1);
                dictionary.setValue(key, value);
                break;
            }
            case LUA_TSTRING: {
                std::string value = lua_tostring(state, ValueTableIndex);
                dictionary.setValue(key, std::move(value));
                break;
            }
            case LUA_TTABLE: {
                Dictionary d = luaDictionaryFromState(state);
                dictionary.setValue(key, std::move(d));
                break;
            }
            case LUA_TLIGHTUSERDATA:
            case LUA_TUSERDATA: {
                void* data = lua_touserdata(state, ValueTableIndex);
                dictionary.setValue(key, data);
                break;
            }
            case LUA_TFUNCTION: {
                auto writer = [](lua_State*, const void* p, size_t sz, void* user) {
                    std::string* buffer = reinterpret_cast<std::string*>(user);
                    const size_t tail = buffer->size();
                    buffer->resize(buffer->size() + sz);
                    std::memcpy(buffer->data() + tail, p, sz);
                    return 0;
                };

                std::string buffer;
                const int StripCode = 1;
                lua_dump(state, writer, &buffer, StripCode);
                dictionary.setValue(key, buffer);
                break;
            }
            default:
                throw LuaFormatException("Unknown type: " + std::to_string(valueType));
        }

        // Get back up one level
        lua_pop(state, 1);
    }

    // @CLEANUP:  This function seems to leak stack space and a lua_settop(state,0)
    //            crashes --- abock(2018-02-15)
    //            Affected: navigationhandler_lua.inl::setCameraState
    //ghoul_assert(lua_gettop(state) == 0, "Incorrect number of items left on stack");
}

Dictionary luaDictionaryFromState(lua_State* state, int location) {
    Dictionary res;
    luaDictionaryFromState(state, res, location);
    return res;
}

void luaArrayDictionaryFromState(lua_State* state, Dictionary& dictionary) {
    const int nValues = lua_gettop(state);

    for (int i = 1; i <= nValues; i++) {
        switch (lua_type(state, i)) {
            case LUA_TNUMBER: {
                const double value = lua_tonumber(state, i);
                dictionary.setValue(std::to_string(i), value);
                break;
            }
            case LUA_TBOOLEAN: {
                const bool value = (lua_toboolean(state, i) == 1);
                dictionary.setValue(std::to_string(i), value);
                break;
            }
            case LUA_TSTRING: {
                std::string value = lua_tostring(state, i);
                dictionary.setValue(std::to_string(i), std::move(value));
                break;
            }
            case LUA_TTABLE: {
                Dictionary d;
                luaDictionaryFromState(state, d, i);
                dictionary.setValue(std::to_string(i), d);
                break;
            }
            default:
                throw LuaFormatException(
                    std::format("Unknown type: {}", lua_type(state, i))
                );
        }
    }
}

std::string_view luaTypeToString(LuaTypes type) {
    switch (type) {
        case LuaTypes::None:          return "None";
        case LuaTypes::Nil:           return "Nil";
        case LuaTypes::Boolean:       return "Boolean";
        case LuaTypes::LightUserData: return "Light UserData";
        case LuaTypes::Number:        return "Number";
        case LuaTypes::String:        return "String";
        case LuaTypes::Table:         return "Table";
        case LuaTypes::Function:      return "Function";
        case LuaTypes::UserData:      return "UserData";
        case LuaTypes::Thread:        return "Thread";
        default:                      throw std::logic_error("Lua only has 9 types");
    }
}

std::string_view luaTypeToString(int type) {
    return luaTypeToString(fromLuaType(type));
}

lua_State* createNewLuaState(bool sandboxed, bool loadStandardLibraries, bool strictState)
{
    LDEBUGC("Lua", "Creating Lua state");
    lua_State* s = luaL_newstate();
    if (!s) {
        throw LuaRuntimeException("Error creating Lua state: Memory allocation");
    }

    if (strictState) {
        ghoul_assert(
            loadStandardLibraries,
            "If requesting strict state, the standard libraries must be loaded"
        );
    }

    if (loadStandardLibraries || strictState) {
        // Fallback to prevent a crash that would otherwise happen in release builds when
        // strict=true and loadStandard=false
        LDEBUGC("Lua", "Open libraries");
        luaL_openlibs(s);
    }
    if (strictState) {
        LDEBUGC("Lua", "Registering strict code");
        runScript(s, StrictStateSource);
    }
    if (sandboxed) {
        LDEBUGC("Lua", "Sandboxing Lua state");
        runScript(s, SandboxedStateSourceBase);
        if (loadStandardLibraries || strictState) {
            runScript(s, SandboxedStateSourceLibrary);
        }
    }
    return s;
}

void destroyLuaState(lua_State* state) {
    ghoul_assert(state, "State must not be nullptr");
    lua_close(state);
}

void runScriptFile(lua_State* state, const std::filesystem::path& filename) {
    ghoul_assert(state, "State must not be nullptr");
    ghoul_assert(!filename.empty(), "filename must not be empty");
    ghoul_assert(
        std::filesystem::is_regular_file(filename),
        "Filename must be a file that exists"
    );

    const std::string fn = filename.string();
    int status = luaL_loadfile(state, fn.c_str());
    if (status != LUA_OK) {
        std::string error = lua_tostring(state, -1);
        throw LuaLoadingException(std::move(error));
    }

    status = lua_pcall(state, 0, LUA_MULTRET, 0);
    if (status != LUA_OK) {
        std::string error = lua_tostring(state, -1);
        throw LuaExecutionException(std::move(error));
    }
}

void runScript(lua_State* state, std::string_view script) {
    ghoul_assert(state, "State must not be nullptr");
    ghoul_assert(!script.empty(), "Script must not be empty");

    const int load = luaL_loadbuffer(state, script.data(), script.size(), script.data());
    if (load != LUA_OK) {
        std::string error = lua_tostring(state, -1);
        throw LuaLoadingException(std::move(error));
    }

    const int call = lua_pcall(state, 0, LUA_MULTRET, 0);
    if (call != LUA_OK) {
        if (lua_isstring(state, -1)) {
            std::string error = lua_tostring(state, -1);
            throw LuaExecutionException(std::move(error));
        }
        else {
            std::string stack = ghoul::lua::stackInformation(state);
            throw LuaExecutionException(std::format(
                "Fatal error in Lua execution. Return value not an error message.\n{}",
                stack
            ));
        }
    }
}

int checkArgumentsAndThrow(lua_State* L, int expected, const char* component) {
    const int nArguments = lua_gettop(L);
    if (nArguments != expected) {
        const std::string s = std::format(
            "Expected {} arguments, got {}", expected, nArguments
        );
        LERRORC(component ? component : "Lua", s);
        return luaError(L, s);
    }
    return nArguments;
}

int checkArgumentsAndThrow(lua_State* L, int expected1, int expected2,
                           const char* component)
{
    const int nArguments = lua_gettop(L);
    if ((nArguments != expected1) && (nArguments != expected2)) {
        const std::string s = std::format(
            "Expected {} or {} arguments, got {}", expected1, expected2, nArguments
        );
        LERRORC(component ? component : "Lua", s);
        return luaError(L, s);
    }
    return nArguments;
}

int checkArgumentsAndThrow(lua_State* L, std::pair<int, int> range, const char* component)
{
    const int nArguments = lua_gettop(L);
    if ((nArguments < range.first) || (nArguments > range.second)) {
        const std::string s = std::format(
            "Expected {}-{} arguments, got {}", range.first, range.second, nArguments
        );
        LERRORC(component ? component : "Lua", s);
        return luaError(L, s);
    }
    return nArguments;
}

int checkArgumentsAndThrow(lua_State* L, int expected, std::pair<int, int> range,
                           const char* component)
{
    const int nArguments = lua_gettop(L);

    if (nArguments != expected &&
       (nArguments < range.first) && (nArguments > range.second))
    {
        const std::string s = std::format(
            "Expected {} or {}-{} arguments, got {}",
            expected, range.first, range.second, nArguments
        );

        LERRORC(component ? component : "Lua", s);
        return luaError(L, s);
    }
    return nArguments;
}

void verifyStackSize(lua_State* L, int expected) {
    const int size = lua_gettop(L);

    if (size != expected) {
        LINFOC("Stack", stackInformation(L));
    }

    ghoul_assert(
        size == expected,
        std::format(
            "Incorrect number of items left on stack. Expected {} got {}", expected, size
        )
    );
}

bool isScriptBinary(std::string_view script) {
    // A script is a binary if it's not empty and it doesn't start with `\x1b`
    return !script.empty() && script[0] == '\x1b';
}

namespace internal {
    void deinitializeGlobalState() {
        if (_state) {
            lua_close(_state);
        }
        _state = nullptr;
    }
} // namespace internal

} // namespace ghoul::lua
