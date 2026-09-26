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

#ifndef __GHOUL___LUA_HELPER___H__
#define __GHOUL___LUA_HELPER___H__

#include <ghoul/lua/lua_types.h>
#include <ghoul/misc/boolean.h>
#include <ghoul/misc/exception.h>
#include <filesystem>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>

namespace ghoul { class Dictionary; }
struct lua_State;

namespace ghoul::lua {

struct LuaError final : public RuntimeError {
    explicit LuaError(std::string msg);
};

struct LuaRuntimeException : public RuntimeError {
    explicit LuaRuntimeException(std::string msg);
};

struct LuaFormatException final : public LuaRuntimeException {
    explicit LuaFormatException(std::string msg, std::filesystem::path file = "");
    const std::filesystem::path filename;
};

struct LuaLoadingException final : public LuaRuntimeException {
    explicit LuaLoadingException(std::string error, std::filesystem::path file = "");
    const std::string errorMessage;
    const std::filesystem::path filename;
};

struct LuaExecutionException final : public LuaRuntimeException {
    explicit LuaExecutionException(std::string error, std::filesystem::path file = "");
    const std::string errorMessage;
    const std::filesystem::path filename;
};

/**
 * If an instance of this struct is passed to the #push method, it will cause a nil value
 * to be pushed onto the stack.
 */
struct nil_t {};

BooleanType(PopValue);

/**
 * Returns the location of the calling function using `luaL_where` and returns that
 * location as a string. This method is just a wrapper around this function and its use is
 * for non-fatal error handling.
 *
 * \param state The Lua state that is to be examined
 * \return The location of the function whose stack is being tested
 *
 * \pre \p state must not be `nullptr`
 */
[[nodiscard]] const char* errorLocation(lua_State* state);

/**
 * Raises a fatal Lua error by calling the `luaL_error` function with the passed
 * parameters.
 *
 * \param state The Lua state in which the error is raised
 * \param message The error message that will be printed to the Lua console alongside the
 *        file name and line number
 * \return The symbolic value that is returned by the `luaL_error` function to signal the
 *         Lua interpreter that an error has occurred
 *
 * \pre \p state must not be `nullptr`
 */
int luaError(lua_State* state, const std::string& message);

/**
 * Returns a string describing the \p state's value at location \p location. Supported
 * value types are `boolean`, `number`, `string` and `table`. For other types, only the
 * type as converted to string is returned.
 *
 * \param state The Lua state from which to read the value
 * \param location The location from which the value should be extracted
 * \return A string representation of the value
 *
 * \pre \p state must not be `nullptr`
 */
[[nodiscard]] std::string luaValueToString(lua_State* state, int location);

/**
 * Returns a string describing a table from the Lua state.
 *
 * \param state The Lua state from which to read the value
 * \param tableLocation The location from which the table should be extracted
 * \return A string representation of the table
 *
 * \pre \p state must not be `nullptr`
 * \pre The Lua object at location \p tableLocation must be a table
 */
[[nodiscard]] std::string luaTableToString(lua_State* state, int tableLocation);

/**
 * Returns a string describing the \p state's current stack. The values of each entry in
 * the stack is printed, which includes tables (printed recursively), but excludes
 * `function`, `thread`, `userdata`, and `light userdata` objects, for which only the type
 * is returned. The returned string is in the the format:
 * ```
 * 1: <entry>
 * 2: <entry>
 * ...
 * ```
 * If the stack does not contain any values, an empty string is returned.
 *
 * \param state The Lua state that will have its stack printed
 * \return The description of the current \p state's stack
 *
 * \pre \p state must not be `nullptr`
 */
[[nodiscard]] std::string stackInformation(lua_State* state);


/**
 * Loads a Lua script into the given #ghoul::Dictionary, extending the passed in
 * dictionary. This method will overwrite values with the same keys, but will not remove
 * any other keys from the dictionary. The script contained in the file must return a
 * single table, which is then parsed and included into the #ghoul::Dictionary. The single
 * restriction on the script is that it can only contain a pure array-style table (= only
 * indexed by numbers) or a pure dictionary-style table (= no numbering indices).
 *
 * \param filename The filename pointing to the script that is executed
 * \param dictionary The #ghoul::Dictionary into which the values from the script are
 *        added
 * \param state If this is set to a valid lua_State, this state is used instead of
 *        creating a new state. It is the callers responsibility to ensure that the passed
 *        state is valid if this parameter is not `nullptr`. After calling this
 *        method, the stack of the passed state will be empty after this function returns
 *
 * \throw FormattingException If the #ghoul::Dictionary contains mixed keys of both type
 *        `string` and type `number`
 * \throw FormattingException If the script did not return anything else but a table
 * \throw LuaRuntimeException If there was an error initializing a new Lua state if it was
 *        necessary
 * \pre \p filename must not be empty
 * \pre \p filename must be a path to an existing file
 * \post The \p state%'s stack is empty
 */
void loadDictionaryFromFile(const std::filesystem::path& filename, Dictionary& dictionary,
    lua_State* state = nullptr);

/**
 * Loads a Lua script and returns it as a #ghoul::Dictionary. The script contained in the
 * file must return a single table, which is then parsed and included into the
 * #ghoul::Dictionary. The single restriction on the script is that it can only contain a
 * pure array-style table (= only indexed by numbers) or a pure dictionary-style table
 * (= no numbering indices).
 *
 * \param filename The filename pointing to the script that is executed
 * \param state If this is set to a valid lua_State, this state is used instead of
 *        creating a new state. It is the callers responsibility to ensure that the passed
 *        state is valid if this parameter is not `nullptr`. After calling this method,
 *        the stack of the passed state will be empty after this function returns
 * \return The ghoul::Dictionary described by the Lua script
 *
 * \throw FormattingException If the #ghoul::Dictionary contains mixed keys of both type
 *        `string` and type `number`
 * \throw FormattingException If the script did not return anything else but a table
 * \throw LuaRuntimeException If there was an error initializing a new Lua state if it was
 *        necessary
 * \pre \p filename must not be empty
 * \pre \p filename must be a path to an existing file
 * \post The \p state%'s stack is empty
 */
Dictionary loadDictionaryFromFile(const std::filesystem::path& filename,
    lua_State* state = nullptr);

/**
 * Loads a Lua configuration into the given #ghoul::Dictionary, extending the passed in
 * dictionary. This method will overwrite values with the same keys, but will not remove
 * any other keys from the dictionary. The script contained in the string must return a
 * single table, which is then parsed and included into the #ghoul::Dictionary. The single
 * restriction on the script is that it can only contain a pure array-style table (= only
 * indexed by numbers) or a pure dictionary-style table (= no numbering indices).
 *
 * \param script The source code of the script that is executed
 * \param dictionary The #ghoul::Dictionary into which the values from the script are
 *        added
 * \param state If this is set to a valid lua_State, this state is used instead of
 *        creating a new state. It is the callers responsibility to ensure that the passed
 *        state is valid. After calling this method, the stack of the passed state will be
 *        empty
 *
 * \throw ghoul::lua::FormattingException If the #ghoul::Dictionary contains mixed keys of
 *        both type `string` and type `number`
 * \throw FormattingException If the script did not return anything else but a table
 * \pre \p script must not be empty
 * \post \p state%'s stack is empty
 */
void loadDictionaryFromString(const std::string& script, Dictionary& dictionary,
    lua_State* state = nullptr);

/**
 * Loads a Lua state into the given #ghoul::Dictionary, extending the passed in
 * dictionary with numeric keys based on the stack indices. This method will overwrite
 * values with the same keys, but will not remove any other keys from the dictionary.
 * The script contained in the string may return mulitple values which will be included
 * into the #ghoul::Dictionary.
 *
 * \param script The source code of the script that is executed
 * \param dictionary The #ghoul::Dictionary into which the values from the script are
 *        added
 * \param state If this is set to a valid lua_State, this state is used instead of
 *        creating a new state. It is the callers responsibility to ensure that the passed
 *        state is valid. After calling this method, the stack of the passed state will be
 *        empty
 *
 * \throw FormattingException If the script did not return something that could not be
 *        turned into a dictionary, such as a function or userdata
 * \pre \p script must not be empty
 * \post \p state%'s stack is empty
 */
void loadArrayDictionaryFromString(const std::string& script, Dictionary& dictionary,
    lua_State* state = nullptr);

/**
 * Loads a Lua script and returns it as a #ghoul::Dictionary. The script contained in the
 * string must return a single table, which is then parsed. The single restriction on the
 * script is that it can only contain a pure array-style table (= only indexed by numbers)
 * or a pure dictionary-style table (= no numbering indices).
 *
 * \param script The source code of the script that is executed
 * \param state If this is set to a valid lua_State, this state is used instead of
 *        creating a new state. It is the callers responsibility to ensure that the passed
 *        state is valid. After calling this method, the stack of the passed state will be
 *        empty
 * \return The ghoul::Dictionary described by the Lua script
 *
 * \throw ghoul::lua::FormattingException If the #ghoul::Dictionary contains mixed keys of
 *        both type `string` and type `number`
 * \throw FormattingException If the script did not return anything else but a table
 * \pre \p script must not be empty
 * \post \p state%'s stack is empty
 */
Dictionary loadDictionaryFromString(const std::string& script,
    lua_State* state = nullptr);

/**
 * Loads a Lua state and returns it as a #ghoul::Dictionary, extending the passed in
 * dictionary with numeric keys based on the stack indices. The script contained in the
 * string may return mulitple values which will be included into the #ghoul::Dictionary.
 *
 * \param script The source code of the script that is executed
 * \param state If this is set to a valid lua_State, this state is used instead of
 *        creating a new state. It is the callers responsibility to ensure that the passed
 *        state is valid. After calling this method, the stack of the passed state will be
 *        empty
 *
 * \throw FormattingException If the script did not return something that could not be
 *        turned into a dictionary, such as a function or userdata
 * \pre \p script must not be empty
 * \post \p state%'s stack is empty
 */
Dictionary loadArrayDictionaryFromString(const std::string& script,
    lua_State* state = nullptr);

/**
 * Uses the Lua \p state to populate the provided ghoul::Dictionary%, extending the passed
 * \p dictionary. This method will overwrite values with the same keys, but will not
 * remove any other keys from the dictionary. The \p state must have a single table object
 * at the top of the stack. The table can only contain a pure array-style table (= only
 * indexed by numbers) or a pure dictionary-style table (= no numbering indices).
 *
 * \param state The Lua state that is used to populate the \p dictionary
 * \param dictionary The #ghoul::Dictionary into which the values from the stack are
 *        added
 * \param relativeLocation The stack index of the item to extract. Defaults to -1 (topmost
 *        item)
 *
 * \throw LuaFormatException If the \p dictionary contains mixed keys of both type
 *        `string` and type `number`
 * \pre \p state must not be `nullptr`
 * \post \p state%'s stack is unchanged
 */
void luaDictionaryFromState(lua_State* state, Dictionary& dictionary,
    int relativeLocation = -1);

/**
 * Uses the Lua \p state to populate the return ghoul::Dictionary%. The \p state must have
 * a single table object at the top of the stack. The table can only contain a pure
 * array-style table (= only indexed by numbers) or a pure dictionary-style table (= no
 * numbering indices).
 *
 * \param state The Lua state that is used to populate the \p dictionary
 * \param location The stack index of the item to extract. Defaults to -1 (topmost item)
 * \return The #ghoul::Dictionary into which the values from the stack are added
 *
 * \throw LuaFormatException If the \p dictionary contains mixed keys of both type
 *        `string` and type `number`
 * \pre \p state must not be `nullptr`
 * \post \p state%'s stack is unchanged
 */
Dictionary luaDictionaryFromState(lua_State* state, int location = -1);

/**
 * Uses the Lua \p state to populate the provided ghoul::Dictionary%, extending the passed
 * \p dictionary with numeric keys based on the values indices on the stack. This method
 * will overwrite values with the same keys, but will not remove any other keys from the
 * dictionary. The \p state may have multiple items on the stack.
 *
 * \param state The Lua state that is used to populate the \p dictionary
 * \param dictionary The #ghoul::Dictionary into which the values from the stack are
 *        added
 *
 * \pre \p state must not be `nullptr`
 * \post \p state%'s stack is unchanged
 */
void luaArrayDictionaryFromState(lua_State* state, Dictionary& dictionary);


/**
 * Converts the Lua type to a human-readable string. The supported types are:
 *   - `LUA_TNONE`: None
 *   - `LUA_TNIL`: Nil
 *   - `LUA_TBOOLEAN`: Boolean
 *   - `LUA_TLIGHTUSERDATA`: Light UserData
 *   - `LUA_TNUMBER`: Number
 *   - `LUA_TSTRING`: String
 *   - `LUA_TTABLE`: Table
 *   - `LUA_TFUNCTION`: Function
 *   - `LUA_TUSERDATA`: UserData
 *   - `LUA_TTHREAD`: Thread
 *
 * \param type A Lua type that should be converted to a string
 * \return The converted string
 */
std::string_view luaTypeToString(int type);

std::string_view luaTypeToString(LuaTypes type);

/**
 * Creates a new Lua state and initializes it with the default Lua libraries.
 *
 * \param sandboxed If this is `true`, then all of the functions that might pose
 *        potential security risks are removed from the state. This includes functions to
 *        load third-party modules or access the file system
 * \param loadStandardLibraries If `true`, the Lua standard libraries will be loaded into
 *        the newly created state by means of a `luaL_openlibs` call
 * \param strictState If this is `true`, the created Lua state will panic if an unused
 *        variable is read or being written to before being defined before
 * \return A valid new Lua state initialized with the default Lua libraries
 *
 * \throw LuaRuntimeException If there was an error creating the new Lua state
 */
lua_State* createNewLuaState(bool sandboxed = true, bool loadStandardLibraries = true,
    bool strictState = false);

/**
 * Destroys the passed lua state and frees all memory that is associated with it.
 *
 * \param state The Lua state that is to be deleted
 *
 * \pre \p state must not be `nullptr`
 */
void destroyLuaState(lua_State* state);

/**
 * This function executes the Lua script pointed to by \p filename using the passed
 * `lua_State` \p state.
 *
 * \param state The Lua state that is used to execute the script
 * \param filename The file path that contains the Lua script that is executed
 *
 * \throw LuaLoadingException If there was an error loading the script
 * \throw LuaExecutionError If there was an error executing the script
 * \throw FileNotFoundError If the file does not exist
 * \pre \p state must not be `nullptr`
 * \pre \p filename must not be empty
 * \pre \p filename must be a file that exist
 */
void runScriptFile(lua_State* state, const std::filesystem::path& filename);

/**
 * This function executes the Lua script provided as plain text in \p script using the
 * passed `lua_State` \p state.
 *
 * \throw LuaLoadingException If there was an error loading the script
 * \throw LuaExecutionError If there was an error executing the script
 * \pre \p state must not be `nullptr`
 * \pre \p script must not be empty
 */
void runScript(lua_State* state, std::string_view script);

/**
 * Checks the number of arguments on the Lua stack against the \p expected number of
 * parameters. If the numbers do not agree, an error is logged and a
 * ghoul::lua::LuaExecutionException is raised.
 *
 * \param L The Lua stack from which the arguments are checked
 * \param expected The number of expected arguments
 * \param component The name of the component that is used for the error message
 * \return The number of arguments
 *
 * \throw ghoul::lua::LuaExecutionException if the number of arguments is wrong
 */
int checkArgumentsAndThrow(lua_State* L, int expected, const char* component = nullptr);

/**
 * Checks if the number of arguments on the Lua stack is equal to \p expected1 or
 * \p expected2 parameters. If the numbers do not agree, an error is logged and a
 * ghoul::lua::LuaExecutionException is raised.
 *
 * \param L The Lua stack from which the arguments are checked
 * \param expected1 The first allowed number of expected arguments
 * \param expected2 The second allowed number of expected arguments
 * \param component The name of the component that is used for the error message
 * \return The number of arguments
 *
 * \throw ghoul::lua::LuaExecutionException if the number of arguments is wrong
 */
int checkArgumentsAndThrow(lua_State* L, int expected1, int expected2,
    const char* component = nullptr);

/**
 * Checks if the number of arguments on the Lua stack is in the \p range of allowed
 * values. If the numbers do not agree, an error is logged and a
 * ghoul::lua::LuaExecutionException is raised.
 *
 * \param L The Lua stack from which the arguments are checked
 * \param range The inclusive range that the number of arguments are tested against
 * \param component The name of the component that is used for the error message
 * \return The number of arguments
 *
 * \throw ghoul::lua::LuaExecutionException if the number of arguments is wrong
 */
int checkArgumentsAndThrow(lua_State* L, std::pair<int, int> range,
    const char* component = nullptr);

/**
 * Checks if the number of arguments on the Lua stack is in either equal to \p expected or
 * inside the \p range of allowed values. If the numbers do not agree, an error is logged
 * and a ghoul::lua::LuaExecutionException is raised.
 *
 * \param L The Lua stack from which the arguments are checked
 * \param expected The expected value
 * \param range The inclusive range that the number of arguments are tested against
 * \param component The name of the component that is used for the error message
 * \return The number of arguments
 *
 * \throw ghoul::lua::LuaExecutionException if the number of arguments is wrong
 */
int checkArgumentsAndThrow(lua_State* L, int expected, std::pair<int, int> range,
    const char* component = nullptr);

/**
 * Checks whether the passed state \p L has the correct size. If it is not equal to
 * \p expected this function asserts in debug configuration and will print the stack
 * information, but is a noop in other configurations.
 *
 * \param L The state that is to be tested
 * \param expected The expected number of items on the stack
 */
void verifyStackSize(lua_State* L, int expected = 0);

/**
 * Returns whether the provided \p script is a binary blob or not. If the \p script is not
 * a binary blob, it is a potentially null-terminated string. If it is a binary blob, the
 * string might contain null-terminated characters within and shouldn't be used as an
 * `std::string`. Passing binary or non-binary scripts to the #runScript function will
 * both work as expected.
 *
 * \param script The script that should be tested whether it is a binary blob or not
 * \return `true` if the \p script is binary, `false` otherwise
 */
bool isScriptBinary(std::string_view script);

/**
 * Checks whether a value of the requested type exists at the provided location of the
 * stack.
 *
 * \tparam T The type of the return value.
 * \param L The stack from which the value is checked
 * \param location The location at which the value is checked
 * \return `true` if the value at location exists and has the requested type `T` `false`
 *         otherwise
 *
 * \pre L must not be `nullptr`
 */
template <typename T>
bool hasValue(lua_State* L, int location = -1);

/**
 * Extracts a value from the provided location of the provided stack and returns it.
 *
 * \tparam T The type of the return value. If the value at the provided location of the
 *           stack is not T a LuaFormatException is thrown
 * \param L The stack from which the value is extracted
 * \param location The location from which the value should be extracted
 * \param shouldPopValue If the value was successfully retrieved, should it be popped from
 *        the stack
 *
 * \throw LuaFormatException If the value at the provided stack location is not T
 * \pre L must not be `nullptr`
 */
template <typename T>
T value(lua_State* L, int location = -1, PopValue shouldPopValue = PopValue::Yes);

/**
 * Extracts multiple values from the provided (and subsequent locations) of the provided
 * stack and return them as a tuple.  If at least one of the value does not exist or is of
 * the wrong type, a LuaFormatException is thrown.  It is possible to pass
 * std::optional<T> as arguments, which do not have to exist, but if they do, they have to
 * be of the right type.  Furthermore, first all non-optional arguments must be listed
 * before any std::optional arguments are listed.
 *
 * \param L The stack from which the values are extracted
 * \param location The base location from which the values are extracted
 * \param shouldPopValue If the values were successfully retrieved, should they be removed
 *        from the stack
 *
 * \throw LuaFormatException If the value at the provided stack location is not T
 * \pre L must not be `nullptr`
 */
template <typename... Ts>
constexpr std::tuple<Ts...> values(lua_State* L, int location = 1,
    PopValue shouldPopValue = PopValue::Yes);

/**
 * Extracts a userdata pointer of the specified type T from the Lua state and returns it
 * to the caller. The value will be returned as the specfied type and it is up to the
 * caller to verify that the user data is, in fact, of the correct type, or else a pointer
 * that is invalid will be returned from this function.
 *
 * This function is equivalent to:
 * ```
 * reinterpret_cast<T*>(lua_touserdata(L, lua_upvalueindex(i)));
 * ```
 *
 * \param L The stack from which the user data is extracted
 * \param location The location on the registry where the user data is stored
 * \return A pointer to the user data at the specified location
 *
 * \pre \p L must not be `nullptr`
 */
template <typename T>
T* userData(lua_State* L, int location = 1);

/**
 * Pushes the passed parameters \p args onto the provided stack \p L in the order that
 * they are specified in this function call. All passed parameters must be of a type that
 * can be converted to a type that can be pushed to the stack, otherwise the function call
 * will generate a compile error at the calling location. If no arguments are passed to
 * this function, it will be a no-op. If one of the parameters is an instance of the
 * nil_t tag struct, it will cause a nil value to be pushed to the stack.
 *
 * The allowed types for T are:
 *   - integer number types
 *   - floating point types
 *   - bool
 *   - nil_t for pushing a nil value
 *   - text (`std::string` or `const char*`)
 *   - std::vectors with integer, floating point, or text
 *   - pointers, which are pushed as light user data
 *   - GLM matrix and vector types
 *
 * \tparam Ts the list of types of passed arguments
 * \param L The lua_State onto which the \p arguments are pushed
 * \param arguments The variable arguments that are pushed to the stack in the order in
 *        which they appear in this function call
 *
 * \pre \p L must not be `nullptr`
 */
template <typename... Ts>
void push(lua_State* L, Ts... arguments);

namespace internal {
    void deinitializeGlobalState();

    /**
     * Handles the extraction of the value, considering the various possible types. The
     * function value() handles the optional variable case, but calls this function to do
     * the actual extraction.
     *
     * \tparam T The type of the return value. If the value at the provided location of
     *           the stack is not T a LuaFormatException is thrown
     * \param L The stack from which the value is extracted
     * \param location The location from which the value should be extracted
     *
     * \throw LuaFormatException If the value at the provided stack location is not T
     * \pre \p L must not be `nullptr`
     */
    template <typename T>
    T valueInner(lua_State* L, int location = 1);
} // namespace internal

} // namespace ghoul::lua

#include "lua_helper.inl"

#endif // __GHOUL___LUA_HELPER___H__
