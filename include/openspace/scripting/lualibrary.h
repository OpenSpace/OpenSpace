/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_CORE___LUALIBRARY___H__
#define __OPENSPACE_CORE___LUALIBRARY___H__

#include <filesystem>
#include <string>
#include <vector>

struct lua_State;

namespace openspace::scripting {

/**
 * This structure represents a Lua library, itself consisting of a unique #name and an
 * arbitrary number of #functions.
 */
struct LuaLibrary {
    /**
     * This structure represents a Lua function with its #name, #function pointer
     * #arguments describe the arguments this function takes, and the #helpText describing
     * the function.
     */
    struct Function {
        using lua_CFunction =int(*)(lua_State* L);

        /// The name of the function
        std::string name;
        /// The function pointer that is executed if the function is called
        lua_CFunction function = nullptr;
        struct Argument {
            /// The name of the arguments
            std::string name;
            /// The type of the argument
            std::string type;
            /// The default value if it exists
            std::optional<std::string> defaultValue = std::nullopt;
        };
        /// The ordered arguments that this function takes
        std::vector<Argument> arguments;
        /// Information about the type that this function returns
        std::string returnType;
        /// A help text describing what the function does/
        std::string helpText;
        /// The source location where the implementation for this Lua file is located
        struct {
            std::string file = "<none>";
            int line = 0;
        } sourceLocation;
    };
    /// The name of the library
    std::string name;
    /// The list of all C-based callback functions for this library
    std::vector<Function> functions;
    /// A list of script files that are executed for each Lua state
    std::vector<std::filesystem::path> scripts = std::vector<std::filesystem::path>();
    /// A list of all libraries that are children for this library
    std::vector<LuaLibrary> subLibraries = std::vector<LuaLibrary>();

    /// The list of documentations will be populated automatically by parsing the Lua
    /// scripts
    std::vector<Function> documentations = std::vector<Function>();

    /// Comparison function that compares two LuaLibrary%s name
    bool operator<(const LuaLibrary& rhs) const;

    /// Merges the contents of a second LuaLibrary into this LuaLibrary
    void merge(LuaLibrary rhs);
};

} // namespace openspace::scripting

#endif // __OPENSPACE_CORE___LUALIBRARY___H__
