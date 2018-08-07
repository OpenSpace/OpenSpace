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

#ifndef __OPENSPACE_CORE___LUALIBRARY___H__
#define __OPENSPACE_CORE___LUALIBRARY___H__

#include <ghoul/lua/ghoul_lua.h>
#include <string>
#include <vector>

namespace openspace::scripting {

/**
* This structure represents a Lua library, itself consisting of a unique #name and
* an arbitrary number of #functions
*/
struct LuaLibrary {
    /**
    * This structure represents a Lua function with its #name, #function pointer
    * #argumentText describing the arguments this function takes, and the the #helpText
    * describing the function.
    */
    struct Function {
        /// The name of the function
        std::string name;
        /// The function pointer that is executed if the function is called
        lua_CFunction function;
        /// A vector of light userdata to be passed into the function
        std::vector<void*> userdata;
        /// A text describing the arugments to this function
        std::string argumentText;
        /// A help text describing what the function does/
        std::string helpText;
    };
    /// The name of the library
    std::string name;
    /// The list of all C-based callback functions for this library
    std::vector<Function> functions;
    /// A list of script files that are executed for each Lua state
    std::vector<std::string> scripts = std::vector<std::string>();

    /// This struct contains information about a function or constant that is defined in
    /// a Lua script
    struct Documentation {
        /// The name of the function/variable
        std::string name;
        /// The description of the parameters for a function
        std::string parameter;
        /// The description of the function/variable
        std::string description;
    };
    /// The list of documentations will be populated automatically by parsing the Lua
    /// scripts
    std::vector<Documentation> documentations = std::vector<Documentation>();

    /// Comparison function that compares two LuaLibrary%s name
    bool operator<(const LuaLibrary& rhs) const;
};

} // namespace openspace::scripting

#endif // __OPENSPACE_CORE___LUALIBRARY___H__
