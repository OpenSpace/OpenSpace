/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/scripting/lualibrary.h>

#include <ghoul/logging/logmanager.h>

namespace openspace::scripting {

bool LuaLibrary::operator<(const LuaLibrary& rhs) const {
    return name < rhs.name;
}

void LuaLibrary::merge(LuaLibrary rhs) {
    for (const LuaLibrary::Function& fun : rhs.functions) {
        const auto itf = std::find_if(
            functions.begin(),
            functions.end(),
            [&fun](const LuaLibrary::Function& function) {
                return fun.name == function.name;
            }
        );
        if (itf != functions.end()) {
            // the function with the desired name is already present, but we don't
            // want to overwrite it
            LERRORC(
                "LuaLibrary",
                std::format(
                    "Lua function '{}' in library '{}' has been defined twice",
                    fun.name, rhs.name
                )
            );
            return;
        }
        else {
            functions.push_back(fun);
        }
    }

    for (LuaLibrary s : rhs.subLibraries) {
        if (s.name.empty()) {
            LERRORC("LuaLibrary", "Sublibraries must have a non-empty name");
        }

        auto it = std::find_if(
            subLibraries.begin(), subLibraries.end(),
            [&s](const LuaLibrary& lib) { return lib.name == s.name; }
        );
        if (it == subLibraries.end()) {
            subLibraries.push_back(std::move(s));
        }
        else {
            it->merge(std::move(s));
        }
    }

    for (const std::filesystem::path& script : rhs.scripts) {
        scripts.push_back(script);
    }
}

} // namespace openspace::scripting
