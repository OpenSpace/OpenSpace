/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_CORE___ACTIONMANAGER___H__
#define __OPENSPACE_CORE___ACTIONMANAGER___H__

#include <openspace/interaction/action.h>
#include <unordered_map>

namespace ghoul { class Dictionary; }
namespace openspace::scripting { struct LuaLibrary; }

namespace openspace::interaction {

class ActionManager {
public:
    bool hasAction(const std::string& identifier) const;
    void registerAction(Action action);
    void removeAction(const std::string& identifier);
    const Action& action(const std::string& identifier) const;
    std::vector<Action> actions() const;

    void triggerAction(const std::string& identifier,
        const ghoul::Dictionary& arguments) const;
    static scripting::LuaLibrary luaLibrary();

private:
    std::unordered_map<unsigned int, Action> _actions;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___ACTIONMANAGER___H__
