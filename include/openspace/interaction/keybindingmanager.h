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

#ifndef __OPENSPACE_CORE___KEYBINDINGMANAGER___H__
#define __OPENSPACE_CORE___KEYBINDINGMANAGER___H__

#include <openspace/util/keys.h>

namespace openspace {
    class Camera;
    class SceneGraphNode;
} // namespace openspace

namespace openspace::scripting { struct LuaLibrary; }

namespace openspace::interaction {

class KeybindingManager {
public:
    void resetKeyBindings();

    void bindKey(Key key, KeyModifier modifier, std::string action);

    void removeKeyBinding(const KeyWithModifier& key);

    std::vector<std::pair<KeyWithModifier, std::string>> keyBinding(
        const KeyWithModifier& key) const;

    static scripting::LuaLibrary luaLibrary();

    void keyboardCallback(Key key, KeyModifier modifier, KeyAction action);

    const std::multimap<KeyWithModifier, std::string>& keyBindings() const;

private:
    std::multimap<KeyWithModifier, std::string> _keyLua;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___KEYBINDINGMANAGER___H__
