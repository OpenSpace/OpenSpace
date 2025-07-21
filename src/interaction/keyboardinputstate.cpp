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

#include <openspace/interaction/keyboardinputstate.h>

#include <algorithm>

namespace openspace::interaction {

void KeyboardInputState::keyboardCallback(Key key, KeyModifier modifier,
                                          KeyAction action)
{
    if (action == KeyAction::Press) {
        _keysDown.emplace_back(key, modifier);
    }
    else if (action == KeyAction::Release) {
        // Remove all key pressings for 'key'
        _keysDown.erase(
            std::remove_if(
                _keysDown.begin(),
                _keysDown.end(),
                [key](const std::pair<Key, KeyModifier>& keyModPair) {
                    return keyModPair.first == key;
                }
            ),
            _keysDown.end()
        );
    }
}

const std::vector<std::pair<Key, KeyModifier>>& KeyboardInputState::pressedKeys() const {
    return _keysDown;
}

bool KeyboardInputState::isKeyPressed(std::pair<Key, KeyModifier> keyModPair) const {
    return std::find(_keysDown.begin(), _keysDown.end(), keyModPair) != _keysDown.end();
}

bool KeyboardInputState::isKeyPressed(Key key) const {
    auto it = std::find_if(
        _keysDown.begin(),
        _keysDown.end(),
        [key](const std::pair<Key, KeyModifier>& keyModPair) {
            return key == keyModPair.first;
        }
    );
    return it != _keysDown.end();
}

} // namespace openspace::interaction
