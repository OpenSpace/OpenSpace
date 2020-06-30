/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <openspace/interaction/websocketinputstate.h>

#include <ghoul/misc/invariants.h>
#include <ghoul/misc/stringconversion.h>
#include <algorithm>
#include <map>
#include <numeric>

namespace openspace::interaction {

float WebsocketInputStates::axis(int axis) const {
    ghoul_precondition(axis >= 0, "axis must be 0 or positive");

    float res = std::accumulate(
        begin(),
        end(),
        0.f,
        [axis](float value,
               const std::pair<const size_t, const WebsocketInputState *> state)
        {
            if (state.second->isConnected) {
                value += state.second->axes[axis];
            }
            return value;
        }
    );

    // If multiple websockets are connected, we might get values outside the -1,1 range by
    // summing them up
    return std::clamp(res, -1.f, 1.f);
}

bool WebsocketInputStates::button(int button, WebsocketAction action) const {
    ghoul_precondition(button >= 0, "button must be 0 or positive");

    bool res = std::any_of(
        begin(),
        end(),
        [button, action](const std::pair<const size_t, const WebsocketInputState *> state)
        {
            return state.second->isConnected ?
                (state.second->buttons[button] == action)
                : false;
        }
    );
    return res;
}

} // namespace openspace::interaction

namespace ghoul {

template <>
std::string to_string(const openspace::interaction::WebsocketAction& action) {
    switch (action) {
        case openspace::interaction::WebsocketAction::Idle:    return "Idle";
        case openspace::interaction::WebsocketAction::Press:   return "Press";
        case openspace::interaction::WebsocketAction::Repeat:  return "Repeat";
        case openspace::interaction::WebsocketAction::Release: return "Release";
        default:                                              return "";
    }
}

template <>
openspace::interaction::WebsocketAction from_string(const std::string& string) {
    static const std::map<std::string, openspace::interaction::WebsocketAction> Map = {
        { "Idle",    openspace::interaction::WebsocketAction::Idle },
        { "Press",   openspace::interaction::WebsocketAction::Press },
        { "Repeat",  openspace::interaction::WebsocketAction::Repeat },
        { "Release", openspace::interaction::WebsocketAction::Release }
    };

    return Map.at(string);

}

} // namespace ghoul
