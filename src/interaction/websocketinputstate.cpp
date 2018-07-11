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

#include <openspace/interaction/websocketinputstate.h>

#include <ghoul/glm.h>
#include <ghoul/misc/invariants.h>
#include <algorithm>
#include <map>
#include <numeric>

namespace openspace::interaction {

float WebsocketInputStates::axis(int axis) const {
    ghoul_precondition(axis >= 0, "axis must be 0 or positive");

    float res = std::accumulate(
        begin(),
        begin()+1,
        0.f,
        [axis](float value, const WebsocketInputState& state) {
            if (state.isConnected) {
                value += state.axes[axis];
            }
            return value;
        }
    );

    // If multiple websockets are connected, we might get values outside the -1,1 range by
    // summing them up
    glm::clamp(res, -1.f, 1.f);
    return res;
}

bool WebsocketInputStates::button(int button, WebsocketAction action) const {
    ghoul_precondition(button >= 0, "button must be 0 or positive");

    bool res = std::any_of(
        begin(),
        end(),
        [button, action](const WebsocketInputState& state) {
            return state.isConnected ? (state.buttons[button] == action) : false;
        }
    );
    return res;
}

} // namespace openspace::interaction

namespace std {

std::string to_string(openspace::interaction::WebsocketAction action) {
    switch (action) {
        case openspace::interaction::WebsocketAction::Idle:    return "Idle";
        case openspace::interaction::WebsocketAction::Press:   return "Press";
        case openspace::interaction::WebsocketAction::Repeat:  return "Repeat";
        case openspace::interaction::WebsocketAction::Release: return "Release";
        default:                                              return "";
    }
}

} // namespace std

namespace ghoul {

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
