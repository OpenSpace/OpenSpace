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

#ifndef __OPENSPACE_MODULE_SERVER___FLIGHTCONTROLLER_TOPIC___H__
#define __OPENSPACE_MODULE_SERVER___FLIGHTCONTROLLER_TOPIC___H__

#include <modules/server/include/topics/topic.h>

#include <openspace/interaction/websocketinputstate.h>

namespace openspace::interaction {
    struct WebsocketInputStates;
    struct WebsocketInputState;
} // namespace openspace::interaction

namespace openspace {

class FlightControllerTopic : public Topic {
public:
    FlightControllerTopic();
    ~FlightControllerTopic() override;

    void handleJson(const nlohmann::json& json) override;
    bool isDone() const override;

    void engageAutopilot(const nlohmann::json &json);
    void disengageAutopilot() const;
    void handleAutopilot(const nlohmann::json &json);

private:
    bool _isDone = false;
    bool _autopilotEngaged;
    nlohmann::json _payload;
    nlohmann::json _focusNodes;
    nlohmann::json _allNodes;
    nlohmann::json _interestingTimes;

    openspace::interaction::WebsocketInputStates _inputStates;
    openspace::interaction::WebsocketInputState _inputState;

    void connect();
    void disconnect();
    void processInputState(const nlohmann::json& json);
    void setFocusNodes();
    void updateView(const nlohmann::json& json) const;
    void changeFocus(const nlohmann::json& json) const;
    void setRenderableEnabled(const nlohmann::json& json) const;
    void processLua(const nlohmann::json& json);
    void setFriction(const nlohmann::json& json) const;
    void setFriction(bool roll, bool rotation, bool zoom) const;
    void setFriction(bool all) const;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SERVER___FLIGHTCONTROLLER_TOPIC___H__
