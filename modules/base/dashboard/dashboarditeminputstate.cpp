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

#include <modules/base/dashboard/dashboarditeminputstate.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/misc/stringhelper.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo ShowWhenEnabledInfo = {
        "ShowWhenEnabled",
        "Show when enabled",
        "Show text when the input is enabled.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ShowWhenDisabledInfo = {
        "ShowWhenDisabled",
        "Show when disabled",
        "Show text when the input is disabled.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ShowKeyboardInfo = {
        "ShowKeyboard",
        "Show Keyboard information",
        "Display the state of the keyboard input.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ShowMouseInfo = {
        "ShowMouse",
        "Show Mouse information",
        "Display the state of the mouse input.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ShowJoystickInfo = {
        "ShowJoystick",
        "Show Joystick information",
        "Display the state of the joystick input.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(DashboardItemPropertyValue)]] Parameters {
        // [[codegen::verbatim(ShowWhenEnabledInfo.description)]]
        std::optional<bool> showWhenEnabled;

        // [[codegen::verbatim(ShowWhenDisabledInfo.description)]]
        std::optional<bool> showWhenDisabled;

        // [[codegen::verbatim(ShowKeyboardInfo.description)]]
        std::optional<bool> showKeyboard;

        // [[codegen::verbatim(ShowMouseInfo.description)]]
        std::optional<bool> showMouse;

        // [[codegen::verbatim(ShowJoystickInfo.description)]]
        std::optional<bool> showJoystick;
    };
#include "dashboarditeminputstate_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardItemInputState::Documentation() {
    return codegen::doc<Parameters>(
        "base_dashboarditem_inputstate",
        DashboardTextItem::Documentation()
    );
}

DashboardItemInputState::DashboardItemInputState(const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary)
    , _showWhenEnabled(ShowWhenEnabledInfo, true)
    , _showWhenDisabled(ShowWhenDisabledInfo, true)
    , _showKeyboard(ShowKeyboardInfo, true)
    , _showMouse(ShowMouseInfo, true)
    , _showJoystick(ShowJoystickInfo, true)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _showWhenEnabled = p.showWhenEnabled.value_or(_showWhenEnabled);
    addProperty(_showWhenEnabled);

    _showWhenDisabled = p.showWhenDisabled.value_or(_showWhenDisabled);
    addProperty(_showWhenDisabled);

    _showKeyboard = p.showKeyboard.value_or(_showKeyboard);
    addProperty(_showKeyboard);

    _showMouse = p.showMouse.value_or(_showMouse);
    addProperty(_showMouse);

    _showJoystick = p.showJoystick.value_or(_showJoystick);
    addProperty(_showJoystick);
}

void DashboardItemInputState::render(glm::vec2& penPosition) {
    ZoneScoped;

    std::vector<std::string> text;
    if (_showKeyboard) {
        if (global::navigationHandler->disabledKeybindings()) {
            if (_showWhenDisabled) {
                text.emplace_back("Keyboard shortcuts disabled");
            }
        }
        else {
            if (_showWhenEnabled) {
                text.emplace_back("Keyboard shortcuts enabled");
            }
        }
    }

    if (_showMouse) {
        if (global::navigationHandler->disabledMouse()) {
            if (_showWhenDisabled) {
                text.emplace_back("Mouse input disabled");
            }
        }
        else {
            if (_showWhenEnabled) {
                text.emplace_back("Mouse input enabled");
            }
        }
    }

    if (_showJoystick) {
        if (global::navigationHandler->disabledJoystick()) {
            if (_showWhenDisabled) {
                text.emplace_back("Joystick input disabled");
            }
        }
        else {
            if (_showWhenEnabled) {
                text.emplace_back("Joystick input enabled");
            }
        }
    }

    if (!text.empty()) {
        penPosition.y -= _font->height();
        const std::string t = ghoul::join(std::move(text), "\n");
        RenderFont(*_font, penPosition, t);
    }
}

glm::vec2 DashboardItemInputState::size() const {
    ZoneScoped;

    std::vector<std::string> text;
    if (_showKeyboard) {
        if (global::navigationHandler->disabledKeybindings()) {
            if (_showWhenDisabled) {
                text.emplace_back("Keyboard shortcuts disabled");
            }
        }
        else {
            if (_showWhenEnabled) {
                text.emplace_back("Keyboard shortcuts enabled");
            }
        }
    }

    if (_showMouse) {
        if (global::navigationHandler->disabledMouse()) {
            if (_showWhenDisabled) {
                text.emplace_back("Mouse input disabled");
            }
        }
        else {
            if (_showWhenEnabled) {
                text.emplace_back("Mouse input disabled");
            }
        }
    }

    if (_showJoystick) {
        if (global::navigationHandler->disabledJoystick()) {
            if (_showWhenDisabled) {
                text.emplace_back("Joystick input disabled");
            }
        }
        else {
            if (_showWhenEnabled) {
                text.emplace_back("Joystick input disabled");
            }
        }
    }

    if (!text.empty()) {
        const std::string t = ghoul::join(std::move(text), "\n");
        return _font->boundingBox(t);
    }
    else {
        return glm::vec2(0.f, 0.f);
    }
}

} // namespace openspace
