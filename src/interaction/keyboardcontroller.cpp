/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/interaction/keyboardcontroller.h>

#include <openspace/interaction/interactionhandler.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/time.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>

#include <chrono>

namespace {
    const std::string _loggerCat = "KeyboardController";
}

namespace openspace {
namespace interaction {

void KeyboardControllerFixed::keyPressed(KeyAction action, Key key, KeyModifier modifier) {
    // TODO package in script
    /*
    const float dt = static_cast<float>( _handler->deltaTime());
    if(action == KeyAction::Press|| action == KeyAction::Repeat) {
        const float speed = 2.75;
        if (key == Key::S) {
            glm::vec3 euler(speed * dt, 0.0, 0.0);
            glm::quat rot = glm::quat(euler);
            _handler->orbitDelta(rot);
        }
        if (key == Key::W) {
            glm::vec3 euler(-speed * dt, 0.0, 0.0);
            glm::quat rot = glm::quat(euler);
            _handler->orbitDelta(rot);
        }
        if (key == Key::A) {
            glm::vec3 euler(0.0, -speed * dt, 0.0);
            glm::quat rot = glm::quat(euler);
            _handler->orbitDelta(rot);
        }
        if (key == Key::D) {
            glm::vec3 euler(0.0, speed * dt, 0.0);
            glm::quat rot = glm::quat(euler);
            _handler->orbitDelta(rot);
        }
        if (key == Key::Q) {
            Time::ref().advanceTime(dt);
        }
        if (key == Key::Right) {
            glm::vec3 euler(0.0, speed * dt, 0.0);
            glm::quat rot = glm::quat(euler);
            _handler->rotateDelta(rot);
        }
        if (key == Key::Left) {
            glm::vec3 euler(0.0, -speed * dt, 0.0);
            glm::quat rot = glm::quat(euler);
            _handler->rotateDelta(rot);
        }
        if (key == Key::Down) {
            glm::vec3 euler(speed * dt, 0.0, 0.0);
            glm::quat rot = glm::quat(euler);
            _handler->rotateDelta(rot);
        }
        if (key == Key::Up) {
            glm::vec3 euler(-speed * dt, 0.0, 0.0);
            glm::quat rot = glm::quat(euler);
            _handler->rotateDelta(rot);
        }
        if (key == Key::R) {
            PowerScaledScalar dist(-speed * dt, 0.0);
            _handler->distanceDelta(dist);
        }
        if (key == Key::F) {
            PowerScaledScalar dist(speed * dt, 0.0);
            _handler->distanceDelta(dist);
        }
        if (key == Key::T) {
            PowerScaledScalar dist(-speed * pow(10.0f, 11.0f) * dt, 0.0f);
            _handler->distanceDelta(dist);
        }
        //if (key == Keys::G) {
        //    acc += 0.001;
        //    PowerScaledScalar dist(speed * pow(10, 8 * acc) * dt, 0.0);
        //    distanceDelta(dist);
        //}
        if (key == Key::Y) {
            PowerScaledScalar dist(-speed * 100.0f * dt, 6.0f);
            _handler->distanceDelta(dist);
        }
        if (key == Key::H) {
            PowerScaledScalar dist(speed * 100.0f * dt, 6.0f);
            _handler->distanceDelta(dist);
        }
    
        if (key == Key::KeypadSubtract) {
            glm::vec2 s = OsEng.renderEngine().camera()->scaling();
            s[1] -= 0.5;
            OsEng.renderEngine().camera()->setScaling(s);
        }
        if (key == Key::KeypadAdd) {
            glm::vec2 s = OsEng.renderEngine().camera()->scaling();
            s[1] += 0.5;
            OsEng.renderEngine().camera()->setScaling(s);
        }
    }
    */
    /*
    if (key == '1') {
        SceneGraphNode* node = getSceneGraphNode("sun");
    
        setFocusNode(node);
        getCamera()->setPosition(node->getWorldPosition() + psc(0.0, 0.0, 0.5, 10.0));
        getCamera()->setCameraDirection(glm::vec3(0.0, 0.0, -1.0));
    }
    
    if (key == '2') {
        SceneGraphNode* node = getSceneGraphNode("earth");
    
        setFocusNode(node);
        getCamera()->setPosition(node->getWorldPosition() + psc(0.0, 0.0, 1.0, 8.0));
        getCamera()->setCameraDirection(glm::vec3(0.0, 0.0, -1.0));
    }
    
    
    if (key == '3') {
        SceneGraphNode* node = getSceneGraphNode("moon");
    
        setFocusNode(node);
        getCamera()->setPosition(node->getWorldPosition() + psc(0.0, 0.0, 0.5, 8.0));
        getCamera()->setCameraDirection(glm::vec3(0.0, 0.0, -1.0));
    }
    */
}

void KeyboardControllerLua::keyPressed(KeyAction action, Key key, KeyModifier modifier) {
    lua_State* s = luaL_newstate();
    luaL_openlibs(s);
    
    int status = luaL_loadfile(s, absPath("${SCRIPTS}/default_keybinding.lua").c_str());
    if (status != LUA_OK) {
        LERROR("Error loading script: '" << lua_tostring(s, -1) << "'");
        return;
    }

    if (lua_pcall(s, 0, LUA_MULTRET, 0)) {
        LERROR("Error executing script: " << lua_tostring(s, -1));
        return;
    }

    auto start = std::chrono::high_resolution_clock::now();

    lua_getfield(s, -1, keyToString(key, modifier).c_str());
    if (!lua_isnil(s, -1))
        lua_pcall(s, 0, 0, 0);
    else
        LINFO("Key not found");

    auto end = std::chrono::high_resolution_clock::now();
    LINFO("Keyboard timing: " << std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count() << "ns");


}

std::string KeyboardControllerLua::keyToString(Key key, KeyModifier mod) const {
    std::string result = "";
    int intMod = static_cast<int>(mod);
    if (intMod & static_cast<int>(KeyModifier::Control))
        result += "CTRL + ";
    if (intMod & static_cast<int>(KeyModifier::Super))
        result += "SUPER + ";
    if (intMod & static_cast<int>(KeyModifier::Alt))
        result += "ALT + ";
    if (intMod & static_cast<int>(KeyModifier::Shift))
        result += "SHIFT + ";

    switch (key) {
        case Key::Unknown:            result += "Unknown";        break;
        case Key::Space:            result += "Space";            break;
        case Key::Apostrophe:        result += "Apostrophe";        break;
        case Key::Comma:            result += "Comma";            break;
        case Key::Minus:            result += "Minus";            break;
        case Key::Period:            result += "Period";            break;
        case Key::Slash:            result += "Slash";            break;
        case Key::Num0:                result += "0";                break;
        case Key::Num1:                result += "1";                break;
        case Key::Num2:                result += "2";                break;
        case Key::Num3:                result += "3";                break;
        case Key::Num4:                result += "4";                break;
        case Key::Num5:                result += "5";                break;
        case Key::Num6:                result += "6";                break;
        case Key::Num7:                result += "7";                break;
        case Key::Num8:                result += "8";                break;
        case Key::Num9:                result += "9";                break;
        case Key::SemiColon:        result += "SemiColon";        break;
        case Key::Equal:            result += "Equal";            break;
        case Key::A:                result += "A";                break;
        case Key::B:                result += "B";                break;
        case Key::C:                result += "C";                break;
        case Key::D:                result += "D";                break;
        case Key::E:                result += "E";                break;
        case Key::F:                result += "F";                break;
        case Key::G:                result += "G";                break;
        case Key::H:                result += "H";                break;
        case Key::I:                result += "I";                break;
        case Key::J:                result += "J";                break;
        case Key::K:                result += "K";                break;
        case Key::L:                result += "L";                break;
        case Key::M:                result += "M";                break;
        case Key::N:                result += "N";                break;
        case Key::O:                result += "O";                break;
        case Key::P:                result += "P";                break;
        case Key::Q:                result += "Q";                break;
        case Key::R:                result += "R";                break;
        case Key::S:                result += "S";                break;
        case Key::T:                result += "T";                break;
        case Key::U:                result += "U";                break;
        case Key::V:                result += "V";                break;
        case Key::W:                result += "W";                break;
        case Key::X:                result += "X";                break;
        case Key::Y:                result += "Y";                break;
        case Key::Z:                result += "Z";                break;
        case Key::LeftBracket:        result += "LeftBracket";    break;
        case Key::BackSlash:        result += "BackSlash";        break;
        case Key::RightBracket:        result += "RightBracket";    break;
        case Key::GraveAccent:        result += "GraveAccent";    break;
        case Key::World1:            result += "World1";            break;
        case Key::World2:            result += "World2";            break;
        case Key::Escape:            result += "Escape";            break;
        case Key::Enter:            result += "Enter";            break;
        case Key::Tab:                result += "Tab";            break;
        case Key::BackSpace:        result += "BackSpace";        break;
        case Key::Insert:            result += "Insert";            break;
        case Key::Delete:            result += "Delete";            break;
        case Key::Right:            result += "Right";            break;
        case Key::Left:                result += "Left";            break;
        case Key::Down:                result += "Down";            break;
        case Key::Up:                result += "Up";                break;
        case Key::PageUp:            result += "PageUp";            break;
        case Key::PageDown:            result += "PageDown";        break;
        case Key::Home:                result += "Home";            break;
        case Key::End:                result += "End";            break;
        case Key::CapsLock:            result += "CapsLock";        break;
        case Key::ScrollLock:        result += "ScrollLock";        break;
        case Key::NumLock:            result += "NumLock";        break;
        case Key::PrintScreen:        result += "PrintScreen";    break;
        case Key::Pause:            result += "Pause";            break;
        case Key::F1:                result += "F1";                break;
        case Key::F2:                result += "F2";                break;
        case Key::F3:                result += "F3";                break;
        case Key::F4:                result += "F4";                break;
        case Key::F5:                result += "F5";                break;
        case Key::F6:                result += "F6";                break;
        case Key::F7:                result += "F7";                break;
        case Key::F8:                result += "F8";                break;
        case Key::F9:                result += "F9";                break;
        case Key::F10:                result += "F10";            break;
        case Key::F11:                result += "F11";            break;
        case Key::F12:                result += "F12";            break;
        case Key::F13:                result += "F13";            break;
        case Key::F14:                result += "F14";            break;
        case Key::F15:                result += "F15";            break;
        case Key::F16:                result += "F16";            break;
        case Key::F17:                result += "F17";            break;
        case Key::F18:                result += "F18";            break;
        case Key::F19:                result += "F19";            break;
        case Key::F20:                result += "F20";            break;
        case Key::F21:                result += "F21";            break;
        case Key::F22:                result += "F22";            break;
        case Key::F23:                result += "F23";            break;
        case Key::F24:                result += "F24";            break;
        case Key::F25:                result += "F25";            break;
        case Key::Keypad0:            result += "Keypad0";        break;
        case Key::Keypad1:            result += "Keypad1";        break;
        case Key::Keypad2:            result += "Keypad2";        break;
        case Key::Keypad3:            result += "Keypad3";        break;
        case Key::Keypad4:            result += "Keypad4";        break;
        case Key::Keypad5:            result += "Keypad5";        break;
        case Key::Keypad6:            result += "Keypad6";        break;
        case Key::Keypad7:            result += "Keypad7";        break;
        case Key::Keypad8:            result += "Keypad8";        break;
        case Key::Keypad9:            result += "Keypad9";        break;
        case Key::KeypadDecimal:    result += "KeypadDecimal";    break;
        case Key::KeypadDivide:        result += "KeypadDivide";    break;
        case Key::KeypadMultiply:    result += "KeypadMultiply";    break;
        case Key::KeypadSubtract:    result += "KeypadSubtract";    break;
        case Key::KeypadAdd:        result += "KeypadAdd";        break;
        case Key::KeypadEnter:        result += "KeypadEnter";    break;
        case Key::LeftShift:        result += "LeftShift";        break;
        case Key::LeftControl:        result += "LeftControl";    break;
        case Key::LeftAlt:            result += "LeftAlt";        break;
        case Key::LeftSuper:        result += "LeftSuper";        break;
        case Key::RightShift:        result += "RightShift";        break;
        case Key::RightControl:        result += "RightControl";    break;
        case Key::RightAlt:            result += "RightAlt";        break;
        case Key::RightSuper:        result += "RightSuper";        break;
        case Key::Menu:                result += "Menu";            break;
        default:
            assert(false);
    }
    return result;
}

} // namespace interaction
} // namespace openspace