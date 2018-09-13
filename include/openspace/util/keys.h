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

// This file is based on the definitions as presented in the GLFW library:
/*************************************************************************
 * GLFW 3.1 - www.glfw.org
 * A library for OpenGL, window and input
 *------------------------------------------------------------------------
 * Copyright (c) 2002-2006 Marcus Geelnard
 * Copyright (c) 2006-2010 Camilla Berglund <elmindreda@elmindreda.org>
 *
 * This software is provided 'as-is', without any express or implied
 * warranty. In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would
 *    be appreciated but is not required.
 *
 * 2. Altered source versions must be plainly marked as such, and must not
 *    be misrepresented as being the original software.
 *
 * 3. This notice may not be removed or altered from any source
 *    distribution.
 *
 *************************************************************************/

#ifndef __OPENSPACE_CORE___KEYS___H__
#define __OPENSPACE_CORE___KEYS___H__

// All values that are defined here are compatible with (and are based on) the
// definitions GLFW v3.1

#include <ghoul/misc/stringconversion.h>
#include <map>
#include <string>

namespace openspace {

enum class KeyAction : int {
    Release = 0,
    Press = 1,
    Repeat = 2
};

bool hasKeyAction(KeyAction lhs, KeyAction rhs);

KeyAction operator|(KeyAction lhs, KeyAction rhs);
KeyAction operator|=(KeyAction& lhs, KeyAction rhs);

enum class KeyModifier : int {
    NoModifier = 0,
    Shift = 0x0001,
    Control = 0x0002,
    Alt = 0x0004,
    Super = 0x0008
};

bool hasKeyModifier(KeyModifier lhs, KeyModifier rhs);

KeyModifier operator|(KeyModifier lhs, KeyModifier rhs);
KeyModifier operator|=(KeyModifier& lhs, KeyModifier rhs);

enum class Key {
    Unknown =           -1,
    Space =             32,
    Apostrophe =        39,
    Comma =             44,
    Minus =             45,
    Period =            46,
    Slash =             47,
    Num0 =              48,
    Num1 =              49,
    Num2 =              50,
    Num3 =              51,
    Num4 =              52,
    Num5 =              53,
    Num6 =              54,
    Num7 =              55,
    Num8 =              56,
    Num9 =              57,
    SemiColon =         59,
    Equal =             61,
    A =                 65,
    B =                 66,
    C =                 67,
    D =                 68,
    E =                 69,
    F =                 70,
    G =                 71,
    H =                 72,
    I =                 73,
    J =                 74,
    K =                 75,
    L =                 76,
    M =                 77,
    N =                 78,
    O =                 79,
    P =                 80,
    Q =                 81,
    R =                 82,
    S =                 83,
    T =                 84,
    U =                 85,
    V =                 86,
    W =                 87,
    X =                 88,
    Y =                 89,
    Z =                 90,
    LeftBracket =       91,
    BackSlash =         92,
    RightBracket =      93,
    GraveAccent =       96,
    World1 =            161,
    World2 =            162,
    Escape =            256,
    Enter =             257,
    Tab =               258,
    BackSpace =         259,
    Insert =            260,
    Delete =            261,
    Right =             262,
    Left =              263,
    Down =              264,
    Up =                265,
    PageUp =            266,
    PageDown =          267,
    Home =              268,
    End =               269,
    CapsLock =          280,
    ScrollLock =        281,
    NumLock =           282,
    PrintScreen =       283,
    Pause =             284,
    F1 =                290,
    F2 =                291,
    F3 =                292,
    F4 =                293,
    F5 =                294,
    F6 =                295,
    F7 =                296,
    F8 =                297,
    F9 =                298,
    F10 =               299,
    F11 =               300,
    F12 =               301,
    F13 =               302,
    F14 =               303,
    F15 =               304,
    F16 =               305,
    F17 =               306,
    F18 =               307,
    F19 =               308,
    F20 =               309,
    F21 =               310,
    F22 =               311,
    F23 =               312,
    F24 =               313,
    F25 =               314,
    Keypad0 =           320,
    Keypad1 =           321,
    Keypad2 =           322,
    Keypad3 =           323,
    Keypad4 =           324,
    Keypad5 =           325,
    Keypad6 =           326,
    Keypad7 =           327,
    Keypad8 =           328,
    Keypad9 =           329,
    KeypadDecimal =     330,
    KeypadDivide =      331,
    KeypadMultiply =    332,
    KeypadSubtract =    333,
    KeypadAdd =         334,
    KeypadEnter =       335,
    LeftShift =         340,
    LeftControl =       341,
    LeftAlt =           342,
    LeftSuper =         343,
    RightShift =        344,
    RightControl =      345,
    RightAlt =          346,
    RightSuper =        347,
    Menu =              348,
    Last =              Menu
};

struct KeyWithModifier {
    Key key;
    KeyModifier modifier;
};

KeyWithModifier stringToKey(std::string str);
bool operator<(const KeyWithModifier& lhs, const KeyWithModifier& rhs);
bool operator==(const KeyWithModifier& lhs, const KeyWithModifier& rhs);

static const std::map<std::string, KeyModifier> KeyModifierMapping = {
    { "SHIFT", KeyModifier::Shift },
    { "ALT", KeyModifier::Alt },
    { "CTRL", KeyModifier::Control },
    { "SUPER", KeyModifier::Super }
};

static const std::map<std::string, Key> KeyMapping = {
    { "0",              Key::Num0 },
    { "1",              Key::Num1 },
    { "2",              Key::Num2 },
    { "3",              Key::Num3 },
    { "4",              Key::Num4 },
    { "5",              Key::Num5 },
    { "6",              Key::Num6 },
    { "7",              Key::Num7 },
    { "8",              Key::Num8 },
    { "9",              Key::Num9 },

    { "A",              Key::A },
    { "B",              Key::B },
    { "C",              Key::C },
    { "D",              Key::D },
    { "E",              Key::E },
    { "F",              Key::F },
    { "G",              Key::G },
    { "H",              Key::H },
    { "I",              Key::I },
    { "J",              Key::J },
    { "K",              Key::K },
    { "L",              Key::L },
    { "M",              Key::M },
    { "N",              Key::N },
    { "O",              Key::O },
    { "P",              Key::P },
    { "Q",              Key::Q },
    { "R",              Key::R },
    { "S",              Key::S },
    { "T",              Key::T },
    { "U",              Key::U },
    { "V",              Key::V },
    { "W",              Key::W },
    { "X",              Key::X },
    { "Y",              Key::Y },
    { "Z",              Key::Z },

    { "LeftBracket",    Key::LeftBracket },
    { "LEFTBRACKET",    Key::LeftBracket },
    { "LEFT_BRACKET",   Key::LeftBracket },
    { "Backslash",      Key::BackSlash },
    { "BACKSLASH",      Key::BackSlash },
    { "RightBracket",   Key::RightBracket },
    { "RIGHTBRACKET",   Key::RightBracket },
    { "RIGHT_BRACKET",  Key::RightBracket },
    { "GraveAccent",    Key::GraveAccent },
    { "GRAVEACCENT",    Key::GraveAccent },
    { "GRAVE_ACCENT",   Key::GraveAccent },
    { "Space",          Key::Space },
    { "SPACE",          Key::Space },
    { "Apostrophe",     Key::Apostrophe },
    { "APOSTROPHE",     Key::Apostrophe },
    { "Comma",          Key::Comma },
    { "COMMA",          Key::Comma },
    { "Minus",          Key::Minus },
    { "MINUS",          Key::Minus },
    { "Period",         Key::Period },
    { "PERIOD",         Key::Period },
    { "Slash",          Key::Slash },
    { "SLASH",          Key::Slash },
    { "Semicolon",      Key::SemiColon },
    { "SEMICOLON",      Key::SemiColon },
    { "SEMI COLON",     Key::SemiColon },
    { "Equal",          Key::Equal },
    { "EQUAL",          Key::Equal },

    { "World1",         Key::World1 },
    { "WORLD1",         Key::World1 },
    { "WORLD_1",        Key::World1 },
    { "World2",         Key::World2 },
    { "WORLD2",         Key::World2 },
    { "WORLD_2",        Key::World2 },
    { "Esc",            Key::Escape },
    { "ESC",            Key::Escape },
    { "ESCAPE",         Key::Escape},
    { "Enter",          Key::Enter },
    { "ENTER",          Key::Enter },
    { "Tab",            Key::Tab },
    { "TAB",            Key::Tab },
    { "Backspace",      Key::BackSpace },
    { "BACKSPACE",      Key::BackSpace },
    { "Insert",         Key::Insert },
    { "INSERT",         Key::Insert },
    { "DEL",            Key::Delete },
    { "Delete",         Key::Delete },
    { "DELETE",         Key::Delete },
    { "Right",          Key::Right },
    { "RIGHT",          Key::Right },
    { "Left",           Key::Left },
    { "LEFT",           Key::Left },
    { "Down",           Key::Down },
    { "DOWN",           Key::Down },
    { "Up",             Key::Up },
    { "UP",             Key::Up },
    { "PageUp",         Key::PageUp },
    { "PAGEUP",         Key::PageUp },
    { "PAGE_UP",        Key::PageUp },
    { "PageDown",       Key::PageDown },
    { "PAGEDOWN",       Key::PageDown },
    { "PAGE_DOWN",      Key::PageDown },
    { "Home",           Key::Home },
    { "HOME",           Key::Home },
    { "End",            Key::End },
    { "END",            Key::End },
    { "CapsLock",       Key::CapsLock },
    { "CAPSLOCK",       Key::CapsLock },
    { "CAPS_LOCK",      Key::CapsLock },
    { "ScrollLock",     Key::ScrollLock },
    { "SCROLLLOCK",     Key::ScrollLock },
    { "SCROLL_LOCK",    Key::ScrollLock },
    { "NumLock",        Key::NumLock },
    { "NUMLOCK",        Key::NumLock },
    { "NUM_LOCK",       Key::NumLock },
    { "PrintScreen",    Key::PrintScreen },
    { "PRINTSCREEN",    Key::PrintScreen },
    { "PRINT_SCREEN",   Key::PrintScreen },
    { "Pause",          Key::Pause },
    { "PAUSE",          Key::Pause },
    { "F1",             Key::F1 },
    { "F2",             Key::F2 },
    { "F3",             Key::F3 },
    { "F4",             Key::F4 },
    { "F5",             Key::F5 },
    { "F6",             Key::F6 },
    { "F7",             Key::F7 },
    { "F8",             Key::F8 },
    { "F9",             Key::F9 },
    { "F10",            Key::F10 },
    { "F11",            Key::F11 },
    { "F12",            Key::F12 },
    { "F13",            Key::F13 },
    { "F14",            Key::F14 },
    { "F15",            Key::F15 },
    { "F16",            Key::F16 },
    { "F17",            Key::F17 },
    { "F18",            Key::F18 },
    { "F19",            Key::F19 },
    { "F20",            Key::F20 },
    { "F21",            Key::F21 },
    { "F22",            Key::F22 },
    { "F23",            Key::F23 },
    { "F24",            Key::F24 },
    { "F25",            Key::F25 },
    { "Keypad0",        Key::Keypad0 },
    { "KP0",            Key::Keypad0 },
    { "KP_0",           Key::Keypad0 },
    { "Keypad1",        Key::Keypad1 },
    { "KP1",            Key::Keypad1 },
    { "KP_1",           Key::Keypad1 },
    { "Keypad2",        Key::Keypad2 },
    { "KP2",            Key::Keypad2 },
    { "KP_2",           Key::Keypad2 },
    { "Keypad3",        Key::Keypad3 },
    { "KP3",            Key::Keypad3 },
    { "KP_3",           Key::Keypad3 },
    { "Keypad4",        Key::Keypad4 },
    { "KP4",            Key::Keypad4 },
    { "KP_4",           Key::Keypad4 },
    { "Keypad5",        Key::Keypad5 },
    { "KP5",            Key::Keypad5 },
    { "KP_5",           Key::Keypad5 },
    { "Keypad6",        Key::Keypad6 },
    { "KP6",            Key::Keypad6 },
    { "KP_6",           Key::Keypad6 },
    { "Keypad7",        Key::Keypad7 },
    { "KP7",            Key::Keypad7 },
    { "KP_7",           Key::Keypad7 },
    { "Keypad8",        Key::Keypad8 },
    { "KP8",            Key::Keypad8 },
    { "KP_8",           Key::Keypad8 },
    { "Keypad9",        Key::Keypad9 },
    { "KP9",            Key::Keypad9 },
    { "KP_9",           Key::Keypad9 },
    { "KeypadDecimal",  Key::KeypadDecimal },
    { "KPDECIMAL",      Key::KeypadDecimal },
    { "KP_DECIMAL",     Key::KeypadDecimal },
    { "KeypadDivide",   Key::KeypadDivide },
    { "KPDIVIDE",       Key::KeypadDivide },
    { "KP_DIVIDE",      Key::KeypadDivide },
    { "KeypadMultiply", Key::KeypadMultiply },
    { "KPMULTIPLY",     Key::KeypadMultiply },
    { "KP_MULTIPLY",    Key::KeypadMultiply },
    { "KeypadSubtract", Key::KeypadSubtract },
    { "KPSUBTRACT",     Key::KeypadSubtract },
    { "KP_SUBTRACT",    Key::KeypadSubtract },
    { "KeypadAdd",      Key::KeypadAdd },
    { "KPADD",          Key::KeypadAdd },
    { "KP_ADD",         Key::KeypadAdd },
    { "KeypadEnter",    Key::KeypadEnter },
    { "KPENTER",        Key::KeypadEnter },
    { "KP_ENTER",       Key::KeypadEnter },
    { "KeypadEqual",    Key::KeypadEnter },
    { "KPEQUAL",        Key::KeypadEnter },
    { "KP_EQUAL",       Key::KeypadEnter },
    { "LeftShift",      Key::LeftShift },
    { "LSHIFT",         Key::LeftShift },
    { "LEFTSHIFT",      Key::LeftShift },
    { "LEFT_SHIFT",     Key::LeftShift },
    { "LeftControl",    Key::LeftControl },
    { "LCTRL",          Key::LeftControl },
    { "LEFTCONTROL",    Key::LeftControl },
    { "LEFT_CONTROL",   Key::LeftControl },
    { "LeftAlt",        Key::LeftAlt },
    { "LALT",           Key::LeftAlt },
    { "LEFTALT",        Key::LeftAlt },
    { "LEFT_ALT",       Key::LeftAlt },
    { "LeftSuper",      Key::LeftSuper },
    { "LEFTSUPER",      Key::LeftSuper },
    { "LEFT_SUPER",     Key::LeftSuper },
    { "RightShift",     Key::RightShift },
    { "RSHIFT",         Key::RightShift },
    { "RIGHTSHIFT",     Key::RightShift },
    { "RIGHT_SHIFT",    Key::RightShift },
    { "RightControl",   Key::RightControl },
    { "RCTRL",          Key::RightControl },
    { "RIGHTCONTROL",   Key::RightControl },
    { "RIGHT_CONTROL",  Key::RightControl },
    { "RightAlt",       Key::RightAlt },
    { "RALT",           Key::RightAlt },
    { "RIGHTALT",       Key::RightAlt },
    { "RIGHT_ALT",      Key::RightAlt },
    { "RightSuper",     Key::RightSuper },
    { "RIGHTSUPER",     Key::RightSuper },
    { "RIGHT_SUPER",    Key::RightSuper },
    { "Menu",           Key::Menu },
    { "MENU",           Key::Menu }
};

} // namespace openspace

namespace ghoul {

template <>
std::string to_string(const openspace::Key& key);

template <>
std::string to_string(const openspace::KeyModifier& mod);

template <>
std::string to_string(const openspace::KeyWithModifier& key);

} // namespace ghoul

#endif // __OPENSPACE_CORE___KEYS___H__
