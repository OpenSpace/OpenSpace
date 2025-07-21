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
#include <array>
#include <map>
#include <string>
#include <unordered_map>

namespace openspace {

//////////////////////////////////////////////////////////////////////////////////////////

enum class KeyAction : uint8_t {
    Release = 0,
    Press = 1,
    Repeat = 2
};

constexpr bool hasKeyAction(KeyAction lhs, KeyAction rhs) {
    return static_cast<std::underlying_type_t<KeyAction>>(lhs) &
        static_cast<std::underlying_type_t<KeyAction>>(rhs);
}

constexpr KeyAction operator|(KeyAction lhs, KeyAction rhs) {
    return static_cast<KeyAction>(
        static_cast<std::underlying_type_t<KeyAction>>(lhs) |
        static_cast<std::underlying_type_t<KeyAction>>(rhs)
    );
}

constexpr KeyAction operator|=(KeyAction& lhs, KeyAction rhs) {
    return (lhs | rhs);
}

//////////////////////////////////////////////////////////////////////////////////////////

enum class KeyModifier : uint8_t {
    None       = 0x00,
    Shift      = 0x01,
    Control    = 0x02,
    Alt        = 0x04,
    Super      = 0x08
};

constexpr KeyModifier operator|(KeyModifier lhs, KeyModifier rhs) {
    return static_cast<KeyModifier>(
        static_cast<std::underlying_type_t<KeyModifier>>(lhs) |
        static_cast<std::underlying_type_t<KeyModifier>>(rhs)
        );
}

constexpr KeyModifier operator|=(KeyModifier& lhs, KeyModifier rhs) {
    return lhs = (lhs | rhs);
}

struct KeyModifierInfo {
    KeyModifier modifier;
    std::string_view name;
    std::string_view identifier;
};

constexpr std::array<KeyModifierInfo, 5> KeyModifierInfos = {
    KeyModifierInfo{ KeyModifier::None,       "",        ""      },
    KeyModifierInfo{ KeyModifier::Shift,      "Shift",   "SHIFT" },
    KeyModifierInfo{ KeyModifier::Control,    "Control", "CTRL"  },
    KeyModifierInfo{ KeyModifier::Alt,        "Alt",     "ALT"   },
    KeyModifierInfo{ KeyModifier::Super,      "Super",   "SUPER" },
};

constexpr bool hasKeyModifier(KeyModifier lhs, KeyModifier rhs) {
    return static_cast<std::underlying_type_t<KeyModifier>>(lhs) &
        static_cast<std::underlying_type_t<KeyModifier>>(rhs);
}

//////////////////////////////////////////////////////////////////////////////////////////

enum class Key : uint16_t {
    Unknown        =  uint16_t(-1),
    Space          =  32,
    Apostrophe     =  39,
    Comma          =  44,
    Minus          =  45,
    Period         =  46,
    Slash          =  47,
    Num0           =  48,
    Num1           =  49,
    Num2           =  50,
    Num3           =  51,
    Num4           =  52,
    Num5           =  53,
    Num6           =  54,
    Num7           =  55,
    Num8           =  56,
    Num9           =  57,
    SemiColon      =  59,
    Equal          =  61,
    A              =  65,
    B              =  66,
    C              =  67,
    D              =  68,
    E              =  69,
    F              =  70,
    G              =  71,
    H              =  72,
    I              =  73,
    J              =  74,
    K              =  75,
    L              =  76,
    M              =  77,
    N              =  78,
    O              =  79,
    P              =  80,
    Q              =  81,
    R              =  82,
    S              =  83,
    T              =  84,
    U              =  85,
    V              =  86,
    W              =  87,
    X              =  88,
    Y              =  89,
    Z              =  90,
    LeftBracket    =  91,
    BackSlash      =  92,
    RightBracket   =  93,
    GraveAccent    =  96,
    World1         = 161,
    World2         = 162,
    Escape         = 256,
    Enter          = 257,
    Tab            = 258,
    BackSpace      = 259,
    Insert         = 260,
    Delete         = 261,
    Right          = 262,
    Left           = 263,
    Down           = 264,
    Up             = 265,
    PageUp         = 266,
    PageDown       = 267,
    Home           = 268,
    End            = 269,
    CapsLock       = 280,
    ScrollLock     = 281,
    NumLock        = 282,
    PrintScreen    = 283,
    Pause          = 284,
    F1             = 290,
    F2             = 291,
    F3             = 292,
    F4             = 293,
    F5             = 294,
    F6             = 295,
    F7             = 296,
    F8             = 297,
    F9             = 298,
    F10            = 299,
    F11            = 300,
    F12            = 301,
    F13            = 302,
    F14            = 303,
    F15            = 304,
    F16            = 305,
    F17            = 306,
    F18            = 307,
    F19            = 308,
    F20            = 309,
    F21            = 310,
    F22            = 311,
    F23            = 312,
    F24            = 313,
    F25            = 314,
    Keypad0        = 320,
    Keypad1        = 321,
    Keypad2        = 322,
    Keypad3        = 323,
    Keypad4        = 324,
    Keypad5        = 325,
    Keypad6        = 326,
    Keypad7        = 327,
    Keypad8        = 328,
    Keypad9        = 329,
    KeypadDecimal  = 330,
    KeypadDivide   = 331,
    KeypadMultiply = 332,
    KeypadSubtract = 333,
    KeypadAdd      = 334,
    KeypadEnter    = 335,
    LeftShift      = 340,
    LeftControl    = 341,
    LeftAlt        = 342,
    LeftSuper      = 343,
    RightShift     = 344,
    RightControl   = 345,
    RightAlt       = 346,
    RightSuper     = 347,
    Menu           = 348,
    Last           = Menu
};

struct KeyInfo {
    Key key;
    std::string_view name;
    std::string_view identifier;
};

constexpr std::array<KeyInfo, 120> KeyInfos = {
    KeyInfo { Key::Unknown,        "",              ""              },
    KeyInfo { Key::Space,          "Space",         "SPACE"         },
    KeyInfo { Key::Apostrophe,     "'",             "APOSTROPHE"    },
    KeyInfo { Key::Comma,          ",",             "COMMA"         },
    KeyInfo { Key::Minus,          "-",             "MINUS"         },
    KeyInfo { Key::Period,         ".",             "PERIOD"        },
    KeyInfo { Key::Slash,          "/",             "SLASH"         },
    KeyInfo { Key::Num0,           "0",             "0"             },
    KeyInfo { Key::Num1,           "1",             "1"             },
    KeyInfo { Key::Num2,           "2",             "2"             },
    KeyInfo { Key::Num3,           "3",             "3"             },
    KeyInfo { Key::Num4,           "4",             "4"             },
    KeyInfo { Key::Num5,           "5",             "5"             },
    KeyInfo { Key::Num6,           "6",             "6"             },
    KeyInfo { Key::Num7,           "7",             "7"             },
    KeyInfo { Key::Num8,           "8",             "8"             },
    KeyInfo { Key::Num9,           "9",             "9"             },
    KeyInfo { Key::SemiColon,      ";",             "SEMICOLON"     },
    KeyInfo { Key::Equal,          "=",             "EQUAL"         },
    KeyInfo { Key::A,              "A",             "A"             },
    KeyInfo { Key::B,              "B",             "B"             },
    KeyInfo { Key::C,              "C",             "C"             },
    KeyInfo { Key::D,              "D",             "D"             },
    KeyInfo { Key::E,              "E",             "E"             },
    KeyInfo { Key::F,              "F",             "F"             },
    KeyInfo { Key::G,              "G",             "G"             },
    KeyInfo { Key::H,              "H",             "H"             },
    KeyInfo { Key::I,              "I",             "I"             },
    KeyInfo { Key::J,              "J",             "J"             },
    KeyInfo { Key::K,              "K",             "K"             },
    KeyInfo { Key::L,              "L",             "L"             },
    KeyInfo { Key::M,              "M",             "M"             },
    KeyInfo { Key::N,              "N",             "N"             },
    KeyInfo { Key::O,              "O",             "O"             },
    KeyInfo { Key::P,              "P",             "P"             },
    KeyInfo { Key::Q,              "Q",             "Q"             },
    KeyInfo { Key::R,              "R",             "R"             },
    KeyInfo { Key::S,              "S",             "S"             },
    KeyInfo { Key::T,              "T",             "T"             },
    KeyInfo { Key::U,              "U",             "U"             },
    KeyInfo { Key::V,              "V",             "V"             },
    KeyInfo { Key::W,              "W",             "W"             },
    KeyInfo { Key::X,              "X",             "X"             },
    KeyInfo { Key::Y,              "Y",             "Y"             },
    KeyInfo { Key::Z,              "Z",             "Z"             },
    KeyInfo { Key::LeftBracket,    "[",             "LEFTBRACKET"   },
    KeyInfo { Key::BackSlash,      "\\",            "BACKSLASH"     },
    KeyInfo { Key::RightBracket,   "]",             "RIGHTBRACKET"  },
    KeyInfo { Key::GraveAccent,    "`",             "GRAVEACCENT"   },
    KeyInfo { Key::World1,         "World1",        "WORLD1"        },
    KeyInfo { Key::World2,         "World2",        "WORLD2"        },
    KeyInfo { Key::Escape,         "Escape",        "ESC"           },
    KeyInfo { Key::Enter,          "Enter",         "ENTER"         },
    KeyInfo { Key::Tab,            "Tab",           "TAB"           },
    KeyInfo { Key::BackSpace,      "Backspace",     "BACKSPACE"     },
    KeyInfo { Key::Insert,         "Insert",        "INSERT"        },
    KeyInfo { Key::Delete,         "Delete",        "DELETE"        },
    KeyInfo { Key::Right,          "Right",         "RIGHT"         },
    KeyInfo { Key::Left,           "Left",          "LEFT"          },
    KeyInfo { Key::Down,           "Down",          "DOWN"          },
    KeyInfo { Key::Up,             "Up",            "UP"            },
    KeyInfo { Key::PageUp,         "PageUp",        "PAGEUP"        },
    KeyInfo { Key::PageDown,       "PageDown",      "PAGEDOWN"      },
    KeyInfo { Key::Home,           "Home",          "HOME"          },
    KeyInfo { Key::End,            "End",           "END"           },
    KeyInfo { Key::CapsLock,       "CapsLock",      "CAPS_LOCK"     },
    KeyInfo { Key::ScrollLock,     "ScrollLock",    "SCROLL_LOCK"   },
    KeyInfo { Key::NumLock,        "NumLock",       "NUM_LOCK"      },
    KeyInfo { Key::PrintScreen,    "PrintScreen",   "PRINT_SCREEN"  },
    KeyInfo { Key::Pause,          "Pause",         "PAUSE"         },
    KeyInfo { Key::F1,             "F1",            "F1"            },
    KeyInfo { Key::F2,             "F2",            "F2"            },
    KeyInfo { Key::F3,             "F3",            "F3"            },
    KeyInfo { Key::F4,             "F4",            "F4"            },
    KeyInfo { Key::F5,             "F5",            "F5"            },
    KeyInfo { Key::F6,             "F6",            "F6"            },
    KeyInfo { Key::F7,             "F7",            "F7"            },
    KeyInfo { Key::F8,             "F8",            "F8"            },
    KeyInfo { Key::F9,             "F9",            "F9"            },
    KeyInfo { Key::F10,            "F10",           "F10"           },
    KeyInfo { Key::F11,            "F11",           "F11"           },
    KeyInfo { Key::F12,            "F12",           "F12"           },
    KeyInfo { Key::F13,            "F13",           "F13"           },
    KeyInfo { Key::F14,            "F14",           "F14"           },
    KeyInfo { Key::F15,            "F15",           "F15"           },
    KeyInfo { Key::F16,            "F16",           "F16"           },
    KeyInfo { Key::F17,            "F17",           "F17"           },
    KeyInfo { Key::F18,            "F18",           "F18"           },
    KeyInfo { Key::F19,            "F19",           "F19"           },
    KeyInfo { Key::F20,            "F20",           "F20"           },
    KeyInfo { Key::F21,            "F21",           "F21"           },
    KeyInfo { Key::F22,            "F22",           "F22"           },
    KeyInfo { Key::F23,            "F23",           "F23"           },
    KeyInfo { Key::F24,            "F24",           "F24"           },
    KeyInfo { Key::F25,            "F25",           "F25"           },
    KeyInfo { Key::Keypad0,        "Keypad 0",      "KP_0"          },
    KeyInfo { Key::Keypad1,        "Keypad 1",      "KP_1"          },
    KeyInfo { Key::Keypad2,        "Keypad 2",      "KP_2"          },
    KeyInfo { Key::Keypad3,        "Keypad 3",      "KP_3"          },
    KeyInfo { Key::Keypad4,        "Keypad 4",      "KP_4"          },
    KeyInfo { Key::Keypad5,        "Keypad 5",      "KP_5"          },
    KeyInfo { Key::Keypad6,        "Keypad 6",      "KP_6"          },
    KeyInfo { Key::Keypad7,        "Keypad 7",      "KP_7"          },
    KeyInfo { Key::Keypad8,        "Keypad 8",      "KP_8"          },
    KeyInfo { Key::Keypad9,        "Keypad 9",      "KP_9"          },
    KeyInfo { Key::KeypadDecimal,  "Keypad .",      "KP_DECIMAL"    },
    KeyInfo { Key::KeypadDivide,   "Keypad /",      "KP_DIVIDE"     },
    KeyInfo { Key::KeypadMultiply, "Keypad *",      "KP_MULTIPLY"   },
    KeyInfo { Key::KeypadSubtract, "Keypad -",      "KP_SUBTRACT"   },
    KeyInfo { Key::KeypadAdd,      "Keypad +",      "KP_ADD"        },
    KeyInfo { Key::KeypadEnter,    "Keypad Enter",  "KP_ENTER"      },
    KeyInfo { Key::LeftShift,      "Left Shift",    "LEFT_SHIFT"    },
    KeyInfo { Key::LeftControl,    "Left Control",  "LEFT_CONTROL"  },
    KeyInfo { Key::LeftAlt,        "Left Alt",      "LEFT_ALT"      },
    KeyInfo { Key::LeftSuper,      "Left Super",    "LEFT_SUPER"    },
    KeyInfo { Key::RightShift,     "Right Shift",   "RIGHT_SHIFT"   },
    KeyInfo { Key::RightControl,   "Right Control", "RIGHT_CONTROL" },
    KeyInfo { Key::RightAlt,       "Right Alt",     "RIGHT_ALT"     },
    KeyInfo { Key::RightSuper,     "Right Super",   "RIGHT_SUPER"   },
    KeyInfo { Key::Menu,           "Menu",          "MENU"          }
};

//////////////////////////////////////////////////////////////////////////////////////////

struct KeyWithModifier {
    Key key = Key::Unknown;
    KeyModifier modifier = KeyModifier::None;

    auto operator<=>(const KeyWithModifier&) const = default;
};

constexpr inline bool isKeypadKey(Key key) noexcept {
    return key == Key::Keypad0 || key == Key::Keypad1 || key == Key::Keypad2 ||
        key == Key::Keypad3 || key == Key::Keypad4 || key == Key::Keypad5 ||
        key == Key::Keypad6 || key == Key::Keypad7 || key == Key::Keypad8 ||
        key == Key::Keypad9 || key == Key::KeypadEnter || key == Key::KeypadAdd ||
        key == Key::KeypadSubtract || key == Key::KeypadMultiply ||
        key == Key::KeypadDivide;
}

KeyWithModifier stringToKey(const std::string& str);
std::string keyToString(KeyWithModifier keyWithModifier);

} // namespace openspace

namespace ghoul {

template <>
std::string to_string(const openspace::Key& key);

template <>
std::string to_string(const openspace::KeyModifier& mod);

template <>
std::string to_string(const openspace::KeyWithModifier& keyMod);

} // namespace ghoul

#endif // __OPENSPACE_CORE___KEYS___H__
