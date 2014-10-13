/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#ifndef __KEYS_H__
#define __KEYS_H__

#include <sgct.h>

namespace openspace {
namespace interaction {

enum class KeyAction {
	Press = SGCT_PRESS,
	Release = SGCT_RELEASE,
	Repeat = SGCT_REPEAT
};

enum class KeyModifier {
	None = 0,
	Shift = GLFW_MOD_SHIFT,
	Control = GLFW_MOD_CONTROL,
	Alt = GLFW_MOD_ALT,
	Super = GLFW_MOD_SUPER
};

enum class Key {
	Unknown = SGCT_KEY_UNKNOWN,
	Space = SGCT_KEY_SPACE,
	Apostrophe = SGCT_KEY_APOSTROPHE,
	Comma = SGCT_KEY_COMMA,
	Minus = SGCT_KEY_MINUS,
	Period = SGCT_KEY_PERIOD,
	Slash = SGCT_KEY_SLASH,
	Num0 = SGCT_KEY_0,
	Num1 = SGCT_KEY_1,
	Num2 = SGCT_KEY_2,
	Num3 = SGCT_KEY_3,
	Num4 = SGCT_KEY_4,
	Num5 = SGCT_KEY_5,
	Num6 = SGCT_KEY_6,
	Num7 = SGCT_KEY_7,
	Num8 = SGCT_KEY_8,
	Num9 = SGCT_KEY_9,
	SemiColon = SGCT_KEY_SEMICOLON,
	Equal = SGCT_KEY_EQUAL,
	A = SGCT_KEY_A,
	B = SGCT_KEY_B,
	C = SGCT_KEY_C,
	D = SGCT_KEY_D,
	E = SGCT_KEY_E,
	F = SGCT_KEY_F,
	G = SGCT_KEY_G,
	H = SGCT_KEY_H,
	I = SGCT_KEY_I,
	J = SGCT_KEY_J,
	K = SGCT_KEY_K,
	L = SGCT_KEY_L,
	M = SGCT_KEY_M,
	N = SGCT_KEY_N,
	O = SGCT_KEY_O,
	P = SGCT_KEY_P,
	Q = SGCT_KEY_Q,
	R = SGCT_KEY_R,
	S = SGCT_KEY_S,
	T = SGCT_KEY_T,
	U = SGCT_KEY_U,
	V = SGCT_KEY_V,
	W = SGCT_KEY_W,
	X = SGCT_KEY_X,
	Y = SGCT_KEY_Y,
	Z = SGCT_KEY_Z,
	LeftBracket = SGCT_KEY_LEFT_BRACKET,
	BackSlash = SGCT_KEY_BACKSLASH,
	RightBracket = SGCT_KEY_RIGHT_BRACKET,
	GraveAccent = SGCT_KEY_GRAVE_ACCENT,
	World1 = SGCT_KEY_WORLD_1,
	World2 = SGCT_KEY_WORLD_2,
	Escape = SGCT_KEY_ESC,
	Enter = SGCT_KEY_ENTER,
	Tab = SGCT_KEY_TAB,
	BackSpace = SGCT_KEY_BACKSPACE,
	Insert = SGCT_KEY_INSERT,
	Delete = SGCT_KEY_DELETE,
	Right = SGCT_KEY_RIGHT,
	Left = SGCT_KEY_LEFT,
	Down = SGCT_KEY_DOWN,
	Up = SGCT_KEY_UP,
	PageUp = SGCT_KEY_PAGE_UP,
	PageDown = SGCT_KEY_PAGE_DOWN,
	Home = SGCT_KEY_HOME,
	End = SGCT_KEY_END,
	CapsLock = SGCT_KEY_CAPS_LOCK,
	ScrollLock = SGCT_KEY_SCROLL_LOCK,
	NumLock = SGCT_KEY_NUM_LOCK,
	PrintScreen = SGCT_KEY_PRINT_SCREEN,
	Pause = SGCT_KEY_PAUSE,
	F1 = SGCT_KEY_F1,
	F2 = SGCT_KEY_F2,
	F3 = SGCT_KEY_F3,
	F4 = SGCT_KEY_F4,
	F5 = SGCT_KEY_F5,
	F6 = SGCT_KEY_F6,
	F7 = SGCT_KEY_F7,
	F8 = SGCT_KEY_F8,
	F9 = SGCT_KEY_F9,
	F10 = SGCT_KEY_F10,
	F11 = SGCT_KEY_F11,
	F12 = SGCT_KEY_F12,
	F13 = SGCT_KEY_F13,
	F14 = SGCT_KEY_F14,
	F15 = SGCT_KEY_F15,
	F16 = SGCT_KEY_F16,
	F17 = SGCT_KEY_F17,
	F18 = SGCT_KEY_F18,
	F19 = SGCT_KEY_F19,
	F20 = SGCT_KEY_F20,
	F21 = SGCT_KEY_F21,
	F22 = SGCT_KEY_F22,
	F23 = SGCT_KEY_F23,
	F24 = SGCT_KEY_F24,
	F25 = SGCT_KEY_F25,
	Keypad0 = SGCT_KEY_KP_0,
	Keypad1 = SGCT_KEY_KP_1,
	Keypad2 = SGCT_KEY_KP_2,
	Keypad3 = SGCT_KEY_KP_3,
	Keypad4 = SGCT_KEY_KP_4,
	Keypad5 = SGCT_KEY_KP_5,
	Keypad6 = SGCT_KEY_KP_6,
	Keypad7 = SGCT_KEY_KP_7,
	Keypad8 = SGCT_KEY_KP_8,
	Keypad9 = SGCT_KEY_KP_9,
	KeypadDecimal = SGCT_KEY_KP_DECIMAL,
	KeypadDivide = SGCT_KEY_KP_DIVIDE,
	KeypadMultiply = SGCT_KEY_KP_MULTIPLY,
	KeypadSubtract = SGCT_KEY_KP_SUBTRACT,
	KeypadAdd = SGCT_KEY_KP_ADD,
	KeypadEnter = SGCT_KEY_KP_ENTER,
	LeftShift = SGCT_KEY_LEFT_SHIFT,
	LeftControl = SGCT_KEY_LEFT_CONTROL,
	LeftAlt = SGCT_KEY_LEFT_ALT,
	LeftSuper = SGCT_KEY_LEFT_SUPER,
	RightShift = SGCT_KEY_RIGHT_SHIFT,
	RightControl = SGCT_KEY_RIGHT_CONTROL,
	RightAlt = SGCT_KEY_RIGHT_ALT,
	RightSuper = SGCT_KEY_RIGHT_SUPER,
	Menu = SGCT_KEY_MENU,
	Last = SGCT_KEY_LAST
};

} // namespace interaction
} // namespace openspace

#endif // __KEYS_H__
