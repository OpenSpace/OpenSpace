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

#ifndef __MOUSE_H__
#define __MOUSE_H__

#include <sgct.h>

namespace openspace {
namespace interaction {

enum class MouseAction {
	Press = SGCT_PRESS,
	Release = SGCT_RELEASE,
	Repeat = SGCT_REPEAT
};

enum class MouseButton {
	Left = SGCT_MOUSE_BUTTON_LEFT,
	Right = SGCT_MOUSE_BUTTON_RIGHT,
	Middle = SGCT_MOUSE_BUTTON_MIDDLE,
	//Button1 = SGCT_MOUSE_BUTTON_1,
	//Button2 = SGCT_MOUSE_BUTTON_2,
	//Button3 = SGCT_MOUSE_BUTTON_3,
	//Button4 = SGCT_MOUSE_BUTTON_4,
	//Button5 = SGCT_MOUSE_BUTTON_5,
	//Button6 = SGCT_MOUSE_BUTTON_6,
	//Button7 = SGCT_MOUSE_BUTTON_7,
	//Button8 = SGCT_MOUSE_BUTTON_8,
	ButtonLast = SGCT_MOUSE_BUTTON_LAST,
};

} // namespace interaction
} // namespace openspace

#endif // __MOUSE_H__
