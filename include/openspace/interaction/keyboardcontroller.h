/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#ifndef __KEYBOARDCONTROLLER_H__
#define __KEYBOARDCONTROLLER_H__

#include <openspace/interaction/controller.h>

#include <openspace/util/keys.h>

namespace openspace {
namespace interaction {

class KeyboardController : public Controller {
public:
	virtual void keyPressed(KeyAction action, Key key, KeyModifier modifier) = 0;
};

class KeyboardControllerFixed : public KeyboardController {
public:
	void keyPressed(KeyAction action, Key key, KeyModifier modifier);
};

class KeyboardControllerLua : public KeyboardController {
public:
	void keyPressed(KeyAction action, Key key, KeyModifier modifier);

protected:
	std::string keyToString(Key key, KeyModifier mod) const;
};

} // namespace interaction
} // namespace openspace

#endif // __KEYBOARDCONTROLLER_H__
