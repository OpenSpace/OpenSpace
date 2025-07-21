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

#ifndef __OPENSPACE_MODULE_WEBBROWSER__WEB_KEYBOARD_HANDLER_H__
#define __OPENSPACE_MODULE_WEBBROWSER__WEB_KEYBOARD_HANDLER_H__

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4100)
#endif // _MSC_VER


#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-parameter"
#endif // __clang__

#include <include/cef_keyboard_handler.h>

#ifdef __clang__
#pragma clang diagnostic pop
#endif // __clang__

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

namespace openspace {

class WebKeyboardHandler : public CefKeyboardHandler {
public:
    bool OnKeyEvent(CefRefPtr<CefBrowser> browser, const CefKeyEvent& event,
        CefEventHandle osEvent) override;
    bool OnPreKeyEvent(CefRefPtr<CefBrowser> browser, const CefKeyEvent& event,
        CefEventHandle osEvent, bool* isKeyboardShortcut) override;

protected:
    bool _keyConsumed = false;
    IMPLEMENT_REFCOUNTING(WebKeyboardHandler);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_WEBBROWSER__WEB_KEYBOARD_HANDLER_H__
