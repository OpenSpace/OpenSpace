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

#include <modules/webbrowser/include/browserclient.h>

#include <modules/webbrowser/include/defaultbrowserlauncher.h>
#include <modules/webbrowser/include/webrenderhandler.h>
#include <modules/webbrowser/include/webkeyboardhandler.h>
#include <ghoul/misc/assert.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>

namespace openspace {

BrowserClient::BrowserClient(WebRenderHandler* handler,
                             WebKeyboardHandler* keyboardHandler)
    : _renderHandler(handler)
    , _keyboardHandler(keyboardHandler)
{
    ghoul_assert(handler, "No WebRenderHandler provided");
    ghoul_assert(keyboardHandler, "No WebKeyboardHandler provided");

    DefaultBrowserLauncher* browserLauncher = new DefaultBrowserLauncher;
    _lifeSpanHandler = browserLauncher;
    _requestHandler = browserLauncher;
    _contextMenuHandler = new BrowserClient::NoContextMenuHandler;
    _displayHandler = new BrowserClient::DisplayHandler;
}

CefRefPtr<CefContextMenuHandler> BrowserClient::GetContextMenuHandler() {
    return _contextMenuHandler;
}

CefRefPtr<CefRenderHandler> BrowserClient::GetRenderHandler() {
    return _renderHandler;
}

CefRefPtr<CefLifeSpanHandler> BrowserClient::GetLifeSpanHandler() {
    return _lifeSpanHandler;
}

CefRefPtr<CefRequestHandler> BrowserClient::GetRequestHandler() {
    return _requestHandler;
}

CefRefPtr<CefKeyboardHandler> BrowserClient::GetKeyboardHandler() {
    return _keyboardHandler;
}

CefRefPtr<CefDisplayHandler> BrowserClient::GetDisplayHandler() {
    return _displayHandler;
}

bool BrowserClient::NoContextMenuHandler::RunContextMenu(CefRefPtr<CefBrowser>,
                                                         CefRefPtr<CefFrame>,
                                                         CefRefPtr<CefContextMenuParams>,
                                                         CefRefPtr<CefMenuModel>,
                                                     CefRefPtr<CefRunContextMenuCallback>)
{
    // Disable the context menu.
    return true;
}

bool BrowserClient::DisplayHandler::OnCursorChange(CefRefPtr<CefBrowser>, CefCursorHandle,
                                                   cef_cursor_type_t type,
                                                   const CefCursorInfo&)
{
    WindowDelegate::Cursor newCursor;
    switch (type) {
        case cef_cursor_type_t::CT_POINTER:
            newCursor = WindowDelegate::Cursor::Arrow;
            break;
        case cef_cursor_type_t::CT_IBEAM:
            newCursor = WindowDelegate::Cursor::IBeam;
            break;
        case cef_cursor_type_t::CT_CROSS:
            newCursor = WindowDelegate::Cursor::CrossHair;
            break;
        case cef_cursor_type_t::CT_HAND:
            newCursor = WindowDelegate::Cursor::PointingHand;
            break;
        case cef_cursor_type_t::CT_EASTWESTRESIZE:
        case cef_cursor_type_t::CT_COLUMNRESIZE:
        case cef_cursor_type_t::CT_EASTRESIZE:
        case cef_cursor_type_t::CT_WESTRESIZE:
            newCursor = WindowDelegate::Cursor::ResizeEW;
            break;
        case cef_cursor_type_t::CT_NORTHSOUTHRESIZE:
        case cef_cursor_type_t::CT_ROWRESIZE:
        case cef_cursor_type_t::CT_NORTHRESIZE:
        case cef_cursor_type_t::CT_SOUTHRESIZE:
            newCursor = WindowDelegate::Cursor::ResizeNS;
            break;
        case cef_cursor_type_t::CT_NORTHWESTSOUTHEASTRESIZE:
        case cef_cursor_type_t::CT_SOUTHEASTRESIZE:
        case cef_cursor_type_t::CT_NORTHWESTRESIZE:
            newCursor = WindowDelegate::Cursor::ResizeNWSE;
            break;
        case cef_cursor_type_t::CT_NORTHEASTSOUTHWESTRESIZE:
        case cef_cursor_type_t::CT_SOUTHWESTRESIZE:
        case cef_cursor_type_t::CT_NORTHEASTRESIZE:
            newCursor = WindowDelegate::Cursor::ResizeNESW;
            break;
        case cef_cursor_type_t::CT_MOVE:
            newCursor = WindowDelegate::Cursor::ResizeAll;
            break;
        case cef_cursor_type_t::CT_NOTALLOWED:
            newCursor = WindowDelegate::Cursor::NotAllowed;
            break;
        case cef_cursor_type_t::CT_GRAB:
        case cef_cursor_type_t::CT_GRABBING:
            // There is no "grabbing" cursors in GLFW, so use the pointing hand instead,
            // to make web objects that use drag-n-drop look more interactive
            newCursor = WindowDelegate::Cursor::PointingHand;
            break;
        default:
            newCursor = WindowDelegate::Cursor::Arrow;
            break;
    }
    global::windowDelegate->setMouseCursor(newCursor);
    return false;
}
} // namespace openspace
