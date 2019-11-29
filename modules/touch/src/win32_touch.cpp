/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#ifdef WIN32
#include <modules/touch/include/win32_touch.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <TUIO/TuioServer.h>

#include <ghoul/logging/logmanager.h>

#include <tchar.h>
#include <tpcshrd.h>

//TODO: Make an application namespace?
constexpr const char* _loggerCat = "win32_touch";

HHOOK g_touchHook{ nullptr };
bool g_started{ false };

TUIO::TuioServer* g_tuioServer{ nullptr };
std::unordered_map<UINT, TUIO::TuioCursor*> g_cursorMap;

//This hook will only work for Win7+ Digitizers.
//  - Once GLFW has native touch support, we can remove this windows-specific code
LRESULT CALLBACK HookCallback(int nCode, WPARAM wParam, LPARAM lParam)
{
    if (nCode < 0) // do not process message 
    {
        return CallNextHookEx(0, nCode, wParam, lParam);
    }

    switch (nCode)
    {
    case HC_ACTION:
        LPMSG pStruct = (LPMSG)lParam;
        UINT message = pStruct->message;
        switch (message) {
        case WM_POINTERDOWN:
        case WM_POINTERUPDATE:
        case WM_POINTERUP:
        {
            POINTER_INFO pointerInfo = {};
            if (GetPointerInfo(GET_POINTERID_WPARAM(pStruct->wParam), &pointerInfo)) {
                RECT rect;
                GetClientRect(pStruct->hwnd, (LPRECT)&rect);

                POINT p = pointerInfo.ptPixelLocation;
                // native touch to screen conversion
                ScreenToClient(pStruct->hwnd, (LPPOINT)&p);

                float xPos = (float)p.x / (float)(rect.right - rect.left);
                float yPos = (float)p.y / (float)(rect.bottom - rect.top);               
                if (pointerInfo.pointerFlags & POINTER_FLAG_DOWN) {
                    //Handle new touchpoint
                    g_tuioServer->initFrame(TUIO::TuioTime::getSessionTime());
                    g_cursorMap[pointerInfo.pointerId] = g_tuioServer->addTuioCursor(xPos, yPos);
                    g_tuioServer->commitFrame();
                }
                else if (pointerInfo.pointerFlags & POINTER_FLAG_UPDATE) {
                    //Handle update of touchpoint
                    TUIO::TuioTime frameTime = TUIO::TuioTime::getSessionTime();
                    if (g_cursorMap[pointerInfo.pointerId]->getTuioTime() == frameTime)
                    {
                        break;
                    }
                    g_tuioServer->initFrame(frameTime);
                    g_tuioServer->updateTuioCursor(g_cursorMap[pointerInfo.pointerId], xPos, yPos);
                    g_tuioServer->commitFrame();
                }
                else if (pointerInfo.pointerFlags & POINTER_FLAG_UP) {
                    //Handle removed touchpoint
                    g_tuioServer->initFrame(TUIO::TuioTime::getSessionTime());
                    g_tuioServer->removeTuioCursor(g_cursorMap[pointerInfo.pointerId]);
                    g_tuioServer->commitFrame();
                    g_cursorMap.erase(pointerInfo.pointerId);
                }
            }
            break;
        }
        }
        break;
    }

    //Pass the hook along!
    return CallNextHookEx(0, nCode, wParam, lParam);
}

Win32TouchHook::Win32TouchHook(void* nativeWindowPtr)
    : _enabled(false)
{
    HWND hWnd = reinterpret_cast<HWND>(nativeWindowPtr);
    if (hWnd == nullptr) {
        LINFO("No windowhandle available for touch input.");
        return;
    }

    // Test for touch:
    int value = GetSystemMetrics(SM_DIGITIZER);
    if (value & NID_READY) { 
        // stack ready, drivers installed and digitizer is ready for input
    }
    else {
        //Don't bother setting up touch hooks?
        return;
    }
    if (value & NID_MULTI_INPUT) {
        /* digitizer is multitouch */
        LINFO("Found Multitouch input digitizer!");
    }
    if (value & NID_INTEGRATED_TOUCH) { /* Integrated touch */ }

    //This should be needed, but we seem to receive messages even without it,
    //probably a Win7+ behaviour
    //Also - RegisterTouchWindow enables Windows gestures, which we don't want
    //since they produce visual feedback for "press-and-tap" etc.
    //RegisterTouchWindow(hWnd, TWF_FINETOUCH | TWF_WANTPALM);

    // TODO: Would be nice to find out if the gesture "press-and-tap" can be disabled
    // basically we don't really care for windows gestures for now...
    // this disables press and hold (right-click) gesture
    const DWORD dwHwndTabletProperty = TABLET_DISABLE_PRESSANDHOLD; 

    ATOM atom = ::GlobalAddAtom(MICROSOFT_TABLETPENSERVICE_PROPERTY);
    ::SetProp(hWnd, MICROSOFT_TABLETPENSERVICE_PROPERTY, 
        reinterpret_cast<HANDLE>(dwHwndTabletProperty));
    ::GlobalDeleteAtom(atom);

    if (!g_started) {
        g_tuioServer = new TUIO::TuioServer("localhost", 3333);
        if (!(g_touchHook = SetWindowsHookExW(WH_GETMESSAGE, HookCallback, GetModuleHandleW(NULL), GetCurrentThreadId()))) {
            LINFO(fmt::format("Failed to setup WindowsHook for touch input redirection"));
        }
        TUIO::TuioTime::initSession();
    }
}

Win32TouchHook::~Win32TouchHook() {
    if (_enabled) {
        UnhookWindowsHookEx(g_touchHook);
        delete g_tuioServer;
    }
}

#endif //WIN32
