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

#include <ghoul/logging/logmanager.h>

#include <TUIO/TuioServer.h>

#include <tchar.h>
#include <tpcshrd.h>

namespace {
    constexpr const char* _loggerCat = "win32_touch";
    HHOOK gTouchHook{ nullptr };
    bool gStarted{ false };
    TUIO::TuioServer* gTuioServer{ nullptr };
    std::unordered_map<UINT, TUIO::TuioCursor*> gCursorMap;
}

namespace openspace {

// This hook will only work for Win7+ Digitizers.
// - Once GLFW has native touch support, we can remove this windows-specific code
LRESULT CALLBACK HookCallback(int nCode, WPARAM wParam, LPARAM lParam) {
    if (nCode < 0) {
        return CallNextHookEx(0, nCode, wParam, lParam);
    }
    if (nCode == HC_ACTION) {
        LPMSG pStruct = reinterpret_cast<LPMSG>(lParam);
        const UINT message = pStruct->message;
        switch (message) {
            case WM_POINTERDOWN:
            case WM_POINTERUPDATE:
            case WM_POINTERUP:
            {
                POINTER_INFO pointerInfo = {};
                if (GetPointerInfo(GET_POINTERID_WPARAM(pStruct->wParam), &pointerInfo)) {
                    RECT rect;
                    GetClientRect(pStruct->hwnd, reinterpret_cast<LPRECT>(&rect));

                    POINT p = pointerInfo.ptPixelLocation;
                    // native touch to screen conversion
                    ScreenToClient(pStruct->hwnd, reinterpret_cast<LPPOINT>(&p));

                    float xPos = (float)p.x / (float)(rect.right - rect.left);
                    float yPos = (float)p.y / (float)(rect.bottom - rect.top);
                    if (pointerInfo.pointerFlags & POINTER_FLAG_DOWN) {
                        // Handle new touchpoint
                        gTuioServer->initFrame(TUIO::TuioTime::getSessionTime());
                        gCursorMap[pointerInfo.pointerId] = gTuioServer->addTuioCursor(xPos, yPos);
                        gTuioServer->commitFrame();
                    }
                    else if (pointerInfo.pointerFlags & POINTER_FLAG_UPDATE) {
                        // Handle update of touchpoint
                        TUIO::TuioTime frameTime = TUIO::TuioTime::getSessionTime();
                        if (gCursorMap[pointerInfo.pointerId]->getTuioTime() == frameTime) {
                            break;
                        }
                        gTuioServer->initFrame(frameTime);
                        gTuioServer->updateTuioCursor(gCursorMap[pointerInfo.pointerId], xPos, yPos);
                        gTuioServer->commitFrame();
                    }
                    else if (pointerInfo.pointerFlags & POINTER_FLAG_UP) {
                        // Handle removed touchpoint
                        gTuioServer->initFrame(TUIO::TuioTime::getSessionTime());
                        gTuioServer->removeTuioCursor(gCursorMap[pointerInfo.pointerId]);
                        gTuioServer->commitFrame();
                        gCursorMap.erase(pointerInfo.pointerId);
                    }
                }
                break;
            }
        }
    }

    // Pass the hook along!
    return CallNextHookEx(0, nCode, wParam, lParam);
}

Win32TouchHook::Win32TouchHook(void* nativeWindow)
{
    HWND hWnd = reinterpret_cast<HWND>(nativeWindow);
    if (hWnd == nullptr) {
        LINFO("No windowhandle available for touch input.");
        return;
    }

    // Test for touch:
    int value = GetSystemMetrics(SM_DIGITIZER);
    if ((value & NID_READY) == 0) { 
        // Don't bother setting up touch hooks?
        return;
    }
    // stack ready, drivers installed and digitizer is ready for input
    if (value & NID_MULTI_INPUT) {
        // Digitizer is multitouch
        LINFO("Found Multitouch input digitizer!");
    }
    if (value & NID_INTEGRATED_TOUCH) { 
        // Integrated touch
    }

    // This should be needed, but we seem to receive messages even without it,
    // probably a Win7+ behaviour
    // Also - RegisterTouchWindow enables Windows gestures, which we don't want
    // since they produce visual feedback for "press-and-tap" etc.
    // RegisterTouchWindow(hWnd, TWF_FINETOUCH | TWF_WANTPALM);

    // TODO: Would be nice to find out if the gesture "press-and-tap" can be disabled
    // basically we don't really care for windows gestures for now...
    // this disables press and hold (right-click) gesture
    const DWORD dwHwndTabletProperty = TABLET_DISABLE_PRESSANDHOLD; 

    ATOM atom = ::GlobalAddAtom(MICROSOFT_TABLETPENSERVICE_PROPERTY);
    ::SetProp(hWnd, MICROSOFT_TABLETPENSERVICE_PROPERTY, reinterpret_cast<HANDLE>(dwHwndTabletProperty));
    ::GlobalDeleteAtom(atom);

    if (!gStarted) {
        gStarted = true;
        gTuioServer = new TUIO::TuioServer("localhost", 3333);
        TUIO::TuioTime::initSession();
        gTouchHook = SetWindowsHookExW(WH_GETMESSAGE, HookCallback, GetModuleHandleW(NULL), GetCurrentThreadId());
        if (!gTouchHook) {
            LINFO(fmt::format("Failed to setup WindowsHook for touch input redirection"));
            delete gTuioServer;
            gStarted = false;
        }
    }
}

Win32TouchHook::~Win32TouchHook() {
    if (gStarted) {
        UnhookWindowsHookEx(gTouchHook);
        delete gTuioServer;
    }
}

} // namespace openspace
#endif // WIN32
