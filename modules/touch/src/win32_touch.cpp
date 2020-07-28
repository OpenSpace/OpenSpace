/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <ghoul/logging/logmanager.h>
#include <TUIO/TuioServer.h>
#include <chrono>
#include <thread>
#include <tchar.h>
#include <tpcshrd.h>

// #define ENABLE_TUIOMESSAGES
#define ENABLE_DIRECTMSG

namespace {
    constexpr const char* _loggerCat = "win32_touch";
    HHOOK gTouchHook = nullptr;
    std::thread* gMouseHookThread = nullptr;
    HHOOK gMouseHook = nullptr;
    bool gStarted = false;
    std::chrono::microseconds gStartTime = std::chrono::microseconds(0);
    std::unordered_map<
        UINT32,
        std::unique_ptr<openspace::TouchInputHolder>
    > gTouchInputsMap;

#ifdef ENABLE_TUIOMESSAGES
    TUIO::TuioServer* gTuioServer = nullptr;
    std::unordered_map<UINT, TUIO::TuioCursor*> gCursorMap;
#endif

    const long long gFrequency = []() -> long long {
        LARGE_INTEGER frequency;
        QueryPerformanceFrequency(&frequency);
        return frequency.QuadPart;
    }();

} // namespace

namespace openspace {

LRESULT CALLBACK LowLevelMouseProc(int nCode, WPARAM wParam, LPARAM lParam);

// This hook will only work for Win8+ Digitizers.
// - Once GLFW has native touch support, we can remove this windows-specific code
LRESULT CALLBACK HookCallback(int nCode, WPARAM wParam, LPARAM lParam) {
    if (nCode != HC_ACTION) {
        return CallNextHookEx(0, nCode, wParam, lParam);
    }

    LPMSG pStruct = reinterpret_cast<LPMSG>(lParam);
    const UINT message = pStruct->message;
    switch (message) {
        case WM_POINTERDOWN:
        case WM_POINTERUPDATE:
        case WM_POINTERUP:
        {
            POINTER_INFO info = {};
            BOOL hasInfo = GetPointerInfo(GET_POINTERID_WPARAM(pStruct->wParam), &info);
            if (!hasInfo) {
                break;
            }

            //Implementation from microsoft STL of high_resolution_clock(steady_clock):
            const long long freq = gFrequency;
            const long long whole = (info.PerformanceCount / freq) * std::micro::den;
            const long long part  = (info.PerformanceCount % freq) *
                                    std::micro::den / freq;
            const std::chrono::microseconds timestamp =
                std::chrono::duration<UINT64, std::micro>(whole + part) - gStartTime;

            RECT rect;
            GetClientRect(pStruct->hwnd, reinterpret_cast<LPRECT>(&rect));

            POINT p = info.ptPixelLocation;
            // native touch to screen conversion
            ScreenToClient(pStruct->hwnd, reinterpret_cast<LPPOINT>(&p));

            const float xPos = static_cast<float>(p.x) /
                               static_cast<float>(rect.right - rect.left);
            const float yPos = static_cast<float>(p.y) /
                               static_cast<float>(rect.bottom - rect.top);

            TouchInput touchInput(
                reinterpret_cast<size_t>(info.sourceDevice),
                static_cast<size_t>(info.pointerId),
                xPos,
                yPos,
                static_cast<double>(timestamp.count()) / 1000000.0
            );

            if (info.pointerFlags & POINTER_FLAG_DOWN) {
#ifdef ENABLE_DIRECTMSG
                std::unique_ptr<TouchInputHolder> points =
                    std::make_unique<TouchInputHolder>(touchInput);
                gTouchInputsMap.emplace(info.pointerId, std::move(points));
                global::openSpaceEngine.touchDetectionCallback(touchInput);
#endif
#ifdef ENABLE_TUIOMESSAGES
                // Handle new touchpoint
                gTuioServer->initFrame(TUIO::TuioTime::getSessionTime());
                gCursorMap[info.pointerId] = gTuioServer->addTuioCursor(
                    xPos,
                    yPos
                );
                gTuioServer->commitFrame();
#endif
            }
            else if (info.pointerFlags & POINTER_FLAG_UPDATE) {
                // Handle update of touchpoint
#ifdef ENABLE_DIRECTMSG
                TouchInputHolder* points = gTouchInputsMap[info.pointerId].get();

                if (points->tryAddInput(touchInput)) {
                    global::openSpaceEngine.touchUpdateCallback(points->latestInput());
                }
#endif
#ifdef ENABLE_TUIOMESSAGES
                TUIO::TuioTime frameTime = TUIO::TuioTime::getSessionTime();
                if (gCursorMap[info.pointerId]->getTuioTime() == frameTime) {
                    break;
                }
                gTuioServer->initFrame(frameTime);
                gTuioServer->updateTuioCursor(gCursorMap[info.pointerId], xPos, yPos);
                gTuioServer->commitFrame();
#endif
            }
            else if (info.pointerFlags & POINTER_FLAG_UP) {
#ifdef ENABLE_DIRECTMSG
                gTouchInputsMap.erase(info.pointerId);
                global::openSpaceEngine.touchExitCallback(touchInput);
#endif
#ifdef ENABLE_TUIOMESSAGES
                // Handle removed touchpoint
                gTuioServer->initFrame(TUIO::TuioTime::getSessionTime());
                gTuioServer->removeTuioCursor(gCursorMap[info.pointerId]);
                gTuioServer->commitFrame();
                gCursorMap.erase(info.pointerId);
#endif
            }
            break;
        }
    }

    // Pass the hook along!
    return CallNextHookEx(0, nCode, wParam, lParam);
}

Win32TouchHook::Win32TouchHook(void* nativeWindow) {
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
    // this ought to be part to the older (< win8) windows touch-api.
    // Also - RegisterTouchWindow enables Windows gestures, which we don't want
    // since they produce visual feedback for "press-and-tap" etc.
    // RegisterTouchWindow(hWnd, TWF_FINETOUCH | TWF_WANTPALM);

    // TODO: Would be nice to find out if the gesture "press-and-tap" can be disabled
    // basically we don't really care for windows gestures for now...
    // this disables press and hold (right-click) gesture
    const UINT_PTR dwHwndTabletProperty = TABLET_DISABLE_PRESSANDHOLD;

    ATOM atom = ::GlobalAddAtom(MICROSOFT_TABLETPENSERVICE_PROPERTY);
    ::SetProp(
        hWnd,
        MICROSOFT_TABLETPENSERVICE_PROPERTY,
        reinterpret_cast<HANDLE>(dwHwndTabletProperty)
    );
    ::GlobalDeleteAtom(atom);

    if (!gStarted) {
        gStarted = true;
        gStartTime = std::chrono::duration_cast<std::chrono::microseconds>(
            std::chrono::high_resolution_clock::now().time_since_epoch()
        );
#ifdef ENABLE_TUIOMESSAGES
        gTuioServer = new TUIO::TuioServer("localhost", 3333);
        TUIO::TuioTime::initSession();
#endif
        gTouchHook = SetWindowsHookExW(
            WH_GETMESSAGE,
            HookCallback,
            GetModuleHandleW(nullptr),
            GetCurrentThreadId()
        );

        // In theory, if our UI is pumped from a different thread, we can
        // handle Low-level mouse events in that thread as well.
        // this might help reduce mouse lag while running OpenSpace?
        // gMouseHookThread = new std::thread([](){
        //     gMouseHook = SetWindowsHookExW(
        //         WH_MOUSE_LL,
        //         LowLevelMouseProc,
        //         GetModuleHandleW(NULL),
        //         0 //<- Global thread id (low-level mouse is global only)
        //     );
        //     if (!gMouseHook) {
        //         LINFO("Could not setup mousehook!");
        //     }

        //     MSG msg;
        //     while (GetMessage(&msg, NULL, 0, 0)) {
        //         DispatchMessage(&msg);
        //     }
        // });

        if (!gTouchHook) {
            LINFO(fmt::format("Failed to setup WindowsHook for touch input redirection"));
#ifdef ENABLE_TUIOMESSAGES
            delete gTuioServer;
#endif
            gStarted = false;
        }
    }
}


Win32TouchHook::~Win32TouchHook() {
    if (gStarted) {
        UnhookWindowsHookEx(gTouchHook);
        UnhookWindowsHookEx(gMouseHook);
#ifdef ENABLE_TUIOMESSAGES
        delete gTuioServer;
#endif
    }
}

// Low-level mouse hook is "needed" if we want to stop mousecursor from moving
// when we get a touch-input on our window A negative effect is that this
// function is for global threads, meaning our application will cause Windows to
// stall the mouse cursor when this function can't be scheduled. This is not yet
// fail-proof...might be a race-condition on message pumping?
// - Seems to move the cursor when we get two fingers as input..
// - If we ourselves would pump windows for events, we can handle this in the
// pump-loop
LRESULT CALLBACK LowLevelMouseProc(int nCode, WPARAM wParam, LPARAM lParam) {
    constexpr const LONG_PTR SIGNATURE_MASK = 0xFFFFFF00;
    constexpr const LONG_PTR MOUSEEVENTF_FROMTOUCH = 0xFF515700;
    if (nCode < 0) {
        // do not process message
        return CallNextHookEx(0, nCode, wParam, lParam);
    }
    LPMSLLHOOKSTRUCT msg = reinterpret_cast<LPMSLLHOOKSTRUCT>(lParam);
    // block injected events (in most cases generated by touches)
    bool isFromTouch = (msg->dwExtraInfo | SIGNATURE_MASK) == MOUSEEVENTF_FROMTOUCH;
    if (msg->flags & LLMHF_INJECTED || isFromTouch) {
        return 1;
    }

    // forward event
    return CallNextHookEx(0, nCode, wParam, lParam);
}

} // namespace openspace

#endif // WIN32
