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

// Copyright (c) 2013 The Chromium Embedded Framework Authors. All rights
// reserved. Use of this source code is governed by a BSD-style license that can
// be found in the LICENSE file.

#include "include/cef_app.h"
#include "include/webbrowserapp.h"
#include <thread>

#ifdef WIN32
#include <tlhelp32.h>
#endif // WIN32

// The solution for GetParentProcess and the thread comes from Mikael who posted it here:
// https://www.magpcss.org/ceforum/viewtopic.php?f=6&t=15817&start=10#p37813

#ifdef WIN32
namespace {
    // Get the HANDLE of the parent process that spawned this process
    HANDLE GetParentProcess() {
        HANDLE snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

        PROCESSENTRY32 processEntry = {};
        processEntry.dwSize = sizeof(PROCESSENTRY32);

        if (Process32First(snapshot, &processEntry)) {
            DWORD currentProcessId = GetCurrentProcessId();

            do {
                if (processEntry.th32ProcessID == currentProcessId) {
                    break;
                }
            } while (Process32Next(snapshot, &processEntry));
        }

        CloseHandle(snapshot);
        return OpenProcess(SYNCHRONIZE, FALSE, processEntry.th32ParentProcessID);
    }
} // namespace
#endif // WIN32

// Entry point function for sub-processes
int main(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow) {
    // Provide CEF with command-line arguments
    CefMainArgs mainArgs = CefMainArgs(hInstance);

    CefRefPtr<openspace::WebBrowserApp> app = new openspace::WebBrowserApp;

#ifdef WIN32
    // This following part is only necessary as we need to guard against OpenSpace getting
    // force terminated. The most common way is by "Stop Debugging" in Visual Studio, but
    // other reasons exist as well.
    // If OpenSpace is terminated, none of its destructors will be executed, which means
    // that `CefShutdown` will not be called which means that this process will never get
    // to learn that OpenSpace is dead.
    // To circumvent that we are:
    //   1. Getting the HANDLE of the parent process (=OpenSpace)
    //   2. Start a new thread that waits for the parent process to die
    //   3. If the parent process die, we kill this process
    // If OpenSpace closes gracefully, the `WaitForSingleObject` will never finish as the
    // `CefShutdown` in OpenSpace will the `CefExecuteProcess` function to finish thus
    // ending the scope and stopping the thread from existing
    HANDLE parent = GetParentProcess();
    std::thread([parent]() {
        // This wait will only continue once the parent process no longer exists
        WaitForSingleObject(parent, INFINITE);

        // Once the parent process is dead, we will kill ourselves
        ExitProcess(0);
    }).detach();
#endif // WIN32

    // Execute the sub-process
    return CefExecuteProcess(mainArgs, app.get(), nullptr);
}
