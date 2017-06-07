/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include "include/cef_host.h"

namespace {
const char* _loggerCat = "CefHost";
}

namespace openspace {

CefHost::CefHost() {
    LDEBUG("Initializing CEF...");
    CefMainArgs args;
    CefSettings settings;

    CefString(&settings.browser_subprocess_path).FromASCII((char*) SUBPROCESS_PATH.c_str());
    attachDebugSettings(settings);

    CefInitialize(args, settings, nullptr, NULL);
    initializeCallbacks();
    LDEBUG("Initializing CEF... done!");
}

CefHost::~CefHost() {
    CefShutdown();
}

void CefHost::attachDebugSettings(CefSettings &settings) {
    settings.remote_debugging_port = 8088;
    LDEBUG(fmt::format("Remote WebGUI debugging available on http://localhost:{}", settings.remote_debugging_port));
}

void CefHost::deinitialize() {
}

void CefHost::initializeCallbacks() {
    OsEng.registerModuleCallback(
            OpenSpaceEngine::CallbackOption::Deinitialize,
            [this]() {
                deinitialize();
            }
    );
    OsEng.registerModuleCallback(
            OpenSpaceEngine::CallbackOption::Render,
            [this](){
                CefDoMessageLoopWork();
//                render();
            }
    );

}

}

