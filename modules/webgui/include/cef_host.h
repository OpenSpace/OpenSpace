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

#ifndef OPENSPACE_MODULE_WEBGUI__CEF_HOST_H
#define OPENSPACE_MODULE_WEBGUI__CEF_HOST_H

#include <ghoul/filesystem/filesystem.h>
#include <include/wrapper/cef_helpers.h>
#include "include/browser_client.h"
#include "include/gui_render_handler.h"
#include "include/event_handler.h"

namespace openspace {

#ifdef __APPLE__
static const std::string SUBPROCESS_PATH = "./webgui_cef_helper";
#endif
#ifdef WIN32
static const std::string SUBPROCESS_PATH = "C:\\Users\\Klas\\Development\\OpenSpace\\bin\\openspace\\Debug\\webgui_cef_helper.exe";
#endif

class CefHost {
public:
    CefHost();
    ~CefHost();

    void load(const std::string&);
    bool loadLocalPath(std::string);
    void internalInitialize();

protected:
    void initialize();
    void initializeCallbacks();
private:
    void render();
    void deinitialize();
    void attachDebugSettings(CefSettings&);

    std::shared_ptr<EventHandler> eventHandler;

    CefRefPtr<GUIRenderHandler> renderHandler;
    CefRefPtr<BrowserClient> client;
    CefRefPtr<CefBrowser> browser;

    void reshape(WindowWrapper&);
};

} // namespace openspace

#endif //OPENSPACE_MODULE_WEBGUI__CEF_HOST_H
