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

#ifndef __OPENSPACE_MODULE_WEBBROWSER__BROWSER_INSTANCE_H
#define __OPENSPACE_MODULE_WEBBROWSER__BROWSER_INSTANCE_H

#include <ghoul/filesystem/filesystem.h>
#include <openspace/engine/moduleengine.h>
#include <include/wrapper/cef_helpers.h>
#include "modules/webbrowser/include/web_render_handler.h"
#include "modules/webbrowser/include/event_handler.h"
#include "modules/webbrowser/include/browser_client.h"

namespace openspace {

class BrowserInstance {
public:
    BrowserInstance(WebRenderHandler*);
    ~BrowserInstance();

    void load(const std::string&);
    bool loadLocalPath(std::string);
    void initialize();
    void reshape(const glm::ivec2&);
    void draw();

private:
    CefRefPtr<WebRenderHandler> renderHandler;
    CefRefPtr<BrowserClient> client;
    CefRefPtr<CefBrowser> browser;
    bool isInitialized;
};

}

#endif //__OPENSPACE_MODULE_WEBBROWSER__BROWSER_INSTANCE_H
