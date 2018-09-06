/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_WEBBROWSER___WEBBROWSERMODULE___H__
#define __OPENSPACE_MODULE_WEBBROWSER___WEBBROWSERMODULE___H__

#include <openspace/util/openspacemodule.h>

#include <modules/webbrowser/include/eventhandler.h>

namespace openspace {

class CefHost;

constexpr const char* SUBPROCESS_NAME = "openspace_web_helper";

#ifdef WIN32
constexpr const char* SUBPROCESS_ENDING = ".exe";
#else
constexpr const char* SUBPROCESS_ENDING = "";
#endif

class WebBrowserModule : public OpenSpaceModule {
public:
    static constexpr const char* Name = "WebBrowser";
    WebBrowserModule();
    virtual ~WebBrowserModule();

    int addBrowser(std::shared_ptr<BrowserInstance>);
    void removeBrowser(std::shared_ptr<BrowserInstance>);

    void attachEventHandler(std::shared_ptr<BrowserInstance> browserInstance);

protected:
    void internalInitialize(const ghoul::Dictionary& configuration) override;
    void internalDeinitialize() override;

private:
    /**
     * Try to find the CEF Helper executable. It looks in the bin/openspace folder.
     * Therefore, if you change that this might cause a crash here.
     *
     * \return the absolute path to the file
     */
    std::string findHelperExecutable();

    std::vector<std::shared_ptr<BrowserInstance>> _browsers;
    EventHandler _eventHandler;
    std::unique_ptr<CefHost> _cefHost;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_WEBBROWSER___WEBBROWSERMODULE___H__
