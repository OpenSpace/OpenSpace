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

#include <include/openspace/util/factorymanager.h>
#include <include/screen_space_browser.h>
#include "webbrowsermodule.h"

namespace {
const char* _loggerCat = "WebBrowser";
}

using ghoul::filesystem::Directory;

namespace openspace {

WebBrowserModule::WebBrowserModule() : OpenSpaceModule(WebBrowserModule::Name) {
    LDEBUG("Starting CEF...");
    CefMainArgs args;
    CefSettings settings;

    CefString(&settings.browser_subprocess_path).FromASCII((char*) findHelperExecutable().c_str());
    attachDebugSettings(settings);

    CefInitialize(args, settings, nullptr, NULL);
    LDEBUG("Starting CEF... done!");
}

/**
 * Try to find the CEF Helper executable. It looks in the bin/openspace folder. Therefore,
 * if you change that this might cause a crash here.
 * @return the absolute path to the file
 */
std::string WebBrowserModule::findHelperExecutable() {
    std::string subprocessName = OsEng.configurationManager().value<std::string>(
            ConfigurationManager::KeyWebHelperLocation);
    subprocessName += SUBPROCESS_ENDING;
	int subLength = (int) subprocessName.length();

    Directory binDir("${BASE_PATH}/bin/openspace", Directory::AbsolutePath::No);
    std::vector<std::string> foundFiles = binDir.readFiles(Directory::Recursive::Yes, Directory::Sort::Yes);

    // find files matching the given file name
    std::vector<std::string> matchingFiles;
    std::copy_if(foundFiles.begin(), foundFiles.end(), std::back_inserter(matchingFiles),
                 [subprocessName, subLength](std::string s) {
                     s = s.substr(s.size() - subLength);
                     return s == subprocessName;
                 });

    if (matchingFiles.size() == 0) {
        LERROR(fmt::format("Could not find requested sub process executable file name: {}", subprocessName));
    }

    return matchingFiles.back();
}

WebBrowserModule::~WebBrowserModule() {
    // we're shutting down, force close all browsers
    for (auto const& browser : browsers) {
        browser->GetHost()->CloseBrowser(true);
    }

    CefShutdown();
}

void WebBrowserModule::attachDebugSettings(CefSettings &settings) {
    settings.remote_debugging_port = 8088;
    LDEBUG(fmt::format("Remote WebBrowser debugging available on http://localhost:{}", settings.remote_debugging_port));
}

void WebBrowserModule::internalInitialize() {
    // register ScreenSpaceRenderable
    auto fScreenSpaceRenderable = FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");
    fScreenSpaceRenderable->registerClass<ScreenSpaceBrowser>("ScreenSpaceBrowser");

    // fire up callbacks used for rendering etc
    OsEng.registerModuleCallback(
            OpenSpaceEngine::CallbackOption::Render,
            [this](){
                CefDoMessageLoopWork();
            }
    );
}

int WebBrowserModule::addBrowser(CefBrowser *browser) {
    static int browserId = 0;
    browsers.push_back(browser);
    int givenId = browserId++;
    return givenId;
}

void WebBrowserModule::removeBrowser(CefBrowser *browser) {
    auto p = std::find(browsers.begin(), browsers.end(), browser);
    if (p != browsers.end()) {
        browsers.erase(p);
    } else {
        LWARNING("Could not find browser in list of browsers.");
    }
}

}
