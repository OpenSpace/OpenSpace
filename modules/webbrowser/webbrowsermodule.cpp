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
#include <include/screenspacebrowser.h>
#include "webbrowsermodule.h"

namespace {
const char* _loggerCat = "WebBrowser";
}

using ghoul::filesystem::Directory;

namespace openspace {

WebBrowserModule::WebBrowserModule() : OpenSpaceModule(WebBrowserModule::Name) {
    LDEBUG("Starting CEF...");
    auto helperLocation = findHelperExecutable();
    LDEBUG("Using web helper executable: " + helperLocation);
    _cefHost = std::make_unique<CefHost>(helperLocation);
    LDEBUG("Starting CEF... done!");

    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::Deinitialize,
        [this](){ deinitialize(); }
    );
}

WebBrowserModule::~WebBrowserModule() {}

void WebBrowserModule::deinitialize() {
    _eventHandler.detachBrowser();

    bool forceBrowserShutdown = true;
    for (auto browser : _browsers) {
        browser->close(forceBrowserShutdown);
    }
}

/**
 * Try to find the CEF Helper executable. It looks in the bin/openspace folder. Therefore,
 * if you change that this might cause a crash here.
 * @return the absolute path to the file
 */
std::string WebBrowserModule::findHelperExecutable() {
    if (OsEng.configurationManager().hasKey(ConfigurationManager::KeyWebHelperLocation)) {
        auto execLocation = absPath(OsEng.configurationManager().value<std::string>(
            ConfigurationManager::KeyWebHelperLocation) + SUBPROCESS_ENDING);
        if (!FileSys.fileExists(execLocation)) {
            LERROR("Could not find web helper executable at location: " + execLocation);
        }
        return execLocation;
    }
    else {
        std::string subprocessName = SUBPROCESS_NAME;
        subprocessName += SUBPROCESS_ENDING;
        LWARNING("Assuming web helper name is " + subprocessName);
        auto subLength = (int)subprocessName.length();

        Directory binDir("${BASE}/bin/openspace", Directory::AbsolutePath::No);
        std::vector<std::string> foundFiles = binDir.readFiles(Directory::Recursive::Yes, Directory::Sort::Yes);

        // find files matching the given file name
        std::vector<std::string> matchingFiles;
        std::copy_if(foundFiles.begin(), foundFiles.end(), std::back_inserter(matchingFiles),
            [subprocessName, subLength](std::string s) {
            s = s.substr(s.size() - subLength);
            return s == subprocessName;
        });

        if (matchingFiles.empty()) {
            LERROR(fmt::format("Could not find requested sub process executable file name: {}", subprocessName));
        }

        return matchingFiles.back();

    }
}

void WebBrowserModule::internalInitialize() {
    _eventHandler.initialize();

    // register ScreenSpaceBrowser
    auto fScreenSpaceRenderable = FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");
    fScreenSpaceRenderable->registerClass<ScreenSpaceBrowser>("ScreenSpaceBrowser");
}

int WebBrowserModule::addBrowser(std::shared_ptr<BrowserInstance> browser) {
    static int browserId = 0;
    _browsers.push_back(browser);
    int givenId = browserId++;
    return givenId;
}

void WebBrowserModule::removeBrowser(std::shared_ptr<BrowserInstance> browser) {
    auto p = std::find(_browsers.begin(), _browsers.end(), browser);
    if (p != _browsers.end()) {
        _browsers.erase(p);
    } else {
        LWARNING("Could not find browser in list of browsers.");
    }

    LDEBUG(fmt::format("Number of browsers stored: {}", _browsers.size()));
}

void WebBrowserModule::attachEventHandler(std::shared_ptr<BrowserInstance> browserInstance) {
    _eventHandler.setBrowserInstance(browserInstance);
}

}
