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

#include <modules/webbrowser/webbrowsermodule.h>

#include <modules/webbrowser/include/browserinstance.h>
#include <modules/webbrowser/include/cefhost.h>
#include <modules/webbrowser/include/screenspacebrowser.h>
#include <openspace/engine/configuration.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "WebBrowser";
} // namespace

namespace openspace {

WebBrowserModule::WebBrowserModule() : OpenSpaceModule(WebBrowserModule::Name) {
    LDEBUG("Starting CEF...");
    std::string helperLocation = findHelperExecutable();
    LDEBUG("Using web helper executable: " + helperLocation);
    _cefHost = std::make_unique<CefHost>(std::move(helperLocation));
    LDEBUG("Starting CEF... done!");

    global::callback::deinitialize.push_back([this]() { deinitialize(); });
}

WebBrowserModule::~WebBrowserModule() {}

void WebBrowserModule::internalDeinitialize() {
    _eventHandler.detachBrowser();

    bool forceBrowserShutdown = true;
    for (const std::shared_ptr<BrowserInstance>& browser : _browsers) {
        browser->close(forceBrowserShutdown);
    }
}

std::string WebBrowserModule::findHelperExecutable() {
    if (!OsEng.configuration().webHelperLocation.empty()) {
        std::string execLocation = absPath(
            OsEng.configuration().webHelperLocation + SUBPROCESS_ENDING
        );
        if (!FileSys.fileExists(execLocation)) {
            LERROR(fmt::format(
                "Could not find web helper executable at location: {}" , execLocation
            ));
        }
        return execLocation;
    }
    else {
        std::string subprocessName = std::string(SUBPROCESS_NAME) + SUBPROCESS_ENDING;
        LWARNING(fmt::format("Assuming web helper name is {}", subprocessName));

        using namespace ghoul::filesystem;
        Directory binDir("${BASE}/bin/openspace", Directory::RawPath::No);
        std::vector<std::string> foundFiles = binDir.readFiles(
            Directory::Recursive::Yes,
            Directory::Sort::Yes
        );

        // find files matching the given file name
        std::vector<std::string> matchingFiles;
        std::copy_if(
            foundFiles.begin(),
            foundFiles.end(),
            std::back_inserter(matchingFiles),
            [subprocessName, subLength = subprocessName.length()](std::string s) {
                s = s.substr(s.size() - subLength);
                return s == subprocessName;
            }
        );

        if (matchingFiles.empty()) {
            LERROR(fmt::format(
                "Could not find requested sub process executable file name: {}",
                subprocessName
            ));
        }

        return matchingFiles.back();
    }
}

void WebBrowserModule::internalInitialize(const ghoul::Dictionary&) {
    _eventHandler.initialize();

    // register ScreenSpaceBrowser
    auto fScreenSpaceRenderable = FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");
    fScreenSpaceRenderable->registerClass<ScreenSpaceBrowser>("ScreenSpaceBrowser");
}

int WebBrowserModule::addBrowser(std::shared_ptr<BrowserInstance> browser) {
    static int browserId = 0;
    _browsers.push_back(browser);
    const int givenId = browserId++;
    return givenId;
}

void WebBrowserModule::removeBrowser(std::shared_ptr<BrowserInstance> browser) {
    const auto p = std::find(_browsers.begin(), _browsers.end(), browser);
    if (p != _browsers.end()) {
        _browsers.erase(p);
    } else {
        LWARNING("Could not find browser in list of browsers.");
    }

    LDEBUG(fmt::format("Number of browsers stored: {}", _browsers.size()));
}

void WebBrowserModule::attachEventHandler(
                                         std::shared_ptr<BrowserInstance> browserInstance)
{
    _eventHandler.setBrowserInstance(browserInstance);
}

} // namespace openspace
