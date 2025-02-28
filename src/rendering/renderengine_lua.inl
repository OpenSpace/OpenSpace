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

#include <ghoul/lua/lua_helper.h>

namespace {

/// Will create a ScreenSpaceRenderable from a lua Table and add it in the RenderEngine
[[codegen::luawrap]] void addScreenSpaceRenderable(ghoul::Dictionary screenSpace) {
    using namespace openspace;
    std::unique_ptr<ScreenSpaceRenderable> s =
        ScreenSpaceRenderable::createFromDictionary(screenSpace);
    global::renderEngine->addScreenSpaceRenderable(std::move(s));
}

/**
 * Given a ScreenSpaceRenderable name this script will remove it from the RenderEngine.
 * The parameter can also be a table in which case the 'Identifier' key is used to look up
 * the name from the table.
 */
[[codegen::luawrap]] void removeScreenSpaceRenderable(
                                  std::variant<std::string, ghoul::Dictionary> identifier)
{
    using namespace openspace;
    std::string identifierStr;
    if (std::holds_alternative<std::string>(identifier)) {
        identifierStr = std::get<std::string>(identifier);
    }
    else {
        ghoul::Dictionary d = std::get<ghoul::Dictionary>(identifier);
        if (!d.hasValue<std::string>("Identifier")) {
            throw ghoul::lua::LuaError("Passed table does not contain an Identifier");
        }
        identifierStr = d.value<std::string>("Identifier");
    }

    global::renderEngine->removeScreenSpaceRenderable(identifierStr);
}

/**
 * Take a screenshot and return the screenshot number. The screenshot will be stored in
 * the ${SCREENSHOTS} folder.
 */
[[codegen::luawrap]] int takeScreenshot() {
    using namespace openspace;
    global::renderEngine->takeScreenshot();
    unsigned int screenshotNumber = global::renderEngine->latestScreenshotNumber();
    return static_cast<int>(screenshotNumber);
}

/**
* Reset screenshot index to 0.
*/
[[codegen::luawrap]] void resetScreenshotNumber() {
    using namespace openspace;
    global::renderEngine->resetScreenshotNumber();
}

// Extracts the DPI scaling for either the GUI window or if there is no dedicated GUI
// window, the first window.
[[codegen::luawrap]] float dpiScaling() {
    return openspace::global::windowDelegate->osDpiScaling();
}

#include "renderengine_lua_codegen.cpp"

} // namespace
