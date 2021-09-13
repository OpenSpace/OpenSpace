/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <openspace/engine/globals.h>

namespace openspace::luascriptfunctions {

int addScreenSpaceRenderable(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::addScreenSpaceRenderable");
    const ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

    std::unique_ptr<ScreenSpaceRenderable> s =
       ScreenSpaceRenderable::createFromDictionary(d);

    global::renderEngine->addScreenSpaceRenderable(std::move(s));
    return 0;
}

int removeScreenSpaceRenderable(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::removeScreenSpaceRenderable");
    const std::string name = ghoul::lua::value<std::string>(L);

    global::renderEngine->removeScreenSpaceRenderable(name);
    return 0;
}

int takeScreenshot(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::takeScreenshot");
    global::renderEngine->takeScreenshot();
    const unsigned int screenshotNumber = global::renderEngine->latestScreenshotNumber();
    ghoul::lua::push(L, screenshotNumber);
    return 1;
}

}// namespace openspace::luascriptfunctions
