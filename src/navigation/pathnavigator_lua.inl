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

/**
 * Continue playing a paused camera path.
 */
[[codegen::luawrap]] void continuePath() {
    openspace::global::navigationHandler->pathNavigator().continuePath();
}

/**
 * Pause a playing camera path.
 */
[[codegen::luawrap]] void pausePath() {
    openspace::global::navigationHandler->pathNavigator().pausePath();
}

/**
 * Stops a path, if one is being played.
 */
[[codegen::luawrap]] void stopPath() {
    openspace::global::navigationHandler->pathNavigator().abortPath();
}

/**
 * Immediately skips to the end of the current camera path, if one is being played.
 */
[[codegen::luawrap]] void skipToEnd() {
    openspace::global::navigationHandler->pathNavigator().skipToEnd();
}

/**
 * Create a camera path as described by the instruction in the input argument.
 *
 * \param pathInstruction A table representing a
 *                        [PathInstruction](#core_path_instruction) that describes a
 *                        camera path to be created
 */
[[codegen::luawrap]] void createPath(ghoul::Dictionary pathInstruction) {
    using namespace openspace;
    global::navigationHandler->pathNavigator().createPath(pathInstruction);
    if (global::navigationHandler->pathNavigator().hasCurrentPath()) {
        global::navigationHandler->pathNavigator().startPath();
    }
}

#include "pathnavigator_lua_codegen.cpp"

} // namespace
