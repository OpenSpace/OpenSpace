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

#include <openspace/engine/globals.h>

#include <openspace/engine/configuration.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/network/networkengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/timemanager.h>

namespace openspace::detail {

Configuration& gConfiguration() {
    static Configuration g;
    return g;
}

ModuleEngine& gModuleEngine() {
    static ModuleEngine g;
    return g;
}

NetworkEngine& gNetworkEngine() {
    static NetworkEngine g;
    return g;
}

RenderEngine& gRenderEngine() {
    static RenderEngine g;
    return g;
}

TimeManager& gTimeManager() {
    static TimeManager g;
    return g;
}

} // namespace openspace::detail
