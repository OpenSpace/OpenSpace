/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

namespace {

/**
 * Adds the given list of planets to the PlanetsSonification internal list of Planets
 * and Moons
 */
[[codegen::luawrap]] void addNodes(ghoul::Dictionary nodes) {
    openspace::TelemetryModule* module =
        openspace::global::moduleEngine->module<openspace::TelemetryModule>();
    if (!module) {
        LERRORC("NodesSonification_lua", "Could not find the SonificationModule");
        return;
    }
    openspace::TelemetryBase* ptr = module->sonification("NodesSonification");
    if (!ptr) {
        LERRORC("NodesSonification_lua", "Could not find the NodesSonification");
        return;
    }

    openspace::NodesTelemetry* nodesSonification =
        reinterpret_cast<openspace::NodesTelemetry*>(ptr);

    for (const std::string_view& k : nodes.keys()) {
        std::string node = nodes.value<std::string>(k);
        nodesSonification->addNode(node);
    }
}

#include "nodestelemetry_lua_codegen.cpp"
} // namespace
