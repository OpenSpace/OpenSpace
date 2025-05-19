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

#include <modules/telemetry/telemetrymodule.h>
#include <openspace/engine/moduleengine.h>

namespace {

/**
 * Adds the given list of nodes to the NodesTelemetry's internal list.
 */
[[codegen::luawrap]] void addNodes(ghoul::Dictionary nodes) {
    openspace::TelemetryModule* module =
        openspace::global::moduleEngine->module<openspace::TelemetryModule>();
    if (!module) {
        throw ghoul::lua::LuaError("Could not find the TelemetryModule");
        return;
    }
    openspace::TelemetryBase* ptr = module->telemetry("NodesTelemetry");
    if (!ptr) {
        throw ghoul::lua::LuaError("Could not find the NodesTelemetry");
        return;
    }

    openspace::NodesTelemetry* nodesTelemetry =
        dynamic_cast<openspace::NodesTelemetry*>(ptr);

    for (const std::string_view& k : nodes.keys()) {
        std::string node = nodes.value<std::string>(k);
        nodesTelemetry->addNode(std::move(node));
    }
}

#include "nodestelemetry_lua_codegen.cpp"

} // namespace
