/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/softwareintegration/session/session.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>

namespace {

/**
 * Loads the data associated to the loaded session into the syncable data storage in the
 * Software Integration Module
 *
 */
[[codegen::luawrap]] std::string loadSessionData(std::string filePath) {
    using namespace openspace;

    if (!global::windowDelegate->isMaster()) {
        throw ghoul::lua::LuaError("Not on master...");
    }

    auto softwareIntegrationModule = global::moduleEngine->module<SoftwareIntegrationModule>();
    if (!softwareIntegrationModule) {
        throw ghoul::lua::LuaError("Module not found...");
    }

    std::string errorMessage;
    if (!softwareintegration::Session::loadSessionData(softwareIntegrationModule, filePath, errorMessage)) {
        return errorMessage;
    }
    else {
        return "Success";
    }
}

/**
 * Saves the current state of the Software Integration Module
 *
 */
[[codegen::luawrap]] std::string saveSession(std::string wantedFilePath) {
    using namespace openspace;

    if (!global::windowDelegate->isMaster()) {
        throw ghoul::lua::LuaError("Not on master...");
    }

    std::string errorMessage;
    if (!softwareintegration::Session::saveSession(wantedFilePath, errorMessage)) {
        LERRORC("SoftwareIntegration::saveSession", errorMessage);
        return errorMessage;
    }
    else {
        return "Success";
    }
}

#include "softwareintegrationmodule_lua_codegen.cpp"

}  // namespace
