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

#include <modules/researchkit/researchkitmodule.h>

#include <modules/researchkit/timing/timing.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/configurationmanager.h>

#include <ghoul/logging/logmanager.h>

namespace {
    const std::string _loggerCat = "ResearchKit";
}

namespace openspace {

ResearchKitModule::ResearchKitModule() 
    : OpenSpaceModule("ResearchKit")
    , _applicationTimer(nullptr) {

    internalInitialize();

    if (_applicationTimer) { addPropertySubOwner(_applicationTimer); };
}

void ResearchKitModule::internalInitialize() {
    if (!OpenSpaceEngine::isCreated()) {
        return;
    }

    _applicationTimer = new rk::timing::ShutdownTimer;
    registerCallbacks();
}

void ResearchKitModule::registerCallbacks() {

    rk::timing::ShutdownTimer * appTimer = dynamic_cast<rk::timing::ShutdownTimer*>(_applicationTimer);

    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::Initialize,
        []() {
        LDEBUG("Initializing Research Kit");
    });

    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::InitializeGL,
        []() {
        LDEBUG("Initializing Research Kit OpenGL");
    });

    if (_applicationTimer) {
        LDEBUG("Adding application-wide timer callback to PostDraw");
        OsEng.registerModuleCallback(
            OpenSpaceEngine::CallbackOption::PostDraw,
            [appTimer]() {
            appTimer->tick();
        });
    }
}

} // namespace openspace
