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

#include <modules/touch/touchmodule.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/settingsengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>

#include <ghoul/logging/logmanager.h>
#include <glm/ext.hpp>

#include <sstream>
#include <string>
#include <iostream>

using namespace TUIO;

namespace {
    const std::string _loggerCat = "TouchModule";
}

namespace openspace {

bool TouchModule::hasNewInput() {
    // Get new input from listener
    listOfContactPoints = ear.getInput();
    ear.clearInput();
    touch.touchActive(!listOfContactPoints.empty()); // Set touch property to active (to void mouse input, mainly for mtdev bridges)

    // Erase old input id's that no longer exists
    lastProcessed.erase(
        std::remove_if(
            lastProcessed.begin(),
            lastProcessed.end(),
            [this](const Point& point) {
        return std::find_if(
            listOfContactPoints.begin(),
            listOfContactPoints.end(),
            [&point](const TuioCursor& c) {
            return point.first == c.getSessionID();
        }
        ) == listOfContactPoints.end(); }),
        lastProcessed.end());

    // if tap occured, we have new input
    if (listOfContactPoints.empty() && lastProcessed.empty() && ear.tap()) {
        TuioCursor c = ear.getTap();
        listOfContactPoints.push_back(c);
        lastProcessed.emplace_back(c.getSessionID(), c.getPath().back());
        touch.tap();
        return true;
    }
    
    // Return true if we got new input
    if (listOfContactPoints.size() == lastProcessed.size() && !listOfContactPoints.empty()) {
        bool newInput = true;
        // go through list and check if the last registrered time is newer than the one in lastProcessed (last frame)
        std::for_each(lastProcessed.begin(), lastProcessed.end(), [this, &newInput](Point& p) {
            std::vector<TuioCursor>::iterator cursor = std::find_if(listOfContactPoints.begin(), listOfContactPoints.end(), 
                [&p](const TuioCursor& c) { return c.getSessionID() == p.first; });
            double now = cursor->getPath().back().getTuioTime().getTotalMilliseconds();
            if (!cursor->isMoving()) { // if current cursor isn't moving, we want to interpret that as new input for interaction purposes
                newInput = true;
            }
            else if (p.second.getTuioTime().getTotalMilliseconds() == now) {
                newInput = false;
            }
        });
        return newInput;
    }		
    else {
        return false;
    }
}

TouchModule::TouchModule()
    : OpenSpaceModule("Touch")
{
    addPropertySubOwner(touch);
    addPropertySubOwner(markers);

    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::InitializeGL,
        [&]() {
        LDEBUGC("TouchModule", "Initializing TouchMarker OpenGL");
        markers.initialize();
    }
    );

    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::DeinitializeGL,
        [&]() {
        LDEBUGC("TouchMarker", "Deinitialize TouchMarker OpenGL");
        markers.deinitialize();
    }
    );

    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::PreSync,
        [&]() {
        touch.setCamera(OsEng.interactionHandler().camera());
        touch.setFocusNode(OsEng.interactionHandler().focusNode());

        if (hasNewInput() && OsEng.windowWrapper().isMaster()) {
            touch.updateStateFromInput(listOfContactPoints, lastProcessed);
        }
        else if (listOfContactPoints.empty()) {
            touch.resetAfterInput();
        }

        // update lastProcessed
        lastProcessed.clear();
        for (const TuioCursor& c : listOfContactPoints) {
            lastProcessed.emplace_back(c.getSessionID(), c.getPath().back());
        }
        touch.unitTest(); // used to save data from solver, only calculated for one frame when user chooses in GUI
        touch.step(OsEng.windowWrapper().deltaTime()); // calculate the new camera state for this frame
    }
    );

    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::Render,
        [&]() {
        markers.render(listOfContactPoints); // render markers, customizable through the GUI
    }
    );
}

} // namespace openspace
