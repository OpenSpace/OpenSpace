/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <modules/touch/include/tuioear.h>
#include <modules/touch/include/win32_touch.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/interactionmonitor.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <ghoul/logging/logmanager.h>
#include <sstream>
#include <string>

using namespace TUIO;

namespace {
    constexpr openspace::properties::Property::PropertyInfo TouchActiveInfo = {
        "TouchActive",
        "True if we want to use touch input as 3d navigation",
        "Use this if we want to turn on or off Touch input navigation. "
        "Disabling this will reset all current touch inputs to the navigation. "
    };
}
namespace openspace {

bool TouchModule::processNewInput() {
    // Get new input from listener
    std::vector<TouchInput> earInputs = _ear->takeInput();
    std::vector<TouchInput> earRemovals = _ear->takeRemovals();

    for(const TouchInput& input : earInputs) {
        updateOrAddTouchInput(input);
    }
    for(const TouchInput& removal : earRemovals) {
        removeTouchInput(removal);
    }

     // Set touch property to active (to void mouse input, mainly for mtdev bridges)
    _touch.touchActive(!_touchPoints.empty());

    if (!_touchPoints.empty()) {
        global::interactionMonitor.markInteraction();
    }

    // Erase old input id's that no longer exists
    _lastTouchInputs.erase(
        std::remove_if(
            _lastTouchInputs.begin(),
            _lastTouchInputs.end(),
            [this](const TouchInput& input) {
                return !std::any_of(
                    _touchPoints.cbegin(),
                    _touchPoints.cend(),
                    [&input](const TouchInputHolder& holder) {
                        return holder.holdsInput(input);
                    }
                );
            }
        ),
        _lastTouchInputs.end()
    );

    if (_tap) {
        _touch.tap();
        _tap = false;
        return true;
    }

    // Return true if we got new input
    if (_touchPoints.size() == _lastTouchInputs.size() &&
        !_touchPoints.empty())
    {
        bool newInput = true;
        // go through list and check if the last registrered time is newer than the one in
        // lastProcessed (last frame)
        std::for_each(
            _lastTouchInputs.begin(),
            _lastTouchInputs.end(),
            [this, &newInput](TouchInput& input) {
                std::vector<TouchInputHolder>::iterator holder = std::find_if(
                    _touchPoints.begin(),
                    _touchPoints.end(),
                    [&input](const TouchInputHolder& inputHolder) {
                        return inputHolder.holdsInput(input);
                    }
            );
            if (!holder->isMoving()) {
                newInput = true;
            }
        });
        return newInput;
    }
    else {
        return false;
    }
}

void TouchModule::clearInputs() {
    for (const TouchInput& input : _deferredRemovals) {
        for (TouchInputHolder& inputHolder : _touchPoints) {
            if (inputHolder.holdsInput(input)) {
                inputHolder = std::move(_touchPoints.back());
                _touchPoints.pop_back();
                break;
            }
        }
    }
    _deferredRemovals.clear();
}

void TouchModule::addTouchInput(TouchInput input) {
    _touchPoints.emplace_back(input);
}

void TouchModule::updateOrAddTouchInput(TouchInput input) {
    for (TouchInputHolder& inputHolder : _touchPoints) {
        if (inputHolder.holdsInput(input)){
            inputHolder.tryAddInput(input);
            return;
        }
    }
    _touchPoints.emplace_back(input);
}

void TouchModule::removeTouchInput(TouchInput input) {
    _deferredRemovals.emplace_back(input);
    //Check for "tap" gesture:
    for (TouchInputHolder& inputHolder : _touchPoints) {
        if (inputHolder.holdsInput(input)) {
            inputHolder.tryAddInput(input);
            const double totalTime = inputHolder.gestureTime();
            const float totalDistance = inputHolder.gestureDistance();
            //Magic values taken from tuioear.cpp:
            const bool isWithinTapTime = totalTime < 0.18;
            const bool wasStationary = totalDistance < 0.0004f;
            if (isWithinTapTime && wasStationary && _touchPoints.size() == 1 &&
                _deferredRemovals.size() == 1)
            {
                _tap = true;
            }
            return;
        }
    }
}

TouchModule::TouchModule()
    : OpenSpaceModule("Touch")
    , _touchActive(TouchActiveInfo, true)
{
    addPropertySubOwner(_touch);
    addPropertySubOwner(_markers);
    addProperty(_touchActive);
    _touchActive.onChange([&] {
        _touch.resetAfterInput();
        _lastTouchInputs.clear();
    });
}

TouchModule::~TouchModule() {
    // intentionally left empty
}

void TouchModule::internalInitialize(const ghoul::Dictionary& /*dictionary*/){
    _ear.reset(new TuioEar());

    global::callback::initializeGL.push_back([&]() {
        LDEBUGC("TouchModule", "Initializing TouchMarker OpenGL");
        _markers.initialize();
#ifdef WIN32
        // We currently only support one window of touch input internally
        // so here we grab the first window-handle and use it.
        void* nativeWindowHandle = global::windowDelegate.getNativeWindowHandle(0);
        if (nativeWindowHandle) {
            _win32TouchHook = std::make_unique<Win32TouchHook>(nativeWindowHandle);
        }
#endif
    });

    global::callback::deinitializeGL.push_back([&]() {
        LDEBUGC("TouchMarker", "Deinitialize TouchMarker OpenGL");
        _markers.deinitialize();
    });

    // These are handled in UI thread, which (as of 20th dec 2019) is in main/rendering
    // thread so we don't need a mutex here
    global::callback::touchDetected.push_back(
        [this](TouchInput i) {
            addTouchInput(i);
            return true;
        }
    );

    global::callback::touchUpdated.push_back(
        [this](TouchInput i) {
            updateOrAddTouchInput(i);
            return true;
        }
    );

    global::callback::touchExit.push_back(
        std::bind(&TouchModule::removeTouchInput, this, std::placeholders::_1)
    );


    global::callback::preSync.push_back([&]() {
        _touch.setCamera(global::navigationHandler.camera());
        _touch.setFocusNode(global::navigationHandler.orbitalNavigator().anchorNode());

        if (processNewInput() && global::windowDelegate.isMaster() && _touchActive) {
            _touch.updateStateFromInput(_touchPoints, _lastTouchInputs);
        }
        else if (_touchPoints.empty()) {
            _touch.resetAfterInput();
        }

        // update lastProcessed
        _lastTouchInputs.clear();
        for (const TouchInputHolder& points : _touchPoints) {
            _lastTouchInputs.emplace_back(points.latestInput());
        }
        // calculate the new camera state for this frame
        _touch.step(global::windowDelegate.deltaTime());
        clearInputs();
    });

    global::callback::render.push_back([&]() {
        _markers.render(_touchPoints);
    });
}

} // namespace openspace
