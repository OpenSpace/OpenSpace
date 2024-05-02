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

#include <modules/touch/touchmodule.h>

#include <modules/touch/include/tuioear.h>
#include <modules/touch/include/win32_touch.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/interactionmonitor.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/factorymanager.h>
#include <algorithm>

using namespace TUIO;

namespace {
    constexpr std::string_view _loggerCat = "TouchModule";

    constexpr openspace::properties::Property::PropertyInfo EnableTouchInfo = {
        "EnableTouchInteraction",
        "Enable Touch Interaction",
        "Use this property to turn on/off touch input navigation in the 3D scene. "
        "Disabling will reset all current touch inputs to the navigation.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EventsInfo = {
        "DetectedTouchEvent",
        "Detected Touch Event",
        "True when there is an active touch event.",
        openspace::properties::Property::Visibility::Hidden
    };

    constexpr openspace::properties::Property::PropertyInfo
        DefaultDirectTouchRenderableTypesInfo =
    {
        "DefaultDirectTouchRenderableTypes",
        "Default Direct Touch Renderable Types",
        "A list of renderable types that will automatically use the \'direct "
        "manipulation\' scheme when interacted with, keeping the finger on a static "
        "position on the interaction sphere of the object when touching. Good for "
        "relatively spherical objects.",
        openspace::properties::Property::Visibility::AdvancedUser
    };
} // namespace openspace

namespace openspace {

TouchModule::TouchModule()
    : OpenSpaceModule("Touch")
    , _touchIsEnabled(EnableTouchInfo, true)
    , _hasActiveTouchEvent(EventsInfo, false)
    , _defaultDirectTouchRenderableTypes(DefaultDirectTouchRenderableTypesInfo)
{
    addPropertySubOwner(_touch);
    addPropertySubOwner(_markers);
    addProperty(_touchIsEnabled);
    _touchIsEnabled.onChange([this]() {
        _touch.resetAfterInput();
        _lastTouchInputs.clear();
    });

    _hasActiveTouchEvent.setReadOnly(true);
    addProperty(_hasActiveTouchEvent);

    _defaultDirectTouchRenderableTypes.onChange([this]() {
        _sortedDefaultRenderableTypes.clear();
        for (const std::string& s : _defaultDirectTouchRenderableTypes.value()) {
            ghoul::TemplateFactory<Renderable>* fRenderable =
                FactoryManager::ref().factory<Renderable>();

            if (!fRenderable->hasClass(s)) {
                LWARNING(std::format(
                    "In property 'DefaultDirectTouchRenderableTypes': '{}' is not a "
                    "registered renderable type. Ignoring", s
                ));
                continue;
            }

            _sortedDefaultRenderableTypes.insert(s);
        }
    });
    addProperty(_defaultDirectTouchRenderableTypes);
}

TouchModule::~TouchModule() {
    // intentionally left empty
}

bool TouchModule::isDefaultDirectTouchType(std::string_view renderableType) const {
    return _sortedDefaultRenderableTypes.find(std::string(renderableType)) !=
        _sortedDefaultRenderableTypes.end();
}

void TouchModule::internalInitialize(const ghoul::Dictionary&){
    _ear.reset(new TuioEar());

    global::callback::initializeGL->push_back([this]() {
        LDEBUG("Initializing TouchMarker OpenGL");
        _markers.initialize();
#ifdef WIN32
        // We currently only support one window of touch input internally
        // so here we grab the first window-handle and use it.
        void* nativeWindowHandle = global::windowDelegate->getNativeWindowHandle(0);
        if (nativeWindowHandle) {
            _win32TouchHook = std::make_unique<Win32TouchHook>(nativeWindowHandle);
        }
#endif
    });

    global::callback::deinitializeGL->push_back([this]() {
        LDEBUG("Deinitialize TouchMarker OpenGL");
        _markers.deinitialize();
    });

    // These are handled in UI thread, which (as of 20th dec 2019) is in main/rendering
    // thread so we don't need a mutex here
    global::callback::touchDetected->push_back(
        [this](TouchInput i) {
            addTouchInput(i);
            return true;
        }
    );

    global::callback::touchUpdated->push_back(
        [this](TouchInput i) {
            updateOrAddTouchInput(i);
            return true;
        }
    );

    global::callback::touchExit->push_back(
        std::bind(&TouchModule::removeTouchInput, this, std::placeholders::_1)
    );


    global::callback::preSync->push_back([this]() {
        if (!_touchIsEnabled) {
            return;
        }

        OpenSpaceEngine::Mode mode = global::openSpaceEngine->currentMode();
        if (mode == OpenSpaceEngine::Mode::CameraPath ||
            mode == OpenSpaceEngine::Mode::SessionRecordingPlayback)
        {
            // Reset everything, to avoid problems once we process inputs again
            _lastTouchInputs.clear();
            _touch.resetAfterInput();
            clearInputs();
            return;
        }

        _touch.setCamera(global::navigationHandler->camera());

        bool gotNewInput = processNewInput();
        if (gotNewInput && global::windowDelegate->isMaster()) {
            _touch.updateStateFromInput(_touchPoints, _lastTouchInputs);
        }
        else if (_touchPoints.empty()) {
            _touch.resetAfterInput();
        }

        // Update last processed touch inputs
        _lastTouchInputs.clear();
        for (const TouchInputHolder& points : _touchPoints) {
            _lastTouchInputs.emplace_back(points.latestInput());
        }
        // Calculate the new camera state for this frame
        _touch.step(global::windowDelegate->deltaTime());
        clearInputs();
    });

    global::callback::render->push_back([this]() {
        _markers.render(_touchPoints);
    });
}

bool TouchModule::processNewInput() {
    // Get new input from listener
    std::vector<TouchInput> earInputs = _ear->takeInput();
    std::vector<TouchInput> earRemovals = _ear->takeRemovals();

    for (const TouchInput& input : earInputs) {
        updateOrAddTouchInput(input);
    }
    for (const TouchInput& removal : earRemovals) {
        removeTouchInput(removal);
    }

    bool touchHappened = !_touchPoints.empty();
    _hasActiveTouchEvent = touchHappened;

    // Set touch property to active (to void mouse input, mainly for mtdev bridges)
    if (touchHappened) {
        global::interactionMonitor->markInteraction();
    }

    // Erase old input ids that no longer exist
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
        // @TODO (emmbr26, 2023-02-03) Looks to me like this code will always return
        // true? That's a bit weird and should probably be investigated
        bool newInput = true;
        // Go through list and check if the last registrered time is newer than the
        // last processed touch inputs (last frame)
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
            }
        );
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
        if (inputHolder.holdsInput(input)) {
            inputHolder.tryAddInput(input);
            return;
        }
    }
    _touchPoints.emplace_back(input);
}

void TouchModule::removeTouchInput(TouchInput input) {
    _deferredRemovals.emplace_back(input);
    // Check for "tap" gesture:
    for (TouchInputHolder& inputHolder : _touchPoints) {
        if (inputHolder.holdsInput(input)) {
            inputHolder.tryAddInput(input);
            const double totalTime = inputHolder.gestureTime();
            const float totalDistance = inputHolder.gestureDistance();
            // Magic values taken from tuioear.cpp:
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

} // namespace openspace
