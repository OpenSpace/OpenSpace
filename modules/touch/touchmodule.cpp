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

#include <modules/touch/touchmodule.h>

#include <modules/touch/include/tuioear.h>
#include <modules/touch/include/win32_touch.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/interaction/interactionmonitor.h>
#include <openspace/interaction/touchinputstate.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/touch.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/templatefactory.h>
#include <algorithm>
#include <functional>
#include <optional>
#include <utility>

using namespace TUIO;

namespace {
    constexpr std::string_view _loggerCat = "TouchModule";

    constexpr openspace::properties::Property::PropertyInfo TuioPortInfo = {
        "TuioPort",
        "TUIO Port",
        "TUIO UDP port, by default 3333. The port cannot be changed after startup.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo EventsInfo = {
        "DetectedTouchEvent",
        "Detected touch event",
        "True when there is an active touch TUIO event.",
        openspace::properties::Property::Visibility::Hidden
    };

    constexpr openspace::properties::Property::PropertyInfo
        DefaultDirectTouchRenderableTypesInfo =
    {
        "DefaultDirectTouchRenderableTypes",
        "Default direct touch renderable types",
        "A list of renderable types that will automatically use the \'direct "
        "manipulation\' scheme when interacted with, keeping the finger on a static "
        "position on the interaction sphere of the object when touching. Good for "
        "relatively spherical objects.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(TouchModule)]] Parameters {
        // [[codegen::verbatim(TuioPortInfo.description)]]
        std::optional<int> tuioPort [[codegen::inrange(1, 65535)]];
    };

    #include "touchmodule_codegen.cpp"
} // namespace

namespace openspace {

TouchModule::TouchModule()
    : OpenSpaceModule("Touch")
    , _tuioPort(TuioPortInfo, 3333, 1, 65535)
    , _hasActiveTouchEvent(EventsInfo, false)
    , _defaultDirectTouchRenderableTypes(DefaultDirectTouchRenderableTypesInfo)
{
    addPropertySubOwner(_touch);
    addPropertySubOwner(_markers);
    _tuioPort.setReadOnly(true);
    addProperty(_tuioPort);

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

void TouchModule::internalInitialize(const ghoul::Dictionary& dict) {
    const Parameters p = codegen::bake<Parameters>(dict);

    _tuioPort = p.tuioPort.value_or(_tuioPort);
    _ear = std::make_unique<TuioEar>(_tuioPort);

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
#endif // WIN32
    });

    global::callback::deinitializeGL->push_back([this]() {
        LDEBUG("Deinitialize TouchMarker OpenGL");
        _markers.deinitialize();
    });

    global::callback::preSync->push_back([this]() {
        interaction::TouchInputState& touchInputState =
            global::interactionHandler->touchInputState();

        if (global::interactionHandler->disabledTouch()) {
            _touch.resetAfterInput();
            touchInputState.clearInputs();
            return;
        }

        // TODO: Move to interaction handler or navigation handler?
        OpenSpaceEngine::Mode mode = global::openSpaceEngine->currentMode();
        if (mode == OpenSpaceEngine::Mode::CameraPath ||
            mode == OpenSpaceEngine::Mode::SessionRecordingPlayback)
        {
            // Reset everything, to avoid problems once we process inputs again
            _touch.resetAfterInput();
            touchInputState.clearInputs();
            return;
        }

        _touch.setCamera(global::navigationHandler->camera());

        bool inputIsvalidForUpdate = processNewInput();
        if (inputIsvalidForUpdate && global::windowDelegate->isMaster()) {
            _touch.updateVelocitiesFromInput(touchInputState); // TODO: Make camera states
        }
        else if (touchInputState.touchPoints().empty()) {
            _touch.resetAfterInput();
        }

        // Calculate the new camera state for this frame
        _touch.step(global::windowDelegate->deltaTime());

        // TODO: This should be moved somewhere more global, as it's the thing that updates the
        // last processed touch inputs. Interaction handler?
        touchInputState.updateLastTouchPoints();

        touchInputState.clearInputs();
    });

    global::callback::render->push_back([this]() {
        if (global::interactionHandler->disabledTouch()) {
            return;
        }
        const std::vector<TouchInputHolder>& touchPoints =
            global::interactionHandler->touchInputState().touchPoints();

        _markers.render(touchPoints);
    });
}

bool TouchModule::processNewInput() {
    // Get new input from listener
    std::vector<TouchInput> earInputs = _ear->takeInputs();
    std::vector<TouchInput> earRemovals = _ear->takeRemovals();

    interaction::TouchInputState& touchInputState =
        global::interactionHandler->touchInputState();

    bool inputIsvalidForUpdate = touchInputState.processTouchInput(
        earInputs,
        earRemovals
    );

    const std::vector<TouchInputHolder>& touchPoints = touchInputState.touchPoints();

    bool touchHappened = !touchPoints.empty();
    _hasActiveTouchEvent = touchHappened;

    // Set touch property to active (to void mouse input, mainly for mtdev bridges)
    if (touchHappened) {
        global::interactionHandler->markInteraction();
    }

    return inputIsvalidForUpdate;
}

} // namespace openspace
