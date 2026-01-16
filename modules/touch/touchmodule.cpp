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
{
    addPropertySubOwner(_markers);
    _tuioPort.setReadOnly(true);
    addProperty(_tuioPort);

    _hasActiveTouchEvent.setReadOnly(true);
    addProperty(_hasActiveTouchEvent);
}

TouchModule::~TouchModule() {
    // intentionally left empty
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
        processNewInput();
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

void TouchModule::processNewInput() {
    // Get new input from listener
    std::vector<TouchInput> earInputs = _ear->takeInputs();
    std::vector<TouchInput> earRemovals = _ear->takeRemovals();

    interaction::TouchInputState& touchInputState =
        global::interactionHandler->touchInputState();

    touchInputState.processTouchInput(earInputs, earRemovals);

    _hasActiveTouchEvent = touchInputState.touchHappened();
}

} // namespace openspace
