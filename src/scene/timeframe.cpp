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

#include <openspace/scene/timeframe.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/memorymanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/templatefactory.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo IsInTimeFrameInfo = {
        "IsInTimeFrame",
        "Is in Time Frame",
        "This property indicates the current state of the TimeFrame time testing. If the "
        "current simulation time is determined to be a valid time, this property is set "
        "to true. Otherwise it will be false, meaning that the scene graph node this "
        "TimeFrame is attached to would not be shown",
        openspace::properties::Property::Visibility::Developer
    };

    // A `TimeFrame` object determines the time frame during which a scene graph node is
    // valid. If the simulation time is outside the time frame range, the scene graph node
    // and all of its children are automatically disabled and any attached
    // [Renderable](#renderable) will not be displayed either. Usually, the time frame
    // corresponds to the time during which datasets are available or to disable a scene
    // graph node during uninteresting periods of time.
    struct [[codegen::Dictionary(TimeFrame)]] Parameters {
        // The type of the time frame that is described in this element. The available
        // types of scaling depend on the configuration of the application and can be
        // written to disk on application startup into the FactoryDocumentation
        std::string type [[codegen::annotation("Must name a valid TimeFrame type")]];
    };
#include "timeframe_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation TimeFrame::Documentation() {
    return codegen::doc<Parameters>("core_time_frame");
}

ghoul::mm_unique_ptr<TimeFrame> TimeFrame::createFromDictionary(
                                                            const ghoul::Dictionary& dict)
{
    ZoneScoped;

    const Parameters p = codegen::bake<Parameters>(dict);

    TimeFrame* result = FactoryManager::ref().factory<TimeFrame>()->create(p.type, dict);
    result->setIdentifier("TimeFrame");
    result->_type = p.type;

    return ghoul::mm_unique_ptr<TimeFrame>(result);
}

TimeFrame::TimeFrame()
    : properties::PropertyOwner({ "TimeFrame", "Time Frame" })
    , _isInTimeFrame(IsInTimeFrameInfo, false)
{
    _isInTimeFrame.setReadOnly(true);
    addProperty(_isInTimeFrame);
}

bool TimeFrame::initialize() {
    return true;
}

bool TimeFrame::isActive() const {
    return _isInTimeFrame;
}

} // namespace openspace
