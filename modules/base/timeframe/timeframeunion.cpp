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

#include <modules/base/timeframe/timeframeunion.h>

#include <openspace/properties/property.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo TimeFramesInfo = {
        "TimeFrames",
        "Time Frames",
        "A vector of time frames to combine into one. The time frame is active when any "
        "of the contained time frames are, but not in gaps between contained time frames",
        // @VISIBILITY(3.75)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(TimeFrameUnion)]] Parameters {
        // [[codegen::verbatim(TimeFramesInfo.description)]]
        std::vector<ghoul::Dictionary> timeFrames
            [[codegen::reference("core_time_frame")]];
    };
#include "timeframeunion_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation TimeFrameUnion::Documentation() {
    return codegen::doc<Parameters>("base_time_frame_union");
}

bool TimeFrameUnion::isActive(const Time& time) const {
    for (const ghoul::mm_unique_ptr<TimeFrame>& tf : _timeFrames) {
        if (tf->isActive(time)) {
            return true;
        }
    }
    return false;
}

TimeFrameUnion::TimeFrameUnion(const ghoul::Dictionary& dictionary)
    : TimeFrame()
{
    // I don't know how we can actually help the reference attribute properly. Since the
    // Parameter list only contains the monostate, there is no need to actually create
    // the object here
    codegen::bake<Parameters>(dictionary);

    ghoul::Dictionary frames =
        dictionary.value<ghoul::Dictionary>(TimeFramesInfo.identifier);

    for (std::string_view k : frames.keys()) {
        const ghoul::Dictionary& subDictionary = frames.value<ghoul::Dictionary>(k);
        _timeFrames.push_back(TimeFrame::createFromDictionary(subDictionary));
        TimeFrame& subFrame = *_timeFrames.back();
        subFrame.setIdentifier(std::string(k));
        subFrame.setGuiName(std::string(k));
        subFrame.setDescription(std::string(k));
        addPropertySubOwner(*_timeFrames.back());
    }
}

} // namespace openspace
