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
        "of the contained time frames are, but not in gaps between contained time "
        "frames.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    // This TimeFrame class will accept the union of all passed-in TimeFrames. This means
    // that this TimeFrame will be active if at least one of the child TimeFrames is
    // active and it will be inactive if none of the child TimeFrames are active.
    //
    // This can be used to create more complex TimeFrames that are made up of several,
    // simpler TimeFrames themselves.
    struct [[codegen::Dictionary(TimeFrameUnion)]] Parameters {
        // [[codegen::verbatim(TimeFramesInfo.description)]]
        std::vector<ghoul::Dictionary> timeFrames
            [[codegen::reference("core_time_frame")]];
    };
#include "timeframeunion_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation TimeFrameUnion::Documentation() {
    return codegen::doc<Parameters>("base_timeframe_union");
}

bool TimeFrameUnion::isActive(const Time& time) const {
    for (const ghoul::mm_unique_ptr<TimeFrame>& tf : _timeFrames) {
        if (tf->isActive(time)) {
            return true;
        }
    }
    return false;
}

TimeFrameUnion::TimeFrameUnion(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    for (size_t i = 0; i < p.timeFrames.size(); i++) {
        const ghoul::Dictionary& frame = p.timeFrames[i];
        _timeFrames.push_back(TimeFrame::createFromDictionary(frame));
        TimeFrame& subFrame = *_timeFrames.back();
        subFrame.setIdentifier(std::format("{}", i));
        subFrame.setGuiName(std::format("{}", i));
        subFrame.setDescription(std::format("{}", i));
        addPropertySubOwner(*_timeFrames.back());
    }
}

} // namespace openspace
