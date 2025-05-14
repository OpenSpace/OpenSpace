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

#include <modules/base/rotation/timelinerotation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/scene/scene.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/time.h>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo ShouldInterpolateInfo = {
        "ShouldInterpolate",
        "Should Interpolate",
        "If this value is set to 'true', an interpolation is applied between the given "
        "keyframes. If this value is set to 'false', the interpolation is not applied.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    // This `Rotation` uses a timeline of other `Rotation` classes to calculate the
    // final rotation for the attached scene graph node. The current in-game time is
    // used to determine which specific keyframe to currently use. It is also possible to
    // interpolate between two adjacent keyframes.
    struct [[codegen::Dictionary(TimelineRotation)]] Parameters {
        // A table of keyframes, with keys formatted as YYYY-MM-DDTHH:MM:SS and values
        // that are valid Rotation objects
        std::map<std::string, ghoul::Dictionary> keyframes
            [[codegen::reference("core_transform_rotation")]];

        // [[codegen::verbatim(ShouldInterpolateInfo.description)]]
        std::optional<bool> shouldInterpolate;
    };
#include "timelinerotation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation TimelineRotation::Documentation() {
    return codegen::doc<Parameters>("base_transform_rotation_keyframe");
}

TimelineRotation::TimelineRotation(const ghoul::Dictionary& dictionary)
    : Rotation(dictionary)
    , _shouldInterpolate(ShouldInterpolateInfo, true)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    for (const std::pair<const std::string, ghoul::Dictionary>& kf : p.keyframes) {
        const double t = Time::convertTime(kf.first);

        ghoul::mm_unique_ptr<Rotation> rotation = Rotation::createFromDictionary(
            kf.second
        );
        rotation->setIdentifier(makeIdentifier(kf.first));
        addPropertySubOwner(rotation.get());
        _timeline.addKeyframe(t, std::move(rotation));
    }

    _shouldInterpolate = p.shouldInterpolate.value_or(_shouldInterpolate);
    addProperty(_shouldInterpolate);
}

void TimelineRotation::update(const UpdateData& data) {
    const double now = data.time.j2000Seconds();
    using KeyframePointer = const Keyframe<ghoul::mm_unique_ptr<Rotation>>*;

    if (KeyframePointer prev = _timeline.lastKeyframeBefore(now, true);  prev) {
        prev->data->update(data);
    }
    if (KeyframePointer next = _timeline.firstKeyframeAfter(now, true);  next) {
        next->data->update(data);
    }
}

glm::dmat3 TimelineRotation::matrix(const UpdateData& data) const {
    const double now = data.time.j2000Seconds();
    using KeyframePointer = const Keyframe<ghoul::mm_unique_ptr<Rotation>>*;

    KeyframePointer prev = _timeline.lastKeyframeBefore(now, true);
    KeyframePointer next = _timeline.firstKeyframeAfter(now, true);

    if (!prev && !next) {
        return glm::dmat3(0.0);
    }
    if (!prev) {
        prev = next;
    }
    if (!next) {
        next = prev;
    }
    const double prevTime = prev->timestamp;
    const double nextTime = next->timestamp;
    if (_shouldInterpolate) {
        double t = 0.0;
        if (nextTime - prevTime > 0.0) {
            t = (now - prevTime) / (nextTime - prevTime);
        }

        const glm::dquat nextRot = glm::quat_cast(next->data->matrix(data));
        const glm::dquat prevRot = glm::quat_cast(prev->data->matrix(data));

        return glm::dmat3(glm::slerp(prevRot, nextRot, t));
    }
    else {
        return now < nextTime ? prev->data->matrix(data) : next->data->matrix(data);
    }
}

} // namespace openspace
