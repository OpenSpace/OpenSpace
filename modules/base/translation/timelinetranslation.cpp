/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/base/translation/timelinetranslation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/time.h>

namespace {
    constexpr const char* KeyType = "Type";
    constexpr const char* KeyKeyframes = "Keyframes";
} // namespace

namespace openspace {

documentation::Documentation TimelineTranslation::Documentation() {
    using namespace documentation;
    return {
        "Timeline Translation",
        "base_transform_translation_keyframe",
        {
            {
                KeyType,
                new StringEqualVerifier("TimelineTranslation"),
                Optional::No
            },
            {
                KeyKeyframes,
                new TableVerifier({
                    { "*", new TableVerifier(), Optional::No, "Any translation object" }
                }),
                Optional::No,
                "A table of keyframes, with keys formatted as YYYY-MM-DDTHH:MM:SS"
                "and values that are valid Translation objects."
            }
        }
    };
}

TimelineTranslation::TimelineTranslation(const ghoul::Dictionary& dictionary) {
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "TimelineTranslation"
    );

    const ghoul::Dictionary& keyframes =
        dictionary.value<ghoul::Dictionary>(KeyKeyframes);

    std::vector<std::string> timeStrings = keyframes.keys();
    for (const std::string& timeString : timeStrings) {
        const double t = Time::convertTime(timeString);

        std::unique_ptr<Translation> translation = 
            Translation::createFromDictionary(
                keyframes.value<ghoul::Dictionary>(timeString)
            );

        if (translation) {
            _timeline.addKeyframe(t, std::move(translation));
        }
    }
}

glm::dvec3 TimelineTranslation::position(const UpdateData& data) const {
    const double now = data.time.j2000Seconds();
    using KeyframePointer = const Keyframe<std::unique_ptr<Translation>>*;

    KeyframePointer prev = _timeline.lastKeyframeBefore(now, true);
    KeyframePointer next = _timeline.firstKeyframeAfter(now, true);

    if (!prev && !next) {
        return glm::dvec3(0.0);
    }
    if (!prev) {
        prev = next;
    }
    if (!next) {
        next = prev;
    }
    const double prevTime = prev->timestamp;
    const double nextTime = next->timestamp;

    double t = 0.0;
    if (nextTime - prevTime > 0.0) {
        t = (now - prevTime) / (nextTime - prevTime);
    }
    return t * next->data->position(data) + (1.0 - t) * prev->data->position(data); 
}

} // namespace openspace
