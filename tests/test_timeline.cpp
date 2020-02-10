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

#include "catch2/catch.hpp"

#include <openspace/util/timeline.h>
#include <openspace/util/time.h>

TEST_CASE("TimeLine: Add and Count Keyframes", "[timeline]") {
    openspace::Timeline<openspace::Time> timeline;
    timeline.addKeyframe(0.0, openspace::Time::now());
    timeline.addKeyframe(1.0, openspace::Time::now());

    REQUIRE(timeline.nKeyframes() == 2);

}

TEST_CASE("TimeLine: Query Keyframes", "[timeline]") {
    openspace::Timeline<float> timeline;
    timeline.addKeyframe(0.0, 0.f);
    timeline.addKeyframe(1.0, 1.f);

    REQUIRE(timeline.nKeyframes() == 2);

    REQUIRE(timeline.firstKeyframeAfter(0.0)->data == Approx(1.f));
    REQUIRE(timeline.firstKeyframeAfter(0.0, false)->data == Approx(1.f));
    REQUIRE(timeline.firstKeyframeAfter(0.0, true)->data == Approx(0.f));

    REQUIRE(timeline.lastKeyframeBefore(1.0)->data == Approx(0.f));
    REQUIRE(timeline.lastKeyframeBefore(1.0, false)->data == Approx(0.f));
    REQUIRE(timeline.lastKeyframeBefore(1.0, true)->data == Approx(1.f));
}

TEST_CASE("TimeLine: Remove Keyframes", "[timeline]") {
    openspace::Timeline<float> timeline;
    timeline.addKeyframe(0.0, 0.f);
    timeline.addKeyframe(1.0, 1.f);

    timeline.removeKeyframesBefore(0.0);
    REQUIRE(timeline.nKeyframes() == 2);

    timeline.removeKeyframesBefore(0.0, false);
    REQUIRE(timeline.nKeyframes() == 2);

    timeline.removeKeyframesBefore(0.0, true);
    REQUIRE(timeline.nKeyframes() == 1);

    timeline.removeKeyframesAfter(1.0);
    REQUIRE(timeline.nKeyframes() == 1);

    timeline.removeKeyframesAfter(1.0, false);
    REQUIRE(timeline.nKeyframes() == 1);

    timeline.removeKeyframesAfter(1.0, true);
    REQUIRE(timeline.nKeyframes() == 0);
}

TEST_CASE("TimeLine: Remove Keyframes In Range", "[timeline]") {
    openspace::Timeline<float> timeline;
    timeline.addKeyframe(0.0, 0.f);
    timeline.addKeyframe(1.0, 1.f);
    timeline.addKeyframe(2.0, 2.f);
    timeline.addKeyframe(3.0, 3.f);

    timeline.removeKeyframesBetween(1.0, 2.0);
    REQUIRE(timeline.nKeyframes() == 4);

    timeline.removeKeyframesBetween(1.0, 2.0, false, true);
    REQUIRE(timeline.nKeyframes() == 3);

    timeline.removeKeyframesBetween(1.0, 2.0, true, true);
    REQUIRE(timeline.nKeyframes() == 2);

    timeline.removeKeyframesBetween(-1.0, 4.0);
    REQUIRE(timeline.nKeyframes() == 0);
}
