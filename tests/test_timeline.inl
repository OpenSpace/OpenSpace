/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include "gtest/gtest.h"

#include <openspace/util/timeline.h>
#include <openspace/util/time.h>

using namespace openspace;

class TimelineTest : public testing::Test {};

TEST_F(TimelineTest, AddAndCountKeyframes) {
    Timeline<Time> timeline;
    timeline.addKeyframe(0.0, Time::now());
    timeline.addKeyframe(1.0, Time::now());

    ASSERT_EQ(timeline.nKeyframes(), 2);
}

TEST_F(TimelineTest, QueryKeyframes) {
    Timeline<float> timeline;
    timeline.addKeyframe(0.0, 0.f);
    timeline.addKeyframe(1.0, 1.f);

    ASSERT_EQ(timeline.nKeyframes(), 2);

    ASSERT_EQ(timeline.firstKeyframeAfter(0.0)->data, 1.f) << "Incorrect keyframe returned";
    ASSERT_EQ(timeline.firstKeyframeAfter(0.0, false)->data, 1.f) << "Incorrect keyframe returned";
    ASSERT_EQ(timeline.firstKeyframeAfter(0.0, true)->data, 0.f) << "Incorrect keyframe returned";

    ASSERT_EQ(timeline.lastKeyframeBefore(1.0)->data, 0.f) << "Incorrect keyframe returned";
    ASSERT_EQ(timeline.lastKeyframeBefore(1.0, false)->data, 0.f) << "Incorrect keyframe returned";
    ASSERT_EQ(timeline.lastKeyframeBefore(1.0, true)->data, 1.f) << "Incorrect keyframe returned";
}

TEST_F(TimelineTest, RemoveKeyframes) {
    Timeline<float> timeline;
    timeline.addKeyframe(0.0, 0.f);
    timeline.addKeyframe(1.0, 1.f);

    timeline.removeKeyframesBefore(0.0);
    ASSERT_EQ(timeline.nKeyframes(), 2);

    timeline.removeKeyframesBefore(0.0, false);
    ASSERT_EQ(timeline.nKeyframes(), 2);

    timeline.removeKeyframesBefore(0.0, true);
    ASSERT_EQ(timeline.nKeyframes(), 1);

    timeline.removeKeyframesAfter(1.0);
    ASSERT_EQ(timeline.nKeyframes(), 1);

    timeline.removeKeyframesAfter(1.0, false);
    ASSERT_EQ(timeline.nKeyframes(), 1);

    timeline.removeKeyframesAfter(1.0, true);
    ASSERT_EQ(timeline.nKeyframes(), 0);
}

TEST_F(TimelineTest, RemoveKeyframesInRange) {
    Timeline<float> timeline;
    timeline.addKeyframe(0.0, 0.f);
    timeline.addKeyframe(1.0, 1.f);
    timeline.addKeyframe(2.0, 2.f);
    timeline.addKeyframe(3.0, 3.f);

    timeline.removeKeyframesBetween(1.0, 2.0);
    ASSERT_EQ(timeline.nKeyframes(), 4);

    timeline.removeKeyframesBetween(1.0, 2.0, false, true);
    ASSERT_EQ(timeline.nKeyframes(), 3);

    timeline.removeKeyframesBetween(1.0, 2.0, true, true);
    ASSERT_EQ(timeline.nKeyframes(), 2);

    timeline.removeKeyframesBetween(-1.0, 4.0);
    ASSERT_EQ(timeline.nKeyframes(), 0);
}
