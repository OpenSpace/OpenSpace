/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

//#include <openspace/scene/scenegraphnode.h>
//#include <modules/globebrowsing/geometry/angle.h>
//
//#include <fstream>
//#include <glm/glm.hpp>
//
//class AngleTest : public testing::Test {};
//
//TEST_F(AngleTest, DoubleConversions) {
//    using namespace openspace::globebrowsing;
//
//    ASSERT_EQ(dAngle::fromRadians(0).asDegrees(), 0) << "from radians to degrees";
//    ASSERT_EQ(dAngle::HALF.asDegrees(), 180) << "from radians to degrees";
//    ASSERT_EQ(dAngle::fromDegrees(180).asRadians(), glm::pi<double>()) << "from degrees to radians";
//
//}
//
//TEST_F(AngleTest, FloatConversions) {
//    using namespace openspace::globebrowsing;
//
//    ASSERT_EQ(fAngle::ZERO.asDegrees(), 0.0) << "from radians to degrees";
//    ASSERT_EQ(fAngle::HALF.asDegrees(), 180.0) << "from radians to degrees";
//    ASSERT_EQ(fAngle::fromDegrees(180).asRadians(), glm::pi<float>()) << "from degrees to radians";
//
//}
//
//
//TEST_F(AngleTest, Normalize) {
//    using namespace openspace::globebrowsing;
//    
//    ASSERT_NEAR(
//        dAngle::fromDegrees(390).normalize().asDegrees(),
//        30.0,
//        dAngle::EPSILON
//    ) << "normalize to [0, 360]";
//
//
//    dAngle a = dAngle::fromDegrees(190);
//    a.normalizeAround(dAngle::ZERO);
//    ASSERT_NEAR(
//        a.asDegrees(),
//        -170,
//        dAngle::EPSILON
//    ) << "normalize to [-180,180]";
//
//
//    dAngle b = dAngle::fromDegrees(190);
//    b.normalizeAround(dAngle::fromDegrees(90));
//    ASSERT_NEAR(
//        b.asDegrees(),
//        190,
//        dAngle::EPSILON
//    ) << "normalize to [-90,270]";
//
//
//    dAngle c = dAngle::fromDegrees(360);
//    c.normalizeAround(dAngle::fromDegrees(1083.2));
//    ASSERT_NEAR(
//        c.asDegrees(),
//        1080,
//        dAngle::EPSILON
//        ) << "normalize to [903.2, 1263.2]";
//}
//
//
//TEST_F(AngleTest, Clamp) {
//    using namespace openspace::globebrowsing;
//
//    ASSERT_EQ(
//        dAngle::fromDegrees(390).clamp(dAngle::ZERO, dAngle::HALF).asDegrees(),
//        180
//        ) << "clamp [0, 180]";
//
//    ASSERT_EQ(
//        dAngle::fromDegrees(390).clamp(dAngle::ZERO, dAngle::FULL).asDegrees(),
//        360
//        ) << "clamp [0, 360]";
//}
//
//
//TEST_F(AngleTest, ConstClamp) {
//    using namespace openspace::globebrowsing;
//
//    const dAngle a = dAngle::fromDegrees(390);
//    ASSERT_EQ(
//        a.getClamped(dAngle::ZERO, dAngle::HALF).asDegrees(),
//        180
//        ) << "clamp [0, 180]";
//
//    const dAngle b = dAngle::fromDegrees(390);
//    ASSERT_EQ(
//        b.getClamped(dAngle::ZERO, dAngle::FULL).asDegrees(),
//        360
//        ) << "clamp [0, 360]";
//}
