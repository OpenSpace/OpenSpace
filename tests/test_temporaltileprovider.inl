/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <modules/globebrowsing/other/temporaltileprovider.h>

#include <openspace/util/time.h>

#define _USE_MATH_DEFINES
#include <math.h>
#include <glm/glm.hpp>
#include <iostream>


class TemporalTileProviderTest : public testing::Test {};

std::string fileName = "data/scene/debugglobe/map_service_configs/VIIRS_SNPP_CorrectedReflectance_TrueColor_temporal.xml";

TEST_F(TemporalTileProviderTest, Basic) {
    double t = 2016.01;
    openspace::Time::ref().setTime(t);
    openspace::Time::ref().preSynchronization();
    openspace::Time::ref().postSynchronizationPreDraw();
    openspace::TemporalTileProvider provider(absPath(fileName));
}

