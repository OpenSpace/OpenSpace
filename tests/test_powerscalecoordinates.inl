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

#include <openspace/scene/scene.h>

#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/powerscaledscalar.h>

class PowerscaleCoordinatesTest : public testing::Test {
protected:
	PowerscaleCoordinatesTest() {
    }

	~PowerscaleCoordinatesTest() {
    }

    void reset() {
    }

    openspace::Scene* scenegraph;
};


TEST_F(PowerscaleCoordinatesTest, psc) {

    openspace::psc reference(2.f, 1.f, 1.1f, 1.f);
    
    openspace::psc first(1.f, 0.f, 1.f, 0.f);
    openspace::psc second(1.9f, 1.f, 1.f, 1.f);
    
    EXPECT_EQ(reference, first + second);
    EXPECT_TRUE(reference == (first + second));
    
    openspace::psc third = first;
    first[0] = 0.0;
    
    EXPECT_TRUE(third != first);
    
    
}

TEST_F(PowerscaleCoordinatesTest, pss) {
    
    openspace::pss first(1.f, 1.f);
    openspace::pss second(1.f, -1.f);
    EXPECT_EQ(openspace::pss(1.01f, 1.f), first + second);
    EXPECT_EQ(openspace::pss(1.01f, 1.f), second + first);
    /*
    EXPECT_TRUE(first < (first + second));
    bool retu =(second < (first + second));
    
    std::cout << "retu: " << retu << std::endl;
    EXPECT_TRUE(retu);
    
    EXPECT_FALSE(first > (first + second));
    EXPECT_FALSE(second > (first + second));
    
    */
}


