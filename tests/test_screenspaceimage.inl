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
#define private public
#include "gtest/gtest.h"

// make private variables public, only for testing!!
#include <modules/base/rendering/screenspaceimage.h>

/*
 * For each test the following is run:
 * Constructor() -> setUp() -> test -> tearDown() -> Deconstructor()
 */

namespace openspace {

class ScreenSpaceRenderableTest : public testing::Test{
protected:

	ScreenSpaceRenderableTest() :
		_ssr(texturePath) 
	{
		_sharedSsr = std::make_shared<ScreenSpaceImage>("${OPENSPACE_DATA}/test3.jpg");
	}

	~ScreenSpaceRenderableTest(){}


	void reset() {}

	// These variables are shared by all tests
	std::string texturePath = "${OPENSPACE_DATA}/test2.jpg";
	ScreenSpaceImage _ssr;
	std::shared_ptr<ScreenSpaceRenderable> _sharedSsr;
};


TEST_F(ScreenSpaceRenderableTest, initialize){
	bool isReady = _ssr.isReady();
	ASSERT_TRUE(!isReady) << "ScreenSpaceImage is ready before initialize";

	// cannot test initialize, crashes at createplane becasue of opengl functions. needs mocking
	//_ssr.initialize();
	//isReady = _ssr.isReady();
	//ASSERT_TRUE(!isReady) << "ScreenSpaceImage is not ready after initialize";
	//_ssr.deinitialize();
	//isReady = _ssr.isReady();
	//ASSERT_TRUE(!isReady) << "ScreenSpaceImage is still ready after deinitialize";
}
}//namespace openspace
