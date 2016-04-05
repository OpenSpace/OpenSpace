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
#ifdef OPENSPACE_MODULE_ISWA_ENABLED
#include "gtest/gtest.h"
#define private public
#include <modules/iswa/util/iswamanager.h>
#define private private
#include <openspace/util/time.h>

/*
 * For each test the following is run:
 * Constructor() -> setUp() -> test -> tearDown() -> Deconstructor()
 */

namespace openspace {

class ISWAManagerTest : public testing::Test{
protected:

	ISWAManagerTest()
	{
		ISWAManager::initialize();
	}

	~ISWAManagerTest(){
		ISWAManager::deinitialize();
	}


	void reset() {}

	//std::shared_ptr<ISWAManager> iSWAManager;
};

TEST_F(ISWAManagerTest, initialize){

	ISWAManager::deinitialize();

	ASSERT_TRUE(!ISWAManager::isInitialized()) << "iSWAManager is initialized before initialize call";

	ISWAManager::initialize();

	ASSERT_TRUE(ISWAManager::isInitialized()) << "iSWAManager is not initialized after initialize call";

	ASSERT_NE(&ISWAManager::ref(), nullptr) << "iSWAManager ref() is not a nullptr";

	EXPECT_EQ(&ISWAManager::ref(), &ISWAManager::ref()) << "iSWAManager ref() returns the same object twice";
}

TEST_F(ISWAManagerTest, iSWAurl){

	//OsEng.loadSpiceKernels();
	//Time::ref().setTime(double(100000.0));
	//Time::ref().preSynchronization();
	//Time::ref().postSynchronizationPreDraw();
	//std::string url = ISWAManager::ref().iSWAurl(7);
	//std::string expectedUrl = "http://iswa2.ccmc.gsfc.nasa.gov/IswaSystemWebApp/iSWACygnetStreamer?timestamp=2000-01-02%2015:45:35&window=-1&cygnetId=7";

	//EXPECT_EQ(expectedUrl, url);
}

}//namespace openspace
#endif
