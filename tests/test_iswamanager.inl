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

#define private public
#include <modules/iswa/util/iswamanager.h>
#define private private

#include <openspace/engine/downloadmanager.h>
#include <openspace/util/time.h>


/*
 * For each test the following is run:
 * Constructor() -> setUp() -> test -> tearDown() -> Deconstructor()
 */

namespace openspace {

class IswaManagerTest : public testing::Test {
protected:

    IswaManagerTest() {
        DownloadManager::initialize("", 0);
        IswaManager::initialize();
    }

    ~IswaManagerTest() {
        IswaManager::deinitialize();
        DownloadManager::deinitialize();
    }

    void reset() {}
};

TEST_F(IswaManagerTest, initialize){
    IswaManager::deinitialize();

    ASSERT_FALSE(IswaManager::isInitialized()) << "IswaManager is initialized before initialize call";

    IswaManager::initialize();

    ASSERT_TRUE(IswaManager::isInitialized()) << "IswaManager is not initialized after initialize call";

    ASSERT_NE(&IswaManager::ref(), nullptr) << "IswaManager ref() is not a nullptr";

    EXPECT_EQ(&IswaManager::ref(), &IswaManager::ref()) << "IswaManager ref() returns the same object twice";
}

TEST_F(IswaManagerTest, iswaUrl){

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
