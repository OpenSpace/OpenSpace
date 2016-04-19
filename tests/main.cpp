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

#include <ghoul/cmdparser/cmdparser>
#include <ghoul/filesystem/filesystem>
#include <ghoul/logging/logging>
#include <ghoul/misc/dictionary.h>
#include <ghoul/lua/ghoul_lua.h>

//#include <test_common.inl>
//#include <test_spicemanager.inl>
//#include <test_scenegraphloader.inl>
//#include <test_chunknode.inl>
//#include <test_lrucache.inl>
#include <test_twmstileprovider.inl>
//#include <test_luaconversions.inl>
//#include <test_powerscalecoordinates.inl>
//#include <test_latlonpatch.inl>
//#include <test_texturetileset.inl>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/engine/configurationmanager.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/time.h>

#include <iostream>

using namespace ghoul::cmdparser;
using namespace ghoul::filesystem;
using namespace ghoul::logging;

namespace {
	std::string _loggerCat = "OpenSpaceTest";
}

int main(int argc, char** argv) {
	std::vector<std::string> args;
	openspace::OpenSpaceEngine::create(argc, argv, std::make_unique<openspace::WindowWrapper>(), args);

	testing::InitGoogleTest(&argc, argv);

	int returnVal = RUN_ALL_TESTS();

	// keep console from closing down
	int dummy; std::cin >> dummy;

	return returnVal;
}
