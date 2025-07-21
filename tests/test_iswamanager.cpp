/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <catch2/catch_test_macros.hpp>

#include <modules/iswa/util/iswamanager.h>
#include <openspace/engine/downloadmanager.h>
#include <openspace/util/time.h>

TEST_CASE("ISWAManager: Initialize", "[iswamanager]") {
    openspace::IswaManager::deinitialize();
    REQUIRE_FALSE(openspace::IswaManager::isInitialized());

    openspace::IswaManager::initialize();
    REQUIRE(openspace::IswaManager::isInitialized());
    CHECK(&openspace::IswaManager::ref() == &openspace::IswaManager::ref());
}

#endif // OPENSPACE_MODULE_ISWA_ENABLED
