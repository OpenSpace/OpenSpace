/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#include <catch2/catch_test_macros.hpp>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/misc/exception.h>
#include <iostream>
#include <filesystem>
#include <fstream>

#ifdef WIN32
#include <Windows.h>
#else // ^^^^ WIN32 // !WIN32 vvvv
#include <unistd.h>
#endif // WIN32

TEST_CASE("FileSystem: OnChangeCallback", "[filesystem]") {
    using ghoul::filesystem::File;
    using ghoul::filesystem::FileSystem;

    const char* cpath = "${TEMPORARY}/tmpfil.txt";
    const std::filesystem::path path = absPath(cpath);
    {
        std::ofstream f = std::ofstream(path);
        f << "tmp";
    }
    bool b1 = false;
    bool b2 = false;

    auto c1 = [&b1]() { b1 = true; };
    auto c2 = [&b2]() { b2 = true; };

    File* f1 = new File(path);
    f1->setCallback(c1);
    File* f2 = new File(path);
    f2->setCallback(c1);
    File* f3 = new File(path);
    f3->setCallback(c2);

    // Check that the file exists
    REQUIRE(std::filesystem::is_regular_file(absPath(cpath)));
    REQUIRE(std::filesystem::is_regular_file(path));
    REQUIRE(std::filesystem::is_regular_file(f1->path()));

    f2->setCallback(nullptr);

    // Check that b still is false so no callback has been fired
    REQUIRE_FALSE(b1);
    REQUIRE_FALSE(b2);

    // overwrite the file
    {
        std::ofstream f = std::ofstream(path);
        f << "tmp";
    }
    FileSys.triggerFilesystemEvents();

    // Sleep the main thread to make sure the filesystem have time to respond
    const int seconds = 4;
#ifdef WIN32
    int count = 0;
    while ((b1 == false || b2 == false) && count < 100 * seconds) {
        Sleep(10);
        count++;
    }
#else // ^^^^ WIN32 // !WIN32 vvvv
    int count = 0;
    while ((!b1 || !b2) && count < 10000 * seconds) {
        usleep(100);
        FileSys.triggerFilesystemEvents();
        count++;
    }
#endif // WIN32
    REQUIRE(b1);
    REQUIRE(b2);

    delete f3;
    delete f2;
    delete f1;

    // Check that we can delete the file
    REQUIRE(std::filesystem::remove(path));
}

TEST_CASE("FileSystem: TokenDefaultState", "[filesystem]") {
    REQUIRE(FileSys.tokens().size() == 3);
    CHECK(FileSys.tokens()[0] == "${TEMPORARY}");
    CHECK(FileSys.tokens()[1] == "${UNIT_SCRIPT}");
    CHECK(FileSys.tokens()[2] == "${UNIT_TEST}");
}

TEST_CASE("FileSystem: Override Non Existing Path Token", "[filesystem]") {
    // @TODO (abock, 2019-12-29) This test needs to be rewritten to not mess with the
    // global state of other tests
    return;

    CHECK_NOTHROW(
        FileSys.registerPathToken(
            "${AddExistingPathToken}",
            absPath("${TEMPORARY}"),
            ghoul::filesystem::FileSystem::Override::Yes
        )
    );
}

TEST_CASE("FileSystem: ExpandingTokensNonExistingToken", "[filesystem]") {
    const std::string p = "${NOTFOUND}";
    REQUIRE_THROWS_AS(FileSys.expandPathTokens(p), ghoul::RuntimeError);
}
