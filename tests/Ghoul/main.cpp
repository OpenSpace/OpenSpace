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

#include <catch2/catch_session.hpp>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/supportmacros.h>
#include <filesystem>

using namespace ghoul::filesystem;
using namespace ghoul::logging;

int main(int argc, char** argv) {
    LogManager::initialize(LogLevel::Fatal);
    LogMgr.addLog(std::make_unique<ConsoleLog>());

    FileSystem::initialize();

    const std::filesystem::path root = absPath(TEST_ROOT_DIR);
    const std::filesystem::path testDirectory = root / "tests";
    const std::filesystem::path scriptDirectory = root / "scripts";

    FileSys.registerPathToken("${UNIT_TEST}", testDirectory);
    FileSys.registerPathToken("${UNIT_SCRIPT}", scriptDirectory);

    if (!std::filesystem::is_directory(testDirectory)) {
        LFATALC("main", "Fix me");
        return 0;
    }

    const int result = Catch::Session().run(argc, argv);
    return result;
}
