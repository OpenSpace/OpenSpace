/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <openspace/util/openspacemodule.h>

#include <ghoul/filesystem/filesystem>
#include <ghoul/logging/logmanager.h>

#include <algorithm>

namespace {
    const std::string _loggerCat = "OpenSpaceModule";
    const std::string ModuleBaseToken = "MODULE_";
}
//ghoul::filesystem::FileSystem::TokenOpeningBraces
//ghoul::filesystem::FileSystem::TokenClosingBraces
namespace openspace {

OpenSpaceModule::OpenSpaceModule(std::string name)
    : _name(std::move(name))
{
    ghoul_assert(!_name.empty(), "Empty module name is not allowed");
}

bool OpenSpaceModule::create() {
    std::string moduleNameUpper = name();
    std::transform(moduleNameUpper.begin(), moduleNameUpper.end(), moduleNameUpper.begin(), toupper);
    std::string moduleToken = 
        ghoul::filesystem::FileSystem::TokenOpeningBraces +
        ModuleBaseToken +
        moduleNameUpper +
        ghoul::filesystem::FileSystem::TokenClosingBraces;

    std::string path = modulePath();
    LDEBUG("Registering module path: " << moduleToken << ": " << path);
    FileSys.registerPathToken(moduleToken, path);
    return true;
}

bool OpenSpaceModule::destroy() {
    return true;
}

std::string OpenSpaceModule::name() const {
    return _name;
}

std::string OpenSpaceModule::modulePath() const {
    std::string moduleName = name();
    std::transform(moduleName.begin(), moduleName.end(), moduleName.begin(), tolower);

    if (FileSys.directoryExists("${MODULES}/" + moduleName))
        return absPath("${MODULES}/" + moduleName);

#ifdef EXTERNAL_MODULES_PATHS

#endif
    LERROR("Could not resolve path for module '" << name() << "'");
    return "";
}

bool OpenSpaceModule::initialize() {
    return true;
}

bool OpenSpaceModule::deinitialize() {
    return true;
}


} // namespace openspace
