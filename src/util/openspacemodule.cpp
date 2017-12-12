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

#include <openspace/util/openspacemodule.h>

#include <openspace/documentation/documentation.h>

#include <ghoul/filesystem/filesystem>
#include <ghoul/logging/logmanager.h>

#include <openspace/modulepath.h>

#include <algorithm>

namespace {
    constexpr const char* _loggerCat = "OpenSpaceModule";
    constexpr const char* ModuleBaseToken = "MODULE_";
} // namespace

namespace openspace {

OpenSpaceModule::OpenSpaceModule(std::string name)
    : properties::PropertyOwner({ std::move(name) })
{}

void OpenSpaceModule::initialize() {
    std::string upperName = name();
    std::transform(
        upperName.begin(),
        upperName.end(),
        upperName.begin(),
        [](char v) { return static_cast<char>(toupper(v)); }
    );

    std::string moduleToken =
        ghoul::filesystem::FileSystem::TokenOpeningBraces +
        std::string(ModuleBaseToken) +
        upperName +
        ghoul::filesystem::FileSystem::TokenClosingBraces;

    std::string path = modulePath();
    LDEBUG("Registering module path: " << moduleToken << ": " << path);
    FileSys.registerPathToken(moduleToken, path);

    internalInitialize();
}

void OpenSpaceModule::deinitialize() {
    internalDeinitialize();
}

std::vector<documentation::Documentation> OpenSpaceModule::documentations() const {
    return {};
}

scripting::LuaLibrary OpenSpaceModule::luaLibrary() const {
    return {};
}

ghoul::systemcapabilities::Version OpenSpaceModule::requiredOpenGLVersion() const {
    return { 3, 3, 0 };
}

std::string OpenSpaceModule::modulePath() const {
    std::string moduleName = name();
    std::transform(
        moduleName.begin(),
        moduleName.end(),
        moduleName.begin(),
        [](char v) { return static_cast<char>(tolower(v)); }
    );

    // First try the internal module directory
    if (FileSys.directoryExists("${MODULES}/" + moduleName)) {
        return absPath("${MODULES}/" + moduleName);
    }
    else { // Otherwise, it might be one of the external directories
        for (const char* dir : ModulePaths) {
            const std::string path = std::string(dir) + '/' + moduleName;
            if (FileSys.directoryExists(path)) {
                return absPath(path);
            }
        }
    }

    // If we got this far, neither the internal module nor any of the external modules fit
    throw ghoul::RuntimeError(
        "Could not resolve path for module '" + name() + "'",
        "OpenSpaceModule"
    );
}

void OpenSpaceModule::internalInitialize() {}
void OpenSpaceModule::internalDeinitialize() {}

} // namespace openspace
