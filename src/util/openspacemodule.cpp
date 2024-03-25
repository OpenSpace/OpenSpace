/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/scripting/lualibrary.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/stringhelper.h>
#include <algorithm>
#include <filesystem>

namespace {
    constexpr std::string_view _loggerCat = "OpenSpaceModule";
    constexpr std::string_view ModuleBaseToken = "MODULE_";
} // namespace

namespace openspace {

OpenSpaceModule::OpenSpaceModule(std::string name)
    : properties::PropertyOwner({ std::move(name) })
{}

void OpenSpaceModule::initialize(const ghoul::Dictionary& configuration) {
    ZoneScoped;
    ZoneName(identifier().c_str(), identifier().size());

    const std::string upperIdentifier = ghoul::toUpperCase(identifier());

    std::string moduleToken = "${" + std::string(ModuleBaseToken) + upperIdentifier + "}";

    std::filesystem::path path = modulePath();
    if (!path.empty()) {
        LDEBUG(std::format("Registering module path '{}' -> {}", moduleToken, path));
        FileSys.registerPathToken(std::move(moduleToken), std::move(path));
    }

    internalInitialize(configuration);
}

void OpenSpaceModule::initializeGL() {
    ZoneScoped;
    ZoneName(identifier().c_str(), identifier().size());

    internalInitializeGL();
}

void OpenSpaceModule::deinitialize() {
    ZoneScoped;
    ZoneName(identifier().c_str(), identifier().size());

    internalDeinitialize();
}

void OpenSpaceModule::deinitializeGL() {
    ZoneScoped;
    ZoneName(identifier().c_str(), identifier().size());

    internalDeinitializeGL();
}

std::vector<documentation::Documentation> OpenSpaceModule::documentations() const {
    return {};
}

scripting::LuaLibrary OpenSpaceModule::luaLibrary() const {
    return {};
}

std::vector<scripting::LuaLibrary> OpenSpaceModule::luaLibraries() const {
    return {};
}

ghoul::systemcapabilities::Version OpenSpaceModule::requiredOpenGLVersion() const {
    return { 3, 3, 0 };
}

std::vector<std::string> OpenSpaceModule::requiredOpenGLExtensions() const {
    return {};
}

std::filesystem::path OpenSpaceModule::modulePath() const {
    const std::string moduleIdentifier = ghoul::toLowerCase(identifier());

    // First try the internal module directory
    const std::filesystem::path path = absPath("${MODULES}/" + moduleIdentifier);
    return std::filesystem::is_directory(path) ? path : "";
}

void OpenSpaceModule::internalInitialize(const ghoul::Dictionary&) {}

void OpenSpaceModule::internalInitializeGL() {}

void OpenSpaceModule::internalDeinitialize() {}

void OpenSpaceModule::internalDeinitializeGL() {}

} // namespace openspace
