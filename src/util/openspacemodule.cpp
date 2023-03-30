/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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
#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/profiling.h>
#include <algorithm>
#include <filesystem>

#include <openspace/modulepath.h>

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

    std::string upperIdentifier = identifier();
    std::transform(
        upperIdentifier.begin(),
        upperIdentifier.end(),
        upperIdentifier.begin(),
        [](char v) { return static_cast<char>(toupper(v)); }
    );

    std::string moduleToken = "${" + std::string(ModuleBaseToken) + upperIdentifier + "}";

    std::filesystem::path path = modulePath();
    LDEBUG(fmt::format("Registering module path {}: {}", moduleToken, path));
    FileSys.registerPathToken(moduleToken, std::move(path));

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

std::string OpenSpaceModule::modulePath() const {
    std::string moduleIdentifier = identifier();
    std::transform(
        moduleIdentifier.begin(),
        moduleIdentifier.end(),
        moduleIdentifier.begin(),
        [](char v) { return static_cast<char>(tolower(v)); }
    );

    // First try the internal module directory
    std::filesystem::path path = absPath("${MODULES}/" + moduleIdentifier);
    if (std::filesystem::is_directory(path)) {
        return path.string();
    }
    else { // Otherwise, it might be one of the external directories
        for (std::string_view dir : ModulePaths) {
            const std::string& p = std::string(dir) + '/' + moduleIdentifier;
            if (std::filesystem::is_directory(absPath(p))) {
                return absPath(p).string();
            }
        }
    }

    // If we got this far, neither the internal module nor any of the external modules fit
    throw ghoul::RuntimeError(
        fmt::format("Could not resolve path for module {}", identifier()),
        "OpenSpaceModule"
    );
}

void OpenSpaceModule::internalInitialize(const ghoul::Dictionary&) {}

void OpenSpaceModule::internalInitializeGL() {}

void OpenSpaceModule::internalDeinitialize() {}

void OpenSpaceModule::internalDeinitializeGL() {}

} // namespace openspace
