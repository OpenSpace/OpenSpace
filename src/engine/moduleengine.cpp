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

#include <openspace/engine/moduleengine.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/moduleregistration.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/openspacemodule.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>

#include "moduleengine_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "ModuleEngine";

    constexpr openspace::properties::Property::PropertyInfo AllModulesInfo = {
        "AllModules",
        "All Modules",
        "The list of all modules that were compiled for this version of OpenSpace in the "
        "same order in which they were initialized",
        // @VISIBILITY(3.5)
        openspace::properties::Property::Visibility::AdvancedUser
    };
} // namespace

namespace openspace {

ModuleEngine::ModuleEngine()
    : properties::PropertyOwner({ "Modules" })
    , _allModules(AllModulesInfo)
{
    _allModules.setReadOnly(true);
    addProperty(_allModules);
}

void ModuleEngine::initialize(
                     const std::map<std::string, ghoul::Dictionary>& moduleConfigurations)
{
    ZoneScoped;

    const std::vector<OpenSpaceModule*> modules = AllModules();

    std::vector<std::string> moduleNames;
    moduleNames.reserve(modules.size());
    for (OpenSpaceModule* m : modules) {
        registerModule(std::unique_ptr<OpenSpaceModule>(m));
        moduleNames.push_back(m->guiName());
    }
    _allModules = moduleNames;

    for (OpenSpaceModule* m : modules) {
        const std::string& identifier = m->identifier();
        auto it = moduleConfigurations.find(identifier);
        ghoul::Dictionary configuration;
        if (it != moduleConfigurations.end()) {
            configuration = it->second;
        }
        try {
            m->initialize(configuration);
        }
        catch (const documentation::SpecificationError& e) {
            logError(e);
            throw;
        }

        addPropertySubOwner(m);
    }
}

void ModuleEngine::initializeGL() {
    ZoneScoped;

    LDEBUG("Initializing OpenGL of modules");
    for (std::unique_ptr<OpenSpaceModule>& m : _modules) {
        LDEBUG(fmt::format("Initializing OpenGL of module '{}'", m->identifier()));
        m->initializeGL();
    }
    LDEBUG("Finished initializing OpenGL of modules");
}

void ModuleEngine::deinitialize() {
    ZoneScoped;

    LDEBUG("Deinitializing modules");

    for (auto mIt = _modules.rbegin(); mIt != _modules.rend(); ++mIt) {
        LDEBUG(fmt::format("Deinitializing module '{}'", (*mIt)->identifier()));
        (*mIt)->deinitialize();
    }
    LDEBUG("Finished deinitializing modules");

    for (auto mIt = _modules.rbegin(); mIt != _modules.rend(); ++mIt) {
        LDEBUG(fmt::format("Destroying module '{}'", (*mIt)->identifier()));
        (*mIt) = nullptr;
    }
    LDEBUG("Finished destroying modules");

    _modules.clear();
}

void ModuleEngine::deinitializeGL() {
    ZoneScoped;

    LDEBUG("Deinitializing OpenGL of modules");
    for (auto mIt = _modules.rbegin(); mIt != _modules.rend(); ++mIt) {
        LDEBUG(fmt::format("Deinitializing OpenGL of module '{}'", (*mIt)->identifier()));
        (*mIt)->deinitializeGL();

    }
    LDEBUG("Finished deinitializing OpenGL of modules");
}

void ModuleEngine::registerModule(std::unique_ptr<OpenSpaceModule> module) {
    ZoneScoped;

    ghoul_assert(module, "Module must not be nullptr");

    auto it = std::find_if(
        _modules.begin(),
        _modules.end(),
        [&module](std::unique_ptr<OpenSpaceModule>& rhs) {
            return rhs->identifier() == module->identifier();
        }
    );
    if (it != _modules.end()) {
        throw ghoul::RuntimeError(
            fmt::format("Module name '{}' was registered before", module->identifier()),
            "ModuleEngine"
        );
    }

    LDEBUG(fmt::format("Registered module '{}'", module->identifier()));
    _modules.push_back(std::move(module));
}

std::vector<OpenSpaceModule*> ModuleEngine::modules() const {
    std::vector<OpenSpaceModule*> result;
    result.reserve(_modules.size());
    for (const std::unique_ptr<OpenSpaceModule>& m : _modules) {
        result.push_back(m.get());
    }
    return result;
}

ghoul::systemcapabilities::Version ModuleEngine::requiredOpenGLVersion() const {
    ghoul::systemcapabilities::Version version = { .major = 0, .minor = 0, .release = 0 };

    for (const std::unique_ptr<OpenSpaceModule>& m : _modules) {
        version = std::max(version, m->requiredOpenGLVersion());
    }

    return version;
}

scripting::LuaLibrary ModuleEngine::luaLibrary() {
    return {
        "modules",
        {
            codegen::lua::IsLoaded
        }
    };
}

} // namespace openspace
