/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/moduleregistration.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/openspacemodule.h>
#include <ghoul/logging/logmanager.h>

#include "moduleengine_lua.inl"

namespace {
    constexpr const char* _loggerCat = "ModuleEngine";
} // namespace

namespace openspace {

ModuleEngine::ModuleEngine() : properties::PropertyOwner({ "Modules" }) {}

void ModuleEngine::initialize(
                     const std::map<std::string, ghoul::Dictionary>& moduleConfigurations)
{
    for (OpenSpaceModule* m : AllModules()) {
        const std::string& identifier = m->identifier();
        auto it = moduleConfigurations.find(identifier);
        ghoul::Dictionary configuration;
        if (it != moduleConfigurations.end()) {
            configuration = it->second;
        }
        registerModule(std::unique_ptr<OpenSpaceModule>(m), configuration);
    }
}

void ModuleEngine::initializeGL() {
    LDEBUG("Initializing OpenGL of modules");
    for (std::unique_ptr<OpenSpaceModule>& m : _modules) {
        LDEBUG(fmt::format("Initializing OpenGL of module '{}'", m->identifier()));
        m->initializeGL();
    }
    LDEBUG("Finished initializing OpenGL of modules");
}

void ModuleEngine::deinitialize() {
    LDEBUG("Deinitializing modules");
    for (std::unique_ptr<OpenSpaceModule>& m : _modules) {
        LDEBUG(fmt::format("Deinitializing module '{}'", m->identifier()));
        m->deinitialize();
    }

    LDEBUG("Finished deinitializing modules");

    for (std::unique_ptr<OpenSpaceModule>& m : _modules) {
        LDEBUG(fmt::format("Destroying module '{}'", m->identifier()));
        m = nullptr;
    }

    LDEBUG("Finished destroying modules");
    _modules.clear();
}

void ModuleEngine::deinitializeGL() {
    LDEBUG("Deinitializing OpenGL of modules");
    for (std::unique_ptr<OpenSpaceModule>& m : _modules) {
        LDEBUG(fmt::format("Deinitializing OpenGL of module '{}'", m->identifier()));
        m->deinitializeGL();

    }
    LDEBUG("Finished deinitializing OpenGL of modules");
}

void ModuleEngine::registerModule(std::unique_ptr<OpenSpaceModule> mod,
                                  const ghoul::Dictionary& configuration)
{
    ghoul_assert(mod, "Module must not be nullptr");

    auto it = std::find_if(
        _modules.begin(),
        _modules.end(),
        [&mod](std::unique_ptr<OpenSpaceModule>& rhs) {
            return rhs->identifier() == mod->identifier();
        }
    );
    if (it != _modules.end()) {
        throw ghoul::RuntimeError(
            "Module name '" + mod->identifier() + "' was registered before",
            "ModuleEngine"
        );
    }

    LDEBUG(fmt::format("Registering module '{}'", mod->identifier()));
    mod->initialize(this, configuration);
    addPropertySubOwner(mod.get());
    LDEBUG(fmt::format("Registered module '{}'", mod->identifier()));
    _modules.push_back(std::move(mod));
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
    ghoul::systemcapabilities::Version version = { 0, 0, 0 };

    for (const std::unique_ptr<OpenSpaceModule>& m : _modules) {
        version = std::max(version, m->requiredOpenGLVersion());
    }

    return version;
}

scripting::LuaLibrary ModuleEngine::luaLibrary() {
    return {
        "modules",
        {
            {
                "isLoaded",
                &luascriptfunctions::isLoaded,
                {},
                "string",
                "Checks whether a specific module is loaded"
            }
        }
    };
}

} // namespace openspace
