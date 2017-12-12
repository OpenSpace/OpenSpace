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

#include <openspace/engine/moduleengine.h>

#include <openspace/moduleregistration.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/openspacemodule.h>

#include <ghoul/logging/logmanager.h>

#include <algorithm>

#include "moduleengine_lua.inl"

namespace {
    constexpr const char* _loggerCat = "ModuleEngine";
} // namespace

namespace openspace {

void ModuleEngine::initialize() {
    for (OpenSpaceModule* m : AllModules()) {
        registerModule(std::unique_ptr<OpenSpaceModule>(m));
    }
}

void ModuleEngine::deinitialize() {
    LDEBUG("Deinitializing modules");
    for (auto& m : _modules) {
        LDEBUG("Deinitializing module '" << m->name() << "'");
        m->deinitialize();
    }
    _modules.clear();
    LDEBUG("Finished destroying modules");
}

void ModuleEngine::registerModule(std::unique_ptr<OpenSpaceModule> m) {
    ghoul_assert(m, "Module must not be nullptr");

    auto it = std::find_if(
        _modules.begin(),
        _modules.end(),
        [&m](std::unique_ptr<OpenSpaceModule>& rhs) {
            return rhs->name() == m->name();
        }
    );
    if (it != _modules.end()) {
        throw ghoul::RuntimeError(
            "Module name '" + m->name() + "' was registered before",
            "ModuleEngine"
        );
    }

    LDEBUG("Registering module '" << m->name() << "'");
    m->initialize();
    LDEBUG("Registered module '" << m->name() << "'");
    _modules.push_back(std::move(m));
}

std::vector<OpenSpaceModule*> ModuleEngine::modules() const {
    std::vector<OpenSpaceModule*> result;
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
                "string",
                "Checks whether a specific module is loaded"
            }
        }
    };
}

} // namespace openspace
