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

#include <openspace/engine/moduleengine.h>

#include <openspace/util/openspacemodule.h>

#include <ghoul/logging/logmanager.h>

#include <openspace/moduleregistration.h>

namespace {
    const std::string _loggerCat = "ModuleEngine";
}

namespace openspace {

void ModuleEngine::create() {
    LDEBUG("Creating modules");

    for (OpenSpaceModule* m : AllModules)
        registerModule(std::unique_ptr<OpenSpaceModule>(m));

    for (auto& m : _modules)
        m->create();
    LDEBUG("Finished creating modules");
    return true;
}

void ModuleEngine::destroy() {
    LDEBUG("Destroying modules");
    for (auto& m : _modules)
        m->destroy();
    _modules.clear();
    LDEBUG("Finished destroying modules");
}

bool ModuleEngine::initialize() {
    LDEBUG("Initializing modules");

    for (auto& m : _modules) {
        bool success = m->initialize();
        if (!success) {
            LERROR("Could not initialize module '" << m->name() << "'");
            return false;
        }
    }
    LDEBUG("Finished initializing modules");
    return true;
}

bool ModuleEngine::deinitialize() {
    LDEBUG("Deinitializing modules");

    for (auto& m : _modules) {
        bool success = m->deinitialize();
        if (!success) {
            LERROR("Could not deinitialize module '" << m->name() << "'");
            return false;
        }
    }
    LDEBUG("Finished Deinitializing modules");
    return true;
}

void ModuleEngine::registerModules(std::vector<std::unique_ptr<OpenSpaceModule>> modules) {
    _modules.insert(
        _modules.end(),
        std::make_move_iterator(modules.begin()),
        std::make_move_iterator(modules.end())
    );
}

void ModuleEngine::registerModule(std::unique_ptr<OpenSpaceModule> module) {
    _modules.push_back(std::move(module));
}

std::vector<OpenSpaceModule*> ModuleEngine::modules() const {
    std::vector<OpenSpaceModule*> result;
    for (auto& m : _modules)
        result.push_back(m.get());
    return result;
}

} // namespace openspace
