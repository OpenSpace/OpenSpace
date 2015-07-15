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

bool ModuleEngine::create() {
    LDEBUG("Creating modules");

    registerModules(AllModules);

    for (OpenSpaceModule* m : _modules) {
        bool success = m->create();
        if (!success) {
            LERROR("Could not initialize module '" << m->name() << "'");
            return false;
        }
    }
    LDEBUG("Finished creating modules");
    return true;
}

bool ModuleEngine::destroy() {
    LDEBUG("Destroying modules");
    for (OpenSpaceModule* m : _modules) {
        bool success = m->destroy();
        if (!success) {
            LERROR("Could not deinitialize module '" << m->name() << "'");
            return false;
        }
        delete m;
    }
    LDEBUG("Finished destroying modules");
    return true;
}

bool ModuleEngine::initialize() {
    LDEBUG("Initializing modules");

    for (OpenSpaceModule* m : _modules) {
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

    for (OpenSpaceModule* m : _modules) {
        bool success = m->deinitialize();
        if (!success) {
            LERROR("Could not deinitialize module '" << m->name() << "'");
            return false;
        }
    }
    LDEBUG("Finished Deinitializing modules");
    return true;
}

void ModuleEngine::registerModules(std::vector<OpenSpaceModule*> modules) {
    _modules.insert(_modules.end(), modules.begin(), modules.end());
}

void ModuleEngine::registerModule(OpenSpaceModule* module) {
    _modules.push_back(std::move(module));
}

const std::vector<OpenSpaceModule*> ModuleEngine::modules() const {
    return _modules;
}

} // namespace openspace
