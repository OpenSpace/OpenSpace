/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/autonavigation/autonavigationmodule.h>

#include <modules/autonavigation/autonavigationmodule_lua.inl>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <ghoul/logging/logmanager.h> 

namespace {
    constexpr const char* _loggerCat = "AutoNavigationModule";
} // namespace

namespace openspace {

AutoNavigationModule::AutoNavigationModule() : OpenSpaceModule(Name) {}

autonavigation::AutoNavigationHandler& AutoNavigationModule::AutoNavigationHandler() {
    return _autoNavigationHandler;
}

std::vector<documentation::Documentation> AutoNavigationModule::documentations() const {
    return {
        // TODO: call documentation methods for the other classes in this module
    };
}

scripting::LuaLibrary AutoNavigationModule::luaLibrary() const {
    scripting::LuaLibrary res;
    res.name = "autonavigation";
    res.functions = {
        {
            "goTo",
            &autonavigation::luascriptfunctions::goTo,
            {},
            "string, [double]",
            "TODO: Description. Go to the node with the given name with optional duration."
        },
        {
            "generatePath",
            &autonavigation::luascriptfunctions::generatePath,
            {},
            "table",
            "Generate the path as described by the lua table input argument. TODO: Describe how a path instruction is defined?. " //TODO also make this one start path from file
        },
        {
            "generatePathFromFile",
            &autonavigation::luascriptfunctions::generatePathFromFile,
            {},
            "string",
            "Read an input file with lua instructions and use those to generate a camera path. TODO: Describe how a path instruction is defined?. " //TODO also make this one start path from file
        }
    };
    return res;  
}

void AutoNavigationModule::internalInitialize(const ghoul::Dictionary&) {
    global::callback::preSync.emplace_back([this]() {
        _autoNavigationHandler.updateCamera(global::windowDelegate.deltaTime());
    });

    // TODO: register other classes (that are Factory created) and used in this module, if any
}

} // namespace openspace
