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

#include <modules/autonavigation/pathspecification.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/camera.h>
#include <openspace/util/updatestructures.h>
#include <openspace/query/query.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/vector_angle.hpp>

namespace openspace::autonavigation::luascriptfunctions {

    const double EPSILON = 1e-12;

    int goTo(lua_State* L) {
        int nArguments = ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "lua::goTo");

        const std::string& nodeIdentifier = ghoul::lua::value<std::string>(L, 1);

        if (!sceneGraphNode(nodeIdentifier)) {
            lua_settop(L, 0);
            return ghoul::lua::luaError(L, "Unknown node name: " + nodeIdentifier);
        }

        ghoul::Dictionary insDict;
        insDict.setValue("Target", nodeIdentifier);

        if (nArguments > 1) {
            double duration = ghoul::lua::value<double>(L, 2);
            if (duration <= EPSILON) {
                lua_settop(L, 0);
                return ghoul::lua::luaError(L, "Duration must be larger than zero.");
            }
            insDict.setValue("Duration", duration);
        }

        PathSpecification spec = PathSpecification(Instruction{insDict});

        AutoNavigationModule* module = global::moduleEngine.module<AutoNavigationModule>(); 
        AutoNavigationHandler& handler = module->AutoNavigationHandler();
        handler.createPath(spec);

        lua_settop(L, 0);
        ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
        return 0;
    }

    int generatePath(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::generatePath");

        ghoul::Dictionary dictionary;
        ghoul::lua::luaDictionaryFromState(L, dictionary);
        PathSpecification spec(dictionary);

        if (spec.instructions()->empty()) {
            lua_settop(L, 0);
            return ghoul::lua::luaError(
                L, fmt::format("No instructions for camera path generation were provided.")
            );
        }

        AutoNavigationModule* module = global::moduleEngine.module<AutoNavigationModule>();
        AutoNavigationHandler& handler = module->AutoNavigationHandler();
        handler.createPath(spec);

        lua_settop(L, 0);
        ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
        return 0;
    }

    int generatePathFromFile(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::generatePathFromFile");

        const std::string& filepath = ghoul::lua::value<std::string>(L, 1, ghoul::lua::PopValue::Yes);

        if (filepath.empty()) {
            return ghoul::lua::luaError(L, "filepath string is empty");
        }

        const std::string absolutePath = absPath(filepath);

        LINFOC("AutoNavigationModule", fmt::format("Reading path instructions from file: {}", absolutePath));

        if (!FileSys.fileExists(absolutePath)) {
            throw ghoul::FileNotFoundError(absolutePath, "PathSpecification");
        }

        // Try to read the dictionary
        ghoul::Dictionary dictionary;
        try {
            ghoul::lua::loadDictionaryFromFile(absolutePath, dictionary);
            openspace::documentation::testSpecificationAndThrow(
                PathSpecification::Documentation(),
                dictionary,
                "PathSpecification"
            );
        }
        catch (ghoul::RuntimeError& e) {
            return ghoul::lua::luaError(
                L, fmt::format("Unable to read dictionary from file: {}", e.message)
            );
        }

        PathSpecification spec(dictionary);

        if (spec.instructions()->empty()) {
            return ghoul::lua::luaError(
                L, fmt::format("No instructions for camera path generation were provided.")
            );
        }

        LINFOC("AutoNavigationModule", "Reading succeeded. Creating path");

        AutoNavigationModule* module = global::moduleEngine.module<AutoNavigationModule>();
        AutoNavigationHandler& handler = module->AutoNavigationHandler();
        handler.createPath(spec);

        ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
        return 0;
    }

} // namespace openspace::autonavigation::luascriptfunctions
