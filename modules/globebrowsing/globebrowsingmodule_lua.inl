/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/globebrowsing/src/renderableglobe.h>

#include <modules/globebrowsing/src/layer.h>
#include <modules/globebrowsing/src/layergroup.h>
#include <modules/globebrowsing/src/layermanager.h>
#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>
#include <openspace/util/updatestructures.h>

namespace openspace::globebrowsing::luascriptfunctions {

/**
 * Adds a layer to the specified globe.
 */
int addLayer(lua_State* L) {
    ZoneScoped

    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::addLayer");
    auto [globeName, layerGroupName, layerDict] =
        ghoul::lua::values<std::string, std::string, ghoul::Dictionary>(L);

    // Get the node and make sure it exists
    SceneGraphNode* n = global::renderEngine->scene()->sceneGraphNode(globeName);
    if (!n) {
        return ghoul::lua::luaError(L, "Unknown globe name: " + globeName);
    }

    // Get the renderable globe
    RenderableGlobe* globe = dynamic_cast<RenderableGlobe*>(n->renderable());
    if (!globe) {
        return ghoul::lua::luaError(L, "Renderable is not a globe: " + globeName);
    }

    // Get the layer group
    layergroupid::GroupID groupID = ghoul::from_string<layergroupid::GroupID>(
        layerGroupName
    );
    if (groupID == layergroupid::GroupID::Unknown) {
        return ghoul::lua::luaError(L, "Unknown layer group: " + layerGroupName);
    }

    // Get the dictionary defining the layer
    Layer* layer = globe->layerManager().addLayer(groupID, layerDict);
    if (layer) {
        layer->initialize();
    }
    return 0;
}

/**
 * Deletes a layer from the specified globe.
 */
int deleteLayer(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::deleteLayer");
    auto [globeName, layerGroupName, layerName] =
        ghoul::lua::values<std::string, std::string, std::string>(L);

    // Get the node and make sure it exists
    SceneGraphNode* n = global::renderEngine->scene()->sceneGraphNode(globeName);
    if (!n) {
        return ghoul::lua::luaError(L, "Unknown globe name: " + globeName);
    }

    // Get the renderable globe
    RenderableGlobe* globe = dynamic_cast<RenderableGlobe*>(n->renderable());
    if (!globe) {
        return ghoul::lua::luaError(L, "Renderable is not a globe: " + globeName);
    }

    // Get the layer group
    layergroupid::GroupID groupID = ghoul::from_string<layergroupid::GroupID>(
        layerGroupName
    );
    if (groupID == layergroupid::GroupID::Unknown) {
        return ghoul::lua::luaError(L, "Unknown layer group: " + layerGroupName);
    }

    globe->layerManager().deleteLayer(groupID, layerName);
    return 0;
}

int getLayers(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::getLayers");
    auto [globeIdentifier, layer] = ghoul::lua::values<std::string, std::string>(L);

    SceneGraphNode* n = sceneGraphNode(globeIdentifier);
    if (!n) {
        return ghoul::lua::luaError(L, "Unknown globe name: " + globeIdentifier);
    }

    const RenderableGlobe* globe = dynamic_cast<const RenderableGlobe*>(n->renderable());
    if (!globe) {
        return ghoul::lua::luaError(L, "Identifier must be a RenderableGlobe");
    }

    globebrowsing::layergroupid::GroupID group =
        ghoul::from_string<globebrowsing::layergroupid::GroupID>(layer);
    if (group == globebrowsing::layergroupid::GroupID::Unknown) {
        return ghoul::lua::luaError(L, "Unknown layer groupd: " + layer);
    }

    const globebrowsing::LayerGroup& lg = globe->layerManager().layerGroup(group);
    std::vector<globebrowsing::Layer*> layers = lg.layers();

    lua_newtable(L);
    int key = 1;
    for (globebrowsing::Layer* l : layers) {
        ghoul::lua::push(L, key, l->identifier());
        lua_settable(L, -3);
        key++;
    }
    return 1;
}

int moveLayer(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 4, "lua::moveLayer");
    auto [globeIdentifier, layer, oldPosition, newPosition] =
        ghoul::lua::values<std::string, std::string, int, int>(L);

    if (oldPosition == newPosition) {
        return 0;
    }

    SceneGraphNode* n = sceneGraphNode(globeIdentifier);
    if (!n) {
        return ghoul::lua::luaError(L, "Unknown globe name: " + globeIdentifier);
    }

    RenderableGlobe* globe = dynamic_cast<RenderableGlobe*>(n->renderable());
    if (!globe) {
        return ghoul::lua::luaError(L, "Identifier must be a RenderableGlobe");
    }

    globebrowsing::layergroupid::GroupID group =
        ghoul::from_string<globebrowsing::layergroupid::GroupID>(layer);
    if (group == globebrowsing::layergroupid::GroupID::Unknown) {
        return ghoul::lua::luaError(L, "Unknown layer groupd: " + layer);
    }

    globebrowsing::LayerGroup& lg = globe->layerManager().layerGroup(group);
    lg.moveLayers(oldPosition, newPosition);
    return 0;
}

int goToChunk(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 4, "lua::goToChunk");
    auto [identifier, x, y, level] = ghoul::lua::values<std::string, int, int, int>(L);

    SceneGraphNode* n = sceneGraphNode(identifier);
    if (!n) {
        return ghoul::lua::luaError(L, "Unknown globe name: " + identifier);
    }

    const RenderableGlobe* globe = dynamic_cast<const RenderableGlobe*>(n->renderable());
    if (!globe) {
        return ghoul::lua::luaError(L, "Identifier must be a RenderableGlobe");
    }

    global::moduleEngine->module<GlobeBrowsingModule>()->goToChunk(*globe, x, y, level);
    return 0;
}

int goToGeo(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 2, 4 }, "lua::goToGeo");

    // Check if the user provided a Scene graph node identifier as the first argument.
    // lua_isstring returns true for both numbers and strings, so better use !lua_isnumber
    const bool providedGlobeIdentifier = !lua_isnumber(L, 1);
    const SceneGraphNode* n;
    if (providedGlobeIdentifier) {
        const std::string& globeIdentifier = ghoul::lua::value<std::string>(L);
        n = sceneGraphNode(globeIdentifier);
        if (!n) {
            return ghoul::lua::luaError(L, "Unknown globe name: " + globeIdentifier);
        }
    }
    else {
        n = global::navigationHandler->orbitalNavigator().anchorNode();
        if (!n) {
            return ghoul::lua::luaError(L, "No anchor node is set.");
        }
    }

    auto [latitude, longitude, altitude] =
        ghoul::lua::values<double, double, std::optional<double>>(L);

    const RenderableGlobe* globe = dynamic_cast<const RenderableGlobe*>(n->renderable());
    if (!globe) {
        if (providedGlobeIdentifier) {
            return ghoul::lua::luaError(L, "Identifier must be a RenderableGlobe");
        }
        else {
            return ghoul::lua::luaError(
                L,
                "Current anchor node is not a RenderableGlobe. Either change the anchor "
                "to a globe, or specify a globe identifier as the first argument"
            );
        }
    }

    if (altitude.has_value()) {
        global::moduleEngine->module<GlobeBrowsingModule>()->goToGeo(
            *globe,
            latitude,
            longitude,
            *altitude
        );
    }
    else {
        global::moduleEngine->module<GlobeBrowsingModule>()->goToGeo(
            *globe,
            latitude,
            longitude
        );
    }
    return 0;
}

int flyToGeo(lua_State* L) {
    int nArguments = ghoul::lua::checkArgumentsAndThrow(L, { 3, 6 }, "lua::flyToGeo");

    // Check if the user provided a Scene graph node identifier as the first argument.
    // lua_isstring returns true for both numbers and strings, so better use !lua_isnumber
    const bool providedGlobeIdentifier = !lua_isnumber(L, 1);
    const int parameterOffset = providedGlobeIdentifier ? 1 : 0;

    const SceneGraphNode* n;
    if (providedGlobeIdentifier) {
        const std::string& globeIdentifier =
            ghoul::lua::value<std::string>(L, 1, ghoul::lua::PopValue::No);
        n = sceneGraphNode(globeIdentifier);
        if (!n) {
            return ghoul::lua::luaError(L, "Unknown globe name: " + globeIdentifier);
        }
    }
    else {
        n = global::navigationHandler->orbitalNavigator().anchorNode();
        if (!n) {
            return ghoul::lua::luaError(L, "No anchor node is set.");
        }
    }

    const RenderableGlobe* globe = dynamic_cast<const RenderableGlobe*>(n->renderable());
    if (!globe) {
        if (providedGlobeIdentifier) {
            return ghoul::lua::luaError(L, "Identifier must be a RenderableGlobe");
        }
        else {
            return ghoul::lua::luaError(L,
                "Current anchor node is not a RenderableGlobe. "
                "Either change the anchor to a globe, or specify a globe identifier "
                "as the first argument"
            );
        }
    }

    const double latitude =
        ghoul::lua::value<double>(L, parameterOffset + 1, ghoul::lua::PopValue::No);
    const double longitude =
        ghoul::lua::value<double>(L, parameterOffset + 2, ghoul::lua::PopValue::No);
    const double altitude =
        ghoul::lua::value<double>(L, parameterOffset + 3, ghoul::lua::PopValue::No);

    // Compute the relative position based on the input values
    auto module = global::moduleEngine->module<GlobeBrowsingModule>();
    const glm::dvec3 positionModelCoords = module->cartesianCoordinatesFromGeo(
        *globe,
        latitude,
        longitude,
        altitude
    );

    using namespace std::string_literals;
    ghoul::Dictionary instruction;
    instruction.setValue("TargetType", "Node"s);
    instruction.setValue("Target", n->identifier());
    instruction.setValue("Position", positionModelCoords);

    // Handle the two optional arguments: duration and use target's up direction argument
    // The user can either provide both, or one of them
    if (nArguments >= parameterOffset + 4) {
        const int firstLocation = parameterOffset + 4;
        const bool firstIsNumber = (lua_isnumber(L, firstLocation) != 0);
        const bool firstIsBool = (lua_isboolean(L, firstLocation) != 0);

        if (!(firstIsNumber || firstIsBool)) {
            const char* msg = lua_pushfstring(
                L,
                "%s or %s expected, got %s",
                lua_typename(L, LUA_TNUMBER),
                lua_typename(L, LUA_TBOOLEAN),
                luaL_typename(L, -1)
            );
            return ghoul::lua::luaError(
                L, fmt::format("bad argument #{} ({})", firstLocation, msg)
            );
        }

        int location = firstLocation;
        if (firstIsBool) {
            const bool useUpFromTarget = (lua_toboolean(L, location) == 1);
            instruction.setValue("UseTargetUpDirection", useUpFromTarget);

            if (nArguments > location) {
                location++;
            }
        }

        if (firstIsNumber || nArguments > firstLocation) {
            double duration =
                ghoul::lua::value<double>(L, location, ghoul::lua::PopValue::No);
            constexpr const double Epsilon = 1e-5;
            if (duration <= Epsilon) {
                return ghoul::lua::luaError(L, "Duration must be larger than zero");
            }
            instruction.setValue("Duration", duration);
        }
    }

    global::navigationHandler->pathNavigator().createPath(instruction);
    global::navigationHandler->pathNavigator().startPath();

    lua_settop(L, 0);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int getLocalPositionFromGeo(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 4, "lua::getLocalPositionFromGeo");
    auto [globeIdentifier, latitude, longitude, altitude] =
        ghoul::lua::values<std::string, double, double, double>(L);

    SceneGraphNode* n = sceneGraphNode(globeIdentifier);
    if (!n) {
        return ghoul::lua::luaError(L, "Unknown globe identifier: " + globeIdentifier);
    }
    const RenderableGlobe* globe = dynamic_cast<const RenderableGlobe*>(n->renderable());
    if (!globe) {
        return ghoul::lua::luaError(L, "Identifier must be a RenderableGlobe");
    }

    GlobeBrowsingModule& mod = *(global::moduleEngine->module<GlobeBrowsingModule>());
    glm::vec3 p = mod.cartesianCoordinatesFromGeo(*globe, latitude, longitude, altitude);
    ghoul::lua::push(L, p.x, p.y, p.z);
    return 3;
}

int getGeoPositionForCamera(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::getGeoPositionForCamera");

    GlobeBrowsingModule* module = global::moduleEngine->module<GlobeBrowsingModule>();
    const RenderableGlobe* globe = module->castFocusNodeRenderableToGlobe();
    if (!globe) {
        return ghoul::lua::luaError(L, "Focus node must be a RenderableGlobe");
    }

    const glm::dvec3 cameraPosition = global::navigationHandler->camera()->positionVec3();
    const SceneGraphNode* anchor =
        global::navigationHandler->orbitalNavigator().anchorNode();
    const glm::dmat4 inverseModelTransform = glm::inverse(anchor->modelTransform());
    const glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1.0));
    const SurfacePositionHandle posHandle = globe->calculateSurfacePositionHandle(
        cameraPositionModelSpace
    );

    const Geodetic2 geo2 = globe->ellipsoid().cartesianToGeodetic2(
        posHandle.centerToReferenceSurface
    );
    const double altitude = glm::length(cameraPositionModelSpace -
                                  posHandle.centerToReferenceSurface);

    ghoul::lua::push(L, glm::degrees(geo2.lat), glm::degrees(geo2.lon), altitude);
    return 3;
}

int loadWMSCapabilities(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::loadWMSCapabilities");
    auto [name, globe, url] =
        ghoul::lua::values<std::string, std::string, std::string>(L);

    global::moduleEngine->module<GlobeBrowsingModule>()->loadWMSCapabilities(
        std::move(name),
        std::move(globe),
        std::move(url)
    );
    return 0;
}

int removeWMSServer(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::removeWMSServer");
    const std::string name = ghoul::lua::value<std::string>(L);

    global::moduleEngine->module<GlobeBrowsingModule>()->removeWMSServer(name);
    return 0;
}

int capabilities(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::capabilities");
    const std::string name = ghoul::lua::value<std::string>(L);

    GlobeBrowsingModule::Capabilities cap =
        global::moduleEngine->module<GlobeBrowsingModule>()->capabilities(name);

    lua_newtable(L);
    for (size_t i = 0; i < cap.size(); ++i) {
        const GlobeBrowsingModule::Layer& l = cap[i];

        lua_newtable(L);
        ghoul::lua::push(L, "Name", l.name);
        lua_settable(L, -3);
        ghoul::lua::push(L, "URL", l.url);
        lua_settable(L, -3);
        lua_rawseti(L, -2, i + 1);
    }
    return 1;
}

} // namespace openspace::globebrowsing::luascriptfunctions
