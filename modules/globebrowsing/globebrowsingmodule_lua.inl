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

#include <modules/globebrowsing/globes/renderableglobe.h>

#include <modules/globebrowsing/geometry/angle.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/rendering/layer/layer.h>

#include <openspace/interaction/navigationhandler.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>
#include <openspace/util/camera.h>
#include <openspace/util/updatestructures.h>

namespace openspace::globebrowsing::luascriptfunctions {

/**
 * Adds a layer to the specified globe.
 */
int addLayer(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::addLayer");

    // String arguments
    const std::string& globeName = ghoul::lua::value<std::string>(L, 1);
    const std::string& layerGroupName = ghoul::lua::value<std::string>(L, 2);

    // Get the node and make sure it exists
    SceneGraphNode* n = global::renderEngine.scene()->sceneGraphNode(globeName);
    if (!n) {
        return ghoul::lua::luaError(L, "Unknown globe name: " + globeName);
    }

    // Get the renderable globe
    const RenderableGlobe* globe = dynamic_cast<const RenderableGlobe*>(n->renderable());
    if (!globe) {
        return ghoul::lua::luaError(L, "Renderable is not a globe: " + globeName);
    }

    // Get the layer group
    layergroupid::GroupID groupID = layergroupid::getGroupIDFromName(layerGroupName);
    if (groupID == layergroupid::GroupID::Unknown) {
        return ghoul::lua::luaError(L, "Unknown layer group: " + layerGroupName);
    }

    // Get the dictionary defining the layer
    ghoul::Dictionary d;
    try {
        ghoul::lua::luaDictionaryFromState(L, d);
    }
    catch (const ghoul::lua::LuaFormatException& e) {
        LERRORC("addLayerFromDictionary", e.what());
        lua_settop(L, 0);
        return 0;
    }
    lua_settop(L, 0);

    std::shared_ptr<Layer> layer = globe->layerManager()->addLayer(groupID, d);
    if (layer) {
        layer->initialize();
    }

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
 * Deletes a layer from the specified globe.
 */
int deleteLayer(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::deleteLayer");

    const std::string& globeName = luaL_checkstring(L, 1);
    const std::string& layerGroupName = luaL_checkstring(L, 2);
    const std::string& layerName = luaL_checkstring(L, 3);
    lua_pop(L, 3);

    // Get the node and make sure it exists
    SceneGraphNode* n = global::renderEngine.scene()->sceneGraphNode(globeName);
    if (!n) {
        return ghoul::lua::luaError(L, "Unknown globe name: " + globeName);
    }

    // Get the renderable globe
    const RenderableGlobe* globe = dynamic_cast<const RenderableGlobe*>(n->renderable());
    if (!globe) {
        return ghoul::lua::luaError(L, "Renderable is not a globe: " + globeName);
    }

    // Get the layer group
    layergroupid::GroupID groupID = layergroupid::getGroupIDFromName(layerGroupName);
    if (groupID == layergroupid::GroupID::Unknown) {
        return ghoul::lua::luaError(L, "Unknown layer group: " + layerGroupName);
    }

    globe->layerManager()->deleteLayer(groupID, layerName);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int goToChunk(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::goToChunk");

    const int x = ghoul::lua::value<int>(L, 1);
    const int y = ghoul::lua::value<int>(L, 2);
    const int level = ghoul::lua::value<int>(L, 3);
    lua_pop(L, 3);

    global::moduleEngine.module<GlobeBrowsingModule>()->goToChunk(x, y, level);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int goToGeo(lua_State* L) {
    const int nArguments = ghoul::lua::checkArgumentsAndThrow(L, 2, 3, "lua::goToGeo");

    const double latitude = ghoul::lua::value<double>(L, 1);
    const double longitude = ghoul::lua::value<double>(L, 2);

    if (nArguments == 2) {
        global::moduleEngine.module<GlobeBrowsingModule>()->goToGeo(latitude, longitude);
    }
    else if (nArguments == 3) {
        const double altitude = ghoul::lua::value<double>(L, 3);
        global::moduleEngine.module<GlobeBrowsingModule>()->goToGeo(
            latitude,
            longitude,
            altitude
        );
    }

    lua_settop(L, 0);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int getGeoPosition(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 4, "lua::getGeoPosition");

    const std::string& name = ghoul::lua::value<std::string>(L, 1);
    const double latitude = ghoul::lua::value<double>(L, 2);
    const double longitude = ghoul::lua::value<double>(L, 3);
    const double altitude = ghoul::lua::value<double>(L, 4);
    lua_pop(L, 4);

    SceneGraphNode* n = sceneGraphNode(name);
    if (!n) {
        return ghoul::lua::luaError(L, "Unknown globe name: " + name);
    }
    const RenderableGlobe* globe = dynamic_cast<const RenderableGlobe*>(n->renderable());
    if (!globe) {
        return ghoul::lua::luaError(L, "Name must be a RenderableGlobe");
    }

    GlobeBrowsingModule& mod = *(global::moduleEngine.module<GlobeBrowsingModule>());
    glm::vec3 pos = mod.cartesianCoordinatesFromGeo(
        *globe,
        latitude,
        longitude,
        altitude
    );

    ghoul::lua::push(L, pos.x, pos.y, pos.z);

    ghoul_assert(lua_gettop(L) == 3, "Incorrect number of items left on stack");
    return 3;
}

int getGeoPositionForCamera(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::getGeoPositionForCamera");

    GlobeBrowsingModule* module = global::moduleEngine.module<GlobeBrowsingModule>();
    const RenderableGlobe* globe = module->castFocusNodeRenderableToGlobe();
    if (!globe) {
        return ghoul::lua::luaError(L, "Focus node must be a RenderableGlobe");
    }

    const glm::dvec3 cameraPosition = global::navigationHandler.camera()->positionVec3();
    const glm::dmat4 inverseModelTransform =
        global::navigationHandler.focusNode()->inverseModelTransform();
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

    ghoul::lua::push(
        L,
        Angle<double>::fromRadians(geo2.lat).asDegrees(),
        Angle<double>::fromRadians(geo2.lon).asDegrees(),
        altitude
    );

    ghoul_assert(lua_gettop(L) == 3, "Incorrect number of items left on stack");
    return 3;
}

#ifdef GLOBEBROWSING_USE_GDAL
int loadWMSCapabilities(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::loadWMSCapabilities");

    std::string name = ghoul::lua::value<std::string>(L, 1);
    std::string globe = ghoul::lua::value<std::string>(L, 2);
    std::string url = ghoul::lua::value<std::string>(L, 3);

    global::moduleEngine.module<GlobeBrowsingModule>()->loadWMSCapabilities(
        std::move(name),
        std::move(globe),
        std::move(url)
    );

    lua_pop(L, 3);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int removeWMSServer(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::removeWMSServer");

    const std::string& name = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );

    global::moduleEngine.module<GlobeBrowsingModule>()->removeWMSServer(name);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int capabilities(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::capabilities");

    const std::string& name = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );
    GlobeBrowsingModule::Capabilities cap =
        global::moduleEngine.module<GlobeBrowsingModule>()->capabilities(name);

    lua_newtable(L);
    for (unsigned long i = 0; i < cap.size(); ++i) {
        const GlobeBrowsingModule::Layer& l = cap[i];

        lua_newtable(L);

        ghoul::lua::push(L, "Name", l.name);
        lua_settable(L, -3);

        ghoul::lua::push(L, "URL", l.url);
        lua_settable(L, -3);

        lua_rawseti(L, -2, i + 1);
    }

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}
#endif // GLOBEBROWSING_USE_GDAL

} // namespace openspace::globebrowsing::luascriptfunctions
