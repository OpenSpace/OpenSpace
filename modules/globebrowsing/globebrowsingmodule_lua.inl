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

#include <modules/globebrowsing/globes/renderableglobe.h>

#include <modules/globebrowsing/geometry/angle.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/rendering/layer/layer.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>

namespace openspace::globebrowsing::luascriptfunctions {

/**
 *Adds a layer to the specified globe.
 */
int addLayer(lua_State* L) {
    using ghoul::lua::errorLocation;

    // Argument locations
    const int GlobeLocation = -3;
    const int LayerGroupLocation = -2;

    int nArguments = lua_gettop(L);
    if (nArguments != 3) {
        return luaL_error(L, "Expected %i arguments, got %i", 3, nArguments);
    }

    // String arguments
    const std::string GlobeName = luaL_checkstring(L, GlobeLocation);
    const std::string LayerGroupName = luaL_checkstring(L, LayerGroupLocation);

    // Get the node and make sure it exists
    SceneGraphNode* node = OsEng.renderEngine().scene()->sceneGraphNode(GlobeName);
    if (!node) {
        return luaL_error(L, ("Unknown globe name: " + GlobeName).c_str());
    }
  
    // Get the renderable globe
    RenderableGlobe* globe = dynamic_cast<RenderableGlobe*>(node->renderable());
    if (!globe) {
        return luaL_error(L, ("Renderable is not a globe: " + GlobeName).c_str());
    }
  
    // Get the layer group
    layergroupid::GroupID groupID = layergroupid::getGroupIDFromName(LayerGroupName);
    if (groupID == layergroupid::GroupID::Unknown) {
        return luaL_error(L, ("Unknown layer group: " + LayerGroupName).c_str());
    }

    // Get the dictionary defining the layer
    ghoul::Dictionary d;
    try {
        ghoul::lua::luaDictionaryFromState(L, d);
    }
    catch (const ghoul::lua::LuaFormatException& e) {
        LERRORC("addLayerFromDictionary", e.what());
        return 0;
    }

    globe->layerManager()->addLayer(groupID, d);
    
    return 0;
}

/**
 Deletes a layer from the specified globe.
 */
int deleteLayer(lua_State* L) {
    using ghoul::lua::errorLocation;

    // Argument locations
    const int GlobeLocation = -3;
    const int LayerGroupLocation = -2;
    const int NameLocation = -1;

    int nArguments = lua_gettop(L);
    if (nArguments != 3) {
        return luaL_error(L, "Expected %i arguments, got %i", 3, nArguments);
    }

    // String arguments
    const std::string GlobeName = luaL_checkstring(L, GlobeLocation);
    const std::string LayerGroupName = luaL_checkstring(L, LayerGroupLocation);
    const std::string LayerName = luaL_checkstring(L, NameLocation);

    // Get the node and make sure it exists
    SceneGraphNode* node = OsEng.renderEngine().scene()->sceneGraphNode(GlobeName);
    if (!node) {
        return luaL_error(L, ("Unknown globe name: " + GlobeName).c_str());
    }
  
    // Get the renderable globe
    RenderableGlobe* globe = dynamic_cast<RenderableGlobe*>(node->renderable());
    if (!globe) {
        return luaL_error(L, ("Renderable is not a globe: " + GlobeName).c_str());
    }
  
    // Get the layer group
    layergroupid::GroupID groupID = layergroupid::getGroupIDFromName(LayerGroupName);
    if (groupID == layergroupid::GroupID::Unknown) {
        return luaL_error(L, ("Unknown layer group: " + LayerGroupName).c_str());
    }

    globe->layerManager()->deleteLayer(groupID, LayerName);
    
    return 0;
}

int goToChunk(lua_State* L) {
    using ghoul::lua::luaTypeToString;

    // Check arguments
    int nArguments = lua_gettop(L);
    if (nArguments != 3) {
        return luaL_error(L, "Expected %i arguments, got %i", 3, nArguments);
    }

    int x = static_cast<int>(lua_tonumber(L, 1));
    int y = static_cast<int>(lua_tonumber(L, 2));
    int level = static_cast<int>(lua_tonumber(L, 3));

    OsEng.moduleEngine().module<GlobeBrowsingModule>()->goToChunk(x, y, level);

    return 0;
}

int goToGeo(lua_State* L) {
    using ghoul::lua::luaTypeToString;
    
    int nArguments = lua_gettop(L);
    if (nArguments != 2 && nArguments != 3) {
        return luaL_error(L, "Expected 2 or 3 arguments.");
    }
    
    double latitude = static_cast<double>(lua_tonumber(L, 1));
    double longitude = static_cast<double>(lua_tonumber(L, 2));

    if (nArguments == 2) {
        OsEng.moduleEngine().module<GlobeBrowsingModule>()->goToGeo(latitude, longitude);
    }
    else if (nArguments == 3) {
        double altitude = static_cast<int>(lua_tonumber(L, 3));
        OsEng.moduleEngine().module<GlobeBrowsingModule>()->goToGeo(latitude, longitude,
                                                                   altitude);
    }
    
    return 0;
}

int getGeoPosition(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 0) {
        return luaL_error(L, "Expected 0 arguments.");
    }

    RenderableGlobe* globe =
     OsEng.moduleEngine().module<GlobeBrowsingModule>()->castFocusNodeRenderableToGlobe();
    if (!globe) {
        return luaL_error(L, "Focus node must be a RenderableGlobe");
    }

    glm::dvec3 cameraPosition = OsEng.navigationHandler().camera()->positionVec3();
    glm::dmat4 inverseModelTransform =
        OsEng.navigationHandler().focusNode()->inverseModelTransform();
    glm::dvec3 cameraPositionModelSpace =
        inverseModelTransform * glm::dvec4(cameraPosition, 1.0);
    SurfacePositionHandle posHandle = globe->calculateSurfacePositionHandle(
                                                                cameraPositionModelSpace);
    
    Geodetic2 geo2 = globe->ellipsoid().cartesianToGeodetic2(posHandle.centerToReferenceSurface);
    double altitude = glm::length(cameraPositionModelSpace - posHandle.centerToReferenceSurface);

    lua_pushnumber(L, Angle<double>::fromRadians(geo2.lat).asDegrees());
    lua_pushnumber(L, Angle<double>::fromRadians(geo2.lon).asDegrees());
    lua_pushnumber(L, altitude);
    
    return 3;
}

} // namespace openspace::globebrowsing::luascriptfunctions
