/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <openspace/util/collisionhelper.h>

namespace {

/**
 * Adds a layer to the specified globe. The first argument specifies the name of the scene
 * graph node of which to add the layer. The renderable of the specified scene graph node
 * needs to be a renderable globe. The second argument is the layer group which can be any
 * of the supported layer groups. The third argument is the dictionary defining the layer.
 */
[[codegen::luawrap]] void addLayer(std::string globeName, std::string layerGroupName,
                                   ghoul::Dictionary layer)
{
    using namespace openspace;
    using namespace globebrowsing;

    // Get the node and make sure it exists
    SceneGraphNode* n = global::renderEngine->scene()->sceneGraphNode(globeName);
    if (!n) {
        throw ghoul::lua::LuaError("Unknown globe name: " + globeName);
    }

    // Get the renderable globe
    RenderableGlobe* globe = dynamic_cast<RenderableGlobe*>(n->renderable());
    if (!globe) {
        throw ghoul::lua::LuaError("Renderable is not a globe: " + globeName);
    }

    // Get the layer group
    layergroupid::GroupID groupID = ghoul::from_string<layergroupid::GroupID>(
        layerGroupName
        );
    if (groupID == layergroupid::GroupID::Unknown) {
        throw ghoul::lua::LuaError("Unknown layer group: " + layerGroupName);
    }

    // Get the dictionary defining the layer
    Layer* l = globe->layerManager().addLayer(groupID, layer);
    if (l) {
        l->initialize();
    }
}

/**
 * Removes a layer from the specified globe. The first argument specifies the name of the
 * scene graph node of which to remove the layer. The renderable of the specified scene
 * graph node needs to be a renderable globe. The second argument is the layer group which
 * can be any of the supported layer groups. The third argument is either the identifier
 * for the layer or a dictionary with the 'Identifier' key that is used instead.
 */
[[codegen::luawrap]] void deleteLayer(std::string globeName, std::string layerGroupName,
                                 std::variant<std::string, ghoul::Dictionary> layerOrName)
{
    using namespace openspace;
    using namespace globebrowsing;

    // Get the node and make sure it exists
    SceneGraphNode* n = global::renderEngine->scene()->sceneGraphNode(globeName);
    if (!n) {
        throw ghoul::lua::LuaError("Unknown globe name: " + globeName);
    }

    // Get the renderable globe
    RenderableGlobe* globe = dynamic_cast<RenderableGlobe*>(n->renderable());
    if (!globe) {
        throw ghoul::lua::LuaError("Renderable is not a globe: " + globeName);
    }

    // Get the layer group
    layergroupid::GroupID groupID = ghoul::from_string<layergroupid::GroupID>(
        layerGroupName
        );
    if (groupID == layergroupid::GroupID::Unknown) {
        throw ghoul::lua::LuaError("Unknown layer group: " + layerGroupName);
    }

    std::string layerName;
    if (std::holds_alternative<std::string>(layerOrName)) {
        layerName = std::get<std::string>(layerOrName);
    }
    else {
        ghoul::Dictionary d = std::get<ghoul::Dictionary>(layerOrName);
        if (!d.hasValue<std::string>("Identifier")) {
            throw ghoul::lua::LuaError(
                "Table passed to deleteLayer does not contain an Identifier"
            );
        }
        layerName = d.value<std::string>("Identifier");
    }

    globe->layerManager().deleteLayer(groupID, layerName);
}

/**
 * Returns the list of layers for the scene graph node specified in the first parameter.
 * The second parameter specifies which layer type should be queried.
 */
[[codegen::luawrap]] std::vector<std::string> getLayers(std::string globeIdentifier,
                                                        std::string layer)
{
    using namespace openspace;
    using namespace globebrowsing;

    SceneGraphNode* n = sceneGraphNode(globeIdentifier);
    if (!n) {
        throw ghoul::lua::LuaError("Unknown globe name: " + globeIdentifier);
    }

    const RenderableGlobe* globe = dynamic_cast<const RenderableGlobe*>(n->renderable());
    if (!globe) {
        throw ghoul::lua::LuaError("Identifier must be a RenderableGlobe");
    }

    globebrowsing::layergroupid::GroupID group =
        ghoul::from_string<globebrowsing::layergroupid::GroupID>(layer);
    if (group == globebrowsing::layergroupid::GroupID::Unknown) {
        throw ghoul::lua::LuaError("Unknown layer groupd: " + layer);
    }

    const globebrowsing::LayerGroup& lg = globe->layerManager().layerGroup(group);
    std::vector<globebrowsing::Layer*> layers = lg.layers();

    std::vector<std::string> res;
    res.reserve(layers.size());
    for (globebrowsing::Layer* l : layers) {
        res.push_back(l->identifier());
    }
    return res;
}

/**
 * Rearranges the order of a single layer on a globe. The first parameter is the
 * identifier of the globe, the second parameter specifies the name of the layer group,
 * the third parameter is the original position of the layer that should be moved and the
 * last parameter is the new position in the list. The first position in the list has
 * index 0, and the last position is given by the number of layers minus one. The new
 * position may be -1 to place the layer at the top or any number bigger than the number
 * of layers to place it at the bottom.
 */
[[codegen::luawrap]] void moveLayer(std::string globeIdentifier, std::string layer,
                                    int oldPosition, int newPosition)
{
    using namespace openspace;
    using namespace globebrowsing;

    if (oldPosition == newPosition) {
        return;
    }

    SceneGraphNode* n = sceneGraphNode(globeIdentifier);
    if (!n) {
        throw ghoul::lua::LuaError("Unknown globe name: " + globeIdentifier);
    }

    RenderableGlobe* globe = dynamic_cast<RenderableGlobe*>(n->renderable());
    if (!globe) {
        throw ghoul::lua::LuaError("Identifier must be a RenderableGlobe");
    }

    globebrowsing::layergroupid::GroupID group =
        ghoul::from_string<globebrowsing::layergroupid::GroupID>(layer);
    if (group == globebrowsing::layergroupid::GroupID::Unknown) {
        throw ghoul::lua::LuaError("Unknown layer groupd: " + layer);
    }

    globebrowsing::LayerGroup& lg = globe->layerManager().layerGroup(group);
    lg.moveLayer(oldPosition, newPosition);
}

/**
 * Go to the chunk on a globe with given index x, y, level.
 */
[[codegen::luawrap]] void goToChunk(std::string identifier, int x, int y, int level) {
    using namespace openspace;
    using namespace globebrowsing;

    SceneGraphNode* n = sceneGraphNode(identifier);
    if (!n) {
        throw ghoul::lua::LuaError("Unknown globe name: " + identifier);
    }

    const RenderableGlobe* globe = dynamic_cast<const RenderableGlobe*>(n->renderable());
    if (!globe) {
        throw ghoul::lua::LuaError("Identifier must be a RenderableGlobe");
    }

    global::moduleEngine->module<GlobeBrowsingModule>()->goToChunk(*globe, x, y, level);
}

/**
 * Go to geographic coordinates of a globe. The first (optional) argument is the
 * identifier of a scene graph node that has a RenderableGlobe attached. If no globe is
 * passed in, the current anchor will be used. The second argument is latitude and the
 * third is longitude (degrees). North and East are expressed as positive angles, while
 * South and West are negative. The optional fourh argument is the altitude in meters. If
 * no altitude is provided, the altitude will be kept as the current distance to the
 * surface of the specified globe.
 */
[[codegen::luawrap]] void goToGeo(std::optional<std::string> globe, double latitude,
                                  double longitude, std::optional<double> altitude)
{
    using namespace openspace;
    using namespace globebrowsing;

    const SceneGraphNode* n;
    if (globe.has_value()) {
        n = sceneGraphNode(*globe);
        if (!n) {
            throw ghoul::lua::LuaError("Unknown globe name: " + *globe);
        }
    }
    else {
        n = global::navigationHandler->orbitalNavigator().anchorNode();
        if (!n) {
            throw ghoul::lua::LuaError("No anchor node is set.");
        }
    }

    const RenderableGlobe* gl = dynamic_cast<const RenderableGlobe*>(n->renderable());
    if (!gl) {
        throw ghoul::lua::LuaError(
            "Current anchor node is not a RenderableGlobe. Either change the anchor "
            "to a globe, or specify a globe identifier as the first argument"
        );
    }

    if (altitude.has_value()) {
        global::moduleEngine->module<GlobeBrowsingModule>()->goToGeo(
            *gl,
            latitude,
            longitude,
            *altitude
        );
    }
    else {
        global::moduleEngine->module<GlobeBrowsingModule>()->goToGeo(
            *gl,
            latitude,
            longitude
        );
    }
}

/**
 * Fly the camera to geographic coordinates of a globe, using the path navigation system.
 * The first (optional) argument is the identifier of a scene graph node with a
 * RenderableGlobe. If no globe is passed in, the current anchor will be used. The second
 * and third argument is latitude and longitude (degrees). The fourth argument is the
 * altitude, in meters. The last two optional arguments are: a bool specifying whether the
 * up vector at the target position should be set to the globe's North vector, and a
 * duration for the motion, in seconds. Either of the two can be left out.
 */
[[codegen::luawrap]] void flyToGeo(std::optional<std::string> globe, double latitude,
                                   double longitude, double altitude,
                                   std::optional<double> duration,
                                   std::optional<bool> shouldUseUpVector)
{
    using namespace openspace;
    using namespace globebrowsing;

    const SceneGraphNode* n;
    if (globe.has_value()) {
        n = sceneGraphNode(*globe);
        if (!n) {
            throw ghoul::lua::LuaError("Unknown globe name: " + *globe);
        }
    }
    else {
        n = global::navigationHandler->orbitalNavigator().anchorNode();
        if (!n) {
            throw ghoul::lua::LuaError("No anchor node is set.");
        }
    }

    const RenderableGlobe* gl = dynamic_cast<const RenderableGlobe*>(n->renderable());
    if (!gl) {
        throw ghoul::lua::LuaError("Current anchor node is not a RenderableGlobe");
    }

    auto module = global::moduleEngine->module<GlobeBrowsingModule>();
    const glm::dvec3 positionModelCoords = module->cartesianCoordinatesFromGeo(
        *gl,
        latitude,
        longitude,
        altitude
    );

    const glm::dvec3 currentPosW = global::navigationHandler->camera()->positionVec3();
    const glm::dvec3 currentPosModelCoords =
        glm::inverse(gl->modelTransform()) * glm::dvec4(currentPosW, 1.0);

    constexpr double LengthEpsilon = 10.0; // meters
    if (glm::distance(currentPosModelCoords, positionModelCoords) < LengthEpsilon) {
        LINFOC("GlobeBrowsing", "flyToGeo: Already at the requested position");
        return;
    }

    ghoul::Dictionary instruction;
    instruction.setValue("TargetType", std::string("Node"));
    instruction.setValue("Target", n->identifier());
    instruction.setValue("Position", positionModelCoords);
    instruction.setValue("PathType", std::string("ZoomOutOverview"));

    if (duration.has_value()) {
        constexpr double Epsilon = 1e-5;
        if (*duration <= Epsilon) {
            throw ghoul::lua::LuaError("Duration must be larger than zero");
        }
        instruction.setValue("Duration", *duration);
    }

    if (shouldUseUpVector.has_value()) {
        instruction.setValue("UseTargetUpDirection", *shouldUseUpVector);

    }

    global::navigationHandler->pathNavigator().createPath(instruction);
    global::navigationHandler->pathNavigator().startPath();
}

/**
 * Returns a position in the local Cartesian coordinate system of the globe identified by
 * the first argument, that corresponds to the given geographic coordinates: latitude,
 * longitude and altitude (in degrees and meters). In the local coordinate system, the
 * position (0,0,0) corresponds to the globe's center.
 */
[[codegen::luawrap]]
std::tuple<double, double, double>
getLocalPositionFromGeo(std::string globeIdentifier, double latitude, double longitude,
                        double altitude)
{
    using namespace openspace;
    using namespace globebrowsing;

    SceneGraphNode* n = sceneGraphNode(globeIdentifier);
    if (!n) {
        throw ghoul::lua::LuaError("Unknown globe identifier: " + globeIdentifier);
    }
    const RenderableGlobe* globe = dynamic_cast<const RenderableGlobe*>(n->renderable());
    if (!globe) {
        throw ghoul::lua::LuaError("Identifier must be a RenderableGlobe");
    }

    GlobeBrowsingModule& mod = *(global::moduleEngine->module<GlobeBrowsingModule>());
    glm::vec3 p = mod.cartesianCoordinatesFromGeo(*globe, latitude, longitude, altitude);
    return { p.x, p.y, p.z };
}

/**
 * Get geographic coordinates of the camera position in latitude, longitude, and altitude
 * (degrees and meters). If the optional bool paramater is specified, the camera
 * eye postion will be used instead
 */
[[codegen::luawrap]] std::tuple<double, double, double>
getGeoPositionForCamera(bool useEyePosition = false)
{
    using namespace openspace;
    using namespace globebrowsing;

    GlobeBrowsingModule* module = global::moduleEngine->module<GlobeBrowsingModule>();
    // focus vs anchor
    const RenderableGlobe* globe = module->castFocusNodeRenderableToGlobe();
    if (!globe) {
        throw ghoul::lua::LuaError("Focus node must be a RenderableGlobe");
    }
    Camera* camera = global::navigationHandler->camera();

    glm::dvec3 cameraPosition = camera->positionVec3();


    const SceneGraphNode* anchor =
        global::navigationHandler->orbitalNavigator().anchorNode();
    const glm::dmat4 inverseModelTransform = glm::inverse(anchor->modelTransform());

    glm::dvec3 target;

    // @TODO (04-08-2022, micahnyc)
    // adjust this to use the camera lookat
    // once we fix this calculation, then we just add true to the function call in the
    // asset
    if (useEyePosition) {
        const glm::dvec3 anchorPos = anchor->worldPosition();
        const glm::dvec3 cameraDir = ghoul::viewDirection(camera->rotationQuaternion());
        const double anchorToPosDistance = glm::distance(
            anchorPos + globe->boundingSphere(),
            cameraPosition
        );
        target = cameraPosition + anchorToPosDistance * cameraDir;
    }
    else {
        target = glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1.0));
    }

    const SurfacePositionHandle posHandle = globe->calculateSurfacePositionHandle(
        target
    );

    const Geodetic2 geo2 = globe->ellipsoid().cartesianToGeodetic2(
        posHandle.centerToReferenceSurface
    );
    const double altitude = glm::length(
        target - posHandle.centerToReferenceSurface
    );

    return { glm::degrees(geo2.lat), glm::degrees(geo2.lon), altitude };
}

/**
 * Loads and parses the WMS capabilities xml file from a remote server. The first argument
 * is the name of the capabilities that can be used to later refer to the set of
 * capabilities. The second argument is the globe for which this server is applicable. The
 * third argument is the URL at which the capabilities file can be found.
 */
[[codegen::luawrap]] void loadWMSCapabilities(std::string name, std::string globe,
                                              std::string url)
{
    using namespace openspace;
    using namespace globebrowsing;
    global::moduleEngine->module<GlobeBrowsingModule>()->loadWMSCapabilities(
        std::move(name),
        std::move(globe),
        std::move(url)
    );
}

/**
 * Removes the WMS server identified by the first argument from the list of available
 * servers. The parameter corrsponds to the first argument in the loadWMSCapabilities call
 * that was used to load the WMS server.
 */
[[codegen::luawrap]] void removeWMSServer(std::string name) {
    using namespace openspace;
    using namespace globebrowsing;
    global::moduleEngine->module<GlobeBrowsingModule>()->removeWMSServer(name);
}

/**
 * Returns an array of tables that describe the available layers that are supported by the
 * WMS server identified by the provided name. The 'URL' component of the returned table
 * can be used in the 'FilePath' argument for a call to the 'addLayer' function to add the
 * value to a globe.
 */
[[codegen::luawrap]] std::vector<ghoul::Dictionary> capabilitiesWMS(std::string name) {
    using namespace openspace;
    using namespace globebrowsing;

    GlobeBrowsingModule::Capabilities cap =
        global::moduleEngine->module<GlobeBrowsingModule>()->capabilities(name);

    std::vector<ghoul::Dictionary> res;
    res.reserve(cap.size());
    for (size_t i = 0; i < cap.size(); ++i) {
        ghoul::Dictionary c;
        c.setValue("Name", cap[i].name);
        c.setValue("URL", cap[i].url);
        res.push_back(c);
    }
    return res;
}

#include "globebrowsingmodule_lua_codegen.cpp"

} // namespace
