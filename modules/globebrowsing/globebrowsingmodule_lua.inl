/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <ghoul/misc/stringhelper.h>

namespace {

/**
 * Adds a layer to the specified globe. The second argument is the layer group which can
 * be any of the supported layer groups. The third argument is the dictionary defining the
 * layer.
 *
 * \param globeIdentifier The identifier of the scene graph node of which to add the
 *                        layer. The renderable of the scene graph node must be a
 *                        [RenderableGlobe](#globebrowsing_renderableglobe)
 * \param layerGroup The identifier of the layer group in which to add the layer
 * \param layer A dictionary defining the layer. See [this page](#globebrowsing_layer)
 *              for details on what fields and settings the dictionary may contain
 */
[[codegen::luawrap]] void addLayer(std::string globeIdentifier, std::string layerGroup,
                                   ghoul::Dictionary layer)
{
    using namespace openspace;
    using namespace globebrowsing;

    // Get the node and make sure it exists
    SceneGraphNode* n = global::renderEngine->scene()->sceneGraphNode(globeIdentifier);
    if (!n) {
        throw ghoul::lua::LuaError("Unknown globe name: " + globeIdentifier);
    }

    // Get the renderable globe
    RenderableGlobe* globe = dynamic_cast<RenderableGlobe*>(n->renderable());
    if (!globe) {
        throw ghoul::lua::LuaError("Renderable is not a globe: " + globeIdentifier);
    }

    // Get the layer group
    layers::Group::ID groupID = ghoul::from_string<layers::Group::ID>(layerGroup);
    if (groupID == layers::Group::ID::Unknown) {
        throw ghoul::lua::LuaError("Unknown layer group: " + layerGroup);
    }

    // Add the name of the enclosing globe to layer dict, it is used to identify a cache
    layer.setValue("GlobeName", globeIdentifier);

    // Get the dictionary defining the layer
    Layer* l = globe->layerManager().addLayer(groupID, layer);
    if (l) {
        l->initialize();
    }
}

/**
 * Removes a layer from the specified globe.

 * \param globeIdentifier The identifier of the scene graph node of which to remove the
 *                        layer. The renderable of the scene graph node must be a
 *                        [RenderableGlobe](#globebrowsing_renderableglobe)
 * \param layerGroup The identifier of the layer group from which to remove the layer
 * \param layerOrName Either the identifier for the layer or a dictionary with the
 *                    `Identifier` key that is used instead
 */
[[codegen::luawrap]] void deleteLayer(std::string globeIdentifier, std::string layerGroup,
                                 std::variant<std::string, ghoul::Dictionary> layerOrName)
{
    using namespace openspace;
    using namespace globebrowsing;

    // Get the node and make sure it exists
    SceneGraphNode* n = global::renderEngine->scene()->sceneGraphNode(globeIdentifier);
    if (!n) {
        throw ghoul::lua::LuaError("Unknown globe name: " + globeIdentifier);
    }

    // Get the renderable globe
    RenderableGlobe* globe = dynamic_cast<RenderableGlobe*>(n->renderable());
    if (!globe) {
        throw ghoul::lua::LuaError("Renderable is not a globe: " + globeIdentifier);
    }

    // Get the layer group
    layers::Group::ID groupID = ghoul::from_string<layers::Group::ID>(layerGroup);
    if (groupID == layers::Group::ID::Unknown) {
        throw ghoul::lua::LuaError("Unknown layer group: " + layerGroup);
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
 * Returns the list of layers for the specified globe, for a specific layer group.
 *
 * \param globeIdentifier The identifier of the scene graph node for the globe
 * \param layerGroup The identifier of the layer group for which to list the layers
 */
[[codegen::luawrap]] std::vector<std::string> layers(std::string globeIdentifier,
                                                     std::string layerGroup)
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

    layers::Group::ID group = ghoul::from_string<layers::Group::ID>(layerGroup);
    if (group == layers::Group::ID::Unknown) {
        throw ghoul::lua::LuaError("Unknown layer group: " + layerGroup);
    }

    const LayerGroup& lg = globe->layerManager().layerGroup(group);
    std::vector<Layer*> layers = lg.layers();

    std::vector<std::string> res;
    res.reserve(layers.size());
    for (Layer* l : layers) {
        res.push_back(l->identifier());
    }
    return res;
}

 /**
  * Returns the list of layers for the specified globe for a specific layer group.
  *
  * Deprecated in favor of `layers`.
  *
  * \param globeIdentifier The identifier of the scene graph node for the globe
  * \param layerGroup The identifier of the layer group for which to list the layers
  */
[[codegen::luawrap("getLayers")]] std::vector<std::string> layersDeprecated(
                                                              std::string globeIdentifier,
                                                                   std::string layerGroup)
{
    LWARNINGC(
        "Deprecation",
        "'getLayers' function is deprecated and should be replaced with 'layers'"
    );

    return layers(std::move(globeIdentifier), std::move(layerGroup));
}

/**
 * Rearranges the order of a single layer on a globe. The first position in the list
 * has index 0, and the last position is given by the number of layers minus one.
 *
 * The `source` and `destination` parameters can also be the identifiers of the layers to
 * be moved. If `destination` is a name, the source layer is moved below that destination
 * layer.
 *
 * \param globeIdentifier The identifier of the globe
 * \param layerGroup The identifier of the layer group
 * \param source The original position of the layer that should be moved, either as an
 *               index in the list or the identifier of the layer to be moved
 * \param destination The new position in the list, either as an index in the list or as
 *                    the identifier of the layer after which to place the moved layer
 */
[[codegen::luawrap]] void moveLayer(std::string globeIdentifier, std::string layerGroup,
                                    std::variant<int, std::string> source,
                                    std::variant<int, std::string> destination)
{
    using namespace openspace;
    using namespace globebrowsing;

    if (source == destination) {
        return;
    }

    SceneGraphNode* n = sceneGraphNode(globeIdentifier);
    if (!n) {
        throw ghoul::lua::LuaError(std::format("Unknown globe '{}'", globeIdentifier));
    }

    RenderableGlobe* globe = dynamic_cast<RenderableGlobe*>(n->renderable());
    if (!globe) {
        throw ghoul::lua::LuaError("Identifier must be a RenderableGlobe");
    }

    layers::Group::ID group = ghoul::from_string<layers::Group::ID>(layerGroup);
    if (group == layers::Group::ID::Unknown) {
        throw ghoul::lua::LuaError(std::format("Unknown layer group '{}'", layerGroup));
    }

    LayerGroup& lg = globe->layerManager().layerGroup(group);
    if (std::holds_alternative<int>(source) && std::holds_alternative<int>(destination)) {
        // Short circut here, no need to get the layers
        lg.moveLayer(std::get<int>(source), std::get<int>(destination));
        return;
    }

    std::vector<Layer*> layers = lg.layers();

    int sourceIdx = 0;
    if (std::holds_alternative<int>(source)) {
        sourceIdx = std::get<int>(source);
    }
    else {
        auto it = std::find_if(
            layers.cbegin(), layers.cend(),
            [s = std::get<std::string>(source)](Layer* l) {
                return l->identifier() == s;
            }
        );
        if (it == layers.cend()) {
            throw ghoul::lua::LuaError(std::format(
                "Could not find source layer '{}'", std::get<std::string>(source)
            ));
        }

        sourceIdx = static_cast<int>(std::distance(layers.cbegin(), it));
    }

    int destinationIdx = 0;
    if (std::holds_alternative<int>(destination)) {
        destinationIdx = std::get<int>(destination);
    }
    else {
        auto it = std::find_if(
            layers.cbegin(), layers.cend(),
            [d = std::get<std::string>(destination)](Layer* l) {
                return l->identifier() == d;
            }
        );
        if (it == layers.cend()) {
            throw ghoul::lua::LuaError(std::format(
                "Could not find destination layer '{}'", std::get<std::string>(source)
            ));
        }

        // +1 since we want to move the layer _below_ the specified layer
        destinationIdx = static_cast<int>(std::distance(layers.cbegin(), it));
    }

    lg.moveLayer(sourceIdx, destinationIdx);
}

/**
 * Go to the chunk on a globe with given index x, y, level.
 *
 * \param globeIdentifier The identifier of the scene graph node for the globe
 * \param x The x value of the tile index
 * \param y The y value of the tile index
 * \param level The level of the tile index
 */
[[codegen::luawrap]] void goToChunk(std::string globeIdentifier, int x, int y, int level) {
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

    global::moduleEngine->module<GlobeBrowsingModule>()->goToChunk(*globe, x, y, level);
}

/**
 * Immediately move the camera to a geographic coordinate on a globe by first fading the
 * rendering to black, jump to the specified coordinate, and then fade in.
 *
 * This is done by triggering another script that handles the logic.
 *
 * \param globe The identifier of a scene graph node that has a RenderableGlobe attached.
 *              If an empty string is provided, the current anchor node is used
 * \param latitude The latitude of the target coordinate, in degrees
 * \param longitude The longitude of the target coordinate, in degrees
 * \param altitude An optional altitude, given in meters over the reference surface of
 *                 the globe. If no altitude is provided, the altitude will be kept as
 *                 the current distance to the reference surface of the specified globe.
 * \param fadeDuration An optional duration for the fading. If not included, the
 *                     property in Navigation Handler will be used
 */
[[codegen::luawrap]] void jumpToGeo(std::string globe, double latitude, double longitude,
                                    std::optional<double> altitude,
                                    std::optional<double> fadeDuration)
{
    using namespace openspace;
    using namespace globebrowsing;

    std::string script;

    if (altitude.has_value()) {
        script = std::format(
            "openspace.globebrowsing.flyToGeo('{}', {}, {}, {}, 0)",
            globe, latitude, longitude, *altitude
        );
    }
    else {
        script = std::format(
            "openspace.globebrowsing.flyToGeo2('{}', {}, {}, true, 0)",
            globe, latitude, longitude
        );
    }

    if (fadeDuration.has_value()) {
        global::navigationHandler->triggerFadeToTransition(
            std::move(script),
            static_cast<float>(*fadeDuration)
        );
    }
    else {
        global::navigationHandler->triggerFadeToTransition(script);
    }
}

/**
 * Immediately move the camera to a geographic coordinate on a globe.
 *
 * \param globe The identifier of a scene graph node that has a RenderableGlobe attached.
 *              If an empty string is provided, the current anchor node is used
 * \param latitude The latitude of the target coordinate, in degrees
 * \param longitude The longitude of the target coordinate, in degrees
 * \param altitude An optional altitude, given in meters over the reference surface of
 *                 the globe. If no altitude is provided, the altitude will be kept as
 *                 the current distance to the reference surface of the specified globe.
 */
[[codegen::luawrap("goToGeo")]] void goToGeoDeprecated(std::string globe, double latitude,
                                                       double longitude,
                                                       std::optional<double> altitude)
{
    LWARNINGC(
        "Deprecation",
        "'goToGeo' function is deprecated and should be replaced with 'jumpToGeo'"
    );

    return jumpToGeo(
        std::move(globe),
        latitude,
        longitude,
        altitude,
        0
    );
}

void flyToGeoInternal(std::string globe, double latitude,
                      double longitude, std::optional<double> altitude,
                      std::optional<double> duration,
                      std::optional<bool> shouldUseUpVector)
{
    using namespace openspace;
    using namespace globebrowsing;

    const SceneGraphNode* n;
    if (!globe.empty()) {
        n = sceneGraphNode(globe);
        if (!n) {
            throw ghoul::lua::LuaError("Unknown globe name: " + globe);
        }
    }
    else {
        n = global::navigationHandler->orbitalNavigator().anchorNode();
        if (!n) {
            throw ghoul::lua::LuaError("No anchor node is set");
        }
    }

    const RenderableGlobe* gl = dynamic_cast<const RenderableGlobe*>(n->renderable());
    if (!gl) {
        throw ghoul::lua::LuaError("The targetted node is not a RenderableGlobe");
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
        if (*duration < 0) {
            throw ghoul::lua::LuaError("Duration must be a positive value");
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
 * Fly the camera to a geographic coordinate (latitude and longitude) on a globe, using
 * the path navigation system.
 *
 * The distance to fly to can either be set to be the current distance of the camera to
 * the target object, or the default distance from the path navigation system.
 *
 * \param globe The identifier of a scene graph node that has a RenderableGlobe attached.
 *              If an empty string is provided, the current anchor node is used
 * \param latitude The latitude of the target coordinate, in degrees
 * \param longitude The longitude of the target coordinate, in degrees
 * \param useCurrentDistance If true, use the current distance of the camera to the
 *                           target globe when going to the specified position. If false,
 *                           or not specified, set the distance based on the bounding
 *                           sphere and the distance factor setting in Path Navigator
 * \param duration An optional duration for the motion to take, in seconds. For example,
 *                 a value of 5 means "fly to this position over a duration of 5 seconds"
 * \param shouldUseUpVector If true, try to use the up-direction when computing the
 *                          target position for the camera. For globes, this means that
 *                          North should be up, in relation to the camera's view
 *                          direction. Note that for this to take effect, rolling motions
 *                          must be enabled in the Path Navigator settings.
 */
[[codegen::luawrap]] void flyToGeo2(std::string globe, double latitude, double longitude,
                                    std::optional<bool> useCurrentDistance,
                                    std::optional<double> duration,
                                    std::optional<bool> shouldUseUpVector)
{
    using namespace openspace;

    std::optional<double> altitude;
    if (useCurrentDistance.has_value() && *useCurrentDistance) {
        altitude = std::nullopt;
    }
    else {
        altitude = global::navigationHandler->pathNavigator().defaultArrivalHeight(globe);
    }

    flyToGeoInternal(
        globe,
        latitude,
        longitude,
        std::nullopt,
        duration,
        shouldUseUpVector
    );
}

 /**
  * Fly the camera to a geographic coordinate (latitude, longitude and altitude) on a globe,
  * using the path navigation system.
  *
  * \param globe The identifier of a scene graph node that has a RenderableGlobe attached.
  *              If an empty string is provided, the current anchor node is used
  * \param latitude The latitude of the target coordinate, in degrees
  * \param longitude The longitude of the target coordinate, in degrees
  * \param altitude The altitude of the target coordinate, in meters
  * \param duration An optional duration for the motion to take, in seconds. For example,
  *                 a value of 5 means "fly to this position over a duration of 5 seconds"
  * \param shouldUseUpVector If true, try to use the up-direction when computing the
  *                          target position for the camera. For globes, this means that
  *                          North should be up, in relation to the camera's view
  *                          direction. Note that for this to take effect, rolling motions
  *                          must be enabled in the Path Navigator settings.
  */
[[codegen::luawrap]] void flyToGeo(std::string globe, double latitude,
                                   double longitude, double altitude,
                                   std::optional<double> duration,
                                   std::optional<bool> shouldUseUpVector)
{
    flyToGeoInternal(globe, latitude, longitude, altitude, duration, shouldUseUpVector);
}

/**
 * Returns the position in the local Cartesian coordinate system of the specified globe
 * that corresponds to the given geographic coordinates. In the local coordinate system,
 * the position (0,0,0) corresponds to the globe's center.
 *
 * \param globeIdentifier The identifier of the scene graph node for the globe
 * \param latitude The latitude of the geograpic position, in degrees
 * \param longitude The longitude of the geographic position, in degrees
 * \param altitude The altitude, in meters
 */
[[codegen::luawrap]]
std::tuple<double, double, double>
localPositionFromGeo(std::string globeIdentifier, double latitude, double longitude,
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
* Returns the position in the local Cartesian coordinate system of the specified globe
* that corresponds to the given geographic coordinates. In the local coordinate system,
* the position (0,0,0) corresponds to the globe's center.
*
* Deprecated in favor of `localPositionFromGeo`.
*
* \param globeIdentifier The identifier of the scene graph node for the globe
* \param latitude The latitude of the geograpic position, in degrees
* \param longitude The longitude of the geographic position, in degrees
* \param altitude The altitude, in meters
*/
[[codegen::luawrap("getLocalPositionFromGeo")]]
std::tuple<double, double, double>
localPositionFromGeoDeprecated(std::string globeIdentifier, double latitude,
                               double longitude, double altitude)
{
    LWARNINGC(
        "Deprecation",
        "'getLocalPositionFromGeo' function is deprecated and should be replaced with "
        "'localPositionFromGeo'"
    );

    return localPositionFromGeo(
        std::move(globeIdentifier),
        latitude,
        longitude,
        altitude
    );
}

/**
 * Get geographic coordinates of the camera position in latitude, longitude, and altitude
 * (degrees and meters).
 *
 * \param useEyePosition If true, use the view direction of the camera instead of the
 *                       camera position
 */
[[codegen::luawrap]] std::tuple<double, double, double> geoPositionForCamera(
                                                              bool useEyePosition = false)
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
  * Get geographic coordinates of the camera position in latitude, longitude, and altitude
  * (degrees and meters).
  *
  * Deprecated in favor of `geoPositionForCamera`.
  *
  * \param useEyePosition If true, use the view direction of the camera instead of the
  *                       camera position
  */
[[codegen::luawrap("getGeoPositionForCamera")]]
std::tuple<double, double, double>
geoPositionForCameraDeprecated(bool useEyePosition = false)
{
    LWARNINGC(
        "Deprecation",
        "'getGeoPositionForCamera' function is deprecated and should be replaced with "
        "'geoPositionForCamera'"
    );

    return geoPositionForCamera(useEyePosition);
}

/**
 * Loads and parses the WMS capabilities XML file from a remote server.
 *
 * \param name The name of the capabilities that can be used to later refer to the set of
 *             capabilities
 * \param globe The identifier of the globe for which this server is applicable
 * \param url The URL at which the capabilities file can be found
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
 * Removes the specified WMS server from the list of available servers. The name parameter
 * corresponds to the first argument in the `loadWMSCapabilities` call that was used to
 * load the WMS server.
 *
 * \param name The name of the WMS server to remove
 */
[[codegen::luawrap]] void removeWMSServer(std::string name) {
    using namespace openspace;
    using namespace globebrowsing;
    global::moduleEngine->module<GlobeBrowsingModule>()->removeWMSServer(name);
}

/**
 * Returns an array of tables that describe the available layers that are supported by the
 * WMS server identified by the provided name. The `URL` component of the returned table
 * can be used in the `FilePath` argument for a call to the `addLayer` function to add the
 * value to a globe.
 *
 * \param name The name of the WMS server for which to get the information
 */
[[codegen::luawrap]] std::vector<ghoul::Dictionary> capabilitiesWMS(std::string name) {
    using namespace openspace;
    using namespace globebrowsing;

    GlobeBrowsingModule::Capabilities cap =
        global::moduleEngine->module<GlobeBrowsingModule>()->capabilities(name);

    std::vector<ghoul::Dictionary> res;
    res.reserve(cap.size());
    for (size_t i = 0; i < cap.size(); i++) {
        ghoul::Dictionary c;
        c.setValue("Name", cap[i].name);
        c.setValue("URL", cap[i].url);
        res.push_back(c);
    }
    return res;
}

/**
 * Add a GeoJson layer specified by the given table to the specified globe.
 *
 * \param globeIdentifier The identifier of the scene graph node for the globe
 * \param table A table with information about the GeoJson layer. See
 *              [this page](#globebrowsing_geojsoncomponent) for details on what fields
 *              and settings the table may contain
 */
[[codegen::luawrap]] void addGeoJson(std::string globeIdentifier, ghoul::Dictionary table)
{
    using namespace openspace;
    using namespace globebrowsing;

    // Get the node and make sure it exists
    SceneGraphNode* n = global::renderEngine->scene()->sceneGraphNode(globeIdentifier);
    if (!n) {
        throw ghoul::lua::LuaError("Unknown globe name: " + globeIdentifier);
    }

    // Get the renderable globe
    RenderableGlobe* globe = dynamic_cast<RenderableGlobe*>(n->renderable());
    if (!globe) {
        throw ghoul::lua::LuaError("Renderable is not a globe: " + globeIdentifier);
    }

    // Get the dictionary defining the layer
    globe->geoJsonManager().addGeoJsonLayer(table);
}

/**
 * Remove the GeoJson layer specified by the given table or string identifier from the
 * specified globe.
 *
 * \param globeIdentifier The identifier of the scene graph node for the globe
 * \param tableOrIdentifier Either an identifier for the GeoJson layer to be removed, or
 *                          a table that includes the identifier
 */
[[codegen::luawrap]] void deleteGeoJson(std::string globeIdentifier,
                          std::variant<std::string, ghoul::Dictionary> tableOrIdentifier)
{
    using namespace openspace;
    using namespace globebrowsing;

    // Get the node and make sure it exists
    SceneGraphNode* n = global::renderEngine->scene()->sceneGraphNode(globeIdentifier);
    if (!n) {
        throw ghoul::lua::LuaError("Unknown globe name: " + globeIdentifier);
    }

    // Get the renderable globe
    RenderableGlobe* globe = dynamic_cast<RenderableGlobe*>(n->renderable());
    if (!globe) {
        throw ghoul::lua::LuaError("Renderable is not a globe: " + globeIdentifier);
    }

    std::string identifier;
    if (std::holds_alternative<std::string>(tableOrIdentifier)) {
        identifier = std::get<std::string>(tableOrIdentifier);
    }
    else {
        ghoul::Dictionary d = std::get<ghoul::Dictionary>(tableOrIdentifier);
        if (!d.hasValue<std::string>("Identifier")) {
            throw ghoul::lua::LuaError(
                "Table passed to deleteLayer does not contain an Identifier"
            );
        }
        identifier = d.value<std::string>("Identifier");
    }

    globe->geoJsonManager().deleteLayer(identifier);
}

/**
 * Add a GeoJson layer from the given file name and add it to the current anchor node,
 * if it is a globe. Note that you might have to increase the height offset for the
 * added feature to be visible on the globe, if using a height map.
 *
 * \param filename The path to the GeoJSON file
 * \param name An optional name that the loaded feature will get in the user interface
 */
[[codegen::luawrap]] void addGeoJsonFromFile(std::string filename,
                                             std::optional<std::string> name)
{
    using namespace openspace;
    using namespace globebrowsing;

    std::filesystem::path path = absPath(filename);
    if (!std::filesystem::is_regular_file(path)) {
        throw ghoul::lua::LuaError(std::format(
            "Could not find the provided file '{}'", filename
        ));
    }

    std::string extension = path.extension().string();
    extension = ghoul::toLowerCase(extension);

    if (extension != ".geojson" && extension != ".json") {
        throw ghoul::lua::LuaError(std::format(
            "Unexpected file type '{}'. Expected '.geojson' or '.json' file", filename
        ));
    }

    SceneGraphNode* n = global::renderEngine->scene()->sceneGraphNode(
        global::navigationHandler->anchorNode()->identifier()
    );
    if (!n) {
        throw ghoul::lua::LuaError("Invalid anchor node");
    }

    RenderableGlobe* globe = dynamic_cast<RenderableGlobe*>(n->renderable());
    if (!globe) {
        throw ghoul::lua::LuaError(
            "Current anchor is not a globe (Expected 'RenderableGlobe')"
        );
    }

    // Make a minimal dictionary to represent the geojson component
    ghoul::Dictionary d;

    std::string identifier = makeIdentifier(name.value_or(path.stem().string()));
    d.setValue("Identifier", identifier);
    d.setValue("File", path);
    if (name.has_value()) {
        d.setValue("Name", *name);
    }

    // Get the dictionary defining the layer
    globe->geoJsonManager().addGeoJsonLayer(d);
}

#include "globebrowsingmodule_lua_codegen.cpp"

} // namespace
