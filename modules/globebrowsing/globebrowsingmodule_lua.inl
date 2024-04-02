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
    layers::Group::ID groupID = ghoul::from_string<layers::Group::ID>(layerGroupName);
    if (groupID == layers::Group::ID::Unknown) {
        throw ghoul::lua::LuaError("Unknown layer group: " + layerGroupName);
    }

    // Add the name of the enclosing globe to layer dict, it is used to identify a cache
    layer.setValue("GlobeName", globeName);

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
    layers::Group::ID groupID = ghoul::from_string<layers::Group::ID>(layerGroupName);
    if (groupID == layers::Group::ID::Unknown) {
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
[[codegen::luawrap]] std::vector<std::string> layers(std::string globeIdentifier,
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

    layers::Group::ID group = ghoul::from_string<layers::Group::ID>(layer);
    if (group == layers::Group::ID::Unknown) {
        throw ghoul::lua::LuaError("Unknown layer groupd: " + layer);
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
 * Returns the list of layers for the scene graph node specified in the first parameter.
 * The second parameter specifies which layer type should be queried. Deprecated in favor
 * of 'layers'.
 */
[[codegen::luawrap("getLayers")]] std::vector<std::string> layersDeprecated(
                                                              std::string globeIdentifier,
                                                                        std::string layer)
{
    LWARNINGC(
        "Deprecation",
        "'getLayers' function is deprecated and should be replaced with 'layers'"
    );

    return layers(std::move(globeIdentifier), std::move(layer));
}

/**
 * Rearranges the order of a single layer on a globe. The first parameter is the
 * identifier of the globe, the second parameter specifies the name of the layer group,
 * the third parameter is the original position of the layer that should be moved and the
 * last parameter is the new position in the list. The third and fourth parameters can
 * also acccept names, in which case these refer to identifiers of the layer to be moved.
 * If the last parameter is a name, the source layer is moved below that destination
 * layer. The first position in the list has index 0, and the last position is given by
 * the number of layers minus one.
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
            throw ghoul::lua::LuaError("No anchor node is set");
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
            throw ghoul::lua::LuaError("No anchor node is set");
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
 * Returns a position in the local Cartesian coordinate system of the globe identified by
 * the first argument, that corresponds to the given geographic coordinates: latitude,
 * longitude and altitude (in degrees and meters). In the local coordinate system, the
 * position (0,0,0) corresponds to the globe's center.
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
 * Returns a position in the local Cartesian coordinate system of the globe identified by
 * the first argument, that corresponds to the given geographic coordinates: latitude,
 * longitude and altitude (in degrees and meters). In the local coordinate system, the
 * position (0,0,0) corresponds to the globe's center. Deprecated in favor of
 * 'localPositionFromGeo'.
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
 * (degrees and meters). If the optional bool paramater is specified, the camera
 * eye postion will be used instead
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
 * (degrees and meters). If the optional bool paramater is specified, the camera
 * eye postion will be used instead. Deprecated in favor of 'geoPositionForCamera'.
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
    for (size_t i = 0; i < cap.size(); i++) {
        ghoul::Dictionary c;
        c.setValue("Name", cap[i].name);
        c.setValue("URL", cap[i].url);
        res.push_back(c);
    }
    return res;
}

/**
 * Add a GeoJson layer specified by the given table to the globe specified by the
 * 'globeName' argument
 */
[[codegen::luawrap]] void addGeoJson(std::string globeName, ghoul::Dictionary table)
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

    // Get the dictionary defining the layer
    globe->geoJsonManager().addGeoJsonLayer(table);
}

/**
 * Remove the GeoJson layer specified by the given table or string identifier from the
 * globe specified by the 'globeName' argument
 */
[[codegen::luawrap]] void deleteGeoJson(std::string globeName,
                          std::variant<std::string, ghoul::Dictionary> tableOrIdentifier)
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
 * added feature to be visible on the globe, if using a height map
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
    d.setValue("File", path.string());
    if (name.has_value()) {
        d.setValue("Name", *name);
    }

    // Get the dictionary defining the layer
    globe->geoJsonManager().addGeoJsonLayer(d);
}

#include "globebrowsingmodule_lua_codegen.cpp"

} // namespace
