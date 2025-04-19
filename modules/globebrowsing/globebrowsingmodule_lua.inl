/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
#include <ghoul/lua/lua_helper.h>
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
[[codegen::luawrap]] void goToChunk(std::string globeIdentifier, int x, int y, int level)
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

    global::moduleEngine->module<GlobeBrowsingModule>()->goToChunk(*n, x, y, level);
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
