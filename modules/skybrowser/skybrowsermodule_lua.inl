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

#include <modules/skybrowser/skybrowsermodule.h>

#include <modules/skybrowser/include/utility.h>
#include <modules/skybrowser/include/targetbrowserpair.h>
#include <modules/skybrowser/include/wwtdatahandler.h>
#include <openspace/events/eventengine.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <scn/scan.h>

namespace {
constexpr std::string_view _loggerCat = "SkyBrowserModule";

bool browserBelongsToCurrentNode(std::string identifier) {
    size_t found = identifier.find('_');
    std::string errorMessage = "The Sky Browser encountered a problem when it tried to "
        "initialize the browser";
    if (found == std::string::npos) {
        throw ghoul::RuntimeError(errorMessage);
    }
    else {
        std::string res = identifier.substr(found + 1, identifier.size());
        if (res.empty()) {
            throw ghoul::RuntimeError(errorMessage);
        }
        // Convert the last char to an int
        int nodeId = std::stoi(res);
        return nodeId == openspace::global::windowDelegate->currentNode();
    }
}

std::string prunedIdentifier(std::string identifier) {
    // Removes the node number at the end of the identifier
    std::string res = identifier.substr(0, identifier.find('_'));
    return res;
}

/**
* Reloads the sky browser display copy for the node index that is sent in.
* .If no ID is sent in, it will reload all display copies on that node.
*/
[[codegen::luawrap]] void reloadDisplayCopyOnNode(int nodeIndex, std::string id = "all") {
    using namespace openspace;

    if (global::windowDelegate->currentNode() != nodeIndex)
        return;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    if (id != "all") {
        TargetBrowserPair* pair = module->pair(id);
        if (pair) {
            pair->browser()->setIsInitialized(false);
            pair->browser()->setImageCollectionIsLoaded(false);
            pair->browser()->reload();
        }
    }
    else {
        const std::vector<std::unique_ptr<TargetBrowserPair>>& pairs = module->pairs();
        for (const std::unique_ptr<TargetBrowserPair>& pair : pairs) {
            pair->browser()->setIsInitialized(false);
            pair->browser()->setImageCollectionIsLoaded(false);
            pair->browser()->reload();
        }
    }
}


/**
 * Takes an index to an image and selects that image in the currently
 * selected sky browser.
 */
[[codegen::luawrap]] void selectImage(std::string imageUrl) {
    using namespace openspace;

    // Load image
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    if (module->isCameraInSolarSystem()) {
        TargetBrowserPair* selected = module->pair(module->selectedBrowserId());
        if (selected) {
            std::optional<const ImageData> found = module->wwtDataHandler().image(
                imageUrl
            );
            if (!found.has_value()) {
                LINFO(std::format(
                    "No image with identifier '{}' was found in the collection.", imageUrl
                ));
                return;
            }
            // Load image into browser
            const ImageData& image = found.value();
            std::string str = image.name;
            // Check if character is ASCII - if it isn't, remove
            str.erase(
                std::remove_if(
                    str.begin(), str.end(),
                    [](char c) {
                        return c < 0;
                    }
                ),
                str.end()
            );
            LINFO("Loading image " + str);
            selected->selectImage(image);

            bool isInView = skybrowser::isCoordinateInView(image.equatorialCartesian);
            // If the coordinate is not in view, rotate camera
            if (image.hasCelestialCoords && !isInView) {
                glm::dvec3 dir = skybrowser::equatorialToGalactic(
                    image.equatorialCartesian * skybrowser::CelestialSphereRadius
                );
                module->startRotatingCamera(dir);
            }

            if (selected->pointSpaceCraft()) {
                global::eventEngine->publishEvent<events::EventPointSpacecraft>(
                    image.equatorialSpherical.x,
                    image.equatorialSpherical.y,
                    module->spaceCraftAnimationTime()
                );
            }
        }
    }
}

/**
 * Takes an identifier to a screen space renderable and adds it to the module.
 */
[[codegen::luawrap]] void setHoverCircle(std::string identifier) {
    using namespace openspace;

    SceneGraphNode* circle = global::renderEngine->scene()->sceneGraphNode(identifier);
    if (!circle) {
        throw ghoul::lua::LuaError(std::format(
            "Could not find node to set as hover circle: {}", identifier
        ));
    }

    global::moduleEngine->module<SkyBrowserModule>()->setHoverCircle(circle);
}

/**
 * Moves the hover circle to the coordinate specified by the image index.
 */
[[codegen::luawrap]] void moveCircleToHoverImage(std::string imageUrl) {
    using namespace openspace;

    global::moduleEngine->module<SkyBrowserModule>()->moveHoverCircle(imageUrl, false);
}

/**
 * Disables the hover circle, if there is one added to the sky browser module.
 */
[[codegen::luawrap]] void disableHoverCircle() {
    using namespace openspace;

    global::moduleEngine->module<SkyBrowserModule>()->disableHoverCircle(false);
}

/**
 * Takes an identifier to a sky browser or a sky target, an image index and the order
 * which it should have in the selected image list. The image is then changed to have this
 * order.
 */
[[codegen::luawrap]] void setImageLayerOrder(std::string identifier, std::string imageUrl,
                                             int imageOrder)
{
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(identifier);
    if (pair) {
        pair->setImageOrder(imageUrl, imageOrder);
    }
}

/**
 * Takes an identifier to a sky browser or target and loads the WWT image collection to
 * that browser.
 */
[[codegen::luawrap]] void loadImagesToWWT(std::string identifier) {
    using namespace openspace;

    if (!browserBelongsToCurrentNode(identifier)) {
        return;
    }
    std::string prunedId = prunedIdentifier(identifier);
    // Load images from url
    LINFO("Connection established to WorldWide Telescope application in " + prunedId);
    LINFO("Loading image collections to " + prunedId);

    // Load the collections here because we know that the browser can execute javascript
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(prunedId);
    if (pair) {
        pair->hideChromeInterface();
        pair->browser()->loadImageCollection(module->wwtImageCollectionUrl());
    }
}

/**
 * Starts the setup process of the sky browers. This function calls the Lua function
 * 'sendOutIdsToBrowsers' in all nodes in the cluster.
 */
[[codegen::luawrap]] void startSetup() {
    using namespace openspace;

    // This is called when the sky_browser website is connected to OpenSpace
    // Set all border colors to the border color in the master node
    if (global::windowDelegate->isMaster()) {
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        const std::vector<std::unique_ptr<TargetBrowserPair>>& pairs = module->pairs();
        for (const std::unique_ptr<TargetBrowserPair>& pair : pairs) {
            std::string id = pair->browserId();
            glm::ivec3 color = pair->borderColor();
            std::string script = std::format(
                "openspace.skybrowser.setBorderColor('{}', {}, {}, {})",
                id, color.r, color.g, color.b
            );

            // No sync or send because this is already inside a Lua script, therefor it
            // has already been synced and sent to the connected nodes and peers
            global::scriptEngine->queueScript(
                script,
                scripting::ScriptEngine::ShouldBeSynchronized::No,
                scripting::ScriptEngine::ShouldSendToRemote::No
            );
        }
    }
    // To ensure each node in a cluster calls its own instance of the wwt application
    // Do not send this script to the other nodes. (Note malej 2023-AUG-23: Due to this
    // already being inside a Lua function that have already been synced out)
    global::scriptEngine->queueScript(
        "openspace.skybrowser.sendOutIdsToBrowsers()",
        scripting::ScriptEngine::ShouldBeSynchronized::No,
        scripting::ScriptEngine::ShouldSendToRemote::No
    );
}

/**
 * Sends all sky browsers' identifiers to their respective CEF browser.
 */
[[codegen::luawrap]] void sendOutIdsToBrowsers() {
    using namespace openspace;

    // This is called when the sky_browser website is connected to OpenSpace
    // Send out identifiers to the browsers
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    const std::vector<std::unique_ptr<TargetBrowserPair>>& pairs = module->pairs();
    for (const std::unique_ptr<TargetBrowserPair>& pair : pairs) {
        pair->sendIdToBrowser();
    }
}

/**
 * Takes an identifier to a sky browser and starts the initialization for that browser.
 * That means that the browser starts to try to connect to the AAS WorldWide Telescope
 * application by sending it messages. And that the target matches its appearance to its
 * corresponding browser.
 */
[[codegen::luawrap]] void initializeBrowser(std::string identifier) {
    using namespace openspace;

    // Initialize browser with ID and its corresponding target
    if (!browserBelongsToCurrentNode(identifier)) {
        return;
    }

    std::string prunedId = prunedIdentifier(identifier);
    LINFO("Initializing sky browser " + prunedId);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(prunedId);
    if (pair) {
        pair->initialize();
    }
}

/**
 * Takes the identifier of the sky target and a sky browser and adds them to the sky
 * browser module.
 */
[[codegen::luawrap]] void addPairToSkyBrowserModule(std::string targetId,
                                                    std::string browserId)
{
    using namespace openspace;

    LINFO("Add browser " + browserId + " to sky browser module");
    LINFO("Add target " + targetId + " to sky browser module");

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    module->addTargetBrowserPair(targetId, browserId);
}

/**
 * Returns the AAS WorldWide Telescope image collection url.
 */
[[codegen::luawrap]] ghoul::Dictionary wwtImageCollectionUrl() {
    using namespace openspace;
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    ghoul::Dictionary url;
    url.setValue("url", module->wwtImageCollectionUrl());
    return url;
}

/**
 * Deprecated in favor of 'wwtImageCollectionUrl'
 */
[[codegen::luawrap("getWwtImageCollectionUrl")]]
ghoul::Dictionary wwtImageCollectionUrlDeprecated()
{
    LWARNINGC(
        "Deprecation",
        "'getWwtImageCollectionUrl' function is deprecated and should be replaced with "
        "'wwtImageCollectionUrl'"
    );
    return wwtImageCollectionUrl();
}

/**
 * Returns a list of all the loaded AAS WorldWide Telescope images that have been loaded.
 * Each image has a name, thumbnail url, equatorial spherical coordinates RA and Dec,
 * equatorial Cartesian coordinates, if the image has celestial coordinates, credits text,
 * credits url and the identifier of the image which is a unique number.
 */
[[codegen::luawrap]] ghoul::Dictionary listOfImages() {
    using namespace openspace;

    // Send image list to GUI
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    std::string url = module->wwtImageCollectionUrl();
    // If no data has been loaded yet, download the data from the web
    if (module->nLoadedImages() == 0) {
        std::filesystem::path directory = absPath("${SYNC}/wwtimagedata/");
        module->loadImages(url, directory);
    }

    // Create Lua table to send to the GUI
    ghoul::Dictionary list;
    for (auto const& [id, img] : module->wwtDataHandler().images()) {
        // Push ("Key", value)
        ghoul::Dictionary image;
        image.setValue("name", img.name);
        image.setValue("thumbnail", img.thumbnailUrl);
        image.setValue("url", img.imageUrl);
        image.setValue("ra", img.equatorialSpherical.x);
        image.setValue("dec", img.equatorialSpherical.y);
        image.setValue("fov", static_cast<double>(img.fov));
        image.setValue("cartesianDirection", img.equatorialCartesian);
        image.setValue("hasCelestialCoords", img.hasCelestialCoords);
        image.setValue("credits", img.credits);
        image.setValue("collection", img.collection);
        image.setValue("creditsUrl", img.creditsUrl);
        image.setValue("identifier", img.identifier);

        list.setValue(img.identifier, image);
    }

    return list;
}

/**
 * Deprecated in favor of 'listOfExoplanets'
 */
[[codegen::luawrap("getListOfImages")]] ghoul::Dictionary listOfImagesDeprecated()
{
    LWARNINGC(
        "Deprecation",
        "'getListOfImages' function is deprecated and should be replaced with "
        "'listOfImages'"
    );
    return listOfImages();
}

/**
 * Returns a table of data regarding the current view and the sky browsers and targets.
 * returns a table of data regarding the current targets.
 */
[[codegen::luawrap]] ghoul::Dictionary targetData() {
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    ghoul::Dictionary data;

    // The current viewport data for OpenSpace
    ghoul::Dictionary openSpace;

    // Camera directions
    glm::dvec3 cartesianCam = skybrowser::cameraDirectionEquatorial();
    glm::dvec2 sphericalCam = skybrowser::cartesianToSpherical(cartesianCam);

    // Calculate the smallest FOV of vertical and horizontal
    glm::dvec2 fovs = skybrowser::fovWindow();
    double FOV = std::min(fovs.x, fovs.y);

    // Set window data
    openSpace.setValue("windowHFOV", FOV);
    openSpace.setValue("cartesianDirection", cartesianCam);
    openSpace.setValue("ra", sphericalCam.x);
    openSpace.setValue("dec", sphericalCam.y);
    openSpace.setValue("selectedBrowserId", module->selectedBrowserId());
    openSpace.setValue("selectedTargetId", module->selectedTargetId());
    openSpace.setValue("isFacingCamera", module->isSelectedPairFacingCamera());
    openSpace.setValue("isUsingRadiusAzimuthElevation", module->isSelectedPairUsingRae());
    openSpace.setValue("cameraInSolarSystem", module->isCameraInSolarSystem());
    // Set table for the current ImageData
    data.setValue("OpenSpace", openSpace);

    // Pass data for all the browsers and the corresponding targets
    if (module->isCameraInSolarSystem()) {
        const std::vector<std::unique_ptr<TargetBrowserPair>>& pairs = module->pairs();

        for (const std::unique_ptr<TargetBrowserPair>& pair : pairs) {
            std::string id = pair->browserId();

            glm::dvec2 spherical = pair->targetDirectionEquatorial();
            glm::dvec3 cartesian = skybrowser::sphericalToCartesian(spherical);

            ghoul::Dictionary target;
            // Set ("Key", value)
            target.setValue("id", id);
            target.setValue("name", pair->browserGuiName());
            target.setValue("FOV", static_cast<double>(pair->verticalFov()));
            target.setValue("selectedImages", pair->selectedImages());
            target.setValue("cartesianDirection", cartesian);
            target.setValue("ra", spherical.x);
            target.setValue("dec", spherical.y);
            target.setValue("roll", pair->targetRoll());
            target.setValue("color", pair->borderColor());
            std::vector<std::pair<std::string, glm::dvec3>> copies =
                pair->displayCopies();
            ghoul::Dictionary copiesData;
            for (size_t i = 0; i < copies.size(); i++) {
                copiesData.setValue(copies[i].first, copies[i].second);
            }
            // Set table for the current target
            target.setValue("displayCopies", copiesData);
            data.setValue(id, target);
        }
    }

    return data;
}

/**
 * Deprecated in favor of 'targetData'
 */
[[codegen::luawrap("getTargetData")]] ghoul::Dictionary targetDataDeprecated() {
    LWARNINGC(
        "Deprecation",
        "'getTargetData' function is deprecated and should be replaced with 'targetData'"
    );
    return targetData();
}

/**
 * Takes an identifier to a sky browser or sky target. Rotates the camera so that the
 * target is placed in the center of the view.
 */
[[codegen::luawrap]] void adjustCamera(std::string id) {
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    if (module->isCameraInSolarSystem()) {
        module->lookAtTarget(id);
    }
}

/**
 * Takes an identifier to a sky browser or sky target, an index to an image and a value
 * for the opacity.
 */
[[codegen::luawrap]] void setOpacityOfImageLayer(std::string identifier,
                                                 std::string imageUrl, float opacity)
{
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(identifier);
    if (pair) {
        pair->setImageOpacity(imageUrl, opacity);
    }
}

/**
 * Takes an identifier to a sky browser and animates its corresponding target to the
 * center of the current view.
 */
[[codegen::luawrap]] void centerTargetOnScreen(std::string identifier) {
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(identifier);
    if (pair) {
        pair->centerTargetOnScreen();
    }
}

/**
 * Takes an identifier to a sky browser or target. Sets that sky browser currently
 * selected.
 */
[[codegen::luawrap]] void setSelectedBrowser(std::string identifier) {
    using namespace openspace;

    global::moduleEngine->module<SkyBrowserModule>()->setSelectedBrowser(identifier);
}

/**
 * Creates a sky browser and a target.
 */
[[codegen::luawrap]] void createTargetBrowserPair() {
    using namespace openspace;

    if (!global::windowDelegate->isMaster()) {
        return;
    }

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    int uniqueIdentifier = module->uniqueIdentifierCounter();
    std::string nameBrowser = "Sky Browser " + std::to_string(uniqueIdentifier);
    std::string nameTarget = "Sky Target " + std::to_string(uniqueIdentifier);
    std::string idBrowser = "SkyBrowser" + std::to_string(uniqueIdentifier);
    std::string idTarget = "SkyTarget" + std::to_string(uniqueIdentifier);
    // Determine starting point on screen for the target
    glm::vec3 positionBrowser = glm::vec3(0.f, 0.f, -2.1f);
    glm::vec3 positionTarget = glm::vec3(0.9f, 0.4f, -2.1f);
    glm::dvec3 galacticTarget = skybrowser::localCameraToGalactic(positionTarget);
    if (glm::any(glm::isnan(galacticTarget))) {
        galacticTarget = glm::dvec3(0.0, 0.0, skybrowser::CelestialSphereRadius);
    }
    std::string guiPath = "/Sky Browser";
    std::string url = "http://wwt.openspaceproject.com/1/openspace/";
    double fov = 70.0;
    double size = skybrowser::sizeFromFov(fov, galacticTarget);

    const std::string browser = "{"
        "Identifier = '" + idBrowser + "',"
        "Type = 'ScreenSpaceSkyBrowser',"
        "Name = '" + nameBrowser + "',"
        "Url = '" + url + "',"
        "FaceCamera = false,"
        "CartesianPosition = " + ghoul::to_string(positionBrowser) +
     "}";

    const std::string target = "{"
        "Identifier = '" + idTarget + "',"
        "Type = 'SkyTarget',"
        "Name = '" + nameTarget + "',"
        "Transform = {"
            "Translation = {"
                "Type = 'StaticTranslation',"
                "Position = {" +
                    std::to_string(galacticTarget.x) + ", " +
                    std::to_string(galacticTarget.y) + ", " +
                    std::to_string(galacticTarget.z) + ", " +
                "},"
            "},"
            "Rotation = {"
                "Type = 'StaticRotation',"
                "Rotation = {0.0, 0.0, 0.0}"
            "}"
        "},"
        "Renderable = {"
            "Identifier = 'RenderableSkyTarget',"
            "Type = 'RenderableSkyTarget',"
            "Size = " + std::to_string(size) + ","
            "VerticalFieldOfView = " + std::to_string(fov) + ","
            "Origin = 'Center',"
            "Billboard = true,"
            "Opacity = 0.99"
        "},"
        "GUI = {"
          "Name = '" + nameTarget + "', "
          "Path = '/SkyBrowser', "
        "}"
    "}";

    // No sync or send because this is already inside a Lua script, therefor it has
    // already been synced and sent to the connected nodes and peers
    global::scriptEngine->queueScript(
        "openspace.addScreenSpaceRenderable(" + browser + ");",
        scripting::ScriptEngine::ShouldBeSynchronized::No,
        scripting::ScriptEngine::ShouldSendToRemote::No
    );

    global::scriptEngine->queueScript(
        "openspace.addSceneGraphNode(" + target + ");",
        scripting::ScriptEngine::ShouldBeSynchronized::No,
        scripting::ScriptEngine::ShouldSendToRemote::No
    );

    global::scriptEngine->queueScript(
        "openspace.skybrowser.addPairToSkyBrowserModule('" + idTarget + "','"
        + idBrowser + "');",
        scripting::ScriptEngine::ShouldBeSynchronized::No,
        scripting::ScriptEngine::ShouldSendToRemote::No
    );

    global::scriptEngine->queueScript(
        "openspace.skybrowser.setSelectedBrowser('" + idBrowser + "');",
        scripting::ScriptEngine::ShouldBeSynchronized::No,
        scripting::ScriptEngine::ShouldSendToRemote::No
    );
}

/**
 * Takes in identifier to a sky browser or target and removes them.
 */
[[codegen::luawrap]] void removeTargetBrowserPair(std::string identifier) {
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* found = module->pair(identifier);
    if (found) {
        std::string browser = found->browserId();
        std::string target = found->targetNodeId();

        module->removeTargetBrowserPair(identifier);

        // Remove from engine.
        // No sync or send because this is already inside a Lua script, therefor it has
        // already been synced and sent to the connected nodes and peers
        global::scriptEngine->queueScript(
            "openspace.removeScreenSpaceRenderable('" + browser + "');",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );

        global::scriptEngine->queueScript(
            "openspace.removeSceneGraphNode('" + target + "');",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );
    }
}

/**
 * Takes an identifier to a sky browser or sky target and the [x, y] starting position and
 * the [x, y] translation vector.
 */
[[codegen::luawrap]] void translateScreenSpaceRenderable(std::string identifier,
                                                         float startingPositionX,
                                                         float startingPositionY,
                                                         float translationX,
                                                         float translationY)
{
    using namespace openspace;

    ScreenSpaceRenderable* renderable =
        global::renderEngine->screenSpaceRenderable(identifier);

    if (renderable) {
        glm::vec2 translation = glm::vec2(translationX, translationY);
        glm::vec2 startingPos = glm::vec2(startingPositionX, startingPositionY);
        renderable->translate(translation, startingPos);
    }
}

/**
 * Takes an identifier to a sky browser or target and an index to an image. Removes that
 * image from that sky browser.
 */
[[codegen::luawrap]] void removeSelectedImageInBrowser(std::string identifier,
                                                       std::string imageUrl)
{
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(identifier);
    if (pair) {
        pair->browser()->removeSelectedImage(imageUrl);
    }
}

/**
 * Takes the identifier of a sky browser or a sky target and equatorial coordinates Right
 * Ascension and Declination. The target will animate to this coordinate and the browser
 * will display the coordinate.
 */
[[codegen::luawrap]] void setEquatorialAim(std::string identifier, double rightAscension,
                                           double declination)
{
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(identifier);
    if (pair) {
        pair->setEquatorialAim(glm::dvec2(rightAscension, declination));
    }
}

/**
 * Takes an identifier to a sky browser or a sky target and a vertical field of view.
 * Changes the field of view as specified by the input.
 */
[[codegen::luawrap]] void setVerticalFov(std::string identifier,
                                         float verticalFieldOfView)
{
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(identifier);
    if (pair) {
        pair->setVerticalFov(verticalFieldOfView);
    }
}

/**
 * Takes an identifier to a sky browser or a sky target and a vertical field of view.
 * Changes the field of view as specified by the input.
 */
[[codegen::luawrap]] void scrollOverBrowser(std::string identifier, float scroll) {
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(identifier);
    if (pair) {
        pair->setVerticalFovWithScroll(scroll);
    }
}

/**
 * Takes an identifier to a sky browser or a sky target and a rgb color in the ranges
 * [0, 255].
 */
[[codegen::luawrap]] void setBorderColor(std::string identifier, int red, int green,
                                         int blue)
{
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(identifier);
    if (pair) {
        pair->setBorderColor(glm::ivec3(red, green, blue));
    }
}

/**
 * Takes an identifier to a sky browser and a radius value between 0 and 1, where 0 is
 * rectangular and 1 is circular
 */
[[codegen::luawrap]] void setBorderRadius(std::string identifier, double radius) {
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(identifier);
    // Make sure the webpage has loaded properly before executing javascript on it
    if (pair && pair->browser()->isInitialized()) {
        pair->setBorderRadius(std::clamp(radius, 0.0, 1.0));
    }
}

/**
 * Sets the screen space size of the sky browser to the numbers specified by the input
 * [x, y].
 */
[[codegen::luawrap]] void setBrowserRatio(std::string identifier, float ratio)
{
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(identifier);
    if (pair) {
        pair->setBrowserRatio(ratio);
    }
}

/**
 * Takes an identifier to a sky browser and adds a rendered copy to it. The first argument
 * is the position of the first copy. The position is in RAE or Cartesian coordinates,
 * depending on if 'Use Radius Azimuth Elevation' is checked. The second argument is the
 * number of copies. If RAE is used, they will be evenly spread out on the azimuth.
 */
[[codegen::luawrap]] void addDisplayCopy(std::string identifier, int numberOfCopies = 1,
                                        glm::vec3 position = glm::vec3(2.1f, 0.f, 0.f))
{
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(identifier);
    if (pair) {
        pair->browser()->addDisplayCopy(position, numberOfCopies);
    }
}

/**
 * Takes an identifier to a sky browser and removes the latest added rendered copy to it.
 */
[[codegen::luawrap]] void removeDisplayCopy(std::string identifier) {
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(identifier);
    if (pair) {
        pair->browser()->removeDisplayCopy();
    }
}

/**
 * Starts the fine-tuning of the target rendered copy to it.
 */
[[codegen::luawrap]] void startFinetuningTarget(std::string identifier) {
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(identifier);
    if (pair) {
        pair->startFinetuningTarget();
    }
}

/**
 * Finetunes the target depending on a mouse drag. rendered copy to it. First argument
 * is the identifier of the sky browser, second is the start position of the drag
 * and third is the end position of the drag.
 */
[[codegen::luawrap]] void finetuneTargetPosition(std::string identifier,
                                                 glm::dvec2 translation)
{
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(identifier);
    if (pair) {
        pair->fineTuneTarget(translation);
    }
}

/**
 * Sets the image collection as loaded in the sky browser. Takes an identifier to the sky
 * browser.
 */
[[codegen::luawrap]] void loadingImageCollectionComplete(std::string identifier) {
    using namespace openspace;

    if (!browserBelongsToCurrentNode(identifier)) {
        return;
    }
    std::string prunedId = prunedIdentifier(identifier);

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(prunedId);
    if (pair) {
        LINFO("Image collection is loaded in Screen Space Sky Browser " + prunedId);
        pair->setImageCollectionIsLoaded(true);
        // Add all selected images to WorldWide Telescope
        const std::vector<std::string>& images = pair->selectedImages();
        std::for_each(
            images.rbegin(), images.rend(),
            [module, pair](std::string imageUrl) {
                std::optional<ImageData> img = module->wwtDataHandler().image(imageUrl);
                ghoul_assert(img.has_value(), "No image found");
                // Index of image is used as layer ID as it's unique in the image data set
                pair->browser()->addImageLayerToWwt(img->imageUrl);
            }
        );
    }
}

/**
 * Show or hide all targets and browsers. Takes a boolean that sets it to either be shown
 * or not.
 */
[[codegen::luawrap]] void showAllTargetsAndBrowsers(bool show) {
    using namespace openspace;
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    const std::vector<std::unique_ptr<TargetBrowserPair>>& pairs = module->pairs();
    for (const std::unique_ptr<TargetBrowserPair>& pair : pairs) {
        pair->setEnabled(show);
    }
}

/**
 * Stop animations. Takes an identifier to a sky browser.
 */
[[codegen::luawrap]] void stopAnimations(std::string identifier) {
    using namespace openspace;
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->pair(identifier);
    pair->stopAnimations();
}

#include "skybrowsermodule_lua_codegen.cpp"

} // namespace
