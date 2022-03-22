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

#include <modules/skybrowser/skybrowsermodule.h>

#include <modules/skybrowser/include/utility.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char _loggerCat[] = "SkyBrowserModule";

    using namespace openspace;

/**
* Takes an index to an image and selects that image in the currently
* selected sky browser.
* \param i Index of image
*/
[[codegen::luawrap]] void selectImage(int i) {
    // Load image
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    if (module->isCameraInSolarSystem()) {
        TargetBrowserPair* selected = module->getPair(module->selectedBrowserId());
        if (selected) {
            const ImageData& image = module->getWwtDataHandler()->getImage(i);
            // Load image into browser
            LINFO("Loading image " + image.name);
            selected->selectImage(image, i);

            bool isInView = skybrowser::isCoordinateInView(image.equatorialCartesian);
            // If the coordinate is not in view, rotate camera
            if (image.hasCelestialCoords && !isInView) {
                module->startRotatingCamera(
                    skybrowser::equatorialToGalactic(
                        image.equatorialCartesian * skybrowser::CelestialSphereRadius
                    )
                );
            }
        }
    }
}
/**
* Takes an identifier to a screen space renderable and adds it to the module.
* \param id Identifier
*/
[[codegen::luawrap]] void setHoverCircle(std::string id) {
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    SceneGraphNode* circle = global::renderEngine->scene()->sceneGraphNode(id);
    module->setHoverCircle(circle);     
}
/**
* Moves the hover circle to the coordinate specified by the image index.
* \param i Index of image
*/
[[codegen::luawrap]] void moveCircleToHoverImage(int i) {
    // Load image
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    module->moveHoverCircle(i);
}
/**
* Disables the hover circle, if there is one added to the sky browser 
* module.
*/
[[codegen::luawrap]] void disableHoverCircle() {
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    module->disableHoverCircle();
}
/**
* Takes an identifier to a sky browser or a sky target, an image index 
* and the order which it should have in the selected image list. The 
* image is then changed to have this order.
* \param id Identifier
* \param i Image index
* \param order Order of image
*/
[[codegen::luawrap]] void setImageLayerOrder(std::string id, int i, int order) {

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    if (module->getPair(id)) {
        module->getPair(id)->setImageOrder(i, order);
    }
}
/**
* Takes an identifier to a sky browser or target and loads the WWT image
* collection to that browser.
* \param id Identifier
*/
[[codegen::luawrap]] void loadImagesToWWT(std::string id) {
    // Load images from url
    LINFO("Connection established to WorldWide Telescope application in " + id);
    LINFO("Loading image collections to " + id);

    // Load the collections here because here we know that the browser can execute 
    // javascript
    std::string root = "https://raw.githubusercontent.com/WorldWideTelescope/"
                        "wwt-web-client/master/assets/webclient-explore-root.wtml";

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    if (module->getPair(id)) {
        module->getPair(id)->hideChromeInterface(true);
        module->getPair(id)->loadImageCollection(root);
    }
}
/**
* Starts the setup process of the sky browers. This function calls
* the lua function 'sendOutIdsToBrowsers' in all nodes in the cluster.
*/
[[codegen::luawrap]] void startSetup() {
    // This is called when the sky_browser website is connected to OpenSpace
    // Set all border colors to the border color in the master node
    if (global::windowDelegate->isMaster()) {
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        std::vector<std::unique_ptr<TargetBrowserPair>>& pairs = module->getPairs();
        for (std::unique_ptr<TargetBrowserPair>& pair : pairs) {
            std::string id = pair->browserId();
            glm::ivec3 color = pair->borderColor();
            std::string script = fmt::format(
                "openspace.skybrowser.setBorderColor('{}', {}, {}, {})", 
                id, 
                color.r, 
                color.g, 
                color.b
            );
            global::scriptEngine->queueScript(
                script,
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    }
   
    // To ensure each node in a cluster calls its own instance of the wwt application
    // Do not send this script to the other nodes
    global::scriptEngine->queueScript(
        "openspace.skybrowser.sendOutIdsToBrowsers();",
        scripting::ScriptEngine::RemoteScripting::No
    );
}
/**
* Sends all sky browsers' identifiers to their respective CEF browser.
*/
[[codegen::luawrap]] void sendOutIdsToBrowsers() {
    // This is called when the sky_browser website is connected to OpenSpace
    // Send out identifiers to the browsers
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    std::vector<std::unique_ptr<TargetBrowserPair>>& pairs = module->getPairs();
    for (std::unique_ptr<TargetBrowserPair>& pair : pairs) {
        pair->sendIdToBrowser();
    }
}
/**
* Takes an identifier to a sky browser and starts the initialization
* for that browser. That means that the browser starts to try to connect
* to the AAS WorldWide Telescope application by sending it messages. And
* that the target matches its appearance to its corresponding browser.
* \param id Identifier
*/
[[codegen::luawrap]] void initializeBrowser(std::string id) {
    // Initialize browser with ID and its corresponding target
    LINFO("Initializing sky browser " + id);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* found = module->getPair(id);
    if (found) {
        found->setIsSyncedWithWwt(true);
        found->initialize();
    }
}
/**
* Takes the identifier of the sky target and a sky browser and adds them
* to the sky browser module.
* \param targetId Identifier of target (either SceneGraphNode or Renderable)
* \param browserId Identifier of browser
*/
[[codegen::luawrap]] void addPairToSkyBrowserModule(std::string targetId, std::string browserId) {
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    LINFO("Add browser " + browserId + " to sky browser module");
    LINFO("Add target " + targetId + " to sky browser module");
        
    module->addTargetBrowserPair(targetId, browserId);
}
/**
* Returns a list of all the loaded AAS WorldWide Telescope images that 
* have been loaded. Each image has a name, thumbnail url, equatorial 
* spherical coordinates RA and Dec, equatorial Cartesian coordinates, 
* if the image has celestial coordinates, credits text, credits url 
* and the identifier of the image which is a unique number.
*/

[[codegen::luawrap]] ghoul::Dictionary getListOfImages() {
    // Send image list to GUI
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    // If no data has been loaded yet, download the data from the web!
    if (module->nLoadedImages() == 0) {
        std::string root = "https://raw.githubusercontent.com/WorldWideTelescope/"
                            "wwt-web-client/master/assets/webclient-explore-root.wtml";

        std::filesystem::path directory = absPath("${MODULE_SKYBROWSER}/wwtimagedata/");

        module->loadImages(root, directory);
    }
    
    // Create Lua table to send to the GUI
    ghoul::Dictionary list;

    for (int i = 0; i < module->nLoadedImages(); i++) {
        const ImageData& img = module->getWwtDataHandler()->getImage(i);
        using namespace std::string_literals;
            
        // Push ("Key", value)
        ghoul::Dictionary image;
        image.setValue("name", img.name);
        image.setValue("thumbnail", img.thumbnailUrl);
        image.setValue("ra", img.equatorialSpherical.x);
        image.setValue("dec", img.equatorialSpherical.y);
        image.setValue("cartesianDirection", img.equatorialCartesian);
        image.setValue("hasCelestialCoords", img.hasCelestialCoords);
        image.setValue("credits", img.credits);
        image.setValue("creditsUrl", img.creditsUrl);
        image.setValue("identifier", std::to_string(i));

        // Index for current ImageData 
        // Set table for the current ImageData
        list.setValue(std::to_string(i + 1), image);
    }

    return list;
}
/**
* Returns a table of data regarding the current view and the sky browsers
* and targets.
* \return Dictionary of data regarding the current targets
*/
[[codegen::luawrap]] ghoul::Dictionary getTargetData() {
    using namespace std::string_literals;
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
        std::vector<std::unique_ptr<TargetBrowserPair>>& pairs = module->getPairs();

        for (std::unique_ptr<TargetBrowserPair>& pair : pairs) {
            std::string id = pair->browserId();
            // Convert deque to vector so ghoul can read it
            std::vector<int> selectedImagesVector;
            const std::deque<int> selectedImages = pair->selectedImages();
            std::for_each(
                selectedImages.begin(), 
                selectedImages.end(), 
                [&](int i) {
                    selectedImagesVector.push_back(i);
                }
            );
                
            glm::dvec2 spherical = pair->targetDirectionEquatorial();
            glm::dvec3 cartesian = skybrowser::sphericalToCartesian(spherical);

            ghoul::Dictionary target;
            // Set ("Key", value)
            target.setValue("id", id);
            target.setValue("name", pair->browserGuiName());
            target.setValue("FOV", static_cast<double>(pair->verticalFov()));
            target.setValue("selectedImages", selectedImagesVector);
            target.setValue("cartesianDirection", cartesian);
            target.setValue("ra", spherical.x);
            target.setValue("dec", spherical.y);
            target.setValue("color", pair->borderColor());
            target.setValue("size", glm::dvec2(pair->size()));
              
            // Set table for the current target
            data.setValue(id, target);
        }
    }

    return data;
}
/**
* Takes an identifier to a sky browser or sky target. Rotates the camera
* so that the target is placed in the center of the view.
* \param id
*/
[[codegen::luawrap]] void adjustCamera(std::string id) {
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    if (module->isCameraInSolarSystem()) {
        module->lookAtTarget(id);
    }
}
/**
* Takes an identifier to a sky browser or sky target, an index to an image
* and a value for the opacity.
* \param id Identifier
* \param i Image index
* \param opacity 
*/
[[codegen::luawrap]] void setOpacityOfImageLayer(std::string id, int i, double opacity) {
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* found = module->getPair(id);
    if (found) {
        found->setImageOpacity(i, opacity);
    }
}
/**
* Takes an identifier to a sky browser and animates its corresponding
* target to the center of the current view.
* \param id Identifier
*/
[[codegen::luawrap]] void centerTargetOnScreen(std::string id) {
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->getPair(id);
    if (pair) {
        pair->centerTargetOnScreen();
    }
}
/**
* Takes an identifier to a sky browser or target. Sets that sky browser
* currently selected.
* \param id
*/
[[codegen::luawrap]] void setSelectedBrowser(std::string id) {
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    module->setSelectedBrowser(id);
}
/**
* Creates a sky browser and a target.
*/
[[codegen::luawrap]] void createTargetBrowserPair() {
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    int noOfPairs = module->nPairs();
    std::string nameBrowser = "Sky Browser " + std::to_string(noOfPairs);
    std::string nameTarget = "Sky Target " + std::to_string(noOfPairs);
    std::string idBrowser = "SkyBrowser" + std::to_string(noOfPairs);
    std::string idTarget = "SkyTarget" + std::to_string(noOfPairs);
    // Determine starting point on screen for the target
    glm::vec3 positionBrowser = { -1.0f, -0.5f, -2.1f };
    glm::vec3 positionTarget = { 1.0f, 0.5f, -2.1f };
    glm::dvec3 galacticTarget = skybrowser::localCameraToGalactic(positionTarget);
    std::string guiPath = "/SkyBrowser";
    std::string url = "https://data.openspaceproject.com/dist/skybrowser/page/";
    double fov = 5.0;
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
                "Position = {"
                        + std::to_string(galacticTarget.x) + ", "
                        + std::to_string(galacticTarget.y) + ", "
                        + std::to_string(galacticTarget.z) + ", "
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
          "Name = 'Sky Target', "
          "Path = '/SkyBrowser', "
        "}"
    "}";

    global::scriptEngine->queueScript(
        "openspace.addScreenSpaceRenderable(" + browser + ");",
        scripting::ScriptEngine::RemoteScripting::No
    );

    global::scriptEngine->queueScript(
        "openspace.addSceneGraphNode(" + target + ");",
        scripting::ScriptEngine::RemoteScripting::No
    );

    global::scriptEngine->queueScript(
        "openspace.skybrowser.addPairToSkyBrowserModule('" + idTarget + "','"
        + idBrowser + "');",
        scripting::ScriptEngine::RemoteScripting::No
    );

    global::scriptEngine->queueScript(
        "openspace.skybrowser.setSelectedBrowser('" + idBrowser + "');",
        scripting::ScriptEngine::RemoteScripting::No
    );
}
/**
* Takes in identifier to a sky browser or target and removes them.
* \param id Identifier
*/
[[codegen::luawrap]] void removeTargetBrowserPair(std::string id) {
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* found = module->getPair(id);
    if (found) {
        std::string browser = found->browserId();
        std::string target = found->targetNodeId();

        module->removeTargetBrowserPair(id);

        // Remove from engine
        global::scriptEngine->queueScript(
            "openspace.removeScreenSpaceRenderable('" + browser + "');",
            scripting::ScriptEngine::RemoteScripting::Yes
        );

        global::scriptEngine->queueScript(
            "openspace.removeSceneGraphNode('" + target + "');",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
}
/**
* Takes an identifier to a sky browser or sky target and the [x, y]
* starting position and the [x, y] translation vector.
* \param id Identifier
* \param startX Starting x-position
* \param startY Starting y-position
* \param transX Translation x-value
* \param transY Translation y-value
*/
[[codegen::luawrap]] void translateScreenSpaceRenderable(std::string id, float startX, 
                                                         float startY, float transX, 
                                                         float transY) {
    ScreenSpaceRenderable* renderable = global::renderEngine->screenSpaceRenderable(id);

    if (renderable) {
        renderable->translate(glm::vec2(transX, transY), glm::vec2(startX, startY));
    }
}
/**
* Takes an identifier to a sky browser or target and an index to an
* image. Removes that image from that sky browser.
* \param id Identifier
* \i Index of image
*/
[[codegen::luawrap]] void removeSelectedImageInBrowser(std::string id, int i) {
    // Get browser
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    const ImageData& image = module->getWwtDataHandler()->getImage(i);
        
    TargetBrowserPair* pair = module->getPair(id);
    if (pair) {
        pair->removeSelectedImage(i);
    }
}
/**
* Takes the identifier of a sky browser or a sky target and equatorial
* coordinates Right Ascension and Declination. The target will animate to
* this coordinate and the browser will display the coordinate.
* \param id Identifier
* \param ra Right Ascension
* \param dec Declination
*/
[[codegen::luawrap]] void setEquatorialAim(std::string id, double ra, double dec) {
    // Get module
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    TargetBrowserPair* pair = module->getPair(id);
    if (pair) {
        pair->setEquatorialAim(glm::dvec2(ra, dec));
    }
}
/**
* Takes an identifier to a sky browser or a sky target and a vertical
* field of view. Changes the field of view as specified by the input.
* \param id Identifier
* \param vfov Vertical Field of View
*/
[[codegen::luawrap]] void setVerticalFov(std::string id, float vfov) {
    // Get module
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    TargetBrowserPair* pair = module->getPair(id);
    if (pair) {
        pair->setVerticalFov(vfov);
    }
}
/**
* Takes an identifier to a sky browser or a sky target and a rgb color
* in the ranges [0, 255].
* \param id Identifier
* \param r Red
* \param g Green
* \param b Blue
*/
[[codegen::luawrap]] void setBorderColor(std::string id, int r, int g, int b) {
    glm::ivec3 color{ r, g, b };
    // Get module
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    TargetBrowserPair* pair = module->getPair(id);
    if (pair) {
        pair->setBorderColor(color);
    }
}
/**
* Sets the screen space size of the sky browser to the numbers specified
* by the input [x, y].
* \param id
* \param sizeX Size on the x-axis
* \param sizeY Size on the y-axis
*/
[[codegen::luawrap]] void setScreenSpaceSize(std::string id, float sizeX, float sizeY) {
    // Get module
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    TargetBrowserPair* pair = module->getPair(id);
    if (pair) {
        pair->setScreenSpaceSize(glm::vec2(sizeX, sizeY));
    }
}
/**
* Takes an identifier to a sky browser and adds a rendered copy to it.
* \param id Identifier
*/
[[codegen::luawrap]] void addRenderCopy(std::string id) {
    // Get module
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    TargetBrowserPair* pair = module->getPair(id);
    if (pair) {
        pair->browser()->addRenderCopy();
    }
}
/**
* Takes an identifier to a sky browser and removes the latest added
* rendered copy to it.
* \param id Identifier
*/
[[codegen::luawrap]] void removeRenderCopy(std::string id) {
    // Get module
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    TargetBrowserPair* pair = module->getPair(id);
    if (pair) {
        pair->browser()->removeRenderCopy();
    }
}
#include "skybrowsermodule_lua_codegen.cpp"

} // namespace

