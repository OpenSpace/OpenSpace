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
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char _loggerCat[] = "SkyBrowserModule";
} // namespace

namespace openspace::skybrowser::luascriptfunctions {

int selectImage(lua_State* L) {
    // Load image
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::selectImage");
    const int i = ghoul::lua::value<int>(L, 1);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    if (module->isCameraInSolarSystem()) {
        TargetBrowserPair* selected = module->getPair(module->selectedBrowserId());
        if (selected) {
            const ImageData& image = module->getWwtDataHandler()->getImage(i);
            // Load image into browser
            LINFO("Loading image " + image.name);
            selected->selectImage(image, i);

            bool isInView = isCoordinateInView(image.equatorialCartesian);
            // If the coordinate is not in view, rotate camera
            if (image.hasCelestialCoords && !isInView) {
                module->startRotatingCamera(
                    equatorialToGalactic(image.equatorialCartesian)
                );
            }
        }
    }
        
    return 0;
}

int setHoverCircle(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::setHoverCircle");
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    std::string id = ghoul::lua::value<std::string>(L, 1);

    ScreenSpaceImageLocal* circle = dynamic_cast<ScreenSpaceImageLocal*>(
        global::renderEngine->screenSpaceRenderable(id));
    module->setHoverCircle(circle);     

    return 0;
}

int moveCircleToHoverImage(lua_State* L) {
    // Load image
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::moveCircleToHoverImage");
    const int i = ghoul::lua::value<int>(L, 1);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    module->moveHoverCircle(i);
        
    return 0;
}

int disableHoverCircle(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::disableHoverCircle");
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    module->disableHoverCircle();
        
    return 0;
}

int lockTarget(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::lockTarget");
    const std::string id = ghoul::lua::value<std::string>(L, 1);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    if (module->getPair(id)) {
        module->getPair(id)->lock();
    }
    return 0;
}

int unlockTarget(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::unlockTarget");
    const std::string id = ghoul::lua::value<std::string>(L, 1);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    if (module->getPair(id)) {
        module->getPair(id)->unlock();
    }
    return 0;
}

int setImageLayerOrder(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::setImageLayerOrder");
    auto [id, i, order] = ghoul::lua::values<std::string, int, int>(L);

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    if (module->getPair(id)) {
        module->getPair(id)->setImageOrder(i, order);
    }
    return 0;
}

int loadImagesToWWT(lua_State* L) {
    // Load images from url
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::loadImagesToWWT");
    const std::string id = ghoul::lua::value<std::string>(L, 1);
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

    return 0;
}

int startSetup(lua_State* L) {
    // This is called when the sky_browser website is connected to OpenSpace
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::startSetup");

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
    
    return 0;
}

int sendOutIdsToBrowsers(lua_State* L) {
    // This is called when the sky_browser website is connected to OpenSpace
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::sendOutIdsToBrowsers");

    // Send out identifiers to the browsers
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    std::vector<std::unique_ptr<TargetBrowserPair>>& pairs = module->getPairs();
    for (std::unique_ptr<TargetBrowserPair>& pair : pairs) {
        pair->sendIdToBrowser();
    }

    return 0;
}

int initializeBrowser(lua_State* L) {
    // Initialize browser with ID and its corresponding target
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::initializeBrowser");
    const std::string id = ghoul::lua::value<std::string>(L, 1);

    LINFO("Initializing sky browser " + id);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* found = module->getPair(id);
    if (found) {
        found->setIsSyncedWithWwt(true);
        found->initialize();
    }

    return 0;
}
    
int addPairToSkyBrowserModule(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addPairToSkyBrowserModule");
    auto [targetId, browserId] = ghoul::lua::values<std::string, std::string>(L);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    LINFO("Add browser " + browserId + " to sky browser module");
    LINFO("Add target " + targetId + " to sky browser module");
        
    module->addTargetBrowserPair(targetId, browserId);

    return 0;
}

int getListOfImages(lua_State* L) {
    // Send image list to GUI
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::getListOfImages");
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    // If no data has been loaded yet, download the data from the web!

    if (module->nLoadedImages() == 0) {
        std::string root = "https://raw.githubusercontent.com/WorldWideTelescope/"
                            "wwt-web-client/master/assets/webclient-explore-root.wtml";

        std::filesystem::path directory = absPath("${MODULE_SKYBROWSER}/wwtimagedata/");

        module->loadImages(root, directory);
    }
    
    // Create Lua table to send to the GUI
    lua_newtable(L);

    for (int i = 0; i < module->nLoadedImages(); i++) {
        const ImageData& img = module->getWwtDataHandler()->getImage(i);
            
        // Index for current ImageData 
        ghoul::lua::push(L, i + 1); 
        lua_newtable(L);
        // Push ("Key", value)
        ghoul::lua::push(L, "name", img.name);
        lua_settable(L, -3);
        ghoul::lua::push(L, "thumbnail", img.thumbnailUrl);
        lua_settable(L, -3);
        ghoul::lua::push(L, "ra", img.equatorialSpherical.x);
        lua_settable(L, -3);
        ghoul::lua::push(L, "dec", img.equatorialSpherical.y);
        lua_settable(L, -3);
        ghoul::lua::push(L, "cartesianDirection", img.equatorialCartesian);
        lua_settable(L, -3);
        ghoul::lua::push(L, "hasCelestialCoords", img.hasCelestialCoords);
        lua_settable(L, -3);
        ghoul::lua::push(L, "credits", img.credits);
        lua_settable(L, -3);
        ghoul::lua::push(L, "creditsUrl", img.creditsUrl);
        lua_settable(L, -3);
        ghoul::lua::push(L, "identifier", std::to_string(i));
        lua_settable(L, -3);

        // Set table for the current ImageData
        lua_settable(L, -3);    
    }

    return 1;
}

int getTargetData(lua_State* L) {
    // Send image list to GUI
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::getTargetData");

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    lua_newtable(L);
        
    // Add the window data for OpenSpace
    ghoul::lua::push(L, "OpenSpace");
    lua_newtable(L);
    glm::dvec3 cartesianCam = skybrowser::cameraDirectionEquatorial();
    glm::dvec2 sphericalCam = skybrowser::cartesianToSpherical(cartesianCam);

    // Calculate the smallest FOV of vertical and horizontal
    glm::dvec2 fovs = skybrowser::fovWindow();
    double FOV = std::min(fovs.x, fovs.y);
    // Push window data
    ghoul::lua::push(L, "windowHFOV", FOV);
    lua_settable(L, -3);
    ghoul::lua::push(L, "cartesianDirection", cartesianCam);
    lua_settable(L, -3);
    ghoul::lua::push(L, "ra", sphericalCam.x);
    lua_settable(L, -3);
    ghoul::lua::push(L, "dec", sphericalCam.y);
    lua_settable(L, -3);
    ghoul::lua::push(L, "selectedBrowserId", module->selectedBrowserId());
    lua_settable(L, -3);
    ghoul::lua::push(L, "selectedTargetId", module->selectedTargetId());
    lua_settable(L, -3);
    ghoul::lua::push(L, "isFacingCamera", module->isSelectedPairFacingCamera());
    lua_settable(L, -3);
    ghoul::lua::push(L, "isUsingRadiusAzimuthElevation", module->isSelectedPairUsingRae());
    lua_settable(L, -3);
    ghoul::lua::push(L, "cameraInSolarSystem", module->isCameraInSolarSystem());
    lua_settable(L, -3);
    // Set table for the current ImageData
    lua_settable(L, -3);

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

            ghoul::lua::push(L, id);
            lua_newtable(L);
            // Push ("Key", value)
            ghoul::lua::push(L, "id", id);
            lua_settable(L, -3);
            ghoul::lua::push(L, "name", pair->browserGuiName());
            lua_settable(L, -3);
            ghoul::lua::push(L, "FOV", pair->verticalFov());
            lua_settable(L, -3);
            ghoul::lua::push(L, "selectedImages", selectedImagesVector);
            lua_settable(L, -3);
            ghoul::lua::push(L, "cartesianDirection", cartesian);
            lua_settable(L, -3);
            ghoul::lua::push(L, "ra", spherical.x);
            lua_settable(L, -3);
            ghoul::lua::push(L, "dec", spherical.y);
            lua_settable(L, -3);
            ghoul::lua::push(L, "color", pair->borderColor());
            lua_settable(L, -3);
            ghoul::lua::push(L, "isLocked", pair->isLocked());
            lua_settable(L, -3);
            ghoul::lua::push(L, "size", pair->size());
            lua_settable(L, -3);
              
            // Set table for the current target
            lua_settable(L, -3);
        }
    }

    return 1;
}

int adjustCamera(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::adjustCamera");
    const std::string id = ghoul::lua::value<std::string>(L, 1);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    if (module->isCameraInSolarSystem()) {
        module->lookAtTarget(id);
    }

    return 0;
}

int setOpacityOfImageLayer(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::setOpacityOfImageLayer");
    auto [id, i, opacity] = ghoul::lua::values<std::string, int, double>(L);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    TargetBrowserPair* found = module->getPair(id);
    if (found) {
        found->setImageOpacity(i, opacity);
    }

    return 0;
}

int centerTargetOnScreen(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::centerTargetOnScreen");
    const std::string id = ghoul::lua::value<std::string>(L, 1);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* pair = module->getPair(id);
    if (pair) {
        pair->centerTargetOnScreen();
    }
        
    return 0;
}

int setSelectedBrowser(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::setSelectedBrowser");
    const std::string id = ghoul::lua::value<std::string>(L, 1);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        
    module->setSelectedBrowser(id);
        
    return 0;
}

int createTargetBrowserPair(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::createTargetBrowserPair");
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    int noOfPairs = module->nPairs();
    std::string nameBrowser = "Sky Browser " + std::to_string(noOfPairs);
    std::string nameTarget = "Sky Target " + std::to_string(noOfPairs);
    std::string idBrowser = "SkyBrowser" + std::to_string(noOfPairs);
    std::string idTarget = "SkyTarget" + std::to_string(noOfPairs);
    glm::vec3 positionBrowser = { -1.0f, -0.5f, -2.1f };
    glm::vec3 positionTarget = { 1.0f, 0.5f, -2.1f };
    std::string guiPath = "/SkyBrowser";
    std::string url = "https://data.openspaceproject.com/dist/skybrowser/page/";

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
        "Type = 'ScreenSpaceSkyTarget',"
        "Name = '" + nameTarget + "',"
        "FaceCamera = false,"
        "CartesianPosition = " + ghoul::to_string(positionTarget) +
        "}";

    global::scriptEngine->queueScript(
        "openspace.addScreenSpaceRenderable(" + browser + ");",
        scripting::ScriptEngine::RemoteScripting::No
    );

    global::scriptEngine->queueScript(
        "openspace.addScreenSpaceRenderable(" + target + ");",
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

    return 0;
}

int removeTargetBrowserPair(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::removeTargetBrowserPair");
    std::string id = ghoul::lua::value<std::string>(L, 1);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    TargetBrowserPair* found = module->getPair(id);
    if (found) {
        std::string browser = found->browserId();
        std::string target = found->targetId();

        module->removeTargetBrowserPair(id);

        // Remove from engine
        global::scriptEngine->queueScript(
            "openspace.removeScreenSpaceRenderable('" + browser + "');",
            scripting::ScriptEngine::RemoteScripting::Yes
        );

        global::scriptEngine->queueScript(
            "openspace.removeScreenSpaceRenderable('" + target + "');",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
    
    return 0;
}

int translateScreenSpaceRenderable(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 5, "lua::translateScreenSpaceRenderable");
    auto [id, startX, startY, transX, transY] = 
        ghoul::lua::values<std::string, float, float, float, float>(L);
    
    ScreenSpaceRenderable* renderable = global::renderEngine->screenSpaceRenderable(id);

    if (renderable) {
        renderable->translate(glm::vec2(transX, transY), glm::vec2(startX, startY));
    }

    return 0;
}

int removeSelectedImageInBrowser(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::removeSelectedImageInBrowser");
    auto [id, i] = ghoul::lua::values<std::string, int>(L);

    // Get browser
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    const ImageData& image = module->getWwtDataHandler()->getImage(i);
        
    TargetBrowserPair* pair = module->getPair(id);
    if (pair) {
        pair->removeSelectedImage(i);
    }

    return 0;
}

int setEquatorialAim(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::setEquatorialAim");
    auto [id, ra, dec] = ghoul::lua::values<std::string, double, double>(L);

    // Get module
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    TargetBrowserPair* pair = module->getPair(id);
    if (pair) {
        pair->setEquatorialAim(glm::dvec2(ra, dec));
    }

    return 0;
}

int setVerticalFov(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::setVerticalFov");
    auto [id, vfov] = ghoul::lua::values<std::string, float>(L);

    // Get module
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    TargetBrowserPair* pair = module->getPair(id);
    if (pair) {
        pair->setVerticalFov(vfov);
    }

    return 0;
}

int setBorderColor(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 4, "lua::setBorderColor");
    auto [id, r, g, b] = ghoul::lua::values<std::string, int, int, int>(L);

    glm::ivec3 color{ r, g, b };
    // Get module
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    TargetBrowserPair* pair = module->getPair(id);
    if (pair) {
        pair->setBorderColor(color);
    }

    return 0;
}

int setScreenSpaceSize(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::setScreenSpaceSize");
    auto [id, sizeX, sizeY] = ghoul::lua::values<std::string, float, float>(L);

    // Get module
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    TargetBrowserPair* pair = module->getPair(id);
    if (pair) {
        pair->setScreenSpaceSize(glm::vec2(sizeX, sizeY));
    }
    return 0;
}
} // namespace openspace::skybrowser::luafunctions

