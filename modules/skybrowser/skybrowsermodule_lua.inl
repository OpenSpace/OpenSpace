
#include <modules/skybrowser/skybrowsermodule.h>

#include <modules/skybrowser/include/utility.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/globals.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/rendering/renderengine.h>
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
        Pair* selected = module->getPair(module->selectedBrowserId());
        if (selected) {

            const ImageData& image = module->getWwtDataHandler()->getImage(i);
            // Load image into browser
            LINFO("Loading image " + image.name);
            selected->selectImage(image, i);

            bool isInView = skybrowser::isCoordinateInView(image.equatorialCartesian);
            // If the coordinate is not in view, rotate camera
            if (image.hasCelestialCoords && !isInView) {
                module->startRotatingCamera(
                    skybrowser::equatorialToGalactic(image.equatorialCartesian)
                );
            }
        }
    }
    else if (module->get3dBrowser()) {
        const ImageData& image = module->getWwtDataHandler()->getImage(i);
        module->get3dBrowser()->displayImage(image.imageUrl, i);
        module->get3dBrowser()->setEquatorialAim(image.equatorialCartesian);
        module->get3dBrowser()->setVerticalFov(image.fov);
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
    const std::string id = ghoul::lua::value<std::string>(L, 1);
    const int i = ghoul::lua::value<int>(L, 1);
    int order = ghoul::lua::value<int>(L, 1);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    if (module->getPair(id)) {
        module->getPair(id)->setImageOrder(i, order);
    }
    else if (module->get3dBrowser(id)) {
        module->get3dBrowser(id)->setImageOrder(i, order);
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
        //module->getPair(id)->hideChromeInterface(true);
        module->getPair(id)->loadImageCollection(root);
    }
    else if (module->get3dBrowser(id)) {
           
        // Load Image collections
        module->get3dBrowser(id)->hideChromeInterface(true);
        module->get3dBrowser(id)->setIsSyncedWithWwt(false);
        LINFO("Load images to " + module->get3dBrowser(id)->identifier());
        module->get3dBrowser(id)->loadImageCollection(root);
        LINFO("Image collection loaded in " + module->get3dBrowser(id)->identifier());
           
    }

	return 0;
}

int startSetup(lua_State* L) {
    // This is called when the sky_browser website is connected to OpenSpace
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::startSetup");

    // To ensure each node in a cluster calls its own instance of the wwt application
    // Do not send this script to the other nodes
    openspace::global::scriptEngine->queueScript(
        "openspace.skybrowser.sendOutIdsToBrowsers();",
        scripting::ScriptEngine::RemoteScripting::No
    );
    
    return 0;
}

int sendOutIdsToBrowsers(lua_State* L) {
    // This is called when the sky_browser website is connected to OpenSpace
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::sendOutIdsToBrowsers");

    // Send out ID's to the browsers
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    std::vector<std::unique_ptr<Pair>>& pairs = module->getPairs();
    for (std::unique_ptr<Pair>& pair : pairs) {
        pair->sendIdToBrowser();
    }
    if(module->get3dBrowser()) {
            
        module->get3dBrowser()->setIdInBrowser();
    }
    return 0;
}

int initializeBrowser(lua_State* L) {
    // Initialize browser with ID and its corresponding target
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::initializeBrowser");
    const std::string id = ghoul::lua::value<std::string>(L, 1);

    LINFO("Initializing sky browser " + id);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    if (module->getPair(id)) {
        module->getPair(id)->setIsSyncedWithWwt(true);
        module->getPair(id)->initialize();
    }
    else if(module->get3dBrowser(id)) {
        // Initialize
        LINFO("Initializing 3D sky browsers");
        module->get3dBrowser()->setIsSyncedWithWwt(true);
    }

    return 0;
}
    
int add3dBrowserToSkyBrowserModule(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::add3dBrowserToSkyBrowserModule");
    const std::string id = ghoul::lua::value<std::string>(L, 1);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    LINFO("Add to sky browser module id " + id);
    module->set3dBrowser(id);

    return 0;
}

int addPairToSkyBrowserModule(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addPairToSkyBrowserModule");
    const std::string targetId = ghoul::lua::value<std::string>(L, 1);
    const std::string browserId = ghoul::lua::value<std::string>(L, 1);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    LINFO("Add browser " + browserId + " to sky browser module.");
    LINFO("Add target " + targetId + " to sky browser module.");
        
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
        //std::string hubble = "http://www.worldwidetelescope.org/wwtweb/"
        //"catalog.aspx?W=hubble";
        std::string directory = absPath("${MODULE_SKYBROWSER}/WWTimagedata/").string();

        // 3D images
        std::string http = "${SYNC}/http/";
        std::string globular = "digitaluniverse_globularclusters_speck/2/gc.speck";
        std::string open = "digitaluniverse_openclusters_speck/2/oc.speck";
        // Load speck files for 3D positions
        std::filesystem::path globularClusters = absPath(http + globular);
        std::filesystem::path openClusters = absPath(http + open);
        std::vector<std::filesystem::path> specks = {
            openClusters, 
            globularClusters
        };

        module->loadImages(root, directory, specks);
	}
	    
    // Create Lua table to send to the GUI
    lua_newtable(L);

	for (int i = 0; i < module->nLoadedImages(); i++) {
        const ImageData& img = module->getWwtDataHandler()->getImage(i);
        glm::dvec3 coords = img.equatorialCartesian;
        glm::dvec3 position = img.position3d;

        // Conversions for ghoul
        std::vector<double> cartCoordsVec = { coords.x, coords.y, coords.z };
        std::vector<double> position3d = { position.x, position.y, position.z };
            
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
        ghoul::lua::push(L, "cartesianDirection", cartCoordsVec);
        lua_settable(L, -3);
        ghoul::lua::push(L, "hasCelestialCoords", img.hasCelestialCoords);
        lua_settable(L, -3);
        ghoul::lua::push(L, "credits", img.credits);
        lua_settable(L, -3);
        ghoul::lua::push(L, "creditsUrl", img.creditsUrl);
        lua_settable(L, -3);
        ghoul::lua::push(L, "identifier", std::to_string(i));
        lua_settable(L, -3);
        ghoul::lua::push(L, "has3dCoords", img.has3dCoords);
        lua_settable(L, -3);
        ghoul::lua::push(L, "position3d", position3d);
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
    // Convert to vector so ghoul can read it
    std::vector<double> viewDirCelestVec = { 
        cartesianCam.x, 
        cartesianCam.y, 
        cartesianCam.z 
    };
        
    // Calculate the smallest FOV of vertical and horizontal
    glm::dvec2 fovs = skybrowser::fovWindow();
    double FOV = std::min(fovs.x, fovs.y);
    // Push window data
    ghoul::lua::push(L, "windowHFOV", FOV);
    lua_settable(L, -3);
    ghoul::lua::push(L, "cartesianDirection", viewDirCelestVec);
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
        std::vector<std::unique_ptr<Pair>>& pairs = module->getPairs();

        for (std::unique_ptr<Pair>& pair : pairs) {
            std::string id = pair->browserId();
            // Convert deque to vector so ghoul can read it
            std::vector<int> selectedImagesVector;
            const std::deque<int> selectedImages = pair->getSelectedImages();
            std::for_each(selectedImages.begin(), selectedImages.end(), [&](int i) {
                selectedImagesVector.push_back(i);
                });
                
            glm::dvec2 spherical = pair->targetDirectionEquatorial();
            glm::dvec3 cartesian = skybrowser::sphericalToCartesian(spherical);
            
                   
            std::vector<double> cartesianVec = { 
                cartesian.x, 
                cartesian.y, 
                cartesian.z 
            };
            // Convert color to vector so ghoul can read it
            glm::ivec3 color = pair->borderColor();
            std::vector<int> colorVec = { color.r, color.g, color.b };

            // Convert color to vector so ghoul can read it
            glm::vec2 size = pair->size();
            std::vector<float> sizeVec = { size.x, size.y };

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
            ghoul::lua::push(L, "cartesianDirection", cartesianVec);
            lua_settable(L, -3);
            ghoul::lua::push(L, "ra", spherical.x);
            lua_settable(L, -3);
            ghoul::lua::push(L, "dec", spherical.y);
            lua_settable(L, -3);
            ghoul::lua::push(L, "color", colorVec);
            lua_settable(L, -3);
            ghoul::lua::push(L, "isLocked", pair->isLocked());
            lua_settable(L, -3);
            ghoul::lua::push(L, "size", sizeVec);
            lua_settable(L, -3);
              
            // Set table for the current target
            lua_settable(L, -3);
                
        }
    }
    else if(module->get3dBrowser()){          
        // Convert deque to vector so ghoul can read it
        std::vector<int> selectedImagesVector;
        std::deque<int> selectedImages = module->get3dBrowser()->getSelectedImages();
        std::for_each(selectedImages.begin(), selectedImages.end(), [&](int index) {
            selectedImagesVector.push_back(index);
            });
        glm::dvec3 position3dBrowser = module->get3dBrowserNode()->position();
        glm::dvec3 cartesian = skybrowser::galacticToEquatorial(position3dBrowser);
        glm::dvec2 spherical = skybrowser::cartesianToSpherical(cartesian);
        std::vector<double> celestialCartVec = { 
            cartesian.x, 
            cartesian.y, 
            cartesian.z 
        };
        // Convert color to vector so ghoul can read it
        glm::ivec3 color = module->get3dBrowser()->borderColor();
        std::vector<int> colorVec = { color.x, color.y, color.z };

        ghoul::lua::push(L, module->get3dBrowser()->identifier());
        lua_newtable(L);
        // Push ("Key", value)
        ghoul::lua::push(L, "id", module->get3dBrowser()->identifier());
        lua_settable(L, -3);
        ghoul::lua::push(L, "name", module->get3dBrowserNode()->guiName());
        lua_settable(L, -3);
        ghoul::lua::push(L, "FOV", module->get3dBrowser()->verticalFov());
        lua_settable(L, -3);
        ghoul::lua::push(L, "selectedImages", selectedImagesVector);
        lua_settable(L, -3);
        ghoul::lua::push(L, "cartesianDirection", celestialCartVec);
        lua_settable(L, -3);
        ghoul::lua::push(L, "ra", spherical.x);
        lua_settable(L, -3);
        ghoul::lua::push(L, "dec", spherical.y);
        lua_settable(L, -3);
        ghoul::lua::push(L, "color", colorVec);
        lua_settable(L, -3);

        // Set table for the current target
        lua_settable(L, -3);
    }
            

    return 1;
}
int adjustCamera(lua_State* L) {
	ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::adjustCamera");
    const std::string id = ghoul::lua::value<std::string>(L, 1);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    if(module->isCameraInSolarSystem()) {
        module->lookAtTarget(id);
    }
    else {
        module->lookAt3dBrowser();
    }

	return 0;
}

int set3dSelectedImagesAs2dSelection(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::set3dSelectedImagesAs2dSelection");
    const std::string pairId = ghoul::lua::value<std::string>(L, 1);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    module->add2dSelectedImagesTo3d(pairId);

    return 0;
}

int setOpacityOfImageLayer(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::setOpacityOfImageLayer");
    const std::string id = ghoul::lua::value<std::string>(L, 1);
    const int i = ghoul::lua::value<int>(L, 1);
    double opacity = ghoul::lua::value<double>(L, 1);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    if (module->getPair(id)) {
        module->getPair(id)->setImageOpacity(i, opacity);
            
    }
    else if (module->get3dBrowser(id)) {
        module->get3dBrowser(id)->setImageOpacity(i, opacity);
    }

    return 0;
}

int centerTargetOnScreen(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::centerTargetOnScreen");
    const std::string id = ghoul::lua::value<std::string>(L, 1);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    Pair* pair = module->getPair(id);
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
    std::string guiPath = "/SkyBrowser";
    //std::string url = "https://data.openspaceproject.com/dist/skybrowser/page/";
    std::string url = "http://localhost:8000"; // check webgl version
    //std::string url = "https://get.webgl.org";

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
        "}";

    openspace::global::scriptEngine->queueScript(
        "openspace.addScreenSpaceRenderable(" + browser + ");",
        scripting::ScriptEngine::RemoteScripting::No
    );

    openspace::global::scriptEngine->queueScript(
        "openspace.addScreenSpaceRenderable(" + target + ");",
        scripting::ScriptEngine::RemoteScripting::No
    );

    openspace::global::scriptEngine->queueScript(
        "openspace.skybrowser.addPairToSkyBrowserModule('" + idTarget + "','"
        + idBrowser + "');",
        scripting::ScriptEngine::RemoteScripting::No
    );

    openspace::global::scriptEngine->queueScript(
        "openspace.skybrowser.setSelectedBrowser('" + idBrowser + "');",
        scripting::ScriptEngine::RemoteScripting::No
    );

    return 0;
}

int removeTargetBrowserPair(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::removeTargetBrowserPair");
    std::string id = ghoul::lua::value<std::string>(L, 1);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    Pair* found = module->getPair(id);
    if (found) {
        std::string browser = found->browserId();
        std::string target = found->targetId();
        found = nullptr;

        module->removeTargetBrowserPair(id);

        // Remove from engine
        openspace::global::scriptEngine->queueScript(
            "openspace.removeScreenSpaceRenderable('" + browser + "');",
            scripting::ScriptEngine::RemoteScripting::Yes
        );

        openspace::global::scriptEngine->queueScript(
            "openspace.removeScreenSpaceRenderable('" + target + "');",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
    

    return 0;
}

int place3dSkyBrowser(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::place3dSkyBrowser");
    // Image index to place in 3D
    const int i = ghoul::lua::value<int>(L, 1);
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    const ImageData image = module->getWwtDataHandler()->getImage(i);

    module->place3dBrowser(image, i);

    return 0;
}

int removeSelectedImageInBrowser(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::removeSelectedImageInBrowser");
    const std::string id = ghoul::lua::value<std::string>(L, 1);
    // Image index
    const int i = ghoul::lua::value<int>(L, 1);
    // Get browser
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    const ImageData& image = module->getWwtDataHandler()->getImage(i);
        
    Pair* pair = module->getPair(id);
    if (pair) {
        pair->removeSelectedImage(i);
    }
    else if (module->get3dBrowser(id)) {
        module->get3dBrowser(id)->removeSelectedImage(i);
    }
    return 0;
}

int setEquatorialAim(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::setEquatorialAim");
    // Browser id
    const std::string id = ghoul::lua::value<std::string>(L, 1);
    double ra = ghoul::lua::value<double>(L, 1);
    double dec = ghoul::lua::value<double>(L, 1);

    // Get module
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    Pair* pair = module->getPair(id);
    if (pair) {
        pair->setEquatorialAim(glm::dvec2(ra, dec));
    }
    else if (module->get3dBrowser(id)) {
        module->get3dBrowser(id)->setEquatorialAim(glm::dvec2(ra, dec));
    }

    return 0;
}

int setVerticalFov(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::setVerticalFov");
    // Browser id
    const std::string id = ghoul::lua::value<std::string>(L, 1);
    float vfov = ghoul::lua::value<float>(L, 1);

    // Get module
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    Pair* pair = module->getPair(id);
    if (pair) {
        pair->setVerticalFov(vfov);
    }
    else if (module->get3dBrowser(id)) {
        module->get3dBrowser(id)->setVerticalFov(vfov);
    }

    return 0;
}

int setBorderColor(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 4, "lua::setBorderColor");
    // Browser id
    const std::string id = ghoul::lua::value<std::string>(L, 1);
    int r = ghoul::lua::value<int>(L, 1);
    int g = ghoul::lua::value<int>(L, 1);
    int b = ghoul::lua::value<int>(L, 1);
    glm::ivec3 color{ r, g, b };
    // Get module
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    Pair* pair = module->getPair(id);
    if (pair) {
        pair->setBorderColor(color);
    }
    else if (module->get3dBrowser(id)) {
        module->get3dBrowser(id)->setBorderColor(color);
    }

    return 0;
}

int setScreenSpaceSize(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::setBorderColor");
    // Browser id
    const std::string id = ghoul::lua::value<std::string>(L, 1);
    float sizeX = ghoul::lua::value<float>(L, 1);
    float sizeY = ghoul::lua::value<float>(L, 1);

    // Get module
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    Pair* pair = module->getPair(id);
    if (pair) {
        pair->setScreenSpaceSize(glm::vec2(sizeX, sizeY));
    }
    return 0;
}
	
}

