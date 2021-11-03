#include <openspace/interaction/navigationhandler.h>
#include <openspace/documentation/documentation.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/globals.h>
#include <openspace/util/openspacemodule.h>
#include <openspace/util/camera.h>
#include <modules/base/rendering/screenspaceimagelocal.h>
#include <modules/base/rendering/renderableplaneimagelocal.h>
#include <modules/skybrowser/include/wwtdatahandler.h>
#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <modules/skybrowser/include/screenspaceskytarget.h>
#include <modules/skybrowser/include/renderableskybrowser.h>
#include <modules/skybrowser/include/pair.h>
#include <modules/skybrowser/skybrowsermodule.h>
#include <modules/skybrowser/include/utility.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/fmt.h>
#include <ghoul/glm.h>
#include <glm/gtx/vector_angle.hpp>
#include <glm/gtx/string_cast.hpp>
#include <fstream>
#include <sstream>
#include <thread> 
#include <limits>

namespace {
	constexpr const char _loggerCat[] = "SkyBrowserModule";
} // namespace


namespace openspace::skybrowser::luascriptfunctions {

	int selectImage(lua_State* L) {
		// Load image
		ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::selectImage");
		const int i = ghoul::lua::value<int>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

        if (module->cameraInSolarSystem()) {
            module->selectImage2dBrowser(i);
        }
        else {
            module->selectImage3dBrowser(i);
        }
        
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
        ScreenSpaceImageLocal* hoverCircle = dynamic_cast<ScreenSpaceImageLocal*>(
            global::renderEngine->screenSpaceRenderable("HoverCircle"));
        if (hoverCircle->isEnabled()) {
            hoverCircle->property("Enabled")->set(false);
        }
        
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
        const int i = ghoul::lua::value<int>(L, 2);
        int order = ghoul::lua::value<int>(L, 3);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

        if (module->getPair(id)) {
            module->getPair(id)->setImageOrder(i, order);
        }
        else if (module->get3dBrowser() != nullptr) {
            RenderableSkyBrowser* browser3d = dynamic_cast<RenderableSkyBrowser*>(
                module->get3dBrowser()->renderable());
            browser3d->setImageLayerOrder(i, order);
        }

        return 0;
    }
	
	int loadImagesToWWT(lua_State* L) {
		// Load images from url
		ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::loadImagesToWWT");
        const std::string id = ghoul::lua::value<std::string>(L, 1);
        LINFO("Connection established to WorldWide Telescope application in " + id);
        LINFO("Loading image collections to " + id);

        // Load the collections here because here we know that the browser can execute javascript
        std::string root = "https://raw.githubusercontent.com/WorldWideTelescope/wwt-web-client/master/assets/webclient-explore-root.wtml";
         
        ScreenSpaceSkyBrowser* browser = dynamic_cast<ScreenSpaceSkyBrowser*>(
            global::renderEngine->screenSpaceRenderable(id));
        if (browser && !browser->hasLoadedImages()) {
                browser->sendMessageToWwt(wwtmessage::loadCollection(root));
                browser->setHasLoadedImages(true);
        }
        else {
            SceneGraphNode* node = global::renderEngine->scene()->sceneGraphNode(id);
            if (node) {
                RenderableSkyBrowser* browser3d = dynamic_cast<RenderableSkyBrowser*>(
                    node->renderable());
                if (browser3d) {
                    // Load Image collections
                    browser3d->stopSyncingWwtView();
                    LINFO("Load images to " + browser3d->identifier());
                    browser3d->sendMessageToWwt(wwtmessage::loadCollection(root));
                    LINFO("Image collection loaded in " + browser3d->identifier());
                }
            }
        }

		return 0;
	}

    int sendOutIdsToBrowsers(lua_State* L) {
        // This is called when the sky_browser website is connected to OpenSpace
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::sendOutIdsToBrowsers");

        // Send out ID's to the browsers
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        std::vector<Pair> pairs = module->getPairs();
        for (Pair pair : pairs) {
            pair.getBrowser()->sendIdToBrowser();
        }
        SceneGraphNode* node = module->get3dBrowser();
        if(node) {
            std::string id = node->identifier();
            RenderableSkyBrowser* browsers3d = dynamic_cast<RenderableSkyBrowser*>(
                node->renderable());
            browsers3d->setIdInBrowser(id);
        }
        return 0;
    }

    int connectBrowserTarget(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::connectBrowserTarget");
        const std::string id = ghoul::lua::value<std::string>(L, 1);

        // Find the ScreenSpaceRenderable that has the id
        ScreenSpaceRenderable* found = global::renderEngine->screenSpaceRenderable(id);
        ScreenSpaceSkyBrowser* browser = dynamic_cast<ScreenSpaceSkyBrowser*>(found);
        ScreenSpaceSkyTarget* target = dynamic_cast<ScreenSpaceSkyTarget*>(found);

        // Connect it to its corresponding target / browser
        if (browser) {
            
            browser->connectToSkyTarget();
        }
        else if (target) {
            
            target->findSkyBrowser();
        }
        return 0;
    }

    int initializeBrowser(lua_State* L) {
        // Initialize browser with ID and its corresponding target
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::initializeBrowser");
        const std::string id = ghoul::lua::value<std::string>(L, 1);
        ScreenSpaceSkyBrowser* browser = dynamic_cast<ScreenSpaceSkyBrowser*>(
            global::renderEngine->screenSpaceRenderable(id));
        LINFO("Initializing sky browser " + id);
        if (browser) {
            browser->initializeBrowser();
            ScreenSpaceSkyTarget* target = browser->getSkyTarget();
            if (target) {
                target->findSkyBrowser();
            }
        }
        else {
            SceneGraphNode* node = global::renderEngine->scene()->sceneGraphNode(id);
            if (node) {
                RenderableSkyBrowser* browser3d = dynamic_cast<RenderableSkyBrowser*>(
                    node->renderable());
                if (browser3d && id == node->identifier()) {
                    // Initialize
                    LINFO("Initializing 3D sky browsers");
                    browser3d->syncWwtView();
                }
            }
        }

        return 0;
    }
    
    int add3dBrowserToSkyBrowserModule(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::add3dBrowserToSkyBrowserModule");
        const std::string id = ghoul::lua::value<std::string>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

        LINFO("Add to sky browser module id " + id);
        SceneGraphNode* node = global::renderEngine->scene()->sceneGraphNode(id);
        if (node) {
            // Add to module
            module->set3dBrowser(node);
        }

        return 0;
    }

    int addPairToSkyBrowserModule(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addPairToSkyBrowserModule");
        const std::string targetId = ghoul::lua::value<std::string>(L, 1);
        const std::string browserId = ghoul::lua::value<std::string>(L, 2);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

        LINFO("Add browser " + browserId + " to sky browser module.");
        LINFO("Add target " + targetId + " to sky browser module.");
        
        ScreenSpaceSkyTarget* target = dynamic_cast<ScreenSpaceSkyTarget*>(
            global::renderEngine->screenSpaceRenderable(targetId)
            );
        ScreenSpaceSkyBrowser* browser = dynamic_cast<ScreenSpaceSkyBrowser*>(
            global::renderEngine->screenSpaceRenderable(browserId)
            );


        module->addTargetBrowserPair(target, browser);

        return 0;
    }

	int getListOfImages(lua_State* L) {
		// Send image list to GUI
		ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::getListOfImages");
		SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

		// If no data has been loaded yet, download the data from the web!

		if (module->nLoadedImages() == 0) {
            std::string root = "https://raw.githubusercontent.com/WorldWideTelescope/wwt-web-client/master/assets/webclient-explore-root.wtml";
            //std::string hubble = "http://www.worldwidetelescope.org/wwtweb/catalog.aspx?W=hubble";
            std::string directory = absPath("${MODULE_SKYBROWSER}/WWTimagedata/");

            // 3D images
            std::string http = "${BASE}/sync/http/";
            std::string globular = "digitaluniverse_globularclusters_speck/2/gc.speck";
            std::string open = "digitaluniverse_openclusters_speck/2/oc.speck";
            // Load speck files for 3D positions
            std::filesystem::path globularClusters = absPath(http + globular);
            std::filesystem::path openClusters = absPath(http + open);
            std::vector<std::filesystem::path> specks = { openClusters, globularClusters };

            module->loadImages(root, directory, specks);
		}
	    
        // Create Lua table to send to the GUI
        lua_newtable(L);

		for (int i = 0; i < module->nLoadedImages(); i++) {
            const ImageData& img = module->getWWTDataHandler()->getImage(i);
            glm::dvec3 cartCoords = img.equatorialCartesian;
            glm::dvec3 position = img.position3d;

            // Conversions for ghoul
            std::vector<double> cartCoordsVec = { cartCoords.x, cartCoords.y, cartCoords.z };
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
        glm::dvec3 cartesianJ2000 = skybrowser::cameraDirectionEquatorial();
        glm::dvec2 sphericalJ2000 = skybrowser::cartesianToSpherical(cartesianJ2000);
        // Convert to vector so ghoul can read it
        std::vector<double> viewDirCelestVec = { cartesianJ2000.x, cartesianJ2000.y, cartesianJ2000.z };
       
        
        // Calculate the smallest FOV of vertical and horizontal
        glm::dvec2 fovs = skybrowser::fovWindow();
        double FOV = std::min(fovs.x, fovs.y);
        // Push window data
        ghoul::lua::push(L, "windowHFOV", FOV);
        lua_settable(L, -3);
        ghoul::lua::push(L, "cartesianDirection", viewDirCelestVec);
        lua_settable(L, -3);
        ghoul::lua::push(L, "ra", sphericalJ2000.x);
        lua_settable(L, -3);
        ghoul::lua::push(L, "dec", sphericalJ2000.y);
        lua_settable(L, -3);
        ghoul::lua::push(L, "selectedBrowserId", module->selectedBrowserId());
        lua_settable(L, -3);
        ghoul::lua::push(L, "cameraInSolarSystem", module->cameraInSolarSystem());
        lua_settable(L, -3);
        // Set table for the current ImageData
        lua_settable(L, -3);

        // Pass data for all the browsers and the corresponding targets
        if (module->cameraInSolarSystem()) {
            std::vector<Pair> pairs = module->getPairs();

            for (Pair pair : pairs) {
                ScreenSpaceSkyBrowser* browser = pair.getBrowser();
                ScreenSpaceSkyTarget* target = pair.getTarget();
                std::string id = browser->identifier();
                // Convert deque to vector so ghoul can read it
                std::vector<int> selectedImagesVector;
                std::deque<int> selectedImages = browser->getSelectedImages();
                std::for_each(selectedImages.begin(), selectedImages.end(), [&](int i) {
                    selectedImagesVector.push_back(i);
                    });
                
                glm::dvec3 celestialCart = target->targetDirectionEquatorial();
                glm::dvec2 celestialSpherical = skybrowser::cartesianToSpherical(celestialCart);
                   
                std::vector<double> celestialCartVec = { celestialCart.x, celestialCart.y, celestialCart.z };
                // Convert color to vector so ghoul can read it
                glm::ivec3 color = browser->borderColor();
                std::vector<int> colorVec = { color.r, color.g, color.b };

                ghoul::lua::push(L, id);
                lua_newtable(L);
                // Push ("Key", value)
                ghoul::lua::push(L, "id", id);
                lua_settable(L, -3);
                ghoul::lua::push(L, "name", browser->guiName());
                lua_settable(L, -3);
                ghoul::lua::push(L, "FOV", browser->verticalFov());
                lua_settable(L, -3);
                ghoul::lua::push(L, "selectedImages", selectedImagesVector);
                lua_settable(L, -3);
                ghoul::lua::push(L, "cartesianDirection", celestialCartVec);
                lua_settable(L, -3);
                ghoul::lua::push(L, "ra", celestialSpherical.x);
                lua_settable(L, -3);
                ghoul::lua::push(L, "dec", celestialSpherical.y);
                lua_settable(L, -3);
                ghoul::lua::push(L, "color", colorVec);
                lua_settable(L, -3);
                ghoul::lua::push(L, "isLocked", target->isLocked());
                lua_settable(L, -3);
              
                // Set table for the current target
                lua_settable(L, -3);
                
            }
        }
        else {
            SceneGraphNode* node = module->get3dBrowser();
            RenderableSkyBrowser* browser3d = dynamic_cast<RenderableSkyBrowser*>(
                node->renderable());
            // Convert deque to vector so ghoul can read it
            std::vector<int> selectedImagesVector;
            std::deque<int> selectedImages = browser3d->getSelectedImages();
            std::for_each(selectedImages.begin(), selectedImages.end(), [&](int index) {
                selectedImagesVector.push_back(index);
                });
            glm::dvec3 worldPosition = node->position();
            glm::dvec3 celestialCart = skybrowser::galacticToEquatorial(worldPosition);
            glm::dvec2 celestialSpherical = skybrowser::cartesianToSpherical(celestialCart);
            std::vector<double> celestialCartVec = { celestialCart.x, celestialCart.y, celestialCart.z };
            // Convert color to vector so ghoul can read it
            //glm::ivec3 color = browser->_borderColor.value();
            std::vector<int> colorVec = { 200, 200, 200 };

            ghoul::lua::push(L, browser3d->identifier());
            lua_newtable(L);
            // Push ("Key", value)
            ghoul::lua::push(L, "id", browser3d->identifier());
            lua_settable(L, -3);
            ghoul::lua::push(L, "name", node->guiName());
            lua_settable(L, -3);
            ghoul::lua::push(L, "FOV", browser3d->verticalFov());
            lua_settable(L, -3);
            ghoul::lua::push(L, "selectedImages", selectedImagesVector);
            lua_settable(L, -3);
            ghoul::lua::push(L, "cartesianDirection", celestialCartVec);
            lua_settable(L, -3);
            ghoul::lua::push(L, "ra", celestialSpherical.x);
            lua_settable(L, -3);
            ghoul::lua::push(L, "dec", celestialSpherical.y);
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

        if(module->cameraInSolarSystem() && module->getPair(id)) {
            Pair* pair = module->getPair(id);
            module->startRotation(pair->getTarget()->targetDirectionGalactic());
        }
        else if (!module->cameraInSolarSystem() && module->get3dBrowser()) {
            module->lookAt3dBrowser();
        }

		return 0;
	}

    int set3dSelectedImagesAs2dSelection(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::set3dSelectedImagesAs2dSelection");
        const std::string id = ghoul::lua::value<std::string>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        Pair* pair = module->getPair(id);

        if (pair && module->get3dBrowser()) { 
            RenderableSkyBrowser* browser3d = dynamic_cast<RenderableSkyBrowser*>(
                module->get3dBrowser()->renderable());
            // Empty 3D browser selection
            browser3d->getSelectedImages().clear();
            // Copy 2D selection of images to 3D browser
            std::deque images = pair->getBrowser()->getSelectedImages();
            std::for_each(std::begin(images), std::end(images), [&](int i) {
                const ImageData& image = module->getWWTDataHandler()->getImage(i);
                browser3d->displayImage(image, i);
                });
        }

        return 0;
    }

    int setOpacityOfImageLayer(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::setOpacityOfImageLayer");
        const std::string id = ghoul::lua::value<std::string>(L, 1);
        const std::string i = std::to_string(ghoul::lua::value<int>(L, 2));
        double opacity = ghoul::lua::value<double>(L, 3);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        ghoul::Dictionary message = wwtmessage::setImageOpacity(i, opacity);

        if (module->getPair(id)) {
            Pair* pair = module->getPair(id);
            pair->getBrowser()->sendMessageToWwt(message);
        }
        else if (module->get3dBrowser() != nullptr) {
            RenderableSkyBrowser* browser3d = dynamic_cast<RenderableSkyBrowser*>(
                module->get3dBrowser()->renderable());
            browser3d->sendMessageToWwt(message);
        }

        return 0;
    }

    int centerTargetOnScreen(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::centerTargetOnScreen");
        const std::string id = ghoul::lua::value<std::string>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        Pair* pair = module->getPair(id);
        if (pair) {
            // Animate the target to the center of the screen
            pair->getTarget()->unlock();
            // Get camera direction in celestial spherical coordinates
            glm::dvec3 viewDirection = skybrowser::cameraDirectionEquatorial();
            // Keep the current fov
            float currentFov = pair->getBrowser()->verticalFov();
            pair->getTarget()->startAnimation(viewDirection, currentFov, false);
        }
        
        return 0;
    }

    int setSelectedBrowser(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::setSelectedBrowser");
        const std::string id = ghoul::lua::value<std::string>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        if (module->getPair(id)) {
            module->setSelectedBrowser(id);
        }
        return 0;
    }

    int createTargetBrowserPair(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::createTargetBrowserPair");
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        module->createTargetBrowserPair();

        return 0;
    }

    int removeTargetBrowserPair(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::removeTargetBrowserPair");
        std::string id = ghoul::lua::value<std::string>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        module->removeTargetBrowserPair(id);

        return 0;
    }

    int place3dSkyBrowser(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::place3dSkyBrowser");
        // Image index to place in 3D
        const int i = ghoul::lua::value<int>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        const ImageData image = module->getWWTDataHandler()->getImage(i);

        // If the image has a 3D position, add it to the scene graph
        if (image.has3dCoords && module->get3dBrowser()) {
            RenderableSkyBrowser* browser = dynamic_cast<RenderableSkyBrowser*>(
                module->get3dBrowser()->renderable());
            browser->displayImage(image, i);
            browser->placeAt3dPosition(image);
        }
        else {
            LINFO("Image has no 3D coordinate!");
        }

        return 0;
    }

    int removeSelectedImageInBrowser(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::removeSelectedImageInBrowser");
        // Image index
        const int i = ghoul::lua::value<int>(L, 1);
        const std::string id = ghoul::lua::value<std::string>(L, 2);
        // Get browser
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        const ImageData& image = module->getWWTDataHandler()->getImage(i);
        

        if (module->getPair(id)) {
            Pair* pair = module->getPair(id);
            // Remove image
            pair->getBrowser()->removeSelectedImage(image, i);
        }
        else if (module->get3dBrowser() != nullptr) {
            RenderableSkyBrowser* browser3d = dynamic_cast<RenderableSkyBrowser*>(
                module->get3dBrowser()->renderable());
            browser3d->removeSelectedImage(image, i);
        }
        return 0;
    }
	
}

