#include <openspace/util/openspacemodule.h>
#include <openspace/documentation/documentation.h>
#include <modules/skybrowser/skybrowsermodule.h>
#include <modules/skybrowser/include/utility.h>
#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <modules/skybrowser/include/screenspaceskytarget.h>
#include <modules/skybrowser/include/renderableskybrowser.h>
#include <modules/base/rendering/screenspaceimagelocal.h>
#include <modules/base/rendering/renderableplaneimagelocal.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/util/camera.h>
#include <openspace/util/distanceconstants.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <glm/gtx/string_cast.hpp>
#include <glm/gtx/vector_angle.hpp>
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
		ScreenSpaceSkyBrowser* selectedBrowser = module->getSkyBrowsers()[module->selectedBrowserId()];
        if (selectedBrowser) {
            ImageData& resultImage = module->getWWTDataHandler()->getLoadedImages()[i];

            // Load image, if the image has not been loaded yet
            if (resultImage.id == ImageData::NO_ID) {
                LINFO("Loading image " + resultImage.name);
                selectedBrowser->addSelectedImage(resultImage, i);
            }

            ScreenSpaceSkyTarget* selectedTarget = selectedBrowser->getSkyTarget();
            // If the image has coordinates, move the target
            if (resultImage.hasCelestCoords && selectedTarget) {
                // Animate the target to the image coord position
                selectedTarget->unlock();
                selectedTarget->startAnimation(resultImage.celestCoords, resultImage.fov);
                // Check if image coordinate is within current FOV
                glm::dvec3 imgCoordsOnScreen = J2000SphericalToScreenSpace(resultImage.celestCoords);
                glm::vec2 windowRatio = global::windowDelegate->currentWindowSize();
                float r = windowRatio.x / windowRatio.y;
                bool coordIsWithinView = (abs(imgCoordsOnScreen.x) < r && abs(imgCoordsOnScreen.y) < 1.f && imgCoordsOnScreen.z < 0);
                bool coordIsBehindCamera = imgCoordsOnScreen.z > 0;
                // If the coordinate is not in view, rotate camera
                if (!coordIsWithinView || coordIsBehindCamera) {
                    module->startRotation(resultImage.celestCoords);
                } 
            }
        }
        else {
            LINFO("No browser selected!");
        }
        
		return 0;
	}

    int moveCircleToHoverImage(lua_State* L) {
        // Load image
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::moveCircleToHoverImage");
        const int i = ghoul::lua::value<int>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        const ImageData& resultImage = module->getWWTDataHandler()->getLoadedImages()[i];

        // Only move and show circle if the image has coordinates
        if (resultImage.hasCelestCoords) {
            // Make circle visible
            ScreenSpaceImageLocal* hoverCircle = dynamic_cast<ScreenSpaceImageLocal*>(global::renderEngine->screenSpaceRenderable("HoverCircle"));
            hoverCircle->property("Enabled")->set(true);
            // Calculate coords for the circle and translate
            glm::vec3 imageCoordsScreenSpace = skybrowser::J2000SphericalToScreenSpace(resultImage.celestCoords);
            hoverCircle->property("CartesianPosition")->set(imageCoordsScreenSpace);
        }
        
        return 0;
    }

    int disableHoverCircle(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::disableHoverCircle");
        ScreenSpaceImageLocal* hoverCircle = dynamic_cast<ScreenSpaceImageLocal*>(global::renderEngine->screenSpaceRenderable("HoverCircle"));
        hoverCircle->property("Enabled")->set(false);
        
        return 0;
    }

    int lockTarget(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::lockTarget");
        const std::string id = ghoul::lua::value<std::string>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        if (module->browserIdExists(id)) {
            ScreenSpaceSkyTarget* target = module->getSkyBrowsers()[id]->getSkyTarget();
            if (target) {
                target->lock();
            }
        }
        return 0;
    }

    int unlockTarget(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::unlockTarget");
        const std::string id = ghoul::lua::value<std::string>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        if (module->browserIdExists(id)) {
            ScreenSpaceSkyTarget* target = module->getSkyBrowsers()[id]->getSkyTarget();
            if (target) {
                target->unlock();
            }
        }
        return 0;
    }
	
	int loadImagesToWWT(lua_State* L) {
		// Load images from url
		ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::loadImagesToWWT");
        const std::string id = ghoul::lua::value<std::string>(L, 1);
        LINFO("Connection established to WorldWide Telescope application in " + id);
        LINFO("Loading image collections to " + id);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

        // Load the collections here because here we know that the browser can execute javascript
         std::string root = "https://raw.githubusercontent.com/WorldWideTelescope/wwt-web-client/master/assets/webclient-explore-root.wtml";
         if (module->browserIdExists(id)) {
             ScreenSpaceSkyBrowser* browser = module->getSkyBrowsers()[id];
             if (!browser->hasLoadedCollections()) {
                 browser->sendMessageToWWT(wwtmessage::loadCollection(root));
                 browser->setHasLoadedCollections(true);
             }
         }

		return 0;
	}

    int sendOutIdsToBrowsers(lua_State* L) {
        // This is called when the sky_browser website is connected to OpenSpace
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::sendOutIdsToBrowsers");

        // Send out ID's to the browsers
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        std::map<std::string, ScreenSpaceSkyBrowser*> browsers = module->getSkyBrowsers();
        for (std::pair<std::string, ScreenSpaceSkyBrowser*> pair : browsers) {
            pair.second->setIdInBrowser();
        }

        return 0;
    }

    int connectBrowserTarget(lua_State* L) {
        // In order to connect, the target and browsers must have been loaded into the 
        // module. This is to ensure that the renderables have been found 

        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::connectBrowserTarget");
        const std::string id = ghoul::lua::value<std::string>(L, 1);

        // Find the screenspace renderable that has the id
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        std::vector<ScreenSpaceRenderable*> renderables = module->getBrowsersAndTargets();
        auto found = std::find_if(std::begin(renderables), std::end(renderables), 
            [&](ScreenSpaceRenderable* renderable) {
                return renderable && id == renderable->identifier();
            });
        if (dynamic_cast<ScreenSpaceSkyBrowser*>(*found)) {
            ScreenSpaceSkyBrowser* browser = dynamic_cast<ScreenSpaceSkyBrowser*>(*found);
            browser->setConnectedTarget();
        }
        else if (dynamic_cast<ScreenSpaceSkyTarget*>(*found)) {
            ScreenSpaceSkyTarget* target = dynamic_cast<ScreenSpaceSkyTarget*>(*found);
            target->setConnectedBrowser();
        }
        return 0;
    }

    int initializeBrowserAndTarget(lua_State* L) {
        // Initialize browser with ID and its corresponding target
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::initializeBrowserAndTarget");
        const std::string id = ghoul::lua::value<std::string>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        if (module->browserIdExists(id)) {
            module->getSkyBrowsers()[id]->initializeBrowser();
            ScreenSpaceSkyTarget* target = module->getSkyBrowsers()[id]->getSkyTarget();
            if (target) {
                target->initializeWithBrowser();
            }
        }

        return 0;
    }
    
    int addToSkyBrowserModule(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::addToSkyBrowserModule");
        const std::string id = ghoul::lua::value<std::string>(L, 1);
        LINFO("Add to sky browser module id " + id);
        ScreenSpaceRenderable* object = global::renderEngine->screenSpaceRenderable(id);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        module->addRenderable(object);

        return 0;
    }

	int getListOfImages(lua_State* L) {
		// Send image list to GUI
		ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::getListOfImages");
		SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

        std::string root = "https://raw.githubusercontent.com/WorldWideTelescope/wwt-web-client/master/assets/webclient-explore-root.wtml";
        std::string hubble = "http://www.worldwidetelescope.org/wwtweb/catalog.aspx?W=hubble";
        std::string directory = absPath("${MODULE_SKYBROWSER}/WWTimagedata/");

		// If no data has been loaded yet, download the data from the web!
		if (module->getWWTDataHandler()->getLoadedImages().size() == 0) {
            module->loadImages(root, directory);
		}
	    
        // Create Lua table to send to the GUI
		const std::vector<ImageData>& images = module->getWWTDataHandler()->getLoadedImages();
        lua_newtable(L);

		for (int i = 0; i < images.size(); i++) {
            std::string name = images[i].name != "" ? images[i].name : "undefined";
            std::string thumbnail = images[i].thumbnailUrl != "" ? images[i].thumbnailUrl : "undefined";
            glm::dvec3 cartCoords = skybrowser::sphericalToCartesian(images[i].celestCoords);
            std::vector<double> cartCoordsVec = { cartCoords.x, cartCoords.y, cartCoords.z };
            glm::dvec3 position = images[i].position3d;
            std::vector<double> position3d = { position.x, position.y, position.z };
            
            // Index for current ImageData 
            ghoul::lua::push(L, i + 1); 
            lua_newtable(L);
            // Push ("Key", value)
            ghoul::lua::push(L, "name", name);
            lua_settable(L, -3);
            ghoul::lua::push(L, "thumbnail", thumbnail);
            lua_settable(L, -3);
            ghoul::lua::push(L, "ra", images[i].celestCoords.x);
            lua_settable(L, -3);
            ghoul::lua::push(L, "dec", images[i].celestCoords.y);
            lua_settable(L, -3);
            ghoul::lua::push(L, "cartesianDirection", cartCoordsVec);
            lua_settable(L, -3);
            ghoul::lua::push(L, "hasCelestialCoords", images[i].hasCelestCoords);
            lua_settable(L, -3);
            ghoul::lua::push(L, "credits", images[i].credits);
            lua_settable(L, -3);
            ghoul::lua::push(L, "creditsUrl", images[i].creditsUrl);
            lua_settable(L, -3);
            ghoul::lua::push(L, "identifier", std::to_string(i));
            lua_settable(L, -3);
            ghoul::lua::push(L, "has3dCoords", images[i].has3dCoords);
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
        // Get the view direction of the screen in cartesian J2000 coordinates
        glm::dvec3 camPos = global::navigationHandler->camera()->positionVec3();
        constexpr double infinity = std::numeric_limits<float>::max();
        glm::dvec3 galCoord = camPos + (infinity * global::navigationHandler->camera()->viewDirectionWorldSpace());
        glm::dvec3 cartesianJ2000 = skybrowser::galacticCartesianToJ2000Cartesian(galCoord);
        glm::dvec2 sphericalJ2000 = skybrowser::cartesianToSpherical(cartesianJ2000);
        // Convert to vector so ghoul can read it
        std::vector<double> viewDirCelestVec = { cartesianJ2000.x, cartesianJ2000.y, cartesianJ2000.z };
       
        

        // Calculate the smallest FOV of vertical and horizontal
        float HFOV = global::windowDelegate->getHorizFieldOfView();
        glm::vec2 windowRatio = global::windowDelegate->currentWindowSize();
        float VFOV = HFOV * (windowRatio.y / windowRatio.x);
        double FOV = std::min(HFOV, VFOV);
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
         std::map<std::string, ScreenSpaceSkyBrowser*> browsers = module->getSkyBrowsers();

        for (std::pair<std::string, ScreenSpaceSkyBrowser*> pair : browsers) {
            ScreenSpaceSkyBrowser* browser = pair.second;
            std::string id = pair.first;
            // Convert deque to vector so ghoul can read it
            std::vector<int> selectedImagesVector;
            std::deque<int> selectedImages = browser->selectedImages();
            std::for_each(selectedImages.begin(), selectedImages.end(), [&](int index) {
                selectedImagesVector.push_back(index);
                });
            // Only add browsers that have an initialized target
            ScreenSpaceSkyTarget* target = browser->getSkyTarget();
            if (target) {
                glm::dvec2 celestialSpherical = target->getTargetDirectionCelestial();
                glm::dvec3 celestialCart = skybrowser::sphericalToCartesian(celestialSpherical);
                std::vector<double> celestialCartVec = { celestialCart.x, celestialCart.y, celestialCart.z };
                // Convert color to vector so ghoul can read it
                glm::ivec3 color = browser->_borderColor.value();
                std::vector<int> colorVec = { color.r, color.g, color.b };

                ghoul::lua::push(L, id);
                lua_newtable(L);
                // Push ("Key", value)
                ghoul::lua::push(L, "id", id);
                lua_settable(L, -3);
                ghoul::lua::push(L, "name", browser->guiName());
                lua_settable(L, -3);
                ghoul::lua::push(L, "FOV", browser->fieldOfView());
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
        }
            

        return 1;
    }
	int adjustCamera(lua_State* L) {
		ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::adjustCamera");
        const std::string id = ghoul::lua::value<std::string>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        if (module->browserIdExists(id)) {
            ScreenSpaceSkyTarget* target = module->getSkyBrowsers()[id]->getSkyTarget();
            if (target) {
                module->startRotation(target->getTargetDirectionCelestial());
            }
        }

		return 0;
	}
    int setOpacityOfImageLayer(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 3, "lua::setOpacityOfImageLayer");
        const std::string browserId = ghoul::lua::value<std::string>(L, 1);
        const int i = ghoul::lua::value<int>(L, 2);
        double opacity = ghoul::lua::value<double>(L, 3);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

        if (module->browserIdExists(browserId)) {
            ImageData& image = module->getWWTDataHandler()->getLoadedImages()[i];
            ghoul::Dictionary message = wwtmessage::setLayerOpacity(image, opacity);
            module->getSkyBrowsers()[browserId]->sendMessageToWWT(message);
        }
        return 0;
    }

    int setSelectedBrowser(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::setSelectedBrowser");
        const std::string id = ghoul::lua::value<std::string>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        if (module->browserIdExists(id)) {
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

    int create3dSkyBrowser(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::create3dSkyBrowser");
        // Image index to place in 3D
        const int i = ghoul::lua::value<int>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        ImageData image = module->getWWTDataHandler()->getLoadedImages()[i];

        // If the image has a 3D position, add it to the scene graph
        if (image.has3dCoords) {
            module->create3dBrowser(image);
        }
        else {
            LINFO("Image has no 3D coordinate!");
        }

        return 0;
    }

    int remove3dSkyBrowser(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::remove3dSkyBrowser");
        // Image index to place in 3D
        std::string id = ghoul::lua::value<std::string>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>(); 
        module->remove3dBrowser(id);
        
        return 0;
    }

    int removeSelectedImageInBrowser(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::removeSelectedImageInBrowser");
        // Image index
        const int i = ghoul::lua::value<int>(L, 1);
        const std::string browserId = ghoul::lua::value<std::string>(L, 2);
        // Get browser
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        ScreenSpaceSkyBrowser* browser = module->getSkyBrowsers()[browserId];
        ImageData& resultImage = module->getWWTDataHandler()->getLoadedImages()[i];
        // Remove image
        browser->removeSelectedImage(resultImage, i);

        return 0;
    }
	
}

