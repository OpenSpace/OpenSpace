#include <openspace/util/openspacemodule.h>
#include <openspace/documentation/documentation.h>
#include <modules/skybrowser/skybrowsermodule.h>
#include <modules/skybrowser/include/utility.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <fstream>
#include <sstream>
#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <modules/skybrowser/include/screenspaceskytarget.h>
#include <modules/base/rendering/screenspaceimagelocal.h>
#include <modules/base/rendering/renderableplaneimagelocal.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/util/camera.h>
#include <thread> 
#include <glm/gtx/string_cast.hpp>
#include <glm/gtx/vector_angle.hpp>
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
		ScreenSpaceSkyBrowser* browser = module->getSkyBrowsers()[module->getSelectedBrowserIndex()];
		
		const ImageData& resultImage = module->getWWTDataHandler()->getLoadedImages()[i];
		// Load image collection, if it isn't loaded already
        // TODO: Update or remove with new WWT API
		const std::vector<ImageCollection>& collections = module->getWWTDataHandler()->getAllImageCollectionUrls();
		auto it = std::find_if(collections.begin(), collections.end(), [&](const ImageCollection& coll) {
				return coll.name == resultImage.collection;
			});
		if (!it->loaded) {
			browser->sendMessageToWWT(browser->createMessageForLoadingWWTImgColl(it->url));
		}
        LINFO("Loading image " + resultImage.name);
		browser->sendMessageToWWT(browser->createMessageForSettingForegroundWWT(resultImage.name));
        browser->sendMessageToWWT(browser->createMessageForSettingForegroundOpacityWWT(100));
		
		// Only move target if the image has coordinates	 
		if (resultImage.hasCoords) {
            // Animate the target to the image coord position
            // In WWT, the definition of ZoomLevel is: VFOV = ZoomLevel / 6
            if (browser->getSkyTarget()) {
                browser->getSkyTarget()->unlock();
                browser->getSkyTarget()->startAnimation(resultImage.celestCoords, resultImage.zoomLevel / 6);
                glm::dvec3 imgCoordsOnScreen = J2000SphericalToScreenSpace(resultImage.celestCoords);
                glm::vec2 windowRatio = global::windowDelegate->currentWindowSize();
                float r = windowRatio.x / windowRatio.y;
                // Check if image coordinate is within current FOV
                if (!(abs(imgCoordsOnScreen.x) < r && abs(imgCoordsOnScreen.y) < 1.f && imgCoordsOnScreen.z < 0)
                    || imgCoordsOnScreen.z > 0) {
                    module->startRotation(resultImage.celestCoords);
                }

            }
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
        if (resultImage.hasCoords) {
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
        const int i = ghoul::lua::value<int>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        std::vector<ScreenSpaceSkyBrowser*> browsers = module->getSkyBrowsers();
        ScreenSpaceSkyTarget* target = browsers[i]->getSkyTarget();
        if (i < browsers.size()) {
            ScreenSpaceSkyTarget* target = browsers[i]->getSkyTarget();
            if (target) {
                target->lock();
            }
        }
        return 0;
    }

    int unlockTarget(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::unlockTarget");
        const int i = ghoul::lua::value<int>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        std::vector<ScreenSpaceSkyBrowser*> browsers = module->getSkyBrowsers();
        if (i < browsers.size()) {
            ScreenSpaceSkyTarget* target = browsers[i]->getSkyTarget();
            if (target) {
                target->unlock();
            }
        }
        return 0;
    }
	
	int followCamera(lua_State* L) {
		// Load images from url
		ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::followCamera");
        LINFO("Loading images from url");
		SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
		std::string root = "https://raw.githubusercontent.com/WorldWideTelescope/wwt-web-client/master/assets/webclient-explore-root.wtml";
        std::string hubble = "http://www.worldwidetelescope.org/wwtweb/catalog.aspx?W=hubble";

		module->getWWTDataHandler()->loadWTMLCollectionsFromURL(hubble, "root");
		LINFO(std::to_string( module->getWWTDataHandler()->loadAllImagesFromXMLs()));

		return 1;
	}

	int moveBrowser(lua_State* L) {
		// Load images from local directory
		ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::moveBrowser");
		SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
		module->getWWTDataHandler()->loadWTMLCollectionsFromDirectory(absPath("${MODULE_SKYBROWSER}/WWTimagedata/"));
		std::string noOfLoadedImgs = std::to_string(module->getWWTDataHandler()->loadAllImagesFromXMLs());
		LINFO("Loaded " + noOfLoadedImgs + " WorldWide Telescope images.");

		ScreenSpaceSkyBrowser* browser = dynamic_cast<ScreenSpaceSkyBrowser*>(global::renderEngine->screenSpaceRenderable("SkyBrowser1"));
        // Load all image collection urls
		//const std::vector<std::string>& imageUrls = module->getWWTDataHandler()->getAllImageCollectionUrls();
		//for (const std::string url : imageUrls) {
		//    browser->sendMessageToWWT(browser->createMessageForLoadingWWTImgColl(url));
		//}
		std::string root = "https://raw.githubusercontent.com/WorldWideTelescope/wwt-web-client/master/assets/webclient-explore-root.wtml";
        std::string hubble = "http://www.worldwidetelescope.org/wwtweb/catalog.aspx?W=hubble";
		browser->sendMessageToWWT(browser->createMessageForLoadingWWTImgColl(hubble));
		return 1;
	}

	int getListOfImages(lua_State* L) {
		// Send image list to GUI
		ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::getListOfImages");
		SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        // Load speck files for 3D positions
        std::filesystem::path globularClusters = absPath("${BASE}/sync/http/digitaluniverse_globularclusters_speck/2/gc.speck");
        std::filesystem::path openClusters = absPath("${BASE}/sync/http/digitaluniverse_openclusters_speck/2/oc.speck");

        speck::Dataset speckGlobularClusters = speck::loadSpeckFile(globularClusters);
        speck::Dataset speckOpenClusters = speck::loadSpeckFile(openClusters);

        module->getWWTDataHandler()->loadSpeckData(speckGlobularClusters);
        module->getWWTDataHandler()->loadSpeckData(speckOpenClusters);

		// If no data has been loaded yet, load it!
		if (module->getWWTDataHandler()->getLoadedImages().size() == 0) {
            // Read from disc
			//moveBrowser(L);
            // Read from URL
			 followCamera(L);
		}
	    
        // Create Lua table to send to the GUI
		const std::vector<ImageData>& images = module->getWWTDataHandler()->getLoadedImages();
        lua_newtable(L);

		for (int i = 0; i < images.size(); i++) {
            std::string name = images[i].name != "" ? images[i].name : "undefined";
            std::string url = images[i].thumbnailUrl != "" ? images[i].thumbnailUrl : "undefined";
            glm::dvec3 cartCoords = skybrowser::sphericalToCartesian(images[i].celestCoords);
            std::vector<double> cartCoordsVec = { cartCoords.x, cartCoords.y, cartCoords.z };
            
            // Index for current ImageData 
            ghoul::lua::push(L, i + 1); 
            lua_newtable(L);
            // Push ("Key", value)
            ghoul::lua::push(L, "Name", name);
            lua_settable(L, -3);
            ghoul::lua::push(L, "Thumbnail", url);
            lua_settable(L, -3);
            ghoul::lua::push(L, "RA", images[i].celestCoords.x);
            lua_settable(L, -3);
            ghoul::lua::push(L, "Dec", images[i].celestCoords.y);
            lua_settable(L, -3);
            ghoul::lua::push(L, "CartesianDirection", cartCoordsVec);
            lua_settable(L, -3);
            ghoul::lua::push(L, "HasCoords", images[i].hasCoords);
            lua_settable(L, -3);
            ghoul::lua::push(L, "Credits", images[i].credits);
            lua_settable(L, -3);
            ghoul::lua::push(L, "CreditsUrl", images[i].creditsUrl);
            lua_settable(L, -3);
            ghoul::lua::push(L, "Index", i);
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
        int index = 1;
        
        // Add the window data for OpenSpace
        ghoul::lua::push(L, index);
        index++;
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
        ghoul::lua::push(L, "WindowHFOV", FOV);
        lua_settable(L, -3);
        ghoul::lua::push(L, "CartesianDirection", viewDirCelestVec);
        lua_settable(L, -3);
        ghoul::lua::push(L, "RA", sphericalJ2000.x);
        lua_settable(L, -3);
        ghoul::lua::push(L, "Dec", sphericalJ2000.y);
        lua_settable(L, -3);
        ghoul::lua::push(L, "SelectedBrowserIndex", module->getSelectedBrowserIndex());
        lua_settable(L, -3);
        // Set table for the current ImageData
        lua_settable(L, -3);

        // Pass data for all the browsers and the corresponding targets
         std::vector<ScreenSpaceSkyBrowser*> browsers = module->getSkyBrowsers();

        for (ScreenSpaceSkyBrowser* browser : browsers) {
            // Only add browsers that have an initialized target
            ScreenSpaceSkyTarget* target = browser->getSkyTarget();
            if (target) {
                glm::dvec2 celestialSpherical = target->getTargetDirectionCelestial();
                glm::dvec3 celestialCart = skybrowser::sphericalToCartesian(celestialSpherical);
                std::vector<double> celestialCartVec = { celestialCart.x, celestialCart.y, celestialCart.z };
                // Convert color to vector so ghoul can read it
                glm::ivec3 color = browser->_borderColor.value();
                std::vector<int> colorVec = { color.r, color.g, color.b };

                ghoul::lua::push(L, index);
                index++;
                lua_newtable(L);
                // Push ("Key", value)
                ghoul::lua::push(L, "FOV", browser->fieldOfView());
                lua_settable(L, -3);
                ghoul::lua::push(L, "CartesianDirection", celestialCartVec);
                lua_settable(L, -3);
                ghoul::lua::push(L, "RA", celestialSpherical.x);
                lua_settable(L, -3);
                ghoul::lua::push(L, "Dec", celestialSpherical.y);
                lua_settable(L, -3);
                ghoul::lua::push(L, "Color", colorVec);
                lua_settable(L, -3);

                // Set table for the current target
                lua_settable(L, -3);
            }
        }
            

        return 1;
    }
	int adjustCamera(lua_State* L) {
		ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::adjustCamera");
        const int i = ghoul::lua::value<int>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        if (module->getSkyBrowsers().size() > i) {
            module->startRotation(module->getSkyBrowsers()[i]->getSkyTarget()->getTargetDirectionCelestial());
        }

		return 0;
	}

    int setSelectedBrowser(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::setSelectedBrowser");
        const int i = ghoul::lua::value<int>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        if (module->getSkyBrowsers().size() < i) {
            module->setSelectedBrowser(i);
        }
        return 0;
    }
	
}

