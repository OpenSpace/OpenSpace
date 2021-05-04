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
	   
		ScreenSpaceSkyBrowser* browser = dynamic_cast<ScreenSpaceSkyBrowser*>(global::renderEngine->screenSpaceRenderable("SkyBrowser1"));
		SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
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
            glm::dvec2 imageCoordsScreenSpace = skybrowser::J2000ToScreenSpace(resultImage.celestCoords.x, resultImage.celestCoords.y);
            browser->getSkyTarget()->property("CartesianPosition")->set(glm::vec3 {imageCoordsScreenSpace, skybrowser::SCREENSPACE_Z });
                 
			// In WWT, the definition of ZoomLevel is: VFOV = ZoomLevel / 6
			browser->setVerticalFieldOfView(resultImage.zoomLevel / 6); 
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
            glm::vec2 imageCoordsScreenSpace = skybrowser::J2000ToScreenSpace(resultImage.celestCoords.x, resultImage.celestCoords.y);
            hoverCircle->property("CartesianPosition")->set(glm::vec3 {imageCoordsScreenSpace, skybrowser::SCREENSPACE_Z });
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
        // Pass data for all the browsers and the corresponding targets
        std::vector<ScreenSpaceSkyBrowser*> browsers = module->getSkyBrowsers();

        for (int i = 0; i < browsers.size(); i++) {
            // Only add browsers that have an initialized target
            ScreenSpaceSkyTarget* target = browsers[i]->getSkyTarget();
            if (target) {
                glm::dvec3 coords = target->getTargetDirectionGalactic();
                glm::dvec2 celestCoords = skybrowser::galacticCartesianToJ2000(coords);
                // Convert color to vector so ghoul can read it
                glm::ivec3 color = browsers[i]->_borderColor.value();
                std::vector<int> colorVec = { color.r, color.g, color.b };

                ghoul::lua::push(L, index);
                index++;
                lua_newtable(L);
                // Push ("Key", value)
                ghoul::lua::push(L, "FOV", browsers[i]->fieldOfView());
                lua_settable(L, -3);
                ghoul::lua::push(L, "RA", celestCoords.x);
                lua_settable(L, -3);
                ghoul::lua::push(L, "Dec", celestCoords.y);
                lua_settable(L, -3);
                ghoul::lua::push(L, "Color", colorVec);
                lua_settable(L, -3);

                // Set table for the current ImageData
                lua_settable(L, -3);
            }
        }
            

        return 1;
    }
	int adjustCamera(lua_State* L) {
		ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::adjustCamera");
		return 0;
	}
	
}

