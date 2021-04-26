#include <openspace/util/openspacemodule.h>
#include <openspace/documentation/documentation.h>
#include <modules/skybrowser/skybrowsermodule.h>
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
#include <openspace/interaction/navigationhandler.h>
#include <openspace/util/camera.h>
#include <thread> 
#include <glm/gtx/string_cast.hpp>
#include <openspace/util/coordinateconversion.h>
#include <glm/gtx/vector_angle.hpp>

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
		const std::vector<ImageCollection>& collections = module->getWWTDataHandler()->getAllImageCollectionUrls();
		auto it = std::find_if(collections.begin(), collections.end(), [&](const ImageCollection& coll) {
				return coll.name == resultImage.collection;
			});
		if (!it->loaded) {
			browser->sendMessageToWWT(browser->createMessageForLoadingWWTImgColl(it->url));
		}
		browser->sendMessageToWWT(browser->createMessageForSettingForegroundWWT(resultImage.name));

		LINFO("Loading image " + resultImage.name);
		// Only move camera if the image has coordinates
		 
		if (resultImage.hasCoords) {

			glm::dvec3 imageCoordsGalactic = module->icrsToGalacticCartesian(resultImage.celestCoords.x, resultImage.celestCoords.y, 1.0);	
			browser->getSkyTarget()->lookAtGalacticCoord(imageCoordsGalactic);

			// In WWT, the definition of ZoomLevel is: VFOV = ZoomLevel / 6
			browser->setVerticalFieldOfView(resultImage.zoomLevel / 6);
		} 
		browser->sendMessageToWWT(browser->createMessageForSettingForegroundOpacityWWT(100));
		return 0;
	}

    int moveCircleToHoverImage(lua_State* L) {
        // Load image
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::moveCircleToHoverImage");
        const int i = ghoul::lua::value<int>(L, 1);

        ScreenSpaceImageLocal* hoverCircle = dynamic_cast<ScreenSpaceImageLocal*>(global::renderEngine->screenSpaceRenderable("HoverCircle"));
        hoverCircle->property("Enabled")->set(true);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        const ImageData& resultImage = module->getWWTDataHandler()->getLoadedImages()[i];

        if (resultImage.hasCoords) {

            glm::dvec3 imageCoordsGalactic = module->icrsToGalacticCartesian(resultImage.celestCoords.x, resultImage.celestCoords.y, 1.0);
            // This is a duplicate of target function "lookAtGalacticCoord". Probably should rewrite this so there are no duplicates...
            glm::dmat4 cameraInvRotMat = global::navigationHandler->camera()->viewRotationMatrix();
            glm::dvec3 viewDirectionLocal = cameraInvRotMat * glm::dvec4(imageCoordsGalactic, 1.f);

            glm::dvec2 angleCoordsLocal = glm::dvec2(atan(viewDirectionLocal.x / viewDirectionLocal.z), atan(viewDirectionLocal.y / viewDirectionLocal.z));
            double projPlaneDistance = -2.1f;
            glm::dvec2 imageCoordsScreenSpace = glm::dvec2(projPlaneDistance * tan(angleCoordsLocal.x), projPlaneDistance * tan(angleCoordsLocal.y));
            // Translate target
            hoverCircle->translate(glm::vec2(imageCoordsScreenSpace) - hoverCircle->getScreenSpacePosition(), hoverCircle->getScreenSpacePosition());
        }
        
        return 0;
    }

    int disableHoverCircle(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::disableHoverCircle");
        ScreenSpaceImageLocal* hoverCircle = dynamic_cast<ScreenSpaceImageLocal*>(global::renderEngine->screenSpaceRenderable("HoverCircle"));
        hoverCircle->property("Enabled")->set(false);
        
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

        ScreenSpaceSkyBrowser* browser = dynamic_cast<ScreenSpaceSkyBrowser*>(global::renderEngine->screenSpaceRenderable("SkyBrowser1"));
        ScreenSpaceSkyTarget* target = dynamic_cast<ScreenSpaceSkyTarget*>(global::renderEngine->screenSpaceRenderable("SkyTarget1"));

        float FOV = browser->fieldOfView();
        
        glm::vec2 coords = target->getCelestialCoords();
        lua_newtable(L);

        // Index for many browsers 
        // For now it's only one
        ghoul::lua::push(L, 1);
        lua_newtable(L);
        // Push ("Key", value)
        ghoul::lua::push(L, "FOV", FOV);
        lua_settable(L, -3);
        ghoul::lua::push(L, "RA", coords.x);
        lua_settable(L, -3);
        ghoul::lua::push(L, "Dec", coords.y);
        lua_settable(L, -3);
       
        // Set table for the current ImageData
        lua_settable(L, -3);

        return 1;
    }
	int adjustCamera(lua_State* L) {
		ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::adjustCamera");

		return 0;
	}
	
}

