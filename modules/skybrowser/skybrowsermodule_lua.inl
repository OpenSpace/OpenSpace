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

	int loadImgCollection(lua_State* L) {
		// Load image
		ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::loadCollection");
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

            // The RA from WWT is in the unit hours: to convert to degrees, multiply with 360 (deg) /24 (h)=15
			glm::dvec3 imageCoordsGalactic = icrsToGalacticCartesian(resultImage.celestCoords.x * 15, resultImage.celestCoords.y, 1.0);	
			browser->getSkyTarget()->lookAtGalacticCoord(imageCoordsGalactic);

			// In WWT, VFOV = ZoomLevel * 6
			browser->setFieldOfView(resultImage.zoomLevel / 6);
		} 
		browser->sendMessageToWWT(browser->createMessageForSettingForegroundOpacityWWT(100));
		return 1;
	}
	
	int followCamera(lua_State* L) {
		// Load images from url
		ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::followCamera");

		//SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
		//std::string root = "https://raw.githubusercontent.com/WorldWideTelescope/wwt-web-client/master/assets/webclient-explore-root.wtml";

		//module->getWWTDataHandler()->loadWTMLCollectionsFromURL(root, "root");
		//LINFO(std::to_string( module->getWWTDataHandler()->loadAllImagesFromXMLs()));

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
		browser->sendMessageToWWT(browser->createMessageForLoadingWWTImgColl(root));
		return 1;
	}

	int createBrowser(lua_State* L) {
		// Send image list to GUI
		ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::createBrowser");
		SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
		// If no data has been loaded yet, load it!
		if (module->getWWTDataHandler()->getLoadedImages().size() == 0) {
			moveBrowser(L);
			//followCamera(L);
		}
	   
		const std::vector<ImageData>& images = module->getWWTDataHandler()->getLoadedImages();

		lua_newtable(L);

		for (int i = 0; i < images.size(); i++) {
			// Push a table { image name, image url } with index : number
			lua_newtable(L);
			lua_pushstring(L, images[i].name.c_str());
			lua_rawseti(L, -2, 1);
			lua_pushstring(L, images[i].thumbnailUrl.c_str());
			lua_rawseti(L, -2, 2);

			lua_rawseti(L, -2, i+1);
		}
		
		return 1;
	}
	int adjustCamera(lua_State* L) {
		ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::adjustCamera");

		return 1;
	}
	
}

