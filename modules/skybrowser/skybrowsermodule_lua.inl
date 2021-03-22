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


namespace {
    constexpr const char _loggerCat[] = "SkyBrowserModule";
} // namespace


namespace openspace::skybrowser::luascriptfunctions {

    int loadImgCollection(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::loadCollection");
        // https://docs.worldwidetelescope.org/data-guide/1/data-file-formats/collections/sample-blank-collection.wtml
        std::string url = ghoul::lua::value<std::string>(L, 1);
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        module->skyBrowser()->sendMessageToWWT(module->skyBrowser()->createMessageForLoadingWWTImgColl(url));
        return 1;
    }
    
    int followCamera(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::followCamera");

        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        ScreenSpaceSkyTarget* target = dynamic_cast<ScreenSpaceSkyTarget*>(global::renderEngine->screenSpaceRenderable("ScreenSpaceTarget"));
        ScreenSpaceSkyBrowser* browser = dynamic_cast<ScreenSpaceSkyBrowser*>(global::renderEngine->screenSpaceRenderable("ScreenSpaceBowser"));

        module->initializeBrowser(browser, target);
        module->skyBrowser()->translate(glm::vec3(-0.8, -0.4, 0.0));

        browser->WWTfollowCamera();
        
        return 1;
    }

    int moveBrowser(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::moveBrowser");

        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();    
        ScreenSpaceSkyBrowser* browser = dynamic_cast<ScreenSpaceSkyBrowser*>(global::renderEngine->screenSpaceRenderable("ScreenSpaceBowser"));

        // target test
        module->createTarget(browser->getScreenSpaceBrowserDimension());

        return 1;
    }

    int createBrowser(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::createBrowser");

        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        module->createBrowser();

        return 1;
    }
    
}

