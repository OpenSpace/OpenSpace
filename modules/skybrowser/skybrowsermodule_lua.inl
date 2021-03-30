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
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::loadCollection");

        ScreenSpaceSkyBrowser* browser = dynamic_cast<ScreenSpaceSkyBrowser*>(global::renderEngine->screenSpaceRenderable("SkyBrowser1"));
        std::string url = "http://www.worldwidetelescope.org/wwtweb/catalog.aspx?W=wise";
        browser->sendMessageToWWT(browser->createMessageForLoadingWWTImgColl(url));
        browser->sendMessageToWWT(browser->createMessageForSettingForegroundWWT("Andromeda Galaxy"));
       // browser->sendMessageToWWT(browser->createMessageForMovingWWTCamera(glm::vec2(0.712305533333333, 41.269167), 24.0f));
        browser->sendMessageToWWT(browser->createMessageForSettingForegroundOpacityWWT(100));
        return 1;
    }
    
    int followCamera(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::followCamera");

        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        std::string url = "http://www.worldwidetelescope.org/wwtweb/catalog.aspx?W=vampfeeds";
        url = "http://www.worldwidetelescope.org/wwtweb/catalog.aspx?W=wise";
        std::string fileDestination = absPath("${MODULE_SKYBROWSER}/WWTimagedata/wise.aspx");
        module->loadImages(url, fileDestination);
        
        return 1;
    }

    int moveBrowser(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::moveBrowser");
        return 1;
    }

    int createBrowser(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::createBrowser");


        return 1;
    }
    
}

