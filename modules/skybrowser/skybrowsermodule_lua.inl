#include <openspace/util/openspacemodule.h>


#include <openspace/documentation/documentation.h>
#include <modules/skybrowser/skybrowsermodule.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>

#include <openspace/scripting/scriptengine.h>
#include <ghoul/misc/dictionaryluaformatter.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <fstream>
#include <sstream>
#include <modules/webbrowser/include/screenspacebrowser.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/util/camera.h>

namespace {
    constexpr const char _loggerCat[] = "SkybrowserModule";
} // namespace


namespace openspace::skybrowser::luascriptfunctions {
    
    int followCamera(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::followCamera");

        const SkybrowserModule* module = global::moduleEngine->module<SkybrowserModule>();
        module->WWTfollowCamera();
        
        return 1;
    }

    int moveBrowser(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::moveBrowser");
        ScreenSpaceBrowser* browser = dynamic_cast<ScreenSpaceBrowser*>(global::renderEngine->screenSpaceRenderable("ScreenSpaceBowser"));
        browser->translate(glm::vec3(-0.8, -0.4, 0.0));
        return 1;
    }

    int createBrowser(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::createBrowser");
        ghoul::lua::value<std::string>(L, 1);

        const SkybrowserModule* module = global::moduleEngine->module<SkybrowserModule>();

        //std::string _url = "https://wallpaperaccess.com/full/3010132.jpg";
        // 'https://cdn.wallpapersafari.com/6/92/0nbCPw.jpg'

        using namespace std::string_literals;

        ghoul::Dictionary node;
        node.setValue("Type", "ScreenSpaceBrowser"s);
        node.setValue("Identifier", "ScreenSpaceBowser"s);
        node.setValue("Name", "Screen Space Bowser"s);
        node.setValue("Url", "http://localhost:7800/sky_browser/index.html"s);
       // node.setValue("Dimensions", "glm::ivec2(1000, 1000)");

        openspace::global::scriptEngine->queueScript(
            "openspace.addScreenSpaceRenderable(" + ghoul::formatLua(node) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        return 1;
    }
    
}

