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

namespace {
    constexpr const char _loggerCat[] = "SkybrowserModule";
} // namespace


namespace openspace::skybrowser::luascriptfunctions {
    
    int updateFunction(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::updateFunction");
        LINFOC(_loggerCat, "yabadadooo");

        ScreenSpaceBrowser* test = dynamic_cast<ScreenSpaceBrowser*>(global::renderEngine->screenSpaceRenderable("ScreenSpaceBowser"));
        test->testMessage();
        
        return 1;
    }

    int testFunction(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::testFunction");

        const SkybrowserModule* module = global::moduleEngine->module<SkybrowserModule>();
        LINFOC(_loggerCat, "hoho");
        LINFOC(_loggerCat, std::to_string(module->zoomFactor()));

        //std::string _url = "https://wallpaperaccess.com/full/3010132.jpg";
        // 'https://cdn.wallpapersafari.com/6/92/0nbCPw.jpg'
        /*
        // get url from user
        const std::string _url = ghoul::lua::value<std::string>(L, 1);
       */
        using namespace std::string_literals;

        ghoul::Dictionary node;
        node.setValue("Type", "ScreenSpaceBrowser"s);
        node.setValue("Identifier", "ScreenSpaceBowser"s);
        node.setValue("Name", "Screen Space Bowser"s);
        node.setValue("Url", "http://localhost:8000/?origin=localhost:4690"s);

        openspace::global::scriptEngine->queueScript(
            "openspace.addScreenSpaceRenderable(" + ghoul::formatLua(node) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        

        return 1;
    }
    
}

