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
#include <thread> 


namespace {
    constexpr const char _loggerCat[] = "SkybrowserModule";
} // namespace


namespace openspace::skybrowser::luascriptfunctions {
    
    int followCamera(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::followCamera");

        SkybrowserModule* module = global::moduleEngine->module<SkybrowserModule>();
        //ghoul::Dictionary message = module->createMessageForPausingWWTTime();
        //module->sendMessageToWWT(message);
        std::thread thread(&SkybrowserModule::WWTfollowCamera, module);
        thread.detach();

        return 1;
    }

    int moveBrowser(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::moveBrowser");
        
        SkybrowserModule* module = global::moduleEngine->module<SkybrowserModule>();     
        ScreenSpaceBrowser* browser = dynamic_cast<ScreenSpaceBrowser*>(global::renderEngine->screenSpaceRenderable("ScreenSpaceBowser"));

        module->initializeBrowser(browser);     
        module->skyBrowser()->translate(glm::vec3(-0.8, -0.4, 0.0));

        return 1;
    }

    int createBrowser(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::createBrowser");
        ghoul::lua::value<std::string>(L, 1);

        SkybrowserModule* module = global::moduleEngine->module<SkybrowserModule>();


        using namespace std::string_literals;

        std::string node = "{"
            "Type = 'ScreenSpaceBrowser',"
            "Identifier = 'ScreenSpaceBowser',"
            "Name = 'Screen Space Bowser',"
            "Url = 'http://localhost:8000/',"
            "FaceCamera = false"
        "}";

        /*
        ghoul::Dictionary node;
        node.setValue("Type", "ScreenSpaceBrowser"s);
        node.setValue("Identifier", "ScreenSpaceBowser"s);
        node.setValue("Name", "Screen Space Bowser"s);
        node.setValue("Url", "http://localhost:8000/"s);

        */

        openspace::global::scriptEngine->queueScript(
            "openspace.addScreenSpaceRenderable(" + node + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );

        return 1;
    }
    
}

