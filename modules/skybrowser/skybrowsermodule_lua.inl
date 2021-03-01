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
    
    int updateFunction(lua_State* L) {
        // Get FOV from argument
        ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::updateFunction");
        float fov = ghoul::lua::value<float>(L, 1);

        // Get camera position
        //const glm::dvec3 cameraPosition = global::navigationHandler->camera()->positionVec3();
        const glm::dvec3 cameraPosition = global::navigationHandler->camera()->viewDirectionWorldSpace();

        // Convert to celestial coordinates
        const SkybrowserModule* module = global::moduleEngine->module<SkybrowserModule>();
        glm::dvec2 celestCoords = module->convertGalacticToCelestial(glm::dvec3(cameraPosition[0], cameraPosition[1], cameraPosition[2]));

        // Execute javascript on browser
        ScreenSpaceBrowser* browser = dynamic_cast<ScreenSpaceBrowser*>(global::renderEngine->screenSpaceRenderable("ScreenSpaceBowser"));
        std::string script = "vm.onMessage({event: 'center_on_coordinates', ra : Number(" + std::to_string(celestCoords[0]) + "), dec : Number(" + std::to_string(celestCoords[1]) + "), fov : Number(" + std::to_string(fov) + "), instant : false})";
        browser->executeJavascript(script);
        
        return 1;
    }

    int testFunction(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::testFunction");

        const SkybrowserModule* module = global::moduleEngine->module<SkybrowserModule>();
        glm::dvec3 testvec = glm::dvec3(0.0, 0.0, 0.0);
        module->convertGalacticToCelestial(testvec);

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
        node.setValue("Url", "http://localhost:8080/?origin=localhost:4690"s);
       // node.setValue("Dimensions", "glm::ivec2(1000, 1000)");

        openspace::global::scriptEngine->queueScript(
            "openspace.addScreenSpaceRenderable(" + ghoul::formatLua(node) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );


        return 1;
    }
    
}

