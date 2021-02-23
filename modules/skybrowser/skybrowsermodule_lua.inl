#include <openspace/util/openspacemodule.h>


#include <openspace/documentation/documentation.h>
#include <modules/skybrowser/skybrowsermodule.h>

#include <openspace/scripting/scriptengine.h>
#include <ghoul/misc/dictionaryluaformatter.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <fstream>
#include <sstream>


namespace {
    constexpr const char _loggerCat[] = "SkybrowserModule";
} // namespace


namespace openspace::skybrowser::luascriptfunctions {
    
    bool testFunction() {
        LINFOC(_loggerCat, "yabadadooo");
        return true;
    }


    int testFunction(lua_State* L) {
        ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::testFunction");


        LINFOC(_loggerCat, "hoho");
        testFunction();

        //std::string _url = "https://wallpaperaccess.com/full/3010132.jpg";
        // 'https://cdn.wallpapersafari.com/6/92/0nbCPw.jpg'
        /*
        // get url from user
        const std::string _url = ghoul::lua::value<std::string>(L, 1);
       
        using namespace std::string_literals;

        std::string identifier = "ImageTest";
        std::string guiname = "Test";
        double size = 1.E11;

        // create renderable renderableplaneimageonline
        ghoul::Dictionary renderable; 
        renderable.setValue("Type", "RenderablePlaneImageOnline"s);
        renderable.setValue("URL", _url);
        renderable.setValue("Origin", "Center"s);
        renderable.setValue("Size", size);

        ghoul::Dictionary gui;
        gui.setValue("Name", guiname);
        gui.setValue("Path", "/Software Integration"s);

        ghoul::Dictionary node;
        node.setValue("Identifier", identifier);
        node.setValue("Renderable", renderable);
        node.setValue("GUI", gui);

        */
        openspace::global::scriptEngine->queueScript(
            "openspace.addScreenSpaceRenderable(" + ghoul::formatLua(node) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        

        return 1;
    }
    
}

