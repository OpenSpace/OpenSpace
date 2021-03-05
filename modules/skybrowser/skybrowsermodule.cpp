/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <modules/skybrowser/skybrowsermodule.h>

 //#include <modules/webbrowser/webbrowsermodule.h>
 //#include <modules/webbrowser/include/screenspacebrowser.h>
#include <modules/base/rendering/screenspaceimagelocal.h>
#include <openspace/rendering/screenspacerenderable.h>

#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>

#include <openspace/engine/moduleengine.h>


#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/factorymanager.h>
#include <thread>
#include <chrono>
#include "skybrowsermodule_lua.inl"

#include <cmath> // For atan2
#include <ghoul/misc/dictionaryjsonformatter.h> // formatJson

namespace {
    constexpr const openspace::properties::Property::PropertyInfo TestInfo = 
    {
        "Test",
        "Test Info",
        "tjobidabidobidabidopp plopp"
    };
    constexpr const openspace::properties::Property::PropertyInfo ZoomInfo =
    {
        "Zoom",
        "Zoom Info",
        "tjobidabidobidabidopp plupp"
    };

    struct [[codegen::Dictionary(SkybrowserModule)]] Parameters {

        // [[codegen::verbatim(TestInfo.description)]]
        std::optional<std::string> test;

        // [[codegen::verbatim(ZoomInfo.description)]]
        std::optional<float> zoom;
    };
    
    #include "skybrowsermodule_codegen.cpp"
    
    
} // namespace

namespace openspace {

SkybrowserModule::SkybrowserModule()
    : OpenSpaceModule(Name)
    , _testProperty(TestInfo)
    , _zoomFactor(ZoomInfo, 50.f ,0.1f ,70.f)
    , _skyBrowser(nullptr)
{
    addProperty(_testProperty);
    addProperty(_zoomFactor);
}



scripting::LuaLibrary SkybrowserModule::luaLibrary() const {
    scripting::LuaLibrary res;
    res.name = "skybrowser";
    res.functions = {
        {
            "create",
            &skybrowser::luascriptfunctions::createBrowser,
            {},
            "string or list of strings",
            "Add one or multiple exoplanet systems to the scene, as specified by the "
            "input. An input string should be the name of the system host star"
        },
        {
            "move",
            &skybrowser::luascriptfunctions::moveBrowser,
            {},
            "string or list of strings",
            "Add one or multiple exoplanet systems to the scene, as specified by the "
            "input. An input string should be the name of the system host star"
        },
        {
            "follow",
            &skybrowser::luascriptfunctions::followCamera,
            {},
            "string or list of strings",
            "Add one or multiple exoplanet systems to the scene, as specified by the "
            "input. An input string should be the name of the system host star"
        }
    };

    return res;
}

float SkybrowserModule::zoomFactor() const{
    return _zoomFactor;
}

void SkybrowserModule::internalInitialize(const ghoul::Dictionary& dict) {
    const Parameters p = codegen::bake<Parameters>(dict);
    _testProperty = p.test.value_or(_testProperty);
    _zoomFactor = p.zoom.value_or(_zoomFactor);
}

bool SkybrowserModule::sendMessageToWWT(const ghoul::Dictionary& msg) {
    if (_skyBrowser) {
        std::string script = "sendMessageToWWT(" + ghoul::formatJson(msg) + ");";
        _skyBrowser->executeJavascript(script);
        return true;
    }
    else {
        LERROR("No sky browser added! Can't send message.");
        return false;
    }
}

void SkybrowserModule::WWTfollowCamera() {
    showTarget();
    while (true) {
        // Get camera view direction
        const glm::dvec3 viewDirection = global::navigationHandler->camera()->viewDirectionWorldSpace();

        // Convert to celestial coordinates
        glm::dvec2 celestCoords = convertGalacticToCelestial(viewDirection);
        ghoul::Dictionary message = createMessageForMovingWWTCamera(celestCoords, _zoomFactor);

        sendMessageToWWT(message);
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }
}

ghoul::Dictionary SkybrowserModule::createMessageForMovingWWTCamera(const glm::dvec2 celestCoords, const float fov,  const bool moveInstantly) const {
    using namespace std::string_literals;
    ghoul::Dictionary msg;
    msg.setValue("event", "center_on_coordinates"s);
    msg.setValue("ra", static_cast<double>(celestCoords[0]));
    msg.setValue("dec", static_cast<double>(celestCoords[1]));
    msg.setValue("fov", static_cast<double>(fov));
    msg.setValue("instant", moveInstantly);
    
    return msg;
}

ghoul::Dictionary SkybrowserModule::createMessageForPausingWWTTime() const {
    using namespace std::string_literals;
    ghoul::Dictionary msg;
    msg.setValue("event", "pause_time"s);

    return msg;
}


void SkybrowserModule::initializeBrowser(ScreenSpaceBrowser* skyBrowser) {
    _skyBrowser = skyBrowser;
}

ScreenSpaceBrowser* SkybrowserModule::skyBrowser() {
    return _skyBrowser;
}

glm::dvec2 SkybrowserModule::convertGalacticToCelestial(glm::dvec3 rGal) const {
    
    // Used the math from this website: https://gea.esac.esa.int/archive/documentation/GD -->
    // R2/Data_processing/chap_cu3ast/sec_cu3ast_intro/ssec_cu3ast_intro_tansforms.html#SSS1
    const glm::dmat3 conversionMatrix = glm::dmat3({
      -0.0548755604162154,  0.4941094278755837, -0.8676661490190047, // col 0
      -0.8734370902348850, -0.4448296299600112, -0.1980763734312015, // col 1
      -0.4838350155487132,  0.7469822444972189,  0.4559837761750669  // col 2
        });
   
    glm::dvec3 rICRS = glm::transpose(conversionMatrix) * rGal;
    float ra = atan2(rICRS[1], rICRS[0]);
    float dec = atan2(rICRS[2], glm::sqrt((rICRS[0] * rICRS[0]) + (rICRS[1] * rICRS[1])));

    ra = ra > 0 ? ra : ra + (2 * glm::pi<float>());

    return glm::dvec2(glm::degrees(ra), glm::degrees(dec));
}

void SkybrowserModule::showTarget() const{

    using namespace std::string_literals;

   
    ghoul::Dictionary node;
    node.setValue("Type", "ScreenSpaceImageLocal"s);
    node.setValue("Identifier", "Target"s);
    node.setValue("TexturePath", "D:/Esters/OpenSpace/modules/skybrowser/target.png"s);
    node.setValue("Scale", 0.07);
    

    openspace::global::scriptEngine->queueScript(
        "openspace.addScreenSpaceRenderable(" + ghoul::formatLua(node) + ")",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

}

/*
std::vector<documentation::Documentation> SkybrowserModule::documentations() const {
    return {
        ExoplanetsDataPreparationTask::documentation(),
        RenderableOrbitDisc::Documentation()
    };
}
*/
} // namespace openspace
