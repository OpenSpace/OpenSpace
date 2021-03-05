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

#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/factorymanager.h>
#include <thread>
#include <chrono>
#include <cmath> // For atan2

#include "skybrowsermodule_lua.inl"


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
    , _zoomFactor(ZoomInfo, 70.f ,0.f ,150.f)
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
    /*
    auto fBrowser = FactoryManager::ref().factory<ScreenSpaceBrowser>();
    ghoul_assert(fBrowser, "No browser factory existed :'-(");
    fBrowser->registerClass<ScreenSpaceBrowser>("ScreenSpaceBrowser");
    */
}

void SkybrowserModule::WWTfollowCamera() const {
    // Get camera view direction
    const glm::dvec3 viewDirection = global::navigationHandler->camera()->viewDirectionWorldSpace();

    // Convert to celestial coordinates
    const SkybrowserModule* module = global::moduleEngine->module<SkybrowserModule>();
    glm::dvec2 celestCoords = module->convertGalacticToCelestial(viewDirection);

    showTarget();
    // Execute javascript on browser
    ScreenSpaceBrowser* browser = dynamic_cast<ScreenSpaceBrowser*>(global::renderEngine->screenSpaceRenderable("ScreenSpaceBowser"));
    std::string script = "window.frames[0].postMessage({event: 'center_on_coordinates', ra : Number(" + std::to_string(celestCoords[0]) + "), dec : Number(" + std::to_string(celestCoords[1]) + "), fov : Number(" + std::to_string(_zoomFactor) + "), instant : false})";
    browser->executeJavascript(script);
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

    std::cout << glm::degrees(dec) << std::endl;

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
