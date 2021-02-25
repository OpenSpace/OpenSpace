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


#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/factorymanager.h>
#include <thread>
#include <chrono>


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
            "test",
            &skybrowser::luascriptfunctions::testFunction,
            {},
            "string or list of strings",
            "Add one or multiple exoplanet systems to the scene, as specified by the "
            "input. An input string should be the name of the system host star"
        },
        {
            "update",
            &skybrowser::luascriptfunctions::updateFunction,
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
/*
std::vector<documentation::Documentation> SkybrowserModule::documentations() const {
    return {
        ExoplanetsDataPreparationTask::documentation(),
        RenderableOrbitDisc::Documentation()
    };
}
*/
} // namespace openspace
