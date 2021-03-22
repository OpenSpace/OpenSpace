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
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/rendering/renderable.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/factorymanager.h>

#include "skybrowsermodule_lua.inl"
#include <openspace/engine/windowdelegate.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <cmath> // For atan2
#include <glm/gtx/string_cast.hpp>
#include <ghoul/filesystem/filesystem.h>

namespace {
    constexpr const openspace::properties::Property::PropertyInfo ShowSkyBrowserInfo =
    {
        "Show Sky Browser",
        "Show Sky Browser",
        "Show sky browser and target for WorldWide Telescope imagery."
    };

    struct [[codegen::Dictionary(SkyBrowserModule)]] Parameters {

        // [[codegen::verbatim(ShowSkyBrowserInfo.description)]]
        std::optional<bool> show;
    };
    
    #include "skybrowsermodule_codegen.cpp"
    
    
} // namespace

namespace openspace {

    scripting::LuaLibrary SkyBrowserModule::luaLibrary() const {

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
            },
            {
                "loacImgCollection",
                &skybrowser::luascriptfunctions::loadImgCollection,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            }
        };

        return res;
    }


SkyBrowserModule::SkyBrowserModule()
    : OpenSpaceModule(SkyBrowserModule::Name)
    , _showBrowserAndTarget(ShowSkyBrowserInfo)
    , _skyBrowser(nullptr)
    , _skyTarget(nullptr)
    , currentlyDraggingBrowser(false)
    , currentlyDraggingTarget(false)
    , currentlyResizingBrowser(false)
    , mouseIsOnBrowser(false)
    , mouseIsOnTarget(false)

{
    addProperty(_showBrowserAndTarget);

    _showBrowserAndTarget.onChange([&]() {
        if (_showBrowserAndTarget) {
            _skyBrowser->setConnectedTarget();
            _skyTarget->setConnectedBrowser();
        }
    });

    global::callback::mousePosition->emplace_back(
        [&](double x, double y) {      
            glm::vec2 pos = glm::vec2(static_cast<float>(x), static_cast<float>(y));

            _mousePosition = getMousePositionInScreenSpaceCoords(pos);

            if (_skyTarget) {
                mouseIsOnTarget = _skyTarget->coordIsInsideCornersScreenSpace(_mousePosition);          
            }
            else {
                mouseIsOnTarget = false;
            }
            if (_skyBrowser) {
                mouseIsOnBrowser = _skyBrowser->coordIsInsideCornersScreenSpace(_mousePosition);       
            }
            else {
                mouseIsOnBrowser = false;
            }

            if (currentlyDraggingBrowser) {
                _skyBrowser->translate(_mousePosition - startDragMousePosBrowser, startDragObjectPosBrowser);
            }
            if (currentlyDraggingTarget) {
                _skyTarget->translate(_mousePosition - startDragMousePosTarget, startDragObjectPosTarget);
            }
            if (currentlyResizingBrowser) {
                // Calculate scaling factor
                glm::vec2 mouseDragVector = (_mousePosition - startDragMousePosBrowser);
                glm::vec2 scalingVector = mouseDragVector * resizeVector;

                glm::vec2 newSizeRelToOld = (startResizeBrowserSize + (scalingVector)) / startResizeBrowserSize;
                _skyBrowser->scale(newSizeRelToOld);

                // Make sure the browser doesn't move in directions it's not supposed to 
                _skyBrowser->translate(mouseDragVector * abs(resizeVector) / 2.f, startDragObjectPosBrowser);

               // _skyTarget->setDimensions(_skyBrowser->getScreenSpaceBrowserDimension());

            }
        }
    );

    global::callback::mouseScrollWheel->emplace_back(
        [&](double, double scroll) -> bool {
            if (mouseIsOnBrowser) {
                _skyBrowser->scrollZoom(scroll);
                return true;
            }
          
            return false;
        }
    );

    global::callback::mouseButton->emplace_back(
        [&](MouseButton button, MouseAction action, KeyModifier modifier) -> bool {

            if (action == MouseAction::Press) {
                
                if (mouseIsOnBrowser && button == MouseButton::Left) {
                    
                    startDragMousePosBrowser = _mousePosition;
                    startDragObjectPosBrowser = _skyBrowser->getScreenSpacePosition();
                    // Resize browser if mouse is over resize button
                    resizeVector = _skyBrowser->coordIsOnResizeArea(_mousePosition);

                    if (resizeVector != glm::vec2{0}) {
                        _skyBrowser->saveResizeStartSize();
                        startResizeBrowserSize = _skyBrowser->getScreenSpaceDimensions();
                        currentlyResizingBrowser = true;
                    }
                    else {
                        currentlyDraggingBrowser = true;
                    }
                    
                    return true;
                }
                else if (mouseIsOnTarget && button == MouseButton::Left) {
                    
                    startDragMousePosTarget = _mousePosition;
                    startDragObjectPosTarget = _skyTarget->getScreenSpacePosition();
                    currentlyDraggingTarget = true;
                    return true;
                }
                else if (mouseIsOnBrowser && button == MouseButton::Right) {

                    startDragMousePosTarget = _mousePosition;
                    startDragObjectPosTarget = _skyTarget->getScreenSpacePosition();
                    currentlyDraggingTarget = true;
                    return true;
                }
            }
            else if (action == MouseAction::Release) {
                if (currentlyDraggingBrowser) {
                    currentlyDraggingBrowser = false;
                    return true;
                }
                if (currentlyDraggingTarget) {
                    currentlyDraggingTarget = false;
                    return true;
                }
                if (currentlyResizingBrowser) {
                    currentlyResizingBrowser = false;
                    _skyBrowser->updateBrowserSize();
                    return true;
                }
            }

            return false;
        }
    );
} 

void SkyBrowserModule::internalDeinitialize() {


}

void SkyBrowserModule::internalInitialize(const ghoul::Dictionary& dict) {
    
    const Parameters p = codegen::bake<Parameters>(dict);
    _showBrowserAndTarget = p.show.value_or(_showBrowserAndTarget);

    // register ScreenSpaceBrowser
    auto fScreenSpaceRenderable = FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");
    fScreenSpaceRenderable->registerClass<ScreenSpaceSkyBrowser>("ScreenSpaceSkyBrowser");

    // register ScreenSpaceTarget
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");
    fScreenSpaceRenderable->registerClass<ScreenSpaceSkyTarget>("ScreenSpaceSkyTarget");

}

glm::vec2 SkyBrowserModule::getMousePositionInScreenSpaceCoords(glm::vec2& mousePos) {
    glm::vec2 size = global::windowDelegate->currentWindowSize();
    // Change origin to middle of the window
    glm::vec2 screenSpacePos = glm::vec2((mousePos - (size / 2.0f)));
    // Ensure the upper right corner is positive on the y axis
    screenSpacePos *= glm::vec2(1.0f, -1.0f);
    // Transform pixel coordinates to screen space coordinates [-1,1][-ratio, ratio]
    screenSpacePos /= (0.5f*size.y);
    return screenSpacePos;
}

void SkyBrowserModule::addSkyBrowser(ScreenSpaceSkyBrowser* browser) {
    _skyBrowser = browser;
}


void SkyBrowserModule::addSkyTarget(ScreenSpaceSkyTarget* target) {
    _skyTarget = target;
}


/*
std::vector<documentation::Documentation> SkyBrowserModule::documentations() const {
    return {
        ExoplanetsDataPreparationTask::documentation(),
        RenderableOrbitDisc::Documentation()
    };
}
*/
} // namespace openspace
