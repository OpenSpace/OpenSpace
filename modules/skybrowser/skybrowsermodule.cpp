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
#include <modules/skybrowser/include/wwtdatahandler.h>
 //#include <modules/webbrowser/webbrowsermodule.h>
 //#include <modules/webbrowser/include/screenspacebrowser.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/rendering/renderable.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/scene/scene.h>
#include <openspace/util/factorymanager.h>
#include "skybrowsermodule_lua.inl"
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <cmath> // For atan2
#include <glm/gtx/string_cast.hpp> // For printing glm data

#include <fstream>    


namespace {
    struct [[codegen::Dictionary(ScreenSpaceSkyBrowser)]] Parameters {

    };
    
    #include "skybrowsermodule_codegen.cpp"
    
    
    
} // namespace

namespace openspace {
  
    scripting::LuaLibrary SkyBrowserModule::luaLibrary() const {

        scripting::LuaLibrary res;
        res.name = "skybrowser";
        res.functions = {
            {
                "getListOfImages",
                &skybrowser::luascriptfunctions::getListOfImages,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            }, 
            {
                "moveCircleToHoverImage",
                &skybrowser::luascriptfunctions::moveCircleToHoverImage,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "disableHoverCircle",
                &skybrowser::luascriptfunctions::disableHoverCircle,
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
                "selectImage",
                &skybrowser::luascriptfunctions::selectImage,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "adjustCamera",
                & skybrowser::luascriptfunctions::adjustCamera,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "setSelectedBrowser",
                & skybrowser::luascriptfunctions::setSelectedBrowser,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "getTargetData",
                &skybrowser::luascriptfunctions::getTargetData,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "lockTarget",
                &skybrowser::luascriptfunctions::lockTarget,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "unlockTarget",
                &skybrowser::luascriptfunctions::unlockTarget,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
        };

        return res;
    }


SkyBrowserModule::SkyBrowserModule()
    : OpenSpaceModule(SkyBrowserModule::Name)
    , _mouseOnObject(nullptr)
    , currentlyResizingBrowser(false)
    , currentlyDraggingObject(false)
    , resizeVector(0.f, 0.f)
    , shouldInitialize(true)
    , changeViewWithinBrowser(false)
{
    global::callback::mousePosition->emplace_back(
        [&](double x, double y) {    
            // Quick fix to make all renderables find its corresponding partner
            if (shouldInitialize) {
                std::for_each(renderables.begin(), renderables.end(), [&](ScreenSpaceRenderable* obj) {
                    if (to_target(obj)) {
                        to_target(obj)->setConnectedBrowser();
                    }
                    else if (to_browser(obj)) {
                        to_browser(obj)->setConnectedTarget();
                    }
                    });
                shouldInitialize = false;
            }
            
            glm::vec2 pos = glm::vec2(static_cast<float>(x), static_cast<float>(y));
            _mousePosition = getMousePositionInScreenSpaceCoords(pos);

            if (currentlyDraggingObject) {

                glm::dvec2 move = _mousePosition - startDragMousePos;
                
                // Change view within the browser and move target accordingly to mousedrag movement
                if (changeViewWithinBrowser) {
                    // WWT FOV
                    double WWTVerticalFOV = to_browser(_mouseOnObject)->fieldOfView();
                    glm::dvec2 browserDim = to_browser(_mouseOnObject)->getScreenSpaceDimensions();
                    double browserRatio = browserDim.x / browserDim.y;
                    glm::dvec2 WWTFOV = glm::dvec2(WWTVerticalFOV * browserRatio, WWTVerticalFOV);

                    // OpenSpace FOV
                    glm::dvec2 windowDim = global::windowDelegate->currentWindowSize();
                    double windowRatio = windowDim.y / windowDim.x;
                    double OpenSpaceHorizontalFOV = global::windowDelegate->getHorizFieldOfView();
                    glm::dvec2 OpenSpaceFOV = glm::dvec2(OpenSpaceHorizontalFOV, OpenSpaceHorizontalFOV * windowRatio);

                    glm::dvec2 angleResult = WWTFOV * (move / browserDim);
                    glm::dvec2 OSresult = angleResult / OpenSpaceFOV;
                    
                    // Calculate translation in ScreenSpaceCoordinates
                    glm::dvec2 screenSpaceCoord{ (2 / windowRatio), 2.f };
                    glm::dvec2 result = screenSpaceCoord * OSresult;

                    to_browser(_mouseOnObject)->getSkyTarget()->translate(-result, startDragObjectPos);
                    
                }
                // Move browser or target
                else _mouseOnObject->translate(move, startDragObjectPos);
               
            }
            else if (currentlyResizingBrowser) {
                // Calculate scaling factor
                glm::vec2 mouseDragVector = (_mousePosition - startDragMousePos);
                glm::vec2 scalingVector = mouseDragVector * resizeVector;
                glm::vec2 newSizeRelToOld = (startResizeBrowserSize + (scalingVector)) / startResizeBrowserSize;
                // Scale the browser
                to_browser(_mouseOnObject)->scale(newSizeRelToOld);

                // For dragging functionality, translate so it looks like the browser isn't moving
                // Make sure the browser doesn't move in directions it's not supposed to 
                _mouseOnObject->translate(mouseDragVector * abs(resizeVector) / 2.f, startDragObjectPos);
            }
            // If there is no dragging or resizing, look for new objects
            else {
                // Save old selection for removing highlight
                ScreenSpaceRenderable* lastObj = _mouseOnObject;

                // Find and save what mouse is currently hovering on
                auto currentlyOnObject = std::find_if(renderables.begin(), renderables.end(), [&](ScreenSpaceRenderable* obj) {
                    return obj->coordIsInsideCornersScreenSpace(_mousePosition);
                    });
                _mouseOnObject = currentlyOnObject != renderables.end() ? *currentlyOnObject : nullptr;

                // Selection has changed
                if (lastObj != _mouseOnObject) {
                    // Remove highlight
                    if (to_browser(lastObj)) {
                        to_browser(lastObj)->setBorderColor(to_browser(lastObj)->getColor() - highlightAddition);
                    }
                    else if (to_target(lastObj)) {
                        to_target(lastObj)->setBorderColor(to_target(lastObj)->getColor() - highlightAddition);
                    }

                    // Add highlight
                    if (to_browser(_mouseOnObject)) {
                        to_browser(_mouseOnObject)->setBorderColor(to_browser(_mouseOnObject)->getColor() + highlightAddition);
                    }
                    else if (to_target(_mouseOnObject)) {
                        to_target(_mouseOnObject)->setBorderColor(to_target(_mouseOnObject)->getColor() + highlightAddition);
                    }
                }
                
            }
        }
    );

    global::callback::mouseScrollWheel->emplace_back(
        [&](double, double scroll) -> bool {
            
            // If mouse is on browser or target, apply zoom
            if (to_browser(_mouseOnObject)) {
                to_browser(_mouseOnObject)->scrollZoom(scroll);
                return true;
            }
            else if (to_target(_mouseOnObject) && to_target(_mouseOnObject)->getSkyBrowser()) {
                to_target(_mouseOnObject)->getSkyBrowser()->scrollZoom(scroll);
            }
            
            return false;
        }
    );

    global::callback::mouseButton->emplace_back(
        [&](MouseButton button, MouseAction action, KeyModifier modifier) -> bool {

            if (_mouseOnObject && action == MouseAction::Press) {

                // Get the currently selected browser
                setSelectedBrowser(_mouseOnObject);


                if (button == MouseButton::Left) {
                    startDragMousePos = _mousePosition;
                    startDragObjectPos = _mouseOnObject->getScreenSpacePosition();

                    // If current object is browser, check for resizing
                    if (to_browser(_mouseOnObject)) {
                        // Resize browser if mouse is over resize button
                        resizeVector = to_browser(_mouseOnObject)->coordIsOnResizeArea(_mousePosition);
                        if (resizeVector != glm::vec2{ 0 }) {
                            to_browser(_mouseOnObject)->saveResizeStartSize();
                            startResizeBrowserSize = to_browser(_mouseOnObject)->getScreenSpaceDimensions();
                            currentlyResizingBrowser = true;
                            return true;
                        }
                    }
                    // If you start dragging around the target, it should unlock
                    if (to_target(_mouseOnObject)) {
                        to_target(_mouseOnObject)->unlock();
                    }
                    currentlyDraggingObject = true;

                    return true;
                }  
                else if (to_browser(_mouseOnObject) && button == MouseButton::Right) {

                    // Change view (by moving target) within browser if right mouse click on browser
                    startDragMousePos = _mousePosition;
                    startDragObjectPos = to_browser(_mouseOnObject)->getSkyTarget()->getScreenSpacePosition();
                    changeViewWithinBrowser = true;
                    currentlyDraggingObject = true;

                    return true;
                }
            }
            else if (action == MouseAction::Release) {
                if (currentlyDraggingObject) {
                    currentlyDraggingObject = false;
                    changeViewWithinBrowser = false;
                    return true;
                }
                if (currentlyResizingBrowser) {
                    currentlyResizingBrowser = false;
                    to_browser(_mouseOnObject)->updateBrowserSize();
                    return true;
                }
            }

            return false;
        }
    );

    global::callback::preSync->emplace_back([this]() {
        for (ScreenSpaceSkyBrowser* browser : getSkyBrowsers()) {
            if (browser->getSkyTarget()) {
                browser->getSkyTarget()->animateToCoord(global::windowDelegate->deltaTime());
            }
        }
        if (isRotating) {
            rotateCamera(global::windowDelegate->deltaTime());
        }
    });
} 

void SkyBrowserModule::internalDeinitialize() {
    delete dataHandler;
}

void SkyBrowserModule::internalInitialize(const ghoul::Dictionary& dict) {
    
    const Parameters p = codegen::bake<Parameters>(dict);

    // register ScreenSpaceBrowser
    auto fScreenSpaceRenderable = FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");
    fScreenSpaceRenderable->registerClass<ScreenSpaceSkyBrowser>("ScreenSpaceSkyBrowser");

    // register ScreenSpaceTarget
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");
    fScreenSpaceRenderable->registerClass<ScreenSpaceSkyTarget>("ScreenSpaceSkyTarget");
    // Create data handler dynamically to avoid the linking error that
    // came up when including the include file in the module header file
    dataHandler = new WWTDataHandler();
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

void SkyBrowserModule::addRenderable(ScreenSpaceRenderable* object) {
    renderables.push_back(object);
    // Sort on z coordinate, objects closer to camera are in beginning of list
    std::sort(renderables.begin(), renderables.end());
}

ScreenSpaceSkyBrowser* SkyBrowserModule::to_browser(ScreenSpaceRenderable* ptr) {
    return dynamic_cast<ScreenSpaceSkyBrowser*>(ptr);
}
ScreenSpaceSkyTarget* SkyBrowserModule::to_target(ScreenSpaceRenderable* ptr) {
    return dynamic_cast<ScreenSpaceSkyTarget*>(ptr);
}

WWTDataHandler* SkyBrowserModule::getWWTDataHandler() {
    return dataHandler;
}

std::vector<ScreenSpaceSkyBrowser*> SkyBrowserModule::getSkyBrowsers() {
    std::vector<ScreenSpaceSkyBrowser*> browsers;
    for (ScreenSpaceRenderable* renderable : renderables) {
        if (to_browser(renderable)) {
            browsers.push_back(to_browser(renderable));
        }
    }
    return browsers;
}

void SkyBrowserModule::startRotation(glm::dvec2 coordsEnd) {
    
    // Save coordinates to rotate to in galactic world coordinates
    glm::dvec3 camPos = global::navigationHandler->camera()->positionVec3();
    _coordsToAnimateTo = skybrowser::J2000SphericalToGalacticCartesian(coordsEnd);
    _coordsStartAnimation = (global::navigationHandler->camera()->viewDirectionWorldSpace() * skybrowser::infinity) + camPos;
    isRotating = true;
}

void SkyBrowserModule::rotateCamera(double deltaTime) {
    
    // Find smallest angle between the two vectors
    double smallestAngle = std::acos(glm::dot(_coordsStartAnimation, _coordsToAnimateTo) / (glm::length(_coordsStartAnimation) * glm::length(_coordsToAnimateTo)));
    // Only keep animating when target is not at final position
    if (abs(smallestAngle) > 0.0001) {
        // Calculate rotation this frame
        double rotationAngle = smallestAngle * deltaTime;
        // Create the rotation matrix for local camera space
        glm::dvec3 rotationAxis = glm::normalize(glm::cross(_coordsStartAnimation, _coordsToAnimateTo));
        glm::dmat4 camMat = global::navigationHandler->camera()->viewRotationMatrix();
        glm::dvec3 viewDirectionLocal = camMat * glm::dvec4(rotationAxis, 1.0);
        glm::dmat4 rotmat = glm::rotate(rotationAngle, rotationAxis);
        // Rotate
        global::navigationHandler->camera()->rotate(glm::quat_cast(rotmat));

        // Update camera direction
        glm::dvec3 camPos = global::navigationHandler->camera()->positionVec3();
        _coordsStartAnimation = (global::navigationHandler->camera()->viewDirectionWorldSpace() * skybrowser::infinity) + camPos;
    }
    else {
        isRotating = false;
    }
}

void SkyBrowserModule::setSelectedBrowser(ScreenSpaceRenderable* ptr) {
    ScreenSpaceSkyBrowser* browser = to_browser(ptr) ? to_browser(ptr) : to_target(ptr)->getSkyBrowser();
    std::vector<ScreenSpaceSkyBrowser*> browsers = getSkyBrowsers();
    auto it = std::find(browsers.begin(), browsers.end(), browser);
    // Get index
    selectedBrowser = std::distance(browsers.begin(), it);
}

void SkyBrowserModule::setSelectedBrowser(int i) {
    selectedBrowser = i;
}

int SkyBrowserModule::getSelectedBrowserIndex() {
    return selectedBrowser;
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
