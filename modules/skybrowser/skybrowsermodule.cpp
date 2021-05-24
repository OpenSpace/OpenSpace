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
#include <openspace/scene/scenegraphnode.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/scene/scene.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/factorymanager.h>
#include "skybrowsermodule_lua.inl"
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <cmath> // For atan2
#include <glm/gtx/string_cast.hpp> // For printing glm data
#include <algorithm>
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
                "loadImagesToWWT",
                &skybrowser::luascriptfunctions::loadImagesToWWT,
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
                "removeSelectedImageInBrowser",
                &skybrowser::luascriptfunctions::removeSelectedImageInBrowser,
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
            {
                "createTargetBrowserPair",
                &skybrowser::luascriptfunctions::createTargetBrowserPair,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            }, 
             {
                "removeTargetBrowserPair",
                &skybrowser::luascriptfunctions::removeTargetBrowserPair,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "create3dSkyBrowser",
                &skybrowser::luascriptfunctions::create3dSkyBrowser,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            }, 
            {
                "remove3dSkyBrowser",
                &skybrowser::luascriptfunctions::remove3dSkyBrowser,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            }, 
            {
                "setOpacityOfImageLayer",
                &skybrowser::luascriptfunctions::setOpacityOfImageLayer,
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
                    return (obj->coordIsInsideCornersScreenSpace(_mousePosition) && obj->isEnabled());
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
                    isRotating = false;
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
                    // If you start dragging around on the browser, the target should unlock
                    if (to_browser(_mouseOnObject) && to_browser(_mouseOnObject)->getSkyTarget()) {
                        to_browser(_mouseOnObject)->getSkyTarget()->unlock();
                    }
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
        // Disable browser and targets when camera is outside of solar system
        double solarSystemRadius = 30.0 * distanceconstants::AstronomicalUnit;
        double cameraSSBDistance = glm::length(
            global::navigationHandler->camera()->positionVec3());
        bool _cameraInSolarSystem = cameraSSBDistance < solarSystemRadius;
        double fadingTime = 2.0;
        double deltaTime = global::windowDelegate->deltaTime();

        // Fade out or in browser & target
        for (std::pair<std::string, ScreenSpaceSkyBrowser*> pair : browsers) {
            ScreenSpaceSkyBrowser* browser = pair.second;
            // If outside solar system and browser is visible
            if (!_cameraInSolarSystem && browser->isEnabled()) {
                bool fadingIsFinished = fadeBrowserAndTarget(true, fadingTime, deltaTime);

                if (fadingIsFinished) {
                    browser->property("Enabled")->set(false);
                }
            }
            // If within solar system and browser is not visible
            else if (_cameraInSolarSystem && !browser->isEnabled()) {
                browser->property("Enabled")->set(true);
            }
            // If within solar system and browser is visible
            if (_cameraInSolarSystem && browser->isEnabled()) {
                fadeBrowserAndTarget(false, fadingTime, deltaTime);

                if (browser->getSkyTarget()) {
                    browser->getSkyTarget()->animateToCoord(deltaTime);
                }
            }
        }
        if (isRotating) {
            rotateCamera(deltaTime);
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
    fScreenSpaceRenderable->registerClass<ScreenSpaceSkyTarget>("ScreenSpaceSkyTarget");

    // Register Renderable Skybrowser
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "Renderable factory was not created");
    fRenderable->registerClass<RenderableSkyBrowser>("RenderableSkyBrowser");
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
    ScreenSpaceSkyBrowser* browser = to_browser(object);
    if (browser) {
        browsers[browser->identifier()] = browser;
    }
}

bool SkyBrowserModule::browserIdExists(std::string id) {
    // If the id doesn't exist, return false
    if (browsers.find(id) == browsers.end()) {
        return false;
    }
    return true;
}

void SkyBrowserModule::createTargetBrowserPair() {
    int noOfPairs = getSkyBrowsers().size() + 1;
    std::string nameBrowser = "Sky Browser " + std::to_string(noOfPairs);
    std::string nameTarget = "Sky Target " + std::to_string(noOfPairs);
    std::string idBrowser = "SkyBrowser" + std::to_string(noOfPairs);
    std::string idTarget = "SkyTarget" + std::to_string(noOfPairs);
    glm::vec3 positionBrowser = { -1.0f, -0.5f, -2.1f };
    std::string guiPath = "/SkyBrowser";

    const std::string browser = "{"
            "Identifier = '" + idBrowser + "',"
            "Type = 'ScreenSpaceSkyBrowser',"
            "Name = '" + nameBrowser + "',"
            "Url = 'http://localhost:8000/',"
            "FaceCamera = false,"
            "TargetID = '" + idTarget + "',"
            "CartesianPosition = " + ghoul::to_string(positionBrowser) + ","
        "}";
    const std::string target = "{"
            "Identifier = '" + idTarget + "',"
            "Type = 'ScreenSpaceSkyTarget',"
            "Name = '" + nameTarget + "',"
            "FaceCamera = false,"
            "BrowserID = '" + idBrowser + "',"
        "}";

    openspace::global::scriptEngine->queueScript(
        "openspace.addScreenSpaceRenderable(" + browser + ");",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    openspace::global::scriptEngine->queueScript(
        "openspace.addScreenSpaceRenderable(" + target + ");",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void SkyBrowserModule::removeTargetBrowserPair(std::string& browserId) {
    if (!browserIdExists(browserId)) return;

    ScreenSpaceSkyBrowser* browser = browsers[browserId];

    // Find corresponding target
    std::string targetId{ "" };
    bool hasTarget = browser->getSkyTarget();
    if (hasTarget) {
        std::string targetId = browser->getSkyTarget()->identifier();

        openspace::global::scriptEngine->queueScript(
            "openspace.removeScreenSpaceRenderable('" + targetId + "');",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
    // Remove pointer to the renderable from browsers vector
    browsers.erase(browserId);

    // Remove pointer to the renderable from screenspace renderable vector
    renderables.erase(std::remove_if(std::begin(renderables), std::end(renderables),
        [&](ScreenSpaceRenderable* renderable) {
            bool foundBrowser = renderable->identifier() == browserId;
            if (hasTarget) {
                bool foundTarget = renderable->identifier() == targetId;
                return foundBrowser || foundTarget;
            }
            else {
                return foundBrowser;
            } 
        }));
    // Remove from engine
    openspace::global::scriptEngine->queueScript(
        "openspace.removeScreenSpaceRenderable('" + browserId + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
    
    
}

void SkyBrowserModule::create3dBrowser(ImageData& image) {
    std::string id = "SkyBrowser3d" + std::to_string(browsers3d.size()+1);
    glm::dvec3 position = image.position3d * distanceconstants::Parsec;
    std::string translation = ghoul::to_string(position);
    std::string guiPath = "/SkyBrowser";

    // Calculate the size of the plane with trigonometry
    // Calculate in equatorial coordinate system since the FOV is from Earth
    //  /|
    // /_|    Adjacent is the horizontal line, opposite the vertical 
    // \ |    Calculate for half the triangle first, then multiply with 2
    //  \|
    glm::dvec3 j2000 = skybrowser::galacticCartesianToJ2000Cartesian(position);
    double adjacent = glm::length(j2000);
    double opposite = 2 * adjacent * glm::tan(glm::radians(image.fov * 0.5));

    // Calculate rotation to make the plane face the solar system barycenter
    glm::dvec3 normal = glm::normalize(-position);
    glm::dvec3 newRight = glm::normalize(
        glm::cross(glm::dvec3(0.0, 0.0, 1.0), normal)
    );
    glm::dvec3 newUp = glm::cross(normal, newRight);

    glm::dmat3 originOrientedRotation = glm::dmat3(1.0);
    originOrientedRotation[0] = newRight;
    originOrientedRotation[1] = newUp;
    originOrientedRotation[2] = normal;


    const std::string browser = "{"
        "Identifier = '" + id + "',"
        "Parent = 'SolarSystemBarycenter',"
        "Renderable = {"
        "Type = 'RenderableSkyBrowser',"
        "Size = " + std::to_string(opposite) + ","
        "Origin = 'Center',"
        "Billboard = false,"
        "Url = 'http://localhost:8000'"
        "},"
            "Transform = {"
            "Translation = {"
            "Type = 'StaticTranslation',"
            "Position = " + translation + ""
        "},"
            "Rotation = {"
            "Type = 'StaticRotation',"
            "Rotation = " + ghoul::to_string(originOrientedRotation) + ""
            "}"
        "},"
        "GUI = {"
        "Name = '" + image.name + "',"
        "Path = '" + guiPath + "'"
        "}"
        "}";
    LINFO(browser);
    openspace::global::scriptEngine->queueScript(
        "openspace.addSceneGraphNode(" + browser + ");",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void SkyBrowserModule::add3dBrowser(RenderableSkyBrowser* node) {
    browsers3d.push_back(node);
}

void SkyBrowserModule::remove3dBrowser(std::string& id) {
    // Remove pointer to the renderable from module vector
    browsers3d.erase(std::remove_if(std::begin(browsers3d), std::end(browsers3d), 
        [&](RenderableSkyBrowser* browser) {
            return browser->identifier() == id;
        }));

    openspace::global::scriptEngine->queueScript(
        "openspace.removeSceneGraphNode(" + id + ");",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
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

std::map<std::string, ScreenSpaceSkyBrowser*>& SkyBrowserModule::getSkyBrowsers() {
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

bool SkyBrowserModule::fadeBrowserAndTarget(bool makeTransparent, double fadeTime, double deltaTime) {
    float opacityDelta = static_cast<float>(deltaTime / fadeTime);
    float highTreshold = 0.99f;
    float lowThreshold = 0.01f;
    float transparent = 0.0;
    float opaque = 1.0;
    if (makeTransparent) {
        opacityDelta *= -1.f;
    }
    bool finished = true;
    for (std::pair <std::string, ScreenSpaceSkyBrowser*> idAndBrowser : browsers) {
        ScreenSpaceSkyBrowser* browser = idAndBrowser.second;
        // If there is a target, fade it as well. Otherwise, skip
        ScreenSpaceSkyTarget* target = browser->getSkyTarget();
        bool targetFinished = true;
        if (target) {
            target->getOpacity() = target->getOpacity().value() + opacityDelta;
            float opacityTarget = abs(target->getOpacity().value());
            targetFinished = makeTransparent ? opacityTarget < lowThreshold : opacityTarget > highTreshold;
            if (targetFinished) {
                target->getOpacity() = makeTransparent ? transparent : opaque;
            }
        }
        // Keep fading the browsers until all are finished
        browser->getOpacity() = browser->getOpacity().value() + opacityDelta;
        float opacityBrowser = abs(browser->getOpacity().value());
        bool browserFinished = makeTransparent ? opacityBrowser < lowThreshold : opacityBrowser > highTreshold;
        if (browserFinished && targetFinished) {
            browser->getOpacity() = makeTransparent ? transparent : opaque;
        }
        else {
            finished = false;
        }
    }
    return finished;
}

void SkyBrowserModule::setSelectedBrowser(ScreenSpaceRenderable* ptr) {
    ScreenSpaceSkyBrowser* browser = to_browser(ptr) ? to_browser(ptr) : to_target(ptr)->getSkyBrowser();
    selectedBrowser = browser->identifier();
}

void SkyBrowserModule::setSelectedBrowser(std::string id) {
    selectedBrowser = id;
}

std::string SkyBrowserModule::selectedBrowserId() {
    return selectedBrowser;
}

int SkyBrowserModule::loadImages(const std::string& root, const std::string& directory) {
   
    // Load speck files for 3D positions
    std::filesystem::path globularClusters = absPath("${BASE}/sync/http/digitaluniverse_globularclusters_speck/2/gc.speck");
    std::filesystem::path openClusters = absPath("${BASE}/sync/http/digitaluniverse_openclusters_speck/2/oc.speck");

    speck::Dataset speckGlobularClusters = speck::loadSpeckFile(globularClusters);
    speck::Dataset speckOpenClusters = speck::loadSpeckFile(openClusters);

    dataHandler->loadSpeckData(speckGlobularClusters);
    dataHandler->loadSpeckData(speckOpenClusters);

    int nLoadedImages;

    // Read from disc
    bool loadedImages = dataHandler->loadWTMLCollectionsFromDirectory(directory);

    // Reading from url if there is no directory
    if (loadedImages) {
        LINFO("Loading images from directory");
    }
    else {
        LINFO("Loading images from url");
        dataHandler->loadWTMLCollectionsFromURL(directory, root, "root");
    }

    nLoadedImages = dataHandler->loadImagesFromLoadedXMLs();
    LINFO("Loaded " + std::to_string(nLoadedImages) + " WorldWide Telescope images.");
   
    return nLoadedImages;
}

bool SkyBrowserModule::cameraInSolarSystem() {
    return _cameraInSolarSystem;
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
