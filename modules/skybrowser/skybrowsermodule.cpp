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
                "place3dSkyBrowser",
                &skybrowser::luascriptfunctions::place3dSkyBrowser,
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
            {
                "sendOutIdsToBrowsers",
                &skybrowser::luascriptfunctions::sendOutIdsToBrowsers,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "initializeBrowser",
                &skybrowser::luascriptfunctions::initializeBrowser,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },  
            {
                "connectBrowserTarget",
                &skybrowser::luascriptfunctions::connectBrowserTarget,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
           {
                "addToSkyBrowserModule",
                &skybrowser::luascriptfunctions::addToSkyBrowserModule,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },  
            {
                "set3dSelectedImagesAs2dSelection",
                &skybrowser::luascriptfunctions::set3dSelectedImagesAs2dSelection,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },   
            {
                "centerTargetOnScreen",
                &skybrowser::luascriptfunctions::centerTargetOnScreen,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            }, 
            {
                "setImageLayerOrder",
                &skybrowser::luascriptfunctions::setImageLayerOrder,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
        };

        return res;
    }

// Transforms a pixel coordinate to a screen space coordinate
glm::vec2 pixelToScreenSpace(glm::vec2& mouseCoordinate) {
    glm::vec2 size = global::windowDelegate->currentWindowSize();
    // Change origin to middle of the window
    glm::vec2 screenSpacePos = glm::vec2((mouseCoordinate - (size / 2.0f)));
    // Ensure the upper right corner is positive on the y axis
    screenSpacePos *= glm::vec2(1.0f, -1.0f);
    // Transform pixel coordinates to screen space coordinates [-1,1][-ratio, ratio]
    screenSpacePos /= (0.5f * size.y);
    return screenSpacePos;
}


SkyBrowserModule::SkyBrowserModule()
    : OpenSpaceModule(SkyBrowserModule::Name)
{
    global::callback::mousePosition->emplace_back(
        [&](double x, double y) {    
            
            glm::vec2 pixel = glm::vec2(static_cast<float>(x), static_cast<float>(y));
            _mousePosition = pixelToScreenSpace(pixel);

            if (_isDragging) {

                glm::dvec2 move = _mousePosition - _startMousePosition;
                
                // Change view within the browser and move target accordingly to mouse drag movement
                if (_fineTuneMode) {
                    // WWT FOV
                    double WWTVerticalFOV = toBrowser(_mouseOnObject)->verticalFov();
                    glm::dvec2 browserDim = toBrowser(_mouseOnObject)->screenSpaceDimensions();
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

                    toBrowser(_mouseOnObject)->getSkyTarget()->translate(-result, _startDragPosition);
                    
                }
                // Move browser or target
                else _mouseOnObject->translate(move, _startDragPosition);
               
            }
            else if (_isResizing) {
                // Calculate scaling factor
                glm::vec2 mouseDragVector = (_mousePosition - _startMousePosition);
                glm::vec2 scalingVector = mouseDragVector * _resizeDirection;
                glm::vec2 newSizeRelToOld = (_startBrowserSize + (scalingVector)) / _startBrowserSize;
                // Scale the browser
                toBrowser(_mouseOnObject)->setScale(newSizeRelToOld);

                // For dragging functionality, translate so it looks like the browser isn't moving
                // Make sure the browser doesn't move in directions it's not supposed to 
                _mouseOnObject->translate(mouseDragVector * abs(_resizeDirection) / 2.f, _startDragPosition);
            }
            // If there is no dragging or resizing, look for new objects
            else {
                // Save old selection for removing highlight
                ScreenSpaceRenderable* lastObj = _mouseOnObject;

                // Find and save what mouse is currently hovering on
                auto currentlyOnObject = std::find_if(_renderables.begin(), _renderables.end(), [&](ScreenSpaceRenderable* obj) {
                    return obj && (obj->coordIsInsideCornersScreenSpace(_mousePosition) && obj->isEnabled());
                    });
                _mouseOnObject = currentlyOnObject != _renderables.end() ? *currentlyOnObject : nullptr;

                // Selection has changed
                if (lastObj != _mouseOnObject) {
                    // Remove highlight
                    if (toBrowser(lastObj)) {
                        toBrowser(lastObj)->setWebpageBorderColor(toBrowser(lastObj)->borderColor() - _highlightAddition);
                    }
                    else if (toTarget(lastObj)) {
                        toTarget(lastObj)->setColor(toTarget(lastObj)->borderColor() - _highlightAddition);
                    }

                    // Add highlight
                    if (toBrowser(_mouseOnObject)) {
                        toBrowser(_mouseOnObject)->setWebpageBorderColor(toBrowser(_mouseOnObject)->borderColor() + _highlightAddition);
                    }
                    else if (toTarget(_mouseOnObject)) {
                        toTarget(_mouseOnObject)->setColor(toTarget(_mouseOnObject)->borderColor() + _highlightAddition);
                    }
                }
                
            }
        }
    );

    global::callback::mouseScrollWheel->emplace_back(
        [&](double, double scroll) -> bool {
            
            // If mouse is on browser or target, apply zoom
            if (toBrowser(_mouseOnObject)) {
                toBrowser(_mouseOnObject)->setVerticalFovWithScroll(static_cast<float>(scroll));
                return true;
            }
            else if (toTarget(_mouseOnObject) && toTarget(_mouseOnObject)->getSkyBrowser()) {
                toTarget(_mouseOnObject)->getSkyBrowser()->setVerticalFovWithScroll(static_cast<float>(scroll));
            }
            
            return false;
        }
    );

    global::callback::mouseButton->emplace_back(
        [&](MouseButton button, MouseAction action, KeyModifier modifier) -> bool {

            if (_mouseOnObject && action == MouseAction::Press) {

                // Get the currently selected browser
                if (toBrowser(_mouseOnObject)) {
                    setSelectedBrowser(toBrowser(_mouseOnObject));
                }
                else if (toTarget(_mouseOnObject) && 
                         toTarget(_mouseOnObject)->getSkyBrowser()) {

                    setSelectedBrowser(toTarget(_mouseOnObject)->getSkyBrowser());
                }

                if (button == MouseButton::Left) {
                    _isRotating = false;
                    _startMousePosition = _mousePosition;
                    _startDragPosition = _mouseOnObject->screenSpacePosition();

                    // If current object is browser, check for resizing
                    if (toBrowser(_mouseOnObject)) {
                        // Resize browser if mouse is over resize button
                        _resizeDirection = toBrowser(_mouseOnObject)->isOnResizeArea(_mousePosition);
                        if (_resizeDirection != glm::vec2{ 0 }) {
                            toBrowser(_mouseOnObject)->saveResizeStartSize();
                            _startBrowserSize = toBrowser(_mouseOnObject)->screenSpaceDimensions();
                            _isResizing = true;
                            return true;
                        }
                    }
                    // If you start dragging around the target, it should unlock
                    if (toTarget(_mouseOnObject)) {
                        toTarget(_mouseOnObject)->unlock();
                    }
                    _isDragging = true;

                    return true;
                }  
                else if (toBrowser(_mouseOnObject) && button == MouseButton::Right) {
                    // If you start dragging around on the browser, the target should unlock
                    if (toBrowser(_mouseOnObject) && toBrowser(_mouseOnObject)->getSkyTarget()) {
                        toBrowser(_mouseOnObject)->getSkyTarget()->unlock();
                    }
                    // Change view (by moving target) within browser if right mouse click on browser
                    _startMousePosition = _mousePosition;
                    _startDragPosition = toBrowser(_mouseOnObject)->getSkyTarget()->screenSpacePosition();
                    _fineTuneMode = true;
                    _isDragging = true;

                    return true;
                }
            }
            else if (action == MouseAction::Release) {
                if (_isDragging) {
                    _isDragging = false;
                    _fineTuneMode = false;
                    return true;
                }
                if (_isResizing) {
                    _isResizing = false;
                    toBrowser(_mouseOnObject)->updateBrowserSize();
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
        _cameraInSolarSystem = cameraSSBDistance < solarSystemRadius;
        double fadingTime = 2.0;
        double deltaTime = global::windowDelegate->deltaTime();

        // Fade out or in browser & target
        for (std::pair<std::string, ScreenSpaceSkyBrowser*> pair : _browsers) {
            ScreenSpaceSkyBrowser* browser = pair.second;
            // If outside solar system and browser is visible
            if (!_cameraInSolarSystem && browser->isEnabled()) {
                bool fadingIsFinished = fadeBrowserAndTarget(true, fadingTime, deltaTime);

                if (fadingIsFinished) {
                    browser->property("Enabled")->set(false);
                    // Select the 3D browser when moving out of the solar system
                    if (_browser3d != nullptr) {
                        _selectedBrowser = _browser3d->renderable()->identifier();
                    }
                }
            }
            // If within solar system and browser is not visible
            else if (_cameraInSolarSystem && !browser->isEnabled()) {
                browser->property("Enabled")->set(true);
                // Select the first 2D browser when moving into the solar system
                if (_browsers.size() != 0) {
                    _selectedBrowser = std::begin(_browsers)->second->identifier();
                }
            }
            // If within solar system and browser is visible
            if (_cameraInSolarSystem && browser->isEnabled()) {
                fadeBrowserAndTarget(false, fadingTime, deltaTime);

                if (browser->getSkyTarget()) {
                    browser->getSkyTarget()->animateToCoordinate(deltaTime);
                }
            }
        }
        if (_isRotating) {
            rotateCamera(deltaTime);
        }
    });
} 

SkyBrowserModule::~SkyBrowserModule() {
    delete _dataHandler;
}

void SkyBrowserModule::internalDeinitialize() {
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
    _dataHandler = new WwtDataHandler();
}

int SkyBrowserModule::getAndIncrementMessageOrder() {
    return _messageOrder++;
}

void SkyBrowserModule::addRenderable(ScreenSpaceRenderable* object) {
    _renderables.push_back(object);
    // Sort on z coordinate, objects closer to camera are in beginning of list
    std::sort(_renderables.begin(), _renderables.end());
    ScreenSpaceSkyBrowser* browser = toBrowser(object);
    if (browser) {      
        _browsers[browser->identifier()] = browser;
    }
}

bool SkyBrowserModule::browserIdExists(std::string id) {
    // If the id doesn't exist, return false
    if (_browsers.find(id) == _browsers.end()) {
        return false;
    }
    return true;
}

void SkyBrowserModule::createTargetBrowserPair() {
    int noOfPairs = static_cast<int>(getSkyBrowsers().size()) + 1;
    std::string nameBrowser = "Sky Browser " + std::to_string(noOfPairs);
    std::string nameTarget = "Sky Target " + std::to_string(noOfPairs);
    std::string idBrowser = "SkyBrowser" + std::to_string(noOfPairs);
    std::string idTarget = "SkyTarget" + std::to_string(noOfPairs);
    glm::vec3 positionBrowser = { -1.0f, -0.5f, -2.1f };
    std::string guiPath = "/SkyBrowser";
    std::string url = "https://data.openspaceproject.com/dist/skybrowser/page/";
    //std::string localHostUrl = "http://localhost:8000";

    const std::string browser = "{"
            "Identifier = '" + idBrowser + "',"
            "Type = 'ScreenSpaceSkyBrowser',"
            "Name = '" + nameBrowser + "',"
            "Url = '"+ url +"',"
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

    openspace::global::scriptEngine->queueScript(
        "openspace.skybrowser.addToSkyBrowserModule('" + idTarget + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    openspace::global::scriptEngine->queueScript(
        "openspace.skybrowser.addToSkyBrowserModule('" + idBrowser + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    openspace::global::scriptEngine->queueScript(
        "openspace.skybrowser.connectBrowserTarget('" + idBrowser + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    openspace::global::scriptEngine->queueScript(
        "openspace.skybrowser.connectBrowserTarget('" + idTarget + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    openspace::global::scriptEngine->queueScript(
        "openspace.skybrowser.setSelectedBrowser('" + idBrowser + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void SkyBrowserModule::removeTargetBrowserPair(std::string& browserId) {
    if (!browserIdExists(browserId)) return;

    ScreenSpaceSkyBrowser* browser = _browsers[browserId];

    // Find corresponding target
    std::string targetId{ "" };
    bool hasTarget = browser->getSkyTarget();
    if (hasTarget) {
        targetId = browser->getSkyTarget()->identifier();
    }
    // Remove pointer to the renderable from browsers vector
    _browsers.erase(browserId);

    // Remove pointer to the renderable from screenspace renderable vector
    _renderables.erase(std::remove_if(std::begin(_renderables), std::end(_renderables),
        [&](ScreenSpaceRenderable* renderable) {
            if (renderable->identifier() == browserId) {
                return true;
            }
            else if (renderable->identifier() == targetId) {
                return true;
            }
            else {
                return false;
            }
        }), std::end(_renderables));
    // Remove from engine
    openspace::global::scriptEngine->queueScript(
        "openspace.removeScreenSpaceRenderable('" + browserId + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    if (hasTarget) {
        openspace::global::scriptEngine->queueScript(
            "openspace.removeScreenSpaceRenderable('" + targetId + "');",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    _mouseOnObject = nullptr;
}

void SkyBrowserModule::place3dBrowser(ImageData& image) {
    if (_browser3d) {
        std::string id = _browser3d->identifier();
        std::string renderableId = _browser3d->renderable()->identifier();
        // Uris for properties
        std::string sizeUri = "Scene." + id + "." + renderableId + ".Size";
        std::string positionUri = "Scene." + id + ".Translation.Position";
        std::string rotationUri = "Scene." + id + ".Rotation.Rotation";
        std::string cameraAim = "NavigationHandler.OrbitalNavigator.Aim";
        glm::dvec3 position = image.position3d * distanceconstants::Parsec;
        // Calculate the size of the plane with trigonometry
        // Calculate in equatorial coordinate system since the FOV is from Earth
        //  /|
        // /_|    Adjacent is the horizontal line, opposite the vertical 
        // \ |    Calculate for half the triangle first, then multiply with 2
        //  \|
        glm::dvec3 j2000 = skybrowser::galacticToEquatorial(position);
        double adjacent = glm::length(j2000);
        double opposite = 2 * adjacent * glm::tan(glm::radians(image.fov * 0.5));

        // Calculate rotation to make the plane face the solar system barycenter
        glm::dvec3 normal = glm::normalize(-position);
        glm::dvec3 newRight = glm::normalize(
            glm::cross(glm::dvec3(0.0, 0.0, 1.0), normal)
        );
        glm::dvec3 newUp = glm::cross(normal, newRight);
        // Face the Solar System Barycenter
        glm::dmat3 rotation = glm::dmat3(1.0);
        rotation[0] = newRight;
        rotation[1] = newUp;
        rotation[2] = normal;

        openspace::global::scriptEngine->queueScript(
            "openspace.setPropertyValueSingle('" + sizeUri + "', " + std::to_string(opposite) + ");",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        openspace::global::scriptEngine->queueScript(
            "openspace.setPropertyValueSingle('" + positionUri + "', " + ghoul::to_string(position) + ");",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        openspace::global::scriptEngine->queueScript(
            "openspace.setPropertyValueSingle('" + rotationUri + "', " + ghoul::to_string(rotation) + ");",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        lookAt3dBrowser();
   
    }
}

void SkyBrowserModule::set3dBrowser(SceneGraphNode* node) {
    _browser3d = node;
}

ScreenSpaceSkyBrowser* SkyBrowserModule::toBrowser(ScreenSpaceRenderable* ptr) {
    return dynamic_cast<ScreenSpaceSkyBrowser*>(ptr);
}
ScreenSpaceSkyTarget* SkyBrowserModule::toTarget(ScreenSpaceRenderable* ptr) {
    return dynamic_cast<ScreenSpaceSkyTarget*>(ptr);
}

WwtDataHandler* SkyBrowserModule::getWWTDataHandler() {
    return _dataHandler;
}

std::map<std::string, ScreenSpaceSkyBrowser*>& SkyBrowserModule::getSkyBrowsers() {
    return _browsers;
}

std::vector<ScreenSpaceRenderable*>& SkyBrowserModule::getBrowsersAndTargets() {
    return _renderables;
}

SceneGraphNode* SkyBrowserModule::get3dBrowser() {
    return _browser3d;
}

void SkyBrowserModule::lookAt3dBrowser() {
    std::string id = _browser3d->identifier();
    // Target camera on the 3D sky browser
    openspace::global::scriptEngine->queueScript(
        "openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.RetargetAnchor\", nil)",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
    openspace::global::scriptEngine->queueScript(
        "openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Anchor\", '" + id + "')",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
    openspace::global::scriptEngine->queueScript(
        "openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Aim\", '')",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void SkyBrowserModule::startRotation(glm::dvec3 endAnimation) {
    // Save coordinates to rotate to in galactic world coordinates
    _endAnimation = endAnimation;
    _startAnimation = skybrowser::cameraDirectionGalactic();
    _isRotating = true;
}

void SkyBrowserModule::rotateCamera(double deltaTime) {
    
    // Find smallest angle between the two vectors
    double smallestAngle = std::acos(glm::dot(_startAnimation, _endAnimation) / (glm::length(_startAnimation) * glm::length(_endAnimation)));
    // Only keep animating when target is not at final position
    if (abs(smallestAngle) > 0.0001) {
        // Calculate rotation this frame
        double rotationAngle = smallestAngle * deltaTime;
        // Create the rotation matrix for local camera space
        glm::dvec3 rotationAxis = glm::normalize(glm::cross(_startAnimation, _endAnimation));
        glm::dmat4 rotmat = glm::rotate(rotationAngle, rotationAxis);
        // Rotate
        global::navigationHandler->camera()->rotate(glm::quat_cast(rotmat));

        // Update camera direction
        _startAnimation = skybrowser::cameraDirectionGalactic();
    }
    else {
        _isRotating = false;
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
    for (std::pair <std::string, ScreenSpaceSkyBrowser*> idAndBrowser : _browsers) {
        ScreenSpaceSkyBrowser* browser = idAndBrowser.second;
        // If there is a target, fade it as well. Otherwise, skip
        ScreenSpaceSkyTarget* target = browser->getSkyTarget();
        bool targetFinished = true;
        if (target) {
            target->setOpacity(target->opacity() + opacityDelta);
            float opacityTarget = abs(target->opacity());
            targetFinished = makeTransparent ? opacityTarget < lowThreshold : opacityTarget > highTreshold;
            if (targetFinished) {
                float newOpacity = makeTransparent ? transparent : opaque;
                target->setOpacity(newOpacity);
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

void SkyBrowserModule::setSelectedBrowser(ScreenSpaceSkyBrowser* browser) {
    if (browser) {
        _selectedBrowser = browser->identifier();
    }
}

void SkyBrowserModule::setSelectedBrowser(std::string id) {
    _selectedBrowser = id;
}

std::string SkyBrowserModule::selectedBrowserId() {
    return _selectedBrowser;
}

int SkyBrowserModule::loadImages(const std::string& root, const std::string& directory) {
   
    // Load speck files for 3D positions
    std::filesystem::path globularClusters = absPath("${BASE}/sync/http/digitaluniverse_globularclusters_speck/2/gc.speck");
    std::filesystem::path openClusters = absPath("${BASE}/sync/http/digitaluniverse_openclusters_speck/2/oc.speck");

    speck::Dataset speckGlobularClusters = speck::loadSpeckFile(globularClusters);
    speck::Dataset speckOpenClusters = speck::loadSpeckFile(openClusters);

    _dataHandler->loadSpeckData(speckGlobularClusters);
    _dataHandler->loadSpeckData(speckOpenClusters);

    int nLoadedImages;

    // Read from disc
    bool loadedImages = _dataHandler->loadWtmlCollectionsFromDirectory(directory);

    // Reading from url if there is no directory
    if (loadedImages) {
        LINFO("Loading images from directory");
    }
    else {
        LINFO("Loading images from url");
        _dataHandler->loadWtmlCollectionsFromUrl(directory, root, "root");
    }

    nLoadedImages = _dataHandler->loadImagesFromLoadedXmls();
    LINFO("Loaded " + std::to_string(nLoadedImages) + " WorldWide Telescope images.");
   
    return nLoadedImages;
}

bool SkyBrowserModule::cameraInSolarSystem() {
    return _cameraInSolarSystem;
}

//std::vector<documentation::Documentation> SkyBrowserModule::documentations() const {
//    return {
//        ExoplanetsDataPreparationTask::documentation(),
//        RenderableOrbitDisc::Documentation()
//    };
//}

} // namespace openspace
