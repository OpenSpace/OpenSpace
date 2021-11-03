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


#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/factorymanager.h>
#include <modules/skybrowser/include/wwtdatahandler.h>
#include <modules/skybrowser/skybrowsermodule.h>
#include "skybrowsermodule_lua.inl"
#include <glm/gtx/vector_angle.hpp>
#include <glm/gtx/string_cast.hpp> // For printing glm data
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <cmath> // For atan2
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
                "add3dBrowserToSkyBrowserModule",
                &skybrowser::luascriptfunctions::add3dBrowserToSkyBrowserModule,
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
            {
                "addPairToSkyBrowserModule",
                &skybrowser::luascriptfunctions::addPairToSkyBrowserModule,
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
{
    global::callback::mouseButton->emplace_back(
        [&](MouseButton button, MouseAction action, KeyModifier modifier) -> bool {

            if (_mouseOnPair && action == MouseAction::Press) {

                // Get the currently selected browser
                setSelectedBrowser(_mouseOnPair->getBrowser());

                if (button == MouseButton::Left) {
                    _cameraIsRotating = false;
                    _startMousePosition = _mousePosition;
                    _startDragPosition = _isBrowser ? _mouseOnPair->getBrowser()->screenSpacePosition()
                        : _mouseOnPair->getTarget()->screenSpacePosition();

                    // If current object is browser, check for resizing
                    if (_isBrowser) {
                        // Resize browser if mouse is over resize button
                        _resizeDirection = _mouseOnPair->getBrowser()->isOnResizeArea(_mousePosition);
                        if (_resizeDirection != glm::vec2{ 0 }) {
                            _mouseOnPair->getBrowser()->saveResizeStartSize();
                            _startBrowserSize = _mouseOnPair->getBrowser()->screenSpaceDimensions();
                            _isResizing = true;
                            return true;
                        }
                    }
                    // If you start dragging around the target, it should unlock
                    else {
                        _mouseOnPair->getTarget()->unlock();
                    }
                    _isDragging = true;

                    return true;
                }
                else if (_isBrowser && button == MouseButton::Right) {
                    // If you start dragging around on the browser, the target should unlock
                    _mouseOnPair->getTarget()->unlock();
                    // Change view (by moving target) within browser if right mouse click on browser
                    _startMousePosition = _mousePosition;
                    _startDragPosition = _mouseOnPair->getTarget()->screenSpacePosition();
                    _fineTuneMode = true;

                    return true;
                }
            }
            else if (action == MouseAction::Release) {
                if (_isDragging) {
                    _isDragging = false;

                    return true;
                }
                if (_fineTuneMode) {
                    _fineTuneMode = false;
                    return true;
                }
                if (_isResizing) {
                    _isResizing = false;
                    _mouseOnPair->getBrowser()->updateBrowserSize();
                    return true;
                }
            }

            return false;
        }
    );

    global::callback::mousePosition->emplace_back(
        [&](double x, double y) {    
            
            if (_cameraInSolarSystem) {
                glm::vec2 pixel{ static_cast<float>(x), static_cast<float>(y) };
                _mousePosition = skybrowser::pixelToScreenSpace(pixel);
                glm::vec2 translation = _mousePosition - _startMousePosition;

                if (_isDragging || _isResizing) {
                    if (_isResizing) {
                        // Calculate scaling factor
                        glm::vec2 mouseDragVector = (_mousePosition - _startMousePosition);
                        glm::vec2 scalingVector = mouseDragVector * _resizeDirection;
                        glm::vec2 newSizeRelToOld = (_startBrowserSize + (scalingVector)) / _startBrowserSize;
                        // Scale the browser
                        _mouseOnPair->getBrowser()->setScale(newSizeRelToOld);

                        // For dragging functionality, translate so it looks like the browser 
                        // isn't moving. Make sure the browser doesn't move in directions it's 
                        // not supposed to 
                        translation = mouseDragVector * abs(_resizeDirection) / 2.f;

                    }
                    // Translate
                    if (_isBrowser) {
                        _mouseOnPair->getBrowser()->translate(translation, _startDragPosition);
                    }
                    else {
                        _mouseOnPair->getTarget()->translate(translation, _startDragPosition);
                    }
                }
                else if (_fineTuneMode) {
                    glm::vec2 fineTune = _mouseOnPair->getBrowser()->fineTuneTarget(translation);
                    _mouseOnPair->getTarget()->translate(fineTune, _startDragPosition);
                }
                // If there is no dragging or resizing, look for new objects
                else {
                    setSelectedObject();
                }
            }
           
        }
    );

    global::callback::mouseScrollWheel->emplace_back(
        [&](double, double scroll) -> bool {
            
            // If mouse is on browser or target, apply zoom
            if (_mouseOnPair) {
                _mouseOnPair->getBrowser()->setVerticalFovWithScroll(static_cast<float>(scroll));
                return true;
            }
            
            return false;
        }
    );


    global::callback::preSync->emplace_back([this]() {
        // Disable browser and targets when camera is outside of solar system
        double cameraSSBDistance = glm::length(
            global::navigationHandler->camera()->positionVec3());
        _cameraInSolarSystem = cameraSSBDistance < _solarSystemRadius;
        double deltaTime = global::windowDelegate->deltaTime();

        // Fade out or in browser & target
        for (SkyBrowserModule::Pair pair : _targetsBrowsers) {
            ScreenSpaceSkyBrowser* browser = pair.getBrowser();
            // If outside solar system and browser is visible
            if (!_cameraInSolarSystem && browser->isEnabled()) {
                bool fadingIsFinished = fadeBrowserAndTarget(true, _fadingTime, deltaTime);

                if (fadingIsFinished) {
                    pair.getBrowser()->property("Enabled")->set(false);
                    // Select the 3D browser when moving out of the solar system
                    if (_browser3d != nullptr) {
                        _selectedBrowser = _browser3d->renderable()->identifier();
                    }
                }
            }
            // If within solar system and browser is not visible
            else if (_cameraInSolarSystem && !browser->isEnabled()) {
                pair.getBrowser()->property("Enabled")->set(true);
                // Select the first 2D browser when moving into the solar system
                if (_targetsBrowsers.size() != 0) {
                    _selectedBrowser = std::begin(_targetsBrowsers)->getBrowser()->identifier();
                }
            }
            // If within solar system and browser is visible
            if (_cameraInSolarSystem && browser->isEnabled()) {
                fadeBrowserAndTarget(false, _fadingTime, deltaTime);

                pair.getTarget()->animateToCoordinate(deltaTime);
            }
        }
        if (_cameraIsRotating) {
            rotateCamera(deltaTime);
        }
    });

    _hoverCircle = dynamic_cast<ScreenSpaceImageLocal*>(
        global::renderEngine->screenSpaceRenderable("HoverCircle"));
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

void SkyBrowserModule::setSelectedObject()
{
    // Save old selection for removing highlight
    Pair* lastObj = _mouseOnPair;

    // Find and save what mouse is currently hovering on
    auto it = std::find_if(std::begin(_targetsBrowsers), std::end(_targetsBrowsers),[&](Pair &pair) {            
            bool onBrowser = pair.getBrowser()->coordIsInsideCornersScreenSpace(_mousePosition);
            bool onTarget = pair.getTarget()->coordIsInsideCornersScreenSpace(_mousePosition);
            if (onBrowser || onTarget) {
                _mouseOnPair = &pair;
                _isBrowser = onBrowser;
                return true;
            }
            return false;
        });

    if (it == std::end(_targetsBrowsers)) {
        _mouseOnPair = nullptr;
    }


    // Selection has changed
    if (lastObj != _mouseOnPair) {
       
        // Remove highlight
        if (lastObj) {
            lastObj->getBrowser()->removeHighlight(_highlightAddition);
            lastObj->getTarget()->removeHighlight(_highlightAddition);
        }
        // Add highlight to new selection
        if (_mouseOnPair) {
            _mouseOnPair->getBrowser()->highlight(_highlightAddition);
            _mouseOnPair->getTarget()->highlight(_highlightAddition);
            std::string id = _mouseOnPair->getBrowser()->identifier();
            std::string info = "Currently selected browser is " + id;
            LINFO(info + id);
        }
        
    }
}

void SkyBrowserModule::addTargetBrowserPair(ScreenSpaceSkyTarget* newTarget, ScreenSpaceSkyBrowser* newBrowser) {
    // Assert pair to have both target and browser
    if (newBrowser && newTarget) {
        Pair newPair(newBrowser, newTarget);
        _targetsBrowsers.push_back(newPair);
    }
}

void SkyBrowserModule::createTargetBrowserPair() {
    int noOfPairs = static_cast<int>(_targetsBrowsers.size()) + 1;
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
            "TargetId = '" + idTarget + "',"
            "CartesianPosition = " + ghoul::to_string(positionBrowser) + ","
        "}";
    const std::string target = "{"
            "Identifier = '" + idTarget + "',"
            "Type = 'ScreenSpaceSkyTarget',"
            "Name = '" + nameTarget + "',"
            "FaceCamera = false,"
            "BrowserId = '" + idBrowser + "',"
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
        "openspace.skybrowser.connectBrowserTarget('" + idBrowser + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    openspace::global::scriptEngine->queueScript(
        "openspace.skybrowser.connectBrowserTarget('" + idTarget + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    openspace::global::scriptEngine->queueScript(
        "openspace.skybrowser.addPairToSkyBrowserModule('" + idTarget + "','" + idBrowser + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    openspace::global::scriptEngine->queueScript(
        "openspace.skybrowser.setSelectedBrowser('" + idBrowser + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void SkyBrowserModule::removeTargetBrowserPair(std::string& id) {

    Pair* found = getPair(id);
    if (!found) {
        return;
    }
    auto it = std::remove(std::begin(_targetsBrowsers), std::end(_targetsBrowsers),
                          *found);

    std::string browserId = found->getBrowser()->identifier();
    std::string targetId = found->getTarget()->identifier();
    // Remove from engine
    openspace::global::scriptEngine->queueScript(
        "openspace.removeScreenSpaceRenderable('" + browserId + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    openspace::global::scriptEngine->queueScript(
        "openspace.removeScreenSpaceRenderable('" + targetId + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    _targetsBrowsers.erase(it, std::end(_targetsBrowsers));
    _mouseOnPair = nullptr;
}


void SkyBrowserModule::set3dBrowser(SceneGraphNode* node) {
    _browser3d = node;
}

void SkyBrowserModule::selectImage2dBrowser(int i)
{
    Pair* selected = getPair(_selectedBrowser);
    if (selected) {

        const ImageData& image = _dataHandler->getImage(i);
        // Load image into browser
        LINFO("Loading image " + image.name);
        selected->getBrowser()->addSelectedImage(image, i);

        // If the image has coordinates, move the target
        if (image.hasCelestialCoords) {

            // Animate the target to the image coordinate position
            selected->getTarget()->unlock();
            selected->getTarget()->startAnimation(image.equatorialCartesian, image.fov);

            // If the coordinate is not in view, rotate camera
            if (skybrowser::coordinateIsOutsideView(image.equatorialCartesian)) {
                glm::dvec3 galactic = skybrowser::equatorialToGalactic(image.equatorialCartesian);
                startRotation(galactic);
            }
        }
    }
}


void SkyBrowserModule::selectImage3dBrowser(int i)
{
    const ImageData& image = _dataHandler->getImage(i);
    if (_browser3d) {
        RenderableSkyBrowser* browser3d = dynamic_cast<RenderableSkyBrowser*>(
            _browser3d->renderable());
        if (browser3d) {
            browser3d->displayImage(image, i);
        }
    }
}

void SkyBrowserModule::moveHoverCircle(int i)
{
    const ImageData& image = _dataHandler->getImage(i);

    // Only move and show circle if the image has coordinates
    if (_hoverCircle && image.hasCelestialCoords && _cameraInSolarSystem) {
        // Make circle visible
        _hoverCircle->property("Enabled")->set(true);

        // Calculate coords for the circle and translate
        glm::vec3 coordsScreen = skybrowser::equatorialToScreenSpace(
            image.equatorialCartesian
        );
        _hoverCircle->property("CartesianPosition")->set(coordsScreen);
    }
}

void SkyBrowserModule::loadImages(const std::string& root, const std::string& directory, 
                                 std::vector<std::filesystem::path>& speckFiles)
{
    _dataHandler->loadImages(root, directory, speckFiles);
}

int SkyBrowserModule::nLoadedImages()
{
    return _dataHandler->nLoadedImages();
}

const WwtDataHandler* SkyBrowserModule::getWWTDataHandler() {
    return _dataHandler;
}

std::vector<SkyBrowserModule::Pair>& SkyBrowserModule::getPairs()
{
    return _targetsBrowsers;
}

SkyBrowserModule::Pair* SkyBrowserModule::getPair(std::string id)
{
    auto it = std::find_if(std::begin(_targetsBrowsers), std::end(_targetsBrowsers),
        [&](Pair& pair) {
            bool foundBrowser = pair.getBrowser()->identifier() == id;
            bool foundTarget = pair.getTarget()->identifier() == id;
            return foundBrowser || foundTarget;
        });
    return &(*it);
}

SceneGraphNode* SkyBrowserModule::get3dBrowser() {
    return _browser3d;
}

void SkyBrowserModule::lookAt3dBrowser() {
    std::string id = _browser3d->identifier();
    // Target camera on the 3D sky browser
    openspace::global::scriptEngine->queueScript(
        "openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator."
        "RetargetAnchor\", nil)",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
    openspace::global::scriptEngine->queueScript(
        "openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator."
        "Anchor\", '" + id + "')",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
    openspace::global::scriptEngine->queueScript(
        "openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator."
        "Aim\", '')",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

void SkyBrowserModule::startRotation(glm::dvec3 endAnimation) {
    // Save coordinates to rotate to in galactic world coordinates
    _endAnimation = endAnimation;
    _startAnimation = skybrowser::cameraDirectionGalactic();
    _cameraIsRotating = true;
}

void SkyBrowserModule::rotateCamera(double deltaTime) {

    // Find smallest angle between the two vectors
    double smallestAngle = skybrowser::angleVector(_startAnimation, _endAnimation);

    if(smallestAngle > _threshold) {
        
        glm::dmat4 rotMat;
        skybrowser::incrementalAnimationMatrix(
            rotMat,
            _startAnimation, 
            _endAnimation, 
            deltaTime, 
            _speed
        );

        // Rotate
        global::navigationHandler->camera()->rotate(glm::quat_cast(rotMat));

        // Update camera direction
        _startAnimation = skybrowser::cameraDirectionGalactic();
    }
    else {
        _cameraIsRotating = false;
    }
}

bool SkyBrowserModule::fadeBrowserAndTarget(bool makeTransparent, double fadeTime, 
                                            double deltaTime) {
    float opacityDelta = static_cast<float>(deltaTime / fadeTime);
    float highTreshold = 0.99f;
    float lowThreshold = 0.01f;
    float transparent = 0.0;
    float opaque = 1.0;

    if (makeTransparent) {
        opacityDelta *= -1.f;
    }
    bool finished = true;

    for (Pair pair : _targetsBrowsers) {
        ScreenSpaceSkyBrowser* browser = pair.getBrowser();
        ScreenSpaceSkyTarget* target = pair.getTarget();

        bool targetFinished = true;
        bool browserFinished = true;

        if (target) {
            target->setOpacity(target->opacity() + opacityDelta);
            float opacityTarget = abs(target->opacity());
            
            if (makeTransparent) {
                targetFinished = opacityTarget < lowThreshold;
            }
            else {
                targetFinished = opacityTarget > highTreshold;
            }
            if (targetFinished) {
                float newOpacity = makeTransparent ? transparent : opaque;
                target->setOpacity(newOpacity);
            }
        }
        // Keep fading the browsers until all are finished
        browser->getOpacity() = browser->getOpacity().value() + opacityDelta;
        float opacityBrowser = abs(browser->getOpacity().value());

        if (makeTransparent) {
            browserFinished = opacityBrowser < lowThreshold;
        }
        else {
            browserFinished = opacityBrowser > highTreshold;
        }
        
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
