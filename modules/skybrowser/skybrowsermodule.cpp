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

#include <modules/skybrowser/include/renderableskybrowser.h>
#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include "skybrowsermodule_lua.inl"
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/camera.h>
#include <openspace/util/factorymanager.h>

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
    // Find the hover circle
    _hoverCircle = dynamic_cast<ScreenSpaceImageLocal*>(
        global::renderEngine->screenSpaceRenderable("HoverCircle"));

    // Set callback functions
    global::callback::mouseButton->emplace_back(
        [&](MouseButton button, MouseAction action, KeyModifier modifier) -> bool {

            if (_mouseOnPair && action == MouseAction::Press) {

                // Get the currently selected browser
                setSelectedBrowser(_mouseOnPair->getBrowser()->identifier());

                if (button == MouseButton::Left) {
                    _isCameraRotating = false;
                    _startMousePosition = _mousePosition;
                    if (_isBrowser) {
                        _startDragPosition = _mouseOnPair->getBrowser()->
                                             screenSpacePosition();
                    }
                    else {
                        _startDragPosition = _mouseOnPair->getTarget()->
                                             screenSpacePosition();
                    }

                    // If current object is browser, check for resizing
                    if (_isBrowser) {
                        // Resize browser if mouse is over resize button
                        _resizeDirection = _mouseOnPair->getBrowser()->isOnResizeArea(
                            _mousePosition
                        );
                        if (_resizeDirection != glm::ivec2{ 0 }) {
                            _mouseOnPair->getBrowser()->saveResizeStartSize();
                            _startBrowserSize = _mouseOnPair->getBrowser()->
                                                screenSpaceDimensions();
                            _isResizing = true;
                            return true;
                        }
                    }
                    // If you start dragging around the target, it should unlock
                    else {
                        _mouseOnPair->unlock();
                    }
                    _isDragging = true;

                    return true;
                }
                else if (_isBrowser && button == MouseButton::Right) {
                    // If you start dragging around on the browser, the target unlocks
                    _mouseOnPair->unlock();
                    // Change view (by moving target) within browser if right mouse 
                    // click on browser
                    _startMousePosition = _mousePosition;
                    _startDragPosition = _mouseOnPair->getTarget()->screenSpacePosition();
                    _isFineTuneMode = true;

                    return true;
                }
            }
            else if (action == MouseAction::Release) {
                if (_isDragging) {
                    _isDragging = false;

                    return true;
                }
                if (_isFineTuneMode) {
                    _isFineTuneMode = false;
                    return true;
                }
                if (_isResizing) {
                    _isResizing = false;
                    _mouseOnPair->updateBrowserSize();
                    return true;
                }
            }

            return false;
        }
    );

    global::callback::mousePosition->emplace_back(
        [&](double x, double y) {    
            
            if (_isCameraInSolarSystem) {
                glm::vec2 pixel{ static_cast<float>(x), static_cast<float>(y) };
                _mousePosition = skybrowser::pixelToScreenSpace2d(pixel);
                glm::vec2 translation = _mousePosition - _startMousePosition;

                if (_isDragging || _isResizing) {
                    if (_isResizing) {
                        // Calculate scaling factor
                        glm::vec2 mouseDragVector = (_mousePosition-_startMousePosition);
                        glm::vec2 scaling = mouseDragVector * glm::vec2(_resizeDirection);
                        glm::vec2 newSizeRelToOld = (_startBrowserSize + (scaling)) / 
                                                     _startBrowserSize;
                        // Scale the browser
                        _mouseOnPair->getBrowser()->setScale(newSizeRelToOld);

                        // For dragging functionality, translate so it looks like the 
                        // browser isn't moving. Make sure the browser doesn't move in 
                        // directions it's not supposed to 
                        translation = 0.5f * mouseDragVector * abs(
                            glm::vec2(_resizeDirection)
                        );

                    }
                    // Translate
                    if (_isBrowser) {
                        _mouseOnPair->getBrowser()->translate(
                            translation, 
                            _startDragPosition
                        );
                    }
                    else {
                        _mouseOnPair->getTarget()->translate(
                            translation, 
                            _startDragPosition
                        );
                    }
                }
                else if (_isFineTuneMode) {
                    glm::vec2 fineTune = _mouseOnPair->getBrowser()->fineTuneVector(
                        translation
                    );
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
                _mouseOnPair->getBrowser()->setVerticalFovWithScroll(
                    static_cast<float>(scroll)
                );
                return true;
            }
            
            return false;
        }
    );


    global::callback::preSync->emplace_back([this]() {

        // Disable browser and targets when camera is outside of solar system
        glm::dvec3 cameraPos = global::navigationHandler->camera()->positionVec3();
        double deltaTime = global::windowDelegate->deltaTime();
        bool camWasInSolarSystem = _isCameraInSolarSystem;
        _isCameraInSolarSystem = glm::length(cameraPos) < SolarSystemRadius;

        // Fading flags
        if (_isCameraInSolarSystem != camWasInSolarSystem) {
            _isTransitioningVizMode = true;

            // Select the 3D browser when moving out of the solar system
            if (!_isCameraInSolarSystem && _browser3dNode) {
                _selectedBrowser = _browser3dNode->renderable()->identifier();
            }
        }

        // Fade pairs if the camera moved in or out the solar system
        if (_isTransitioningVizMode) {
            if (_isCameraInSolarSystem) {
                incrementallyFadeBrowserTargets(Transparency::Opaque, deltaTime);
            }
            else {
                incrementallyFadeBrowserTargets(Transparency::Transparent, deltaTime);
            }
        }

        if (_isCameraInSolarSystem) {
            incrementallyAnimateTargets(deltaTime);
        }
        if (_isCameraRotating) {
            incrementallyRotateCamera(deltaTime);
        }
    }); 
} 

SkyBrowserModule::~SkyBrowserModule() {
}

void SkyBrowserModule::internalDeinitialize() {
}

void SkyBrowserModule::internalInitialize(const ghoul::Dictionary& dict) {
    
    const Parameters p = codegen::bake<Parameters>(dict);

    // register ScreenSpaceBrowser
    auto fScreenSpaceRenderable = FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");

    // register ScreenSpaceTarget
    fScreenSpaceRenderable->registerClass<ScreenSpaceSkyTarget>("ScreenSpaceSkyTarget");

    // register ScreenSpaceTarget
    fScreenSpaceRenderable->registerClass<ScreenSpaceSkyBrowser>("ScreenSpaceSkyBrowser");

    // Register Renderable Skybrowser
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "Renderable factory was not created");
    fRenderable->registerClass<RenderableSkyBrowser>("RenderableSkyBrowser");
    // Create data handler dynamically to avoid the linking error that
    // came up when including the include file in the module header file
    _dataHandler = std::make_unique<WwtDataHandler>();
}

void SkyBrowserModule::setSelectedObject()
{
    // Save old selection for removing highlight
    Pair* lastObj = _mouseOnPair;

    // Find and save what mouse is currently hovering on
    auto it = std::find_if(std::begin(_targetsBrowsers), std::end(_targetsBrowsers),
        [&] (const std::unique_ptr<Pair> &pair) {      
            bool onBrowser = pair->getBrowser()->coordIsInsideCornersScreenSpace(
                _mousePosition
            );
            bool onTarget = pair->getTarget()->coordIsInsideCornersScreenSpace(
                _mousePosition
            );
            if (onBrowser) {
                _selectedBrowser = pair->getBrowser()->identifier();
            }
            _isBrowser = onBrowser;

            return onBrowser || onTarget;
        });

    if (it == std::end(_targetsBrowsers)) {
        _mouseOnPair = nullptr;
    }
    else {
        _mouseOnPair = it->get();
    }

    // Selection has changed
    if (lastObj != _mouseOnPair) {
       
        // Remove highlight
        if (lastObj) {
            lastObj->removeHighlight(_highlightAddition);
        }
        // Add highlight to new selection
        if (_mouseOnPair) {
            _mouseOnPair->highlight(_highlightAddition);
        }
        
    }
}

void SkyBrowserModule::addTargetBrowserPair(std::string targetId, std::string browserId) {
    
    ScreenSpaceSkyTarget* target = dynamic_cast<ScreenSpaceSkyTarget*>(
        global::renderEngine->screenSpaceRenderable(targetId)
        );
    ScreenSpaceSkyBrowser* browser = dynamic_cast<ScreenSpaceSkyBrowser*>(
        global::renderEngine->screenSpaceRenderable(browserId)
        );
    
    // Assert pair to have both target and browser
    if (browser && target) {
        _targetsBrowsers.push_back(std::make_unique<Pair>(browser, target));
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
            "CartesianPosition = " + ghoul::to_string(positionBrowser) + ","
        "}";
    const std::string target = "{"
            "Identifier = '" + idTarget + "',"
            "Type = 'ScreenSpaceSkyTarget',"
            "Name = '" + nameTarget + "',"
            "FaceCamera = false,"
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
        "openspace.skybrowser.addPairToSkyBrowserModule('" + idTarget + "','"
        + idBrowser + "');",
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
    auto it = std::remove_if(std::begin(_targetsBrowsers), std::end(_targetsBrowsers),
        [&](const std::unique_ptr<Pair>& pair) {
            return *found == *(pair.get());
        });
                          
    std::string targetId = found->getTarget()->identifier();
    // Remove from engine
    openspace::global::scriptEngine->queueScript(
        "openspace.removeScreenSpaceRenderable('" + found->browserId() + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    openspace::global::scriptEngine->queueScript(
        "openspace.removeScreenSpaceRenderable('" + targetId + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    _targetsBrowsers.erase(it, std::end(_targetsBrowsers));
    _mouseOnPair = nullptr;
}

void SkyBrowserModule::set3dBrowser(const std::string& id)
{
    SceneGraphNode* node = global::renderEngine->scene()->sceneGraphNode(id);
    if (node) {
        // Add to module
        _browser3dNode = node;
        _browser3d = dynamic_cast<RenderableSkyBrowser*>(_browser3dNode->renderable());
    }
}

void SkyBrowserModule::selectImage2dBrowser(int i)
{
    Pair* selected = getPair(_selectedBrowser);
    if (selected) {

        const ImageData& image = _dataHandler->getImage(i);
        // Load image into browser
        LINFO("Loading image " + image.name);
        selected->selectImage(image, i);
        
        bool isInView = skybrowser::isCoordinateInView(image.equatorialCartesian);
        // If the coordinate is not in view, rotate camera
        if (image.hasCelestialCoords) {
            if(!isInView) {
                startRotatingCamera(
                    skybrowser::equatorialToGalactic(image.equatorialCartesian)
                );
            }
        }
    }
}

void SkyBrowserModule::selectImage3dBrowser(int i)
{
    if (!_browser3dNode) {
        return;
    }
    RenderableSkyBrowser* renderable = dynamic_cast<RenderableSkyBrowser*>(
        _browser3dNode->renderable());
    if (renderable) {
        const ImageData& image = _dataHandler->getImage(i);
        renderable->displayImage(image.imageUrl, i);
    }
    
}

void SkyBrowserModule::lookAtTarget(std::string id)
{
    Pair* pair = getPair(id);
    if (pair) {
        startRotatingCamera(pair->targetDirectionGalactic());
    }
}
    
void SkyBrowserModule::moveHoverCircle(int i)
{
    const ImageData& image = _dataHandler->getImage(i);

    // Only move and show circle if the image has coordinates
    if (_hoverCircle && image.hasCelestialCoords && _isCameraInSolarSystem) {
        // Make circle visible
        _hoverCircle->property("Enabled")->set(true);

        // Calculate coords for the circle and translate
        glm::vec3 coordsScreen = skybrowser::equatorialToScreenSpace3d(
            image.equatorialCartesian
        );
        _hoverCircle->property("CartesianPosition")->set(coordsScreen);
    }
}

void SkyBrowserModule::disableHoverCircle()
{
    if (_hoverCircle && _hoverCircle->isEnabled()) {
        _hoverCircle->property("Enabled")->set(false);
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

void SkyBrowserModule::add2dSelectedImagesTo3d(const std::string& pairId)
{
    Pair* pair = getPair(pairId);

    if (pair && get3dBrowser()) {

        // Copy 2D selection of images to 3D browser
        const std::deque<int> images = pair->getSelectedImages();
        std::for_each(std::begin(images), std::end(images), [&](const int i) {
            const ImageData& image = _dataHandler->getImage(i);
            get3dBrowser()->displayImage(image.imageUrl, i);
            });
    }
}

const std::unique_ptr<WwtDataHandler>& SkyBrowserModule::getWWTDataHandler() {
    return _dataHandler;
}

std::vector<std::unique_ptr<Pair>>& SkyBrowserModule::getPairs()
{
    return _targetsBrowsers;
}

Pair* SkyBrowserModule::getPair(const std::string& id)
{
    auto it = std::find_if(std::begin(_targetsBrowsers), std::end(_targetsBrowsers),
        [&](const std::unique_ptr<Pair>& pair) {
            bool foundBrowser = pair->browserId() == id;
            bool foundTarget = pair->targetId() == id;
            return foundBrowser || foundTarget;
        });
    if (it == std::end(_targetsBrowsers)) {
        return nullptr;
    }
    else {
        return it->get();
    }
}

SceneGraphNode* SkyBrowserModule::get3dBrowserNode() {
    return _browser3dNode;
}

RenderableSkyBrowser* SkyBrowserModule::get3dBrowser(const std::string& id)
{  
    if (_browser3dNode->identifier() == id || _browser3d->identifier() == id) {
        return _browser3d;
    }
    else {
        return nullptr;
    }
    
}

RenderableSkyBrowser* SkyBrowserModule::get3dBrowser()
{
    return _browser3d;
}

void SkyBrowserModule::lookAt3dBrowser() {
    if (!_browser3dNode) {
        return;
    }
    std::string id = _browser3dNode->identifier();
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

void SkyBrowserModule::place3dBrowser(const ImageData& image, const int i)
{
    // If the image has a 3D position, add it to the scene graph
    if (image.has3dCoords && get3dBrowser()) {
        get3dBrowser()->displayImage(image.imageUrl, i);
        get3dBrowser()->placeAt3dPosition(image);
    }
    else {
        LINFO("Image has no 3D coordinate!");
    }
}

void SkyBrowserModule::startRotatingCamera(glm::dvec3 endAnimation) {
    // Save coordinates to rotate to in galactic world coordinates
    _endAnimation = endAnimation;
    _startAnimation = skybrowser::cameraDirectionGalactic();
    _isCameraRotating = true;
}

void SkyBrowserModule::incrementallyRotateCamera(double deltaTime) {

    // Find smallest angle between the two vectors
    double smallestAngle = skybrowser::angleBetweenVectors(_startAnimation, _endAnimation);

    if(smallestAngle > _stopAnimationThreshold) {
        
        glm::dmat4 rotMat = skybrowser::incrementalAnimationMatrix(
            _startAnimation, 
            _endAnimation, 
            deltaTime, 
            _animationSpeed
        );

        // Rotate
        global::navigationHandler->camera()->rotate(glm::quat_cast(rotMat));

        // Update camera direction
        _startAnimation = skybrowser::cameraDirectionGalactic();
    }
    else {
        _isCameraRotating = false;
    }
}

void SkyBrowserModule::incrementallyFadeBrowserTargets(Transparency goal, float deltaTime)
{
    float transparency = [](Transparency goal) {
        switch (goal) {
        case Transparency::Transparent:
            return 0.f;

        case Transparency::Opaque:
            return 1.f;
        }
    }(goal);
    
     bool isAllFinished{ false };
     for (std::unique_ptr<Pair>& pair : _targetsBrowsers) {
         if (pair->isEnabled()) {
             bool isPairFinished = pair->hasFinishedFading(transparency);
             if (!isPairFinished) {
                 pair->incrementallyFade(transparency, _fadingTime, deltaTime);
             }
             else if (isPairFinished && goal == Transparency::Transparent) {
                 pair->disable();
             }
             isAllFinished &= isPairFinished;
         }
     }

     // The transition is over when the fade is finished
     if (isAllFinished) {
         _isTransitioningVizMode = false;
     }
}

void SkyBrowserModule::incrementallyAnimateTargets(double deltaTime)
{
    for (std::unique_ptr<Pair>& pair : _targetsBrowsers) {
        if (pair->isEnabled()) {
            pair->incrementallyAnimateToCoordinate(deltaTime);
        }
    }
}

void SkyBrowserModule::setSelectedBrowser(const std::string& id) {
    if (getPair(id) || (_browser3dNode && _browser3dNode->identifier() == id)) {
        _selectedBrowser = id;
    }
}

std::string SkyBrowserModule::selectedBrowserId() {
    return _selectedBrowser;
}

bool SkyBrowserModule::isCameraInSolarSystem() {
    return _isCameraInSolarSystem;
}

//std::vector<documentation::Documentation> SkyBrowserModule::documentations() const {
//    return {
//        ExoplanetsDataPreparationTask::documentation(),
//        RenderableOrbitDisc::Documentation()
//    };
//}

} // namespace openspace
