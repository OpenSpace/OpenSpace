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
#include <modules/skybrowser/include/pair.h>
#include <modules/skybrowser/include/screenspaceskytarget.h>
#include <modules/skybrowser/include/wwtdatahandler.h>
#include <modules/base/rendering/screenspaceimagelocal.h>
#include "skybrowsermodule_lua.inl"
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/camera/camera.h>
#include <openspace/util/factorymanager.h>


namespace {

    constexpr const openspace::properties::Property::PropertyInfo AllowInteractionInfo =
    {
        "AllowMouseInteraction",
        "Allow Mouse Interaction",
        "Toggles if it is possible to interact with the sky browser and sky targets with "
        "the mouse or not."
    };
    
    constexpr const openspace::properties::Property::PropertyInfo AllowRotationInfo =
    {
        "AllowCameraRotation",
        "Allow Camera Rotation",
        "Toggles if the camera should rotate to look at the sky target if it is going "
        "outside of the current field of view."
    };

    struct [[codegen::Dictionary(SkyBrowserModule)]] Parameters {
        // [[codegen::verbatim(AllowInteractionInfo.description)]]
        std::optional<bool> allowMouseInteraction;

        // [[codegen::verbatim(AllowRotationInfo.description)]]
        std::optional<bool> allowCameraRotation;
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
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            }, 
            {
                "setHoverCircle",
                &skybrowser::luascriptfunctions::setHoverCircle,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "moveCircleToHoverImage",
                &skybrowser::luascriptfunctions::moveCircleToHoverImage,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "disableHoverCircle",
                &skybrowser::luascriptfunctions::disableHoverCircle,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "loadImagesToWWT",
                &skybrowser::luascriptfunctions::loadImagesToWWT,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "selectImage",
                &skybrowser::luascriptfunctions::selectImage,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            }, 
            {
                "removeSelectedImageInBrowser",
                &skybrowser::luascriptfunctions::removeSelectedImageInBrowser,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "adjustCamera",
                & skybrowser::luascriptfunctions::adjustCamera,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "setSelectedBrowser",
                & skybrowser::luascriptfunctions::setSelectedBrowser,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "getTargetData",
                &skybrowser::luascriptfunctions::getTargetData,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "lockTarget",
                &skybrowser::luascriptfunctions::lockTarget,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "unlockTarget",
                &skybrowser::luascriptfunctions::unlockTarget,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "createTargetBrowserPair",
                &skybrowser::luascriptfunctions::createTargetBrowserPair,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            }, 
             {
                "removeTargetBrowserPair",
                &skybrowser::luascriptfunctions::removeTargetBrowserPair,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "place3dSkyBrowser",
                &skybrowser::luascriptfunctions::place3dSkyBrowser,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            }, 
            {
                "setOpacityOfImageLayer",
                &skybrowser::luascriptfunctions::setOpacityOfImageLayer,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },   
            {
                "sendOutIdsToBrowsers",
                &skybrowser::luascriptfunctions::sendOutIdsToBrowsers,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "initializeBrowser",
                &skybrowser::luascriptfunctions::initializeBrowser,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },  
           {
                "add3dBrowserToSkyBrowserModule",
                &skybrowser::luascriptfunctions::add3dBrowserToSkyBrowserModule,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },  
            {
                "set3dSelectedImagesAs2dSelection",
                &skybrowser::luascriptfunctions::set3dSelectedImagesAs2dSelection,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },   
            {
                "centerTargetOnScreen",
                &skybrowser::luascriptfunctions::centerTargetOnScreen,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            }, 
            {
                "setImageLayerOrder",
                &skybrowser::luascriptfunctions::setImageLayerOrder,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            }, 
            {
                "addPairToSkyBrowserModule",
                &skybrowser::luascriptfunctions::addPairToSkyBrowserModule,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "setEquatorialAim",
                &skybrowser::luascriptfunctions::setEquatorialAim,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "setVerticalFov",
                &skybrowser::luascriptfunctions::setVerticalFov,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "setBorderColor",
                &skybrowser::luascriptfunctions::setBorderColor,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "setScreenSpaceSize",
                &skybrowser::luascriptfunctions::setScreenSpaceSize,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "startSetup",
                &skybrowser::luascriptfunctions::startSetup,
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
        };

        return res;
    }



SkyBrowserModule::SkyBrowserModule()
    : OpenSpaceModule(SkyBrowserModule::Name)
    , _allowMouseInteraction(AllowInteractionInfo, true)
    , _allowCameraRotation(AllowRotationInfo, true)
{
    addProperty(_allowMouseInteraction);
    addProperty(_allowCameraRotation);

    // Set callback functions
    global::callback::mouseButton->emplace_back(
        [&](MouseButton button, MouseAction action, KeyModifier modifier) -> bool {

            if (!_isCameraInSolarSystem || !_allowMouseInteraction) {
                return false;
            }

            if (action == MouseAction::Press) {      
                _isCameraRotating = false;
                if (_mouseOnPair) {
                    handleMouseClick(button);
                    return true;
                }
                return false; 
            }
            else if (_interactionMode != MouseInteraction::Hover && 
                     action == MouseAction::Release) {
             
                _interactionMode = MouseInteraction::Hover;
                return true;
            }
            else {
                return false;
            }
        }
    );

    global::callback::mousePosition->emplace_back(
        [&](double x, double y) {    
            
            if (!_isCameraInSolarSystem || !_allowMouseInteraction) {
                return false;
            }

            glm::vec2 pixel{ static_cast<float>(x), static_cast<float>(y) };
            _mousePosition = skybrowser::pixelToScreenSpace2d(pixel);
            glm::vec2 translation = _mousePosition - _startMousePosition;

            switch (_interactionMode) {
            case MouseInteraction::Hover:
                setSelectedObject();
                break;

            case MouseInteraction::Drag:
                _mouseOnPair->translateSelected(_startDragPosition, translation);
                break;

            case MouseInteraction::FineTune:
                _mouseOnPair->fineTuneTarget(_startDragPosition, translation);
                break;

            default:
                setSelectedObject();
                break;
            }
            return false;
        }
    );

    global::callback::mouseScrollWheel->emplace_back(
        [&](double, double scroll) -> bool {
            
            if (!_isCameraInSolarSystem || !_mouseOnPair || !_allowMouseInteraction) {
                return false;
            }
            // If mouse is on browser or target, apply zoom
            _mouseOnPair->getBrowser()->setVerticalFovWithScroll(
                static_cast<float>(scroll)
            );
            return true;
        }
    );


    global::callback::preSync->emplace_back([this]() {

        // Disable browser and targets when camera is outside of solar system
        bool camWasInSolarSystem = _isCameraInSolarSystem;
        glm::dvec3 cameraPos = global::navigationHandler->camera()->positionVec3();
        _isCameraInSolarSystem = glm::length(cameraPos) < SolarSystemRadius;
        bool vizModeChanged = _isCameraInSolarSystem != camWasInSolarSystem;

        // Visualization mode changed. Start fading
        if (vizModeChanged) {
            _isFading = true;
            // Camera moved into the solar system
            if (!_isCameraInSolarSystem) {
                _goal = Transparency::Transparent;
                // Select the 3D browser when moving out of the solar system
                if (_browser3dNode) {
                    _selectedBrowser = _browser3dNode->renderable()->identifier();
                }
            }
            else {
                _goal = Transparency::Opaque;
            }
        }
        double deltaTime = global::windowDelegate->deltaTime();
        // Fade pairs if the camera moved in or out the solar system
        if (_isFading) {
            incrementallyFadeBrowserTargets(_goal, deltaTime);
        }
        if (_isCameraInSolarSystem) {
            std::for_each(std::begin(_targetsBrowsers), std::end(_targetsBrowsers),
                [&](const std::unique_ptr<Pair>& pair) {
                    pair->synchronizeAim();
                });
            incrementallyAnimateTargets(deltaTime);
        }
        if (_isCameraRotating && _allowCameraRotation) {
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
    if (_interactionMode != MouseInteraction::Hover) {
        return;
    }
    // Save old selection for removing highlight
    Pair* previousPair = _mouseOnPair;

    // Find and save what mouse is currently hovering on
    auto it = std::find_if(std::begin(_targetsBrowsers), std::end(_targetsBrowsers),
        [&] (const std::unique_ptr<Pair> &pair) {      
            return pair->checkMouseIntersection(_mousePosition);
        });

    if (it == std::end(_targetsBrowsers)) {
        _mouseOnPair = nullptr;
    }
    else {
        _mouseOnPair = it->get();
    }

    // Selection has changed
    if (previousPair != _mouseOnPair) {
       
        // Remove highlight
        if (previousPair) {
            previousPair->removeHighlight(_highlightAddition);
        }
        // Add highlight to new selection
        if (_mouseOnPair) {
            _mouseOnPair->highlight(_highlightAddition);
        }
    }
}

void SkyBrowserModule::addTargetBrowserPair(const std::string& targetId, const std::string& browserId) {
    
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

void SkyBrowserModule::removeTargetBrowserPair(const std::string& id) {

    Pair* found = getPair(id);
    if (!found) {
        return;
    }

    auto it = std::remove_if(std::begin(_targetsBrowsers), std::end(_targetsBrowsers),
        [&](const std::unique_ptr<Pair>& pair) {
            return *found == *(pair.get());
        });

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

void SkyBrowserModule::lookAtTarget(std::string id)
{
    Pair* pair = getPair(id);
    if (pair) {
        startRotatingCamera(pair->targetDirectionGalactic());
    }
}
    
void SkyBrowserModule::setHoverCircle(ScreenSpaceImageLocal* circle)
{
    _hoverCircle = circle;
}

void SkyBrowserModule::moveHoverCircle(int i)
{
    const ImageData& image = _dataHandler->getImage(i);

    // Only move and show circle if the image has coordinates
    if (_hoverCircle && image.hasCelestialCoords && _isCameraInSolarSystem) {
        // Make circle visible
        _hoverCircle->setEnabled(true);

        // Calculate coords for the circle and translate
        glm::vec3 coordsScreen = skybrowser::equatorialToScreenSpace3d(
            image.equatorialCartesian
        );
        _hoverCircle->setPosition(coordsScreen);
    }
}

void SkyBrowserModule::disableHoverCircle()
{
    if (_hoverCircle && _hoverCircle->isEnabled()) {
        _hoverCircle->setEnabled(false);
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

void SkyBrowserModule::handleMouseClick(const MouseButton& button)
{
    setSelectedBrowser(_mouseOnPair->browserId());

    if (button == MouseButton::Left) {
        _startMousePosition = _mousePosition;
        _startDragPosition = _mouseOnPair->selectedScreenSpacePosition();

        // If it's not resize mode, it's drag mode
        _interactionMode = MouseInteraction::Drag;
        
        // If target is clicked, it should unlock
        if (_mouseOnPair->isTargetSelected()) {
            _mouseOnPair->unlock();
        }
    }
    // Fine tuning mode of target
    else if (button == MouseButton::Right && _mouseOnPair->isBrowserSelected()) {
        // If you start dragging around on the browser, the target unlocks
        _mouseOnPair->unlock();
        // Change view (by moving target) within browser if right mouse 
        // click on browser
        _startMousePosition = _mousePosition;
        _startDragPosition = _mouseOnPair->getTarget()->screenSpacePosition();
        _interactionMode = MouseInteraction::FineTune;
    }
}

const std::unique_ptr<WwtDataHandler>& SkyBrowserModule::getWwtDataHandler() {
    return _dataHandler;
}

std::vector<std::unique_ptr<Pair>>& SkyBrowserModule::getPairs()
{
    return _targetsBrowsers;
}

int SkyBrowserModule::nPairs()
{
    return static_cast<int>(_targetsBrowsers.size());
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
    if (image.has3dCoords && _browser3d) {
        _browser3d->displayImage(image.imageUrl, i);
        _browser3d->placeAt3dPosition(image.position3d, image.fov, 
            _browser3dNode->identifier());
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
    double angle = skybrowser::angleBetweenVectors(_startAnimation, _endAnimation);

    if(angle > StopAnimationThreshold) {
        
        glm::dmat4 rotMat = skybrowser::incrementalAnimationMatrix(
            _startAnimation, 
            _endAnimation, 
            deltaTime, 
            AnimationSpeed
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
                 pair->incrementallyFade(transparency, FadingTime, deltaTime);
             }
             else if (isPairFinished && goal == Transparency::Transparent) {
                 pair->setEnabled(false);
             }
             isAllFinished &= isPairFinished;
         }
     }

     // The transition is over when the fade is finished
     if (isAllFinished) {
         _isFading = false;
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
