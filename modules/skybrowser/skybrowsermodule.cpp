/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <modules/skybrowser/include/renderableskytarget.h>
#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <modules/skybrowser/include/targetbrowserpair.h>
#include <modules/skybrowser/include/wwtdatahandler.h>
#include <openspace/camera/camera.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/factorymanager.h>

#include "skybrowsermodule_lua.inl"

namespace {
    constexpr const openspace::properties::Property::PropertyInfo AllowInteractionInfo = {
        "AllowMouseInteraction",
        "Allow Mouse Interaction",
        "Toggles if it is possible to interact with the sky browser and sky targets with "
        "the mouse or not."
    };
    
    constexpr const openspace::properties::Property::PropertyInfo AllowRotationInfo = {
        "AllowCameraRotation",
        "Allow Camera Rotation",
        "Toggles if the camera should rotate to look at the sky target if it is going "
        "outside of the current field of view."
    };

    constexpr const openspace::properties::Property::PropertyInfo CameraRotSpeedInfo = {
        "CameraRotationSpeed",
        "Camera Rotation Speed",
        "The speed of the rotation of the camera when the camera rotates to look at a "
        "coordinate which is outside of the field of view."
    };

    constexpr const openspace::properties::Property::PropertyInfo TargetSpeedInfo = {
        "TargetAnimationSpeed",
        "Target Animation Speed",
        "This determines the speed of the animation of the sky target."
    };

    constexpr const openspace::properties::Property::PropertyInfo BrowserSpeedInfo = {
        "BrowserAnimationSpeed",
        "Field Of View Animation Speed",
        "This determines the speed of the animation of the field of view in the browser."
    };

    struct [[codegen::Dictionary(SkyBrowserModule)]] Parameters {
        // [[codegen::verbatim(AllowInteractionInfo.description)]]
        std::optional<bool> allowMouseInteraction;

        // [[codegen::verbatim(AllowRotationInfo.description)]]
        std::optional<bool> allowCameraRotation;

        // [[codegen::verbatim(CameraRotSpeedInfo.description)]]
        std::optional<double> cameraRotSpeed;

        // [[codegen::verbatim(TargetSpeedInfo.description)]]
        std::optional<double> targetSpeed;

        // [[codegen::verbatim(BrowserSpeedInfo.description)]]
        std::optional<double> browserSpeed;
    };
    
    #include "skybrowsermodule_codegen.cpp"
} // namespace

namespace openspace {
SkyBrowserModule::SkyBrowserModule()
    : OpenSpaceModule(SkyBrowserModule::Name)
    , _allowMouseInteraction(AllowInteractionInfo, true)
    , _allowCameraRotation(AllowRotationInfo, true)
    , _cameraRotationSpeed(CameraRotSpeedInfo, 0.5, 0.0, 1.0)
    , _targetAnimationSpeed(TargetSpeedInfo, 0.2, 0.0, 1.0)
    , _browserAnimationSpeed(BrowserSpeedInfo, 5.0, 0.0, 10.0)
{
    addProperty(_allowMouseInteraction);
    addProperty(_allowCameraRotation);
    addProperty(_cameraRotationSpeed);
    addProperty(_targetAnimationSpeed);
    addProperty(_browserAnimationSpeed);

    // Set callback functions
    global::callback::mouseButton->emplace_back(
        [&](MouseButton button, MouseAction action, KeyModifier modifier) -> bool {
            if (!_isCameraInSolarSystem || !_allowMouseInteraction) {
                return false;
            }

            if (action == MouseAction::Press) {      
                _cameraRotation.stop();
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

    if (global::windowDelegate->isMaster()) {
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
                        _mouseOnPair->fineTuneTarget(
                            _startTargetPosition, 
                            _startDragPosition, 
                            translation
                        );
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
                _mouseOnPair->setVerticalFovWithScroll(
                    static_cast<float>(scroll)
                );
                return true;
            }
        );
    }

    global::callback::preSync->emplace_back([this]() {
        // Disable browser and targets when camera is outside of solar system
        bool camWasInSolarSystem = _isCameraInSolarSystem;
        glm::dvec3 cameraPos = global::navigationHandler->camera()->positionVec3();
        _isCameraInSolarSystem = glm::length(cameraPos) < SolarSystemRadius;
        bool vizModeChanged = _isCameraInSolarSystem != camWasInSolarSystem;

        // Visualization mode changed. Start fading
        if (vizModeChanged && !_isCameraInSolarSystem) {
            // Camera moved into the solar system
            _isFading = true;
            _goal = Transparency::Transparent;
            
            float transparency = [](Transparency goal) {
                switch (goal) {
                case Transparency::Transparent:
                    return 0.f;

                case Transparency::Opaque:
                    return 1.f;
                }
            }(_goal);

            std::for_each(
                _targetsBrowsers.begin(),
                _targetsBrowsers.end(),
                [&](const std::unique_ptr<TargetBrowserPair>& pair) {
                    pair->startFading(transparency, 2.f);
                }
            );
        }
        // Fade pairs if the camera moved in or out the solar system
        if (_isFading) {
            incrementallyFadeBrowserTargets(_goal);
        }
        if (_isCameraInSolarSystem) {
            std::for_each(
                _targetsBrowsers.begin(), 
                _targetsBrowsers.end(),
                [&](const std::unique_ptr<TargetBrowserPair>& pair) {
                    pair->synchronizeAim();
                }
            );
            incrementallyAnimateTargets();
        }
        if (_cameraRotation.isAnimating() && _allowCameraRotation) {
            incrementallyRotateCamera();
        }
    }); 
} 

void SkyBrowserModule::internalInitialize(const ghoul::Dictionary& dict) {
    const Parameters p = codegen::bake<Parameters>(dict);

    // Register ScreenSpaceRenderable
    ghoul::TemplateFactory<ScreenSpaceRenderable>* fScreenSpaceRenderable = 
        FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");


    // Register ScreenSpaceSkyBrowser
    fScreenSpaceRenderable->registerClass<ScreenSpaceSkyBrowser>("ScreenSpaceSkyBrowser");

    // Register ScreenSpaceRenderable
    ghoul::TemplateFactory<Renderable>* fRenderable =
        FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "Renderable factory was not created");

    // Register ScreenSpaceSkyTarget
    fRenderable->registerClass<RenderableSkyTarget>("RenderableSkyTarget");
    
    // Create data handler dynamically to avoid the linking error that
    // came up when including the include file in the module header file
    _dataHandler = std::make_unique<WwtDataHandler>();
}

void SkyBrowserModule::setSelectedObject() {
    if (_interactionMode != MouseInteraction::Hover) {
        return;
    }
    // Save old selection for removing highlight
    TargetBrowserPair* previousPair = _mouseOnPair;

    // Find and save what mouse is currently hovering on
    auto it = std::find_if(
        _targetsBrowsers.begin(),
        _targetsBrowsers.end(),
        [&] (const std::unique_ptr<TargetBrowserPair> &pair) {      
            return pair->checkMouseIntersection(_mousePosition) && 
                   !pair->isUsingRadiusAzimuthElevation();
        });

    if (it == _targetsBrowsers.end()) {
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
    if (!global::renderEngine->scene()) { 
        return; 
    }

    SceneGraphNode* target = global::renderEngine->scene()->sceneGraphNode(targetId);
    ScreenSpaceSkyBrowser* browser = dynamic_cast<ScreenSpaceSkyBrowser*>(
        global::renderEngine->screenSpaceRenderable(browserId)
    );
    
    // Ensure pair has both target and browser
    if (browser && target) {
        _targetsBrowsers.push_back(std::make_unique<TargetBrowserPair>(target, browser));
    }
}

void SkyBrowserModule::removeTargetBrowserPair(const std::string& id) {
    TargetBrowserPair* found = getPair(id);
    if (!found) {
        return;
    }

    auto it = std::remove_if(
        _targetsBrowsers.begin(), 
        _targetsBrowsers.end(),
        [&](const std::unique_ptr<TargetBrowserPair>& pair) {
            return *found == *(pair.get());
        }
    );

    _targetsBrowsers.erase(it, _targetsBrowsers.end());
    _mouseOnPair = nullptr;
}

void SkyBrowserModule::lookAtTarget(const std::string& id) {
    TargetBrowserPair* pair = getPair(id);
    if (pair) {
        startRotatingCamera(pair->targetDirectionGalactic());
    }
}
    
void SkyBrowserModule::setHoverCircle(SceneGraphNode* circle) {
    _hoverCircle = circle;
}

void SkyBrowserModule::moveHoverCircle(int i) {
    const ImageData& image = _dataHandler->getImage(i);

    // Only move and show circle if the image has coordinates
    if (_hoverCircle && image.hasCelestialCoords && _isCameraInSolarSystem) {
        // Make circle visible
        _hoverCircle->renderable()->property("Enabled")->set(true);

        // Set the exact target position 
        // Move it slightly outside of the celestial sphere so it doesn't overlap with 
        // the target
        glm::dvec3 pos = skybrowser::equatorialToGalactic(image.equatorialCartesian);
        pos *= skybrowser::CelestialSphereRadius * 1.1; 
        // Uris for properties
        std::string id = _hoverCircle->identifier();
        std::string positionUri = "Scene." + id + ".Translation.Position";
        std::string setValue = "openspace.setPropertyValueSingle('";

        openspace::global::scriptEngine->queueScript(
            setValue + positionUri + "', " + ghoul::to_string(pos) + ");",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
}

void SkyBrowserModule::disableHoverCircle() {
    if (_hoverCircle && _hoverCircle->renderable()) {
        _hoverCircle->renderable()->property("Enabled")->set(false);
    }
}

void SkyBrowserModule::loadImages(const std::string& root, 
                                  const std::filesystem::path& directory) {
    _dataHandler->loadImages(root, directory);
}

int SkyBrowserModule::nLoadedImages() {
    return _dataHandler->nLoadedImages();
}

void SkyBrowserModule::handleMouseClick(const MouseButton& button) {
    setSelectedBrowser(_mouseOnPair->browserId());

    if (button == MouseButton::Left) {
        _startMousePosition = _mousePosition;
        _startDragPosition = _mouseOnPair->selectedScreenSpacePosition();

        // If it's not resize mode, it's drag mode
        _interactionMode = MouseInteraction::Drag;
    }
    // Fine tuning mode of target
    else if (button == MouseButton::Right) {
        // Change view (by moving target) within browser if right mouse 
        // click on browser
        _startMousePosition = _mousePosition;
        _startTargetPosition = _mouseOnPair->targetNode()->worldPosition();
        _interactionMode = MouseInteraction::FineTune;
    }
}

const std::unique_ptr<WwtDataHandler>& SkyBrowserModule::getWwtDataHandler() const {
    return _dataHandler;
}

std::vector<std::unique_ptr<TargetBrowserPair>>& SkyBrowserModule::getPairs() {
    return _targetsBrowsers;
}

int SkyBrowserModule::nPairs() {
    return static_cast<int>(_targetsBrowsers.size());
}

TargetBrowserPair* SkyBrowserModule::getPair(const std::string& id) {
    auto it = std::find_if(
        _targetsBrowsers.begin(), 
        _targetsBrowsers.end(),
        [&](const std::unique_ptr<TargetBrowserPair>& pair) {
            bool foundBrowser = pair->browserId() == id;
            bool foundTarget = pair->targetRenderableId() == id;
            bool foundTargetNode = pair->targetNodeId() == id;
            return foundBrowser || foundTarget || foundTargetNode;
        }
    );
    if (it == std::end(_targetsBrowsers)) {
        return nullptr;
    }
    else {
        return it->get();
    }
}

void SkyBrowserModule::startRotatingCamera(glm::dvec3 endAnimation) {
    // Save coordinates to rotate to in galactic world coordinates
    glm::dvec3 start = skybrowser::cameraDirectionGalactic();
    double angle = skybrowser::angleBetweenVectors(start, endAnimation);
    double time = angle / _cameraRotationSpeed;
    _cameraRotation = skybrowser::Animation(start, endAnimation, time);
    _cameraRotation.start();
}   

void SkyBrowserModule::incrementallyRotateCamera() {
    if(_cameraRotation.isAnimating()) {
        glm::dmat4 rotMat = _cameraRotation.getRotationMatrix();
        // Rotate
        global::navigationHandler->camera()->rotate(glm::quat_cast(rotMat));
    }
}

void SkyBrowserModule::incrementallyFadeBrowserTargets(Transparency goal) 
{    
     bool isAllFinished = true;
     for (std::unique_ptr<TargetBrowserPair>& pair : _targetsBrowsers) {
         if (pair->isEnabled()) {
             bool isPairFinished = pair->hasFinishedFading();
             if (!isPairFinished) {
                 pair->incrementallyFade();
             }
             else if (isPairFinished && goal == Transparency::Transparent) {
                 pair->setEnabled(false);
                 pair->setOpacity(1.0);

             }
             isAllFinished &= isPairFinished;
         }
     }

     // The transition is over when the fade is finished
     if (isAllFinished) {
         _isFading = false;
     }
}

void SkyBrowserModule::incrementallyAnimateTargets() {
    for (std::unique_ptr<TargetBrowserPair>& pair : _targetsBrowsers) {
        if (pair->isEnabled()) {
            pair->incrementallyAnimateToCoordinate();
        }
    }
}

double SkyBrowserModule::targetAnimationSpeed() const {
    return _targetAnimationSpeed;
}

double SkyBrowserModule::browserAnimationSpeed() const {
    return _browserAnimationSpeed;
}

void SkyBrowserModule::setSelectedBrowser(const std::string& id) {
    TargetBrowserPair* found = getPair(id);
    if (found) {
        _selectedBrowser = id;
    }
}

std::string SkyBrowserModule::selectedBrowserId() const {
    return _selectedBrowser;
}

std::string SkyBrowserModule::selectedTargetId() {
    TargetBrowserPair* found = getPair(_selectedBrowser);
    if (found) {
        return found->targetRenderableId();
    }
    else {
        return "";
    }
}

glm::ivec3 SkyBrowserModule::highlight() const {
    return _highlightAddition;
}

bool SkyBrowserModule::isCameraInSolarSystem() const {
    return _isCameraInSolarSystem;
}

bool SkyBrowserModule::isSelectedPairUsingRae() {
    TargetBrowserPair* found = getPair(_selectedBrowser);
    if (found) {
        return found->isUsingRadiusAzimuthElevation();
    }
    else {
        return false;
    }
}

bool SkyBrowserModule::isSelectedPairFacingCamera() {
    TargetBrowserPair* found = getPair(_selectedBrowser);
    if (found) {
        return found->isFacingCamera();
    }
    else {
        return false;
    }
}

scripting::LuaLibrary SkyBrowserModule::luaLibrary() const {
    return {
        "skybrowser",
        {
            codegen::lua::StartSetup,
            codegen::lua::InitializeBrowser,
            codegen::lua::SendOutIdsToBrowsers,
            codegen::lua::GetListOfImages,
            codegen::lua::SetHoverCircle,
            codegen::lua::MoveCircleToHoverImage,
            codegen::lua::DisableHoverCircle,
            codegen::lua::LoadImagesToWWT,
            codegen::lua::SelectImage,
            codegen::lua::RemoveSelectedImageInBrowser,
            codegen::lua::AdjustCamera,
            codegen::lua::SetSelectedBrowser,
            codegen::lua::GetTargetData,
            codegen::lua::CreateTargetBrowserPair,
            codegen::lua::RemoveTargetBrowserPair,
            codegen::lua::SetOpacityOfImageLayer,
            codegen::lua::CenterTargetOnScreen,
            codegen::lua::SetImageLayerOrder,
            codegen::lua::AddPairToSkyBrowserModule,
            codegen::lua::SetEquatorialAim,
            codegen::lua::SetVerticalFov,
            codegen::lua::SetBorderColor,
            codegen::lua::TranslateScreenSpaceRenderable,
            codegen::lua::AddRenderCopy,
            codegen::lua::SetScreenSpaceSize,
            codegen::lua::RemoveRenderCopy
        }
    };
}

} // namespace openspace
