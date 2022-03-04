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

#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <modules/skybrowser/include/targetbrowserpair.h>
#include <modules/skybrowser/include/screenspaceskytarget.h>
#include <modules/skybrowser/include/wwtdatahandler.h>
#include <modules/base/rendering/screenspaceimagelocal.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/camera/camera.h>
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
                "",
                "Returns a list of all the loaded AAS WorldWide Telescope images that "
                "have been loaded. Each image has a name, thumbnail url, equatorial "
                "spherical coordinates RA and Dec, equatorial Cartesian coordinates, "
                "if the image has celestial coordinates, credits text, credits url "
                "and the identifier of the image which is a unique number."
            }, 
            {
                "setHoverCircle",
                &skybrowser::luascriptfunctions::setHoverCircle,
                "string",
                "Takes an identifier to a screen space renderable and adds it to the "
                "module."
            },
            {
                "moveCircleToHoverImage",
                &skybrowser::luascriptfunctions::moveCircleToHoverImage,
                "int",
                "Moves the hover circle to the coordinate specified by the image index."
            },
            {
                "disableHoverCircle",
                &skybrowser::luascriptfunctions::disableHoverCircle,
                "",
                "Disables the hover circle, if there is one added to the sky browser "
                "module."
            },
            {
                "loadImagesToWWT",
                &skybrowser::luascriptfunctions::loadImagesToWWT,
                "string",
                "Takes an identifier to a sky browser or target and loads the WWT image "
                "collection to that browser."
            },
            {
                "selectImage",
                &skybrowser::luascriptfunctions::selectImage,
                "int",
                "Takes an index to an image and selects that image in the currently "
                "selected sky browser."
            }, 
            {
                "removeSelectedImageInBrowser",
                &skybrowser::luascriptfunctions::removeSelectedImageInBrowser,
                "string, int",
                "Takes an identifier to a sky browser or target and an index to an "
                "image. Removes that image from that sky browser."
            },
            {
                "adjustCamera",
                & skybrowser::luascriptfunctions::adjustCamera,
                "string",
                "Takes an identifier to a sky browser or sky target. Rotates the camera "
                "so that the target is placed in the center of the view."
            },
            {
                "setSelectedBrowser",
                & skybrowser::luascriptfunctions::setSelectedBrowser,
                "string",
                "Takes an identifier to a sky browser or target. Sets that sky browser " 
                "currently selected."
            },
            {
                "getTargetData",
                &skybrowser::luascriptfunctions::getTargetData,
                "",
                "Returns a table of data regarding the current view and the sky browsers "
                "and targets."
            },
            {
                "lockTarget",
                &skybrowser::luascriptfunctions::lockTarget,
                "string",
                "Takes an identifier to a sky browser or target. Locks the target " 
                "to its current position."
            },
            {
                "unlockTarget",
                &skybrowser::luascriptfunctions::unlockTarget,
                "string",
                "Takes an identifier to a sky browser or target. Unlocks the target."
            },
            {
                "createTargetBrowserPair",
                &skybrowser::luascriptfunctions::createTargetBrowserPair,
                "",
                "Creates a sky browser and a target."
            }, 
             {
                "removeTargetBrowserPair",
                &skybrowser::luascriptfunctions::removeTargetBrowserPair,
                "string",
                "Takes in identifier to a sky browser or target and removes them."
            },
            {
                "setOpacityOfImageLayer",
                &skybrowser::luascriptfunctions::setOpacityOfImageLayer,
                "string, int, double",
                "Takes an identifier to a sky browser or sky target, an index to an image"
                "and a value for the opacity."
            },   
            {
                "sendOutIdsToBrowsers",
                &skybrowser::luascriptfunctions::sendOutIdsToBrowsers,
                "",
                "Sends all sky browsers' identifiers to their respective CEF browser. "
            },
            {
                "initializeBrowser",
                &skybrowser::luascriptfunctions::initializeBrowser,
                "string",
                "Takes an identifier to a sky browser and starts the initialization "
                "for that browser. That means that the browser starts to try to connect "
                "to the AAS WorldWide Telescope application by sending it messages. And "
                "that the target matches its appearance to its corresponding browser."
            },  
            {
                "centerTargetOnScreen",
                &skybrowser::luascriptfunctions::centerTargetOnScreen,
                "string",
                "Takes an identifier to a sky browser and animates its corresponding "
                "target to the center of the current view."
            }, 
            {
                "setImageLayerOrder",
                &skybrowser::luascriptfunctions::setImageLayerOrder,
                "string, int, int",
                "Takes an identifier to a sky browser or a sky target, an image index "
                "and the order which it should have in the selected image list. The "
                "image is then changed to have this order."
            }, 
            {
                "addPairToSkyBrowserModule",
                &skybrowser::luascriptfunctions::addPairToSkyBrowserModule,
                "string, string",
                "Takes the identifier of the sky target and a sky browser and adds them "
                "to the sky browser module."
            },
            {
                "setEquatorialAim",
                &skybrowser::luascriptfunctions::setEquatorialAim,
                "string, double, double",
                "Takes the identifier of a sky browser or a sky target and equatorial "
                "coordinates Right Ascension and Declination. The target will animate to "
                "this coordinate and the browser will display the coordinate."
            },
            {
                "setVerticalFov",
                &skybrowser::luascriptfunctions::setVerticalFov,
                "string, float",
                "Takes an identifier to a sky browser or a sky target and a vertical "
                "field of view. Changes the field of view as specified by the input."
            },
            {
                "setBorderColor",
                &skybrowser::luascriptfunctions::setBorderColor,
                "string, int, int, int",
                "Takes an identifier to a sky browser or a sky target and a rgb color "
                "in the ranges [0, 255]."
            },
            {
                "setScreenSpaceSize",
                &skybrowser::luascriptfunctions::setScreenSpaceSize,
                "string, float, float",
                "Sets the screen space size of the sky browser to the numbers specified "
                "by the input [x, y]."
            },
            {
                "startSetup",
                &skybrowser::luascriptfunctions::startSetup,
                "",
                "Starts the setup process of the sky browers. This function calls "
                "the lua function 'sendOutIdsToBrowsers' in all nodes in the cluster."
            },
            {
                "translateScreenSpaceRenderable",
                &skybrowser::luascriptfunctions::translateScreenSpaceRenderable,
                "string, float, float, float, float",
                "Takes an identifier to a sky browser or sky target and the [x, y] "
                "starting position and the [x, y] translation vector."
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
        if (vizModeChanged) {
            _isFading = true;
            // Camera moved into the solar system
            if (!_isCameraInSolarSystem) {
                _goal = Transparency::Transparent;
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
            std::for_each(
                _targetsBrowsers.begin(), 
                _targetsBrowsers.end(),
                [&](const std::unique_ptr<TargetBrowserPair>& pair) {
                    pair->synchronizeAim();
                }
            );
            incrementallyAnimateTargets(deltaTime);
        }
        if (_isCameraRotating && _allowCameraRotation) {
            incrementallyRotateCamera(deltaTime);
        }
    }); 
} 

void SkyBrowserModule::internalInitialize(const ghoul::Dictionary& dict) {
    const Parameters p = codegen::bake<Parameters>(dict);

    // Register ScreenSpaceRenderable
    ghoul::TemplateFactory<ScreenSpaceRenderable>* fScreenSpaceRenderable = 
        FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");

    // Register ScreenSpaceSkyTarget
    fScreenSpaceRenderable->registerClass<ScreenSpaceSkyTarget>("ScreenSpaceSkyTarget");

    // Register ScreenSpaceSkyBrowser
    fScreenSpaceRenderable->registerClass<ScreenSpaceSkyBrowser>("ScreenSpaceSkyBrowser");

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
    ScreenSpaceSkyTarget* target = dynamic_cast<ScreenSpaceSkyTarget*>(
        global::renderEngine->screenSpaceRenderable(targetId)
    );
    ScreenSpaceSkyBrowser* browser = dynamic_cast<ScreenSpaceSkyBrowser*>(
        global::renderEngine->screenSpaceRenderable(browserId)
    );
    
    // Ensure pair has both target and browser
    if (browser && target) {
        _targetsBrowsers.push_back(std::make_unique<TargetBrowserPair>(browser, target));
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
    
void SkyBrowserModule::setHoverCircle(ScreenSpaceImageLocal* circle) {
    _hoverCircle = circle;
}

void SkyBrowserModule::moveHoverCircle(int i) {
    const ImageData& image = _dataHandler->getImage(i);

    // Only move and show circle if the image has coordinates
    if (_hoverCircle && image.hasCelestialCoords && _isCameraInSolarSystem) {
        // Make circle visible
        _hoverCircle->setEnabled(true);

        // Set the exact target position 
        glm::dvec3 localCamera = skybrowser::equatorialToLocalCamera(image.equatorialCartesian);

        if (_hoverCircle->isUsingRaeCoords()) {
            glm::vec3 position = _hoverCircle->raePosition().x * glm::vec3(localCamera);
            _hoverCircle->setRaeFromCartesianPosition(position);
        }
        else {
            _hoverCircle->setCartesianPosition(
                skybrowser::localCameraToScreenSpace3d(localCamera)
            );
        }
    }
}

void SkyBrowserModule::disableHoverCircle() {
    if (_hoverCircle && _hoverCircle->isEnabled()) {
        _hoverCircle->setEnabled(false);
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
        _startDragPosition = _mouseOnPair->target()->screenSpacePosition();
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
            bool foundTarget = pair->targetId() == id;
            return foundBrowser || foundTarget;
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

void SkyBrowserModule::incrementallyFadeBrowserTargets(Transparency goal, 
                                                       float deltaTime) 
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
     for (std::unique_ptr<TargetBrowserPair>& pair : _targetsBrowsers) {
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

void SkyBrowserModule::incrementallyAnimateTargets(double deltaTime) {
    for (std::unique_ptr<TargetBrowserPair>& pair : _targetsBrowsers) {
        if (pair->isEnabled()) {
            pair->incrementallyAnimateToCoordinate(deltaTime);
        }
    }
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
        return found->targetId();
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
} // namespace openspace
