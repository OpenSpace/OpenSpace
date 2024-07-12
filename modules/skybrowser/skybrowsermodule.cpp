/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "Decides if the GUI for this module should be enabled.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ShowTitleInGuiBrowserInfo = {
        "ShowTitleInGuiBrowser",
        "Show Title in Gui Browser",
        "If true, the name of the currently selected sky browser is shown in the WebGUI "
        "browser.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AllowRotationInfo = {
        "AllowCameraRotation",
        "Allow Camera Rotation",
        "Toggles if the camera should rotate to look at the sky target if it is going "
        "outside of the current field of view.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo CameraRotSpeedInfo = {
        "CameraRotationSpeed",
        "Camera Rotation Speed",
        "The speed of the rotation of the camera when the camera rotates to look at a "
        "coordinate which is outside of the field of view.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo TargetSpeedInfo = {
        "TargetAnimationSpeed",
        "Target Animation Speed",
        "This determines the speed of the animation of the sky target.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo BrowserSpeedInfo = {
        "BrowserAnimationSpeed",
        "Field of View Animation Speed",
        "This determines the speed of the animation of the field of view in the browser.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo HideWithGuiInfo = {
        "HideTargetsBrowsersWithGui",
        "Hide Targets and Browsers with GUI",
        "If checked, the targets and browsers will be disabled when the sky browser "
        "panel is minimized.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo InverseZoomInfo = {
        "InverseZoomDirection",
        "Inverse Zoom Direction",
        "If checked, the zoom direction of the scroll over the AAS WWT browser will be "
        "inversed.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo SynchronizeAimInfo = {
        "SynchronizeAim",
        "Synchronize Aim",
        "If checked, the target and the browser will have synchronized aim.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SpaceCraftTimeInfo = {
        "SpaceCraftAnimationTime",
        "Space Craft Animation Time",
        "Sets the duration (in seconds) of the animation of the space craft when it is "
        "pointed to where the target is aiming.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ImageCollectionInfo = {
       "WwtImageCollectionUrl",
       "AAS WorldWide Telescope Image Collection Url",
       "The url of the image collection which is loaded into AAS WorldWide Telescope.",
       openspace::properties::Property::Visibility::AdvancedUser
    };


    struct [[codegen::Dictionary(SkyBrowserModule)]] Parameters {
        // [[codegen::verbatim(EnabledInfo.description)]]
        std::optional<bool> enabled;

        // [[codegen::verbatim(AllowRotationInfo.description)]]
        std::optional<bool> allowCameraRotation;

        // [[codegen::verbatim(CameraRotSpeedInfo.description)]]
        std::optional<double> cameraRotSpeed;

        // [[codegen::verbatim(TargetSpeedInfo.description)]]
        std::optional<double> targetSpeed;

        // [[codegen::verbatim(BrowserSpeedInfo.description)]]
        std::optional<double> browserSpeed;

        // [[codegen::verbatim(HideWithGuiInfo.description)]]
        std::optional<bool> hideTargetsBrowsersGui;

        // [[codegen::verbatim(InverseZoomInfo.description)]]
        std::optional<bool> inverseZoomDirection;

        // [[codegen::verbatim(SynchronizeAimInfo.description)]]
        std::optional<bool> synchronizeAim;

        // [[codegen::verbatim(SpaceCraftTimeInfo.description)]]
        std::optional<double> spaceCraftAnimationTime;

        // [[codegen::verbatim(SpaceCraftTimeInfo.description)]]
        std::optional<std::string> wwtImageCollectionUrl;
    };

#include "skybrowsermodule_codegen.cpp"
} // namespace

namespace openspace {

SkyBrowserModule::SkyBrowserModule()
    : OpenSpaceModule(SkyBrowserModule::Name)
    , _enabled(EnabledInfo)
    , _showTitleInGuiBrowser(ShowTitleInGuiBrowserInfo, true)
    , _allowCameraRotation(AllowRotationInfo, true)
    , _cameraRotationSpeed(CameraRotSpeedInfo, 0.5, 0.0, 1.0)
    , _targetAnimationSpeed(TargetSpeedInfo, 0.2, 0.0, 1.0)
    , _browserAnimationSpeed(BrowserSpeedInfo, 5.0, 0.0, 10.0)
    , _hideTargetsBrowsersWithGui(HideWithGuiInfo, false)
    , _inverseZoomDirection(InverseZoomInfo, false)
    , _synchronizeAim(SynchronizeAimInfo, true)
    , _spaceCraftAnimationTime(SpaceCraftTimeInfo, 2.0, 0.0, 10.0)
    , _wwtImageCollectionUrl(
        ImageCollectionInfo,
        "https://data.openspaceproject.com/wwt/1/imagecollection.wtml"
    )
{
    addProperty(_enabled);
    addProperty(_showTitleInGuiBrowser);
    addProperty(_allowCameraRotation);
    addProperty(_cameraRotationSpeed);
    addProperty(_targetAnimationSpeed);
    addProperty(_browserAnimationSpeed);
    addProperty(_hideTargetsBrowsersWithGui);
    addProperty(_inverseZoomDirection);
    addProperty(_spaceCraftAnimationTime);
    addProperty(_wwtImageCollectionUrl);
    addProperty(_synchronizeAim);
    _wwtImageCollectionUrl.setReadOnly(true);

    // Set callback functions
    global::callback::mouseButton->emplace(
        global::callback::mouseButton->begin(),
        [this](MouseButton, MouseAction action, KeyModifier, IsGuiWindow) -> bool {
            if (action == MouseAction::Press) {
                _cameraRotation.stop();
            }
            return false;
        }
    );

    global::callback::preSync->emplace_back([this]() {
        constexpr double SolarSystemRadius = 30.0 * distanceconstants::AstronomicalUnit;

        // Disable browser and targets when camera is outside of solar system
        const bool camWasInSolarSystem = _isCameraInSolarSystem;
        const glm::dvec3 cameraPos = global::navigationHandler->camera()->positionVec3();
        _isCameraInSolarSystem = glm::length(cameraPos) < SolarSystemRadius;
        const bool vizModeChanged = _isCameraInSolarSystem != camWasInSolarSystem;

        // Visualization mode changed. Start fading in/out
        if (vizModeChanged) {
            constexpr float FadeDuration = 2.f;

            if (camWasInSolarSystem) {
                // Camera moved out of the solar system => fade out
                for (const std::unique_ptr<TargetBrowserPair>& pair : _targetsBrowsers) {
                    pair->startFading(0.f, FadeDuration);
                }

                // Also hide the hover circle
                disableHoverCircle();
            }
            else {
                // Camera moved into the solar system => fade in
                for (const std::unique_ptr<TargetBrowserPair>& pair : _targetsBrowsers) {
                    pair->startFading(1.f, FadeDuration);
                }
            }
        }

        if (_isCameraInSolarSystem) {
            if (_synchronizeAim) {
                for (const std::unique_ptr<TargetBrowserPair>& pair : _targetsBrowsers) {
                    pair->synchronizeAim();
                }
            }
            incrementallyAnimateTargets();
        }
        if (_cameraRotation.isAnimating() && _allowCameraRotation) {
            incrementallyRotateCamera();
        }
    });
}

void SkyBrowserModule::internalInitialize(const ghoul::Dictionary& dict) {
    const Parameters p = codegen::bake<Parameters>(dict);

    _enabled = p.enabled.value_or(_enabled);
    _allowCameraRotation = p.allowCameraRotation.value_or(_allowCameraRotation);
    _cameraRotationSpeed = p.cameraRotSpeed.value_or(_cameraRotationSpeed);
    _targetAnimationSpeed = p.targetSpeed.value_or(_targetAnimationSpeed);
    _browserAnimationSpeed = p.browserSpeed.value_or(_browserAnimationSpeed);
    _inverseZoomDirection = p.inverseZoomDirection.value_or(_inverseZoomDirection);
    _wwtImageCollectionUrl = p.wwtImageCollectionUrl.value_or(_wwtImageCollectionUrl);
    _synchronizeAim = p.synchronizeAim.value_or(_synchronizeAim);
    _hideTargetsBrowsersWithGui = p.hideTargetsBrowsersGui.value_or(
        _hideTargetsBrowsersWithGui
    );
    _spaceCraftAnimationTime = p.spaceCraftAnimationTime.value_or(
        _spaceCraftAnimationTime
    );

    ghoul::TemplateFactory<ScreenSpaceRenderable>* fScreenSpaceRenderable =
        FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");

    // Register ScreenSpaceSkyBrowser
    fScreenSpaceRenderable->registerClass<ScreenSpaceSkyBrowser>("ScreenSpaceSkyBrowser");

    ghoul::TemplateFactory<Renderable>* fRenderable =
        FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "Renderable factory was not created");

    // Register ScreenSpaceSkyTarget
    fRenderable->registerClass<RenderableSkyTarget>("RenderableSkyTarget");
}

void SkyBrowserModule::addTargetBrowserPair(const std::string& targetId,
                                            const std::string& browserId)
{
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
    _uniqueIdentifierCounter++;
}

void SkyBrowserModule::removeTargetBrowserPair(const std::string& id) {
    TargetBrowserPair* found = pair(id);
    if (!found) {
        return;
    }

    auto it = std::remove_if(
        _targetsBrowsers.begin(),
        _targetsBrowsers.end(),
        [&found](const std::unique_ptr<TargetBrowserPair>& pair) {
            return found == pair.get();
        }
    );

    _targetsBrowsers.erase(it, _targetsBrowsers.end());
}

void SkyBrowserModule::lookAtTarget(const std::string& id) {
    TargetBrowserPair* found = pair(id);
    if (found) {
        startRotatingCamera(found->targetDirectionGalactic());
    }
}

void SkyBrowserModule::setHoverCircle(SceneGraphNode* circle) {
    ghoul_assert(circle, "No circle specified");
    _hoverCircle = circle;

    // Always disable it per default. It should only be visible on interaction
    disableHoverCircle();
}

void SkyBrowserModule::moveHoverCircle(const std::string& imageUrl, bool useScript) {
    std::optional<const ImageData> found = _dataHandler.image(imageUrl);
    if (!found.has_value()) {
        return;
    }
    const ImageData image = *found;
    // Only move and show circle if the image has coordinates
    if (!(_hoverCircle && image.hasCelestialCoords && _isCameraInSolarSystem)) {
        return;
    }

    const std::string id = _hoverCircle->identifier();

    // Show the circle
    if (useScript) {
        const std::string script = std::format(
            "openspace.setPropertyValueSingle('Scene.{}.Renderable.Fade', 1.0);",
            id
        );
        global::scriptEngine->queueScript(
            script,
            scripting::ScriptEngine::ShouldBeSynchronized::Yes,
            scripting::ScriptEngine::ShouldSendToRemote::Yes
        );
    }
    else {
        Renderable* renderable = _hoverCircle->renderable();
        if (renderable) {
            renderable->setFade(1.f);
        }
    }

    // Set the exact target position
    // Move it slightly outside of the celestial sphere so it doesn't overlap with
    // the target
    glm::dvec3 pos = skybrowser::equatorialToGalactic(image.equatorialCartesian);
    pos *= skybrowser::CelestialSphereRadius * 1.1;

    // Note that the position can only be set through the script engine
    const std::string script = std::format(
        "openspace.setPropertyValueSingle('Scene.{}.Translation.Position', {});",
        id, ghoul::to_string(pos)
    );
    global::scriptEngine->queueScript(
        script,
        scripting::ScriptEngine::ShouldBeSynchronized::Yes,
        scripting::ScriptEngine::ShouldSendToRemote::Yes
    );
}

void SkyBrowserModule::disableHoverCircle(bool useScript) {
    if (_hoverCircle && _hoverCircle->renderable()) {
        if (useScript) {
            const std::string script = std::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.Fade', 0.0);",
                _hoverCircle->identifier()
            );
            global::scriptEngine->queueScript(
                script,
                scripting::ScriptEngine::ShouldBeSynchronized::Yes,
                scripting::ScriptEngine::ShouldSendToRemote::Yes
            );
        }
        else {
            _hoverCircle->renderable()->property("Fade")->set(0.f);
        }
    }
}

void SkyBrowserModule::loadImages(const std::string& root,
                                  const std::filesystem::path& directory)
{
    _dataHandler.loadImages(root, directory);
}

int SkyBrowserModule::nLoadedImages() const {
    return _dataHandler.nLoadedImages();
}

const WwtDataHandler& SkyBrowserModule::wwtDataHandler() const {
    return _dataHandler;
}

std::vector<std::unique_ptr<TargetBrowserPair>>& SkyBrowserModule::pairs() {
    return _targetsBrowsers;
}

int SkyBrowserModule::nPairs() const {
    return static_cast<int>(_targetsBrowsers.size());
}

TargetBrowserPair* SkyBrowserModule::pair(std::string_view id) const {
    auto it = std::find_if(
        _targetsBrowsers.begin(),
        _targetsBrowsers.end(),
        [&id](const std::unique_ptr<TargetBrowserPair>& pair) {
            const bool foundBrowser = pair->browserId() == id;
            const bool foundTarget = pair->targetRenderableId() == id;
            const bool foundTargetNode = pair->targetNodeId() == id;
            return foundBrowser || foundTarget || foundTargetNode;
        }
    );
    TargetBrowserPair* found = it != _targetsBrowsers.end() ? it->get() : nullptr;
    if (found == nullptr) {
        LINFO(std::format("Identifier '{}' not found", id));
    }
    return found;
}

void SkyBrowserModule::startRotatingCamera(const glm::dvec3& endAnimation) {
    // Save coordinates to rotate to in galactic world coordinates
    const glm::dvec3 start = skybrowser::cameraDirectionGalactic();
    const double angle = skybrowser::angleBetweenVectors(start, endAnimation);
    const double time = angle / _cameraRotationSpeed;
    _cameraRotation = skybrowser::Animation(start, endAnimation, time);
    _cameraRotation.start();
}

void SkyBrowserModule::incrementallyRotateCamera() {
    if (_cameraRotation.isAnimating()) {
        const glm::dmat4 rotMat = _cameraRotation.rotationMatrix();
        global::navigationHandler->camera()->rotate(glm::quat_cast(rotMat));
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

double SkyBrowserModule::spaceCraftAnimationTime() const {
    return _spaceCraftAnimationTime;
}

std::string SkyBrowserModule::wwtImageCollectionUrl() const {
    return _wwtImageCollectionUrl;
}

void SkyBrowserModule::setSelectedBrowser(std::string_view id) {
    TargetBrowserPair* p = pair(id);
    if (p) {
        _selectedBrowser = id;
    }
}

std::string SkyBrowserModule::selectedBrowserId() const {
    return _selectedBrowser;
}

std::string SkyBrowserModule::selectedTargetId() const {
    TargetBrowserPair* found = pair(_selectedBrowser);
    return found ? found->targetRenderableId() : "";
}

int SkyBrowserModule::uniqueIdentifierCounter() const {
    return _uniqueIdentifierCounter;
}

bool SkyBrowserModule::isCameraInSolarSystem() const {
    return _isCameraInSolarSystem;
}

bool SkyBrowserModule::isSelectedPairUsingRae() const {
    TargetBrowserPair* found = pair(_selectedBrowser);
    return found ? found->isUsingRadiusAzimuthElevation() : false;
}

bool SkyBrowserModule::isSelectedPairFacingCamera() const {
    TargetBrowserPair* found = pair(_selectedBrowser);
    return found ? found->isFacingCamera() : false;
}

std::vector<documentation::Documentation> SkyBrowserModule::documentations() const {
    return {
        RenderableSkyTarget::Documentation(),
        ScreenSpaceSkyBrowser::Documentation()
    };
}

scripting::LuaLibrary SkyBrowserModule::luaLibrary() const {
    return {
        "skybrowser",
        {
            codegen::lua::StartSetup,
            codegen::lua::InitializeBrowser,
            codegen::lua::SendOutIdsToBrowsers,
            codegen::lua::ListOfImages,
            codegen::lua::ListOfImagesDeprecated,
            codegen::lua::SetHoverCircle,
            codegen::lua::MoveCircleToHoverImage,
            codegen::lua::DisableHoverCircle,
            codegen::lua::LoadImagesToWWT,
            codegen::lua::SelectImage,
            codegen::lua::RemoveSelectedImageInBrowser,
            codegen::lua::AdjustCamera,
            codegen::lua::SetSelectedBrowser,
            codegen::lua::TargetData,
            codegen::lua::TargetDataDeprecated,
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
            codegen::lua::AddDisplayCopy,
            codegen::lua::SetBrowserRatio,
            codegen::lua::RemoveDisplayCopy,
            codegen::lua::StartFinetuningTarget,
            codegen::lua::FinetuneTargetPosition,
            codegen::lua::ScrollOverBrowser,
            codegen::lua::LoadingImageCollectionComplete,
            codegen::lua::ShowAllTargetsAndBrowsers,
            codegen::lua::WwtImageCollectionUrl,
            codegen::lua::WwtImageCollectionUrlDeprecated,
            codegen::lua::StopAnimations,
            codegen::lua::SetBorderRadius,
            codegen::lua::ReloadDisplayCopyOnNode
        }
    };
}

} // namespace openspace
