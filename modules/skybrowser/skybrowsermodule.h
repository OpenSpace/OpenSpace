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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___SKYBROWSERMODULE___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SKYBROWSERMODULE___H__

#include <modules/skybrowser/include/utility.h>

#include <openspace/util/openspacemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/mouse.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/stringproperty.h>
#include <fstream>

namespace openspace {

class ScreenSpaceImageLocal;
class WwtDataHandler;
class TargetBrowserPair;
class SceneGraphNode;
struct ImageData;

class SkyBrowserModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "SkyBrowser";
    using CallbackHandle = int;
    using CallbackFunction = std::function<void()>;

    SkyBrowserModule();

    std::vector<std::unique_ptr<TargetBrowserPair>>& getPairs();
    int nPairs() const;
    TargetBrowserPair* pair(const std::string& id) const;
    const std::unique_ptr<WwtDataHandler>& getWwtDataHandler() const;
    std::string selectedBrowserId() const;
    std::string selectedTargetId() const;
    int uniqueIdentifierCounter() const;

    void setSelectedBrowser(const std::string& id);
    void setHoverCircle(SceneGraphNode* circle);

    // Rotation, animation, placement
    void lookAtTarget(const std::string& id);
    void startRotatingCamera(glm::dvec3 endAnimation); // Pass in galactic coordinate
    void incrementallyRotateCamera();
    void incrementallyAnimateTargets();
    double targetAnimationSpeed() const;
    double browserAnimationSpeed() const;
    double spaceCraftAnimationTime() const;
    std::string wwtImageCollectionUrl() const;

    bool isCameraInSolarSystem() const;
    bool isSelectedPairFacingCamera() const;
    bool isSelectedPairUsingRae() const;

    // Managing the target browser pairs
    void removeTargetBrowserPair(const std::string& browserId);
    void addTargetBrowserPair(const std::string& targetId, const std::string& browserId);

    // Hover circle
    void moveHoverCircle(int i, bool useScript = true);
    void disableHoverCircle(bool useScript = true);

    // Image collection handling
    void loadImages(const std::string& root, const std::filesystem::path& directory);
    int nLoadedImages() const;

    CallbackHandle addPreSyncCallback(CallbackFunction cb);
    void removePreSyncCallback(CallbackHandle handle);

    scripting::LuaLibrary luaLibrary() const override;
    std::vector<documentation::Documentation> documentations() const override;

protected:
    void internalInitialize(const ghoul::Dictionary& dict) override;

private:
    properties::BoolProperty _enabled;
    properties::BoolProperty _showTitleInGuiBrowser;
    properties::BoolProperty _allowCameraRotation;
    properties::BoolProperty _hideTargetsBrowsersWithGui;
    properties::BoolProperty _inverseZoomDirection;
    properties::DoubleProperty _cameraRotationSpeed;
    properties::DoubleProperty _targetAnimationSpeed;
    properties::DoubleProperty _browserAnimationSpeed;
    properties::DoubleProperty _spaceCraftAnimationTime;
    properties::StringProperty _wwtImageCollectionUrl;

    // The browsers and targets
    std::vector<std::unique_ptr<TargetBrowserPair>> _targetsBrowsers;
    SceneGraphNode* _hoverCircle = nullptr;
    std::string _selectedBrowser = ""; // Currently selected browser
    int _uniqueIdentifierCounter = 0;

    // Flags
    bool _isCameraInSolarSystem = true; // Visualization modes

    // Animation of rotation of camera to look at coordinate galactic coordinates
    skybrowser::Animation<glm::dvec3> _cameraRotation =
        skybrowser::Animation(glm::dvec3(0.0), glm::dvec3(0.0), 0.0);

    // Data handler for the image collections
    std::unique_ptr<WwtDataHandler> _dataHandler;

    // Callbacks for tiggering topic
    int _nextCallbackHandle = 0;
    std::vector<std::pair<CallbackHandle, CallbackFunction>> _preSyncCallbacks;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___SKYBROWSERMODULE___H__
