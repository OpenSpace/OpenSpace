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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___SKYBROWSERMODULE___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SKYBROWSERMODULE___H__

#include <openspace/documentation/documentation.h>
#include <openspace/util/openspacemodule.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/mouse.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <fstream>

namespace openspace {

class ScreenSpaceImageLocal;
class WwtDataHandler;
class TargetBrowserPair;
class SceneGraphNode;
class ImageData;

enum class Transparency {
    Transparent,
    Opaque
};

enum class MouseInteraction {
    Hover,
    Drag,
    FineTune,
};

class SkyBrowserModule : public OpenSpaceModule {
public:

    constexpr static const char* Name = "SkyBrowser";
    constexpr static const double StopAnimationThreshold{ 0.0005 };
    constexpr static const double FadingTime = { 2.0 };
    constexpr static const double AnimationSpeed = { 1.0 };
    const double SolarSystemRadius = 30.0 * distanceconstants::AstronomicalUnit;

    // Constructor & destructor
    SkyBrowserModule();
    virtual ~SkyBrowserModule();

    // Getters
    std::vector<std::unique_ptr<TargetBrowserPair>>& getPairs();
    int nPairs();
    TargetBrowserPair* getPair(const std::string& id);
    const std::unique_ptr<WwtDataHandler>& getWwtDataHandler();
    std::string selectedBrowserId();
    std::string selectedTargetId();
    glm::ivec3 highlight();

    // Setters
    void setSelectedBrowser(const std::string& id);
    void setSelectedObject(); // Manage mouse interactions
    void setHoverCircle(ScreenSpaceImageLocal* circle);
   
    // Rotation, animation, placement
    void lookAtTarget(std::string id);
    void startRotatingCamera(glm::dvec3 endAnimation); // Pass in galactic coordinate
    void incrementallyRotateCamera(double deltaTime);
    void incrementallyFadeBrowserTargets(Transparency goal, float deltaTime);
    void incrementallyAnimateTargets(double deltaTime);
   
    // Boolean functions
    bool isCameraInSolarSystem();
    bool isSelectedPairFacingCamera();
    bool isSelectedPairUsingRae();

    // Managing the target browser pairs 
    void removeTargetBrowserPair(const std::string& browserId);
    void addTargetBrowserPair(const std::string& targetId, const std::string& browserId);

    // Hover circle
    void moveHoverCircle(int i);
    void disableHoverCircle();
    
    // Image collection handling
    void loadImages(const std::string& root, const std::string& directory);
    int nLoadedImages();

    // Mouse interaction
    void handleMouseClick(const MouseButton& button);

    scripting::LuaLibrary luaLibrary() const override;
    //std::vector<documentation::Documentation> documentations() const override;

protected: 
    void internalInitialize(const ghoul::Dictionary& dict) override;
    void internalDeinitialize() override;

private:
    properties::BoolProperty _allowMouseInteraction;
    properties::BoolProperty _allowCameraRotation;
    glm::ivec3 _highlightAddition{ 35 }; // Highlight object when mouse hovers

    // The browsers and targets
    std::vector<std::unique_ptr<TargetBrowserPair>> _targetsBrowsers;
    TargetBrowserPair* _mouseOnPair{ nullptr };
    ScreenSpaceImageLocal* _hoverCircle{ nullptr };
    std::string _selectedBrowser{ "" }; // Currently selected browser

    // Fading
    Transparency _goal;
    
    // Flags
    bool _isCameraInSolarSystem{ true }; // Visualization modes
    bool _isFading{ false };
    bool _isCameraRotating = false;

    // Mouse interaction
    MouseInteraction _interactionMode;
    glm::vec2 _mousePosition; // Current mouse position in screen space coordinates
    glm::vec2 _startMousePosition;
    glm::vec2 _startDragPosition;

    // Animation of rotation of camera to look at coordinate galactic coordinates
    glm::dvec3 _startAnimation;
    glm::dvec3 _endAnimation;
    
    // Data handler for the image collections
    std::unique_ptr<WwtDataHandler> _dataHandler;    
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___SKYBROWSERMODULE___H__
