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


#include <modules/skybrowser/include/pair.h>
#include <modules/skybrowser/include/screenspaceskytarget.h>
#include <modules/skybrowser/include/wwtdatahandler.h>
 //#include <modules/skybrowser/include/screenspaceskybrowser.h>
#include <modules/base/rendering/screenspaceimagelocal.h>
#include <openspace/documentation/documentation.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/openspacemodule.h>
#include <openspace/util/distanceconstants.h>
#include <fstream>

namespace openspace {

class ScreenSpaceSkyBrowser;

enum class Transparency {
    Transparent = 0,
    Opaque = 1
};

class SkyBrowserModule : public OpenSpaceModule {
public:

    constexpr static const char* Name = "SkyBrowser";
    const double SolarSystemRadius = 30.0 * distanceconstants::AstronomicalUnit;

    // Constructor & destructor
    SkyBrowserModule();
    virtual ~SkyBrowserModule();

    // Getters
    std::vector<Pair>& getPairs();
    Pair* getPair(std::string id);
    SceneGraphNode* get3dBrowser();
    const std::unique_ptr<WwtDataHandler>& getWWTDataHandler();
    std::string selectedBrowserId();

    // Setters
    void setSelectedBrowser(ScreenSpaceSkyBrowser* ptr);
    void setSelectedBrowser(std::string id);
    void set3dBrowser(SceneGraphNode* node);
    void selectImage2dBrowser(int i);
    void selectImage3dBrowser(int i);
   
    // Rotation and animation
    
    void lookAtTarget(std::string id);
    void incrementallyRotateCamera(double deltaTime);
    void incrementallyFadeBrowserTargets(Transparency goal, float deltaTime);
    void incrementallyAnimateTargets(double deltaTime);
    void lookAt3dBrowser();
   
    // Boolean functions
    bool isCameraInSolarSystem();

    // Managing the browsers 
    void createTargetBrowserPair();
    void removeTargetBrowserPair(std::string& browserId);
    void addTargetBrowserPair(std::string targetId, std::string browserId);
    void moveHoverCircle(int i);
    
    // Image collection handling
    void loadImages(const std::string& root, const std::string& directory, 
                   std::vector<std::filesystem::path>& speckFiles);
    int nLoadedImages();

    // Manage mouse interactions
    void setSelectedObject();

    scripting::LuaLibrary luaLibrary() const override;
    //std::vector<documentation::Documentation> documentations() const override;


protected: 
    void internalInitialize(const ghoul::Dictionary& dict) override;
    void internalDeinitialize() override;

private:

    void startRotatingCamera(glm::dvec3 endAnimation); // Pass in galactic coordinate
    // The browsers and targets
    std::vector<Pair> _targetsBrowsers;
    Pair* _mouseOnPair{ nullptr };
    Pair* _selectedPair{ nullptr };
    bool _isBrowser{ false };
    ScreenSpaceImageLocal* _hoverCircle{ nullptr };
    SceneGraphNode* _browser3d{ nullptr };
    std::string _selectedBrowser{ "" }; // Currently selected browser (2D or 3D)

    // Fading
    double _fadingTime = 2.0;
    
    // Flags
    bool _isFineTuneMode{ false };
    bool _isResizing{ false };
    bool _isDragging{ false };
    bool _isCameraInSolarSystem{ true };
    bool _isCameraRotating = false;
    bool _isTransitioningVizMode{ false };

    // Mouse interaction - dragging and resizing
    glm::ivec3 _highlightAddition{ 35 }; // Highlight object when mouse hovers
    glm::vec2 _mousePosition; // Current mouse position in screen space coordinates
    glm::vec2 _startMousePosition;
    glm::vec2 _startDragPosition;
    glm::vec2 _startBrowserSize;
    glm::vec2 _resizeDirection{ 0.f };

    // Animation of rotation of camera to look at coordinate galactic coordinates
    glm::dvec3 _startAnimation;
    glm::dvec3 _endAnimation;
    double _stopAnimationThreshold{ 0.05 };
    double _animationSpeed{ 1.0 };
    
    // Data handler for the image collections
    std::unique_ptr<WwtDataHandler> _dataHandler;    
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___SKYBROWSERMODULE___H__
