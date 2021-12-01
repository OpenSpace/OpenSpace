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
#include <modules/base/rendering/screenspaceimagelocal.h>
#include <openspace/documentation/documentation.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/openspacemodule.h>
#include <openspace/util/distanceconstants.h>
#include <fstream>

namespace openspace {

class RenderableSkyBrowser;

enum class Transparency {
    Transparent,
    Opaque
};

class SkyBrowserModule : public OpenSpaceModule {
public:

    constexpr static const char* Name = "SkyBrowser";
    const double SolarSystemRadius = 30.0 * distanceconstants::AstronomicalUnit;

    // Constructor & destructor
    SkyBrowserModule();
    virtual ~SkyBrowserModule();

    // Getters
    std::vector<std::unique_ptr<Pair>>& getPairs();
    Pair* getPair(const std::string& id);
    SceneGraphNode* get3dBrowserNode();
    RenderableSkyBrowser* get3dBrowser();
    RenderableSkyBrowser* get3dBrowser(const std::string& id);
    const std::unique_ptr<WwtDataHandler>& getWwtDataHandler();
    std::string selectedBrowserId();

    // Setters
    void set3dBrowser(const std::string& id);
    void setSelectedBrowser(const std::string& id);
    void setSelectedObject(); // Manage mouse interactions
    void setHoverCircle(ScreenSpaceImageLocal* circle);
   
    // Rotation, animation, placement
    void lookAtTarget(std::string id);
    void startRotatingCamera(glm::dvec3 endAnimation); // Pass in galactic coordinate
    void incrementallyRotateCamera(double deltaTime);
    void incrementallyFadeBrowserTargets(Transparency goal, float deltaTime);
    void incrementallyAnimateTargets(double deltaTime);
    void lookAt3dBrowser();
    void place3dBrowser(const ImageData& image, const int i);
   
    // Boolean functions
    bool isCameraInSolarSystem();

    // Managing the target browser pairs 
    void createTargetBrowserPair();
    void removeTargetBrowserPair(std::string& browserId);
    void addTargetBrowserPair(std::string targetId, std::string browserId);

    // Hover circle
    void moveHoverCircle(int i);
    void disableHoverCircle();
    
    // Image collection handling
    void loadImages(const std::string& root, const std::string& directory, 
                   std::vector<std::filesystem::path>& speckFiles);
    int nLoadedImages();
    void add2dSelectedImagesTo3d(const std::string& pairId);

    scripting::LuaLibrary luaLibrary() const override;
    //std::vector<documentation::Documentation> documentations() const override;

protected: 
    void internalInitialize(const ghoul::Dictionary& dict) override;
    void internalDeinitialize() override;

private:

    
    // The browsers and targets
    std::vector<std::unique_ptr<Pair>> _targetsBrowsers;
    Pair* _mouseOnPair{ nullptr };
    Pair* _selectedPair{ nullptr };
    bool _isBrowser{ false };
    ScreenSpaceImageLocal* _hoverCircle{ nullptr };
    SceneGraphNode* _browser3dNode{ nullptr };
    RenderableSkyBrowser* _browser3d{ nullptr };
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
    glm::ivec2 _resizeDirection{ 0 };

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
