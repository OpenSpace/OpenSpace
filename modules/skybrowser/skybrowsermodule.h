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

#include <openspace/util/openspacemodule.h>

#include <openspace/documentation/documentation.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/stringproperty.h>
#include <thread>
#include <string>

#include <iostream>
#include <fstream>

namespace openspace {

class ScreenSpaceSkyBrowser;
class ScreenSpaceSkyTarget;
class RenderableSkyBrowser;
class ScreenSpaceRenderable;
class WWTDataHandler;
class SceneGraphNode;
class ImageData;


class SkyBrowserModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "SkyBrowser";
    constexpr static const int FROM_DIRECTORY = 0;
    constexpr static const int FROM_URL = 1;

    SkyBrowserModule();
    virtual ~SkyBrowserModule() = default;
    glm::vec2 getMousePositionInScreenSpaceCoords(glm::vec2& mousePos);
    void addRenderable(ScreenSpaceRenderable* object);
    WWTDataHandler* getWWTDataHandler();
    std::map<std::string, ScreenSpaceSkyBrowser*>& getSkyBrowsers();
    std::vector<ScreenSpaceRenderable*>& getBrowsersAndTargets();
    SceneGraphNode* get3dBrowser();
    void startRotation(glm::dvec2 coordsEnd);
    void rotateCamera(double deltaTime);
    bool fadeBrowserAndTarget(bool makeTransparent, double fadeTime, double deltaTime);
    void setSelectedBrowser(ScreenSpaceSkyBrowser* ptr);
    void setSelectedBrowser(std::string id);
    bool browserIdExists(std::string id);
    std::string selectedBrowserId();
    int loadImages(const std::string& root, const std::string& directory);
    void add3dBrowser(SceneGraphNode* node);
    bool cameraInSolarSystem();
    void createTargetBrowserPair();
    void removeTargetBrowserPair(std::string& browserId);
    void place3dBrowser(ImageData& image);

    scripting::LuaLibrary luaLibrary() const override;
    //std::vector<documentation::Documentation> documentations() const override;

protected: 
    void internalInitialize(const ghoul::Dictionary& dict) override;
    void internalDeinitialize() override;
    
    // Using snake case on these casting functions to make them similar to eg std::to_string
    ScreenSpaceSkyBrowser* to_browser(ScreenSpaceRenderable* ptr);
    ScreenSpaceSkyTarget* to_target(ScreenSpaceRenderable* ptr);

    // The browsers and targets
    std::vector<ScreenSpaceRenderable*> renderables;
    // Only the browsers
    std::map<std::string, ScreenSpaceSkyBrowser*> browsers;
    // 3D browser
    SceneGraphNode* _browser3d;
    // Pointer to what mouse is currently on
    ScreenSpaceRenderable* _mouseOnObject;
    // Dragging
    glm::vec2 startDragMousePos;
    glm::vec2 startDragObjectPos;
    bool changeViewWithinBrowser;
    // Resizing
    glm::vec2 startResizeBrowserSize;
    glm::vec2 resizeVector;
    // The current mouse position in screenspace coordinates
    glm::vec2 _mousePosition;
    // Current interaction status
    bool currentlyResizingBrowser;
    bool currentlyDraggingObject;
    // Data handler
    WWTDataHandler* dataHandler; 
    // For animating rotation of camera to look at coordinate
    glm::dvec3 _coordsToAnimateTo;
    glm::dvec3 _coordsStartAnimation;
    bool isRotating = false;
    // For tracking the currently selected browser
    std::string selectedBrowser;
    glm::ivec3 highlightAddition{ 35, 35, 35 };
    // Mode of browsing
    bool _cameraInSolarSystem{ true };
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___SKYBROWSERMODULE___H__
