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

namespace openspace {

class ScreenSpaceSkyBrowser;
class ScreenSpaceSkyTarget;

class SkyBrowserModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "SkyBrowser";

    SkyBrowserModule();
    virtual ~SkyBrowserModule() = default;

    float zoomFactor() const;
    glm::dvec2 convertGalacticToCelestial(glm::dvec3 coords) const;

    void WWTfollowCamera();

    void createBrowser();
    void createTarget(glm::ivec2 dimension);

    ghoul::Dictionary createMessageForMovingWWTCamera(const glm::dvec2 celestCoords, const float fov, const bool moveInstantly = true) const;
    ghoul::Dictionary createMessageForPausingWWTTime() const;
    ghoul::Dictionary createMessageForLoadingWWTImgColl(const std::string& url) const;

    bool sendMessageToWWT(const ghoul::Dictionary& msg);
    void handleInteractions();
    glm::vec2 getMousePositionInScreenSpaceCoords(glm::vec2& mousePos);

    void initializeBrowser(ScreenSpaceSkyBrowser* skyBrowser, ScreenSpaceSkyTarget* skyTarget);
    ScreenSpaceSkyBrowser* skyBrowser();
    scripting::LuaLibrary luaLibrary() const override;
    //std::vector<documentation::Documentation> documentations() const override;

protected:
    void internalInitialize(const ghoul::Dictionary& dict) override;
    void internalDeinitialize() override;

    properties::StringProperty _testProperty;
    properties::FloatProperty _zoomFactor;
    ScreenSpaceSkyBrowser* _skyBrowser;
    ScreenSpaceSkyTarget* _skyTarget;
    bool _camIsSyncedWWT;
    bool _listenForInteractions;
    std::thread _threadWWTMessages;
    std::thread _threadHandleInteractions;
    glm::vec2 startDragMousePosBrowser;
    glm::vec2 startDragObjectPosBrowser;
    glm::vec2 startDragMousePosTarget;
    glm::vec2 startDragObjectPosTarget;
    glm::vec2 startResizeBrowserSize;
    glm::vec2 resizeVector;
    bool currentlyDraggingBrowser;
    bool currentlyResizingBrowser;
    bool currentlyDraggingTarget;
    glm::vec2 _mousePosition;
    double _mouseScroll;
    bool mouseIsOnBrowser;
    bool mouseIsOnTarget;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___SKYBROWSERMODULE___H__
