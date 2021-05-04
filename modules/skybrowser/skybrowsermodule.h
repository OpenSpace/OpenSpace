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


#include <iostream>
#include <fstream>

namespace openspace {

class ScreenSpaceSkyBrowser;
class ScreenSpaceSkyTarget;
class ScreenSpaceRenderable;
class WWTDataHandler;


class SkyBrowserModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "SkyBrowser";

    SkyBrowserModule();
    virtual ~SkyBrowserModule() = default;
    glm::vec2 getMousePositionInScreenSpaceCoords(glm::vec2& mousePos);
    void addRenderable(ScreenSpaceRenderable* object);
    WWTDataHandler* getWWTDataHandler();
    std::vector<ScreenSpaceSkyBrowser*> getSkyBrowsers();

    scripting::LuaLibrary luaLibrary() const override;
    //std::vector<documentation::Documentation> documentations() const override;

protected: 
    void internalInitialize(const ghoul::Dictionary& dict) override;
    void internalDeinitialize() override;
    
    // Using snake case on these casting functions to make them similar to eg std::to_string
    ScreenSpaceSkyBrowser* to_browser(ScreenSpaceRenderable* ptr);
    ScreenSpaceSkyTarget* to_target(ScreenSpaceRenderable* ptr);

    bool shouldInitialize;

    // Renderable vector and ptr to where mouse is
    std::vector<ScreenSpaceRenderable*> renderables;
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
    WWTDataHandler* dataHandler; 
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___SKYBROWSERMODULE___H__
