/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_MODULE_WEBBROWSER__SCREEN_SPACE_BROWSER_H
#define __OPENSPACE_MODULE_WEBBROWSER__SCREEN_SPACE_BROWSER_H

#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vectorproperty.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/opengl/texture.h>
#include <fmt/format.h>
#include "include/screen_space_render_handler.h"
#include "include/browser_instance.h"

namespace openspace {

class ScreenSpaceBrowser : public ScreenSpaceRenderable {
public:
    ScreenSpaceBrowser(const ghoul::Dictionary& dictionary);

    bool initialize() override;
    bool deinitialize() override;
    void render() override;
    void update() override;
    bool isReady() const override;

private:
    properties::StringProperty _url;
    properties::Vec2Property _dimensions;
    CefRefPtr<ScreenSpaceRenderHandler> _renderHandler;
    std::unique_ptr<BrowserInstance> _browserInstance;

    bool _urlIsDirty;
    bool _dimensionsAreDirty;
};

};

#endif //__OPENSPACE_MODULE_WEBBROWSER__SCREEN_SPACE_BROWSER_H
