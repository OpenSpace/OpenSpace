/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_CORE___SCREENSPACERENDERABLETEXT___H__
#define __OPENSPACE_CORE___SCREENSPACERENDERABLETEXT___H__

#include <openspace/rendering/screenspacerenderable.h>

#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opengl/texture.h>

namespace openspace {

class ScreenSpaceRenderableText : public ScreenSpaceRenderable {
public:
    explicit ScreenSpaceRenderableText(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;
    bool isReady() const override;

    void update() override;
    void render(const RenderData& renderData) override;

    static documentation::Documentation Documentation();

protected:
    std::string _buffer;

private:
    void updateFramebuffer();
    void bindTexture() override;

    properties::StringProperty _fontName;
    properties::FloatProperty _fontSize;

    std::shared_ptr<ghoul::fontrendering::Font> _font;
    std::unique_ptr<ghoul::fontrendering::FontRenderer> _fontRenderer;

    std::unique_ptr<ghoul::opengl::FramebufferObject> _framebuffer;
    std::unique_ptr<ghoul::opengl::Texture> _texture;
};

} //namespace openspace

#endif // __OPENSPACE_CORE___SCREENSPACERENDERABLETEXT___H__
