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

#ifndef __OPENSPACE_CORE___SCREENSPACERENDERABLEFRAMEBUFFER___H__
#define __OPENSPACE_CORE___SCREENSPACERENDERABLEFRAMEBUFFER___H__

#include <openspace/rendering/screenspacerenderable.h>

#include <openspace/properties/vector/vec2property.h>
#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opengl/texture.h>
#include <functional>

namespace openspace {

/**
 * Creates a texture by rendering to a framebuffer, this is then used on a screen space
 * plane. This class lets you add renderfunctions that should render to a framebuffer with
 * an attached texture. The texture is then used on a screen space plane that works both
 * in fisheye and flat screens.
 */
class ScreenSpaceRenderableFramebuffer : public ScreenSpaceRenderable {
public:
    using RenderFunction = std::function<void()>;

    explicit ScreenSpaceRenderableFramebuffer(const ghoul::Dictionary& dictionary);
    ~ScreenSpaceRenderableFramebuffer() override;

    void initializeGL() override;
    void deinitializeGL() override;
    void render(const RenderData& renderData) override;
    bool isReady() const override;

    void addRenderFunction(RenderFunction renderFunction);
    void removeAllRenderFunctions();

    static documentation::Documentation Documentation();

protected:
    void createFramebuffer();
    properties::Vec2Property _size;

private:
    void bindTexture() override;

    static int id();

    std::unique_ptr<ghoul::opengl::FramebufferObject> _framebuffer;
    std::vector<std::function<void()>> _renderFunctions;

    std::unique_ptr<ghoul::opengl::Texture> _texture;
};

} //namespace openspace

#endif // __OPENSPACE_CORE___SCREENSPACERENDERABLEFRAMEBUFFER___H__
