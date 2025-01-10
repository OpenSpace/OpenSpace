/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___SCREENSPACEFRAMEBUFFER___H__
#define __OPENSPACE_MODULE_BASE___SCREENSPACEFRAMEBUFFER___H__

#include <openspace/rendering/screenspacerenderable.h>

#include <openspace/properties/vector/vec4property.h>

namespace ghoul::opengl {
    class FramebufferObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

namespace documentation { struct Documentation; }

/**
 * Creates a texture by rendering to a framebuffer, this is then used on a screen space
 * plane. This class lets you ass renderfunctions that should render to a framebuffer with
 * an attached texture. The texture is then used on a screen space plane that works both
 * in fisheye and flat screens.
 */
class ScreenSpaceFramebuffer : public ScreenSpaceRenderable {
public:
    using RenderFunction = std::function<void()>;

    ScreenSpaceFramebuffer(const ghoul::Dictionary& dictionary = ghoul::Dictionary());
    virtual ~ScreenSpaceFramebuffer() override;

    bool initializeGL() override;
    bool deinitializeGL() override;
    void render(const RenderData& renderData) override;
    bool isReady() const override;

    void setSize(glm::vec4 size);
    void addRenderFunction(RenderFunction renderFunction);
    void removeAllRenderFunctions();

    static documentation::Documentation Documentation();

protected:
    void createFramebuffer();
    properties::Vec4Property _size;

private:
    void bindTexture() override;

    static int id();

    std::unique_ptr<ghoul::opengl::FramebufferObject> _framebuffer;
    std::vector<std::function<void()>> _renderFunctions;

    std::unique_ptr<ghoul::opengl::Texture> _texture;
};

} //namespace openspace

#endif // __OPENSPACE_MODULE_BASE___SCREENSPACEFRAMEBUFFER___H__
