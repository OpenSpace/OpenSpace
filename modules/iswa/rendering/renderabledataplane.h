/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_ISWA___RENDERABLEDATAPLANE___H__
#define __OPENSPACE_MODULE_ISWA___RENDERABLEDATAPLANE___H__

#include <modules/iswa/rendering/renderabledatacygnet.h>

#include <ghoul/opengl/ghoul_gl.h>

namespace openspace {

namespace documentation { struct Documentation; }

/**
 * DataPlane is a concrete IswaCygnet with data files as its input source. The class
 * handles creation, destruction and rendering of a plane geometry. It also specifies what
 * uniforms to use and what GUI properties it needs.
 */
class RenderableDataPlane : public RenderableDataCygnet {
friend class IswaBaseGroup;
public:
    explicit RenderableDataPlane(const ghoul::Dictionary& dictionary);
     ~RenderableDataPlane() = default;

     void initializeGL() override;
     void deinitializeGL() override;

     static documentation::Documentation Documentation();

private:
    /**
     * Creates a plane geometry.
     */
    bool createGeometry() override;
    bool destroyGeometry() override;
    void renderGeometry() const override;
    void setUniforms() override;
    std::vector<float*> textureData() override;

    GLuint _quad;
    GLuint _vertexPositionBuffer;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_ISWA___RENDERABLEDATAPLANE___H__
