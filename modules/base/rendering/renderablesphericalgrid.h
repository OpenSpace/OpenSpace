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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLESPHERICALGRID___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLESPHERICALGRID___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/matrix/dmat4property.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec4property.h>

#include <ghoul/opengl/ghoul_gl.h>

namespace ghoul::opengl {
    class ProgramObject;
} // namespace ghoul::opengl

namespace openspace::documentation { struct Documentation; }

namespace openspace {

class RenderableSphericalGrid : public Renderable {
public:
    RenderableSphericalGrid(const ghoul::Dictionary& dictionary);
    ~RenderableSphericalGrid();

    void initialize() override;
    void deinitialize() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

protected:
    struct Vertex {
        float location[3];
    };

    std::unique_ptr<ghoul::opengl::ProgramObject> _gridProgram;

    properties::DMat4Property _gridMatrix;
    properties::Vec4Property _gridColor;
    properties::IntProperty _segments;
    properties::FloatProperty _lineWidth;
    properties::FloatProperty _radius;

    bool _gridIsDirty;

    GLuint _vaoID;
    GLuint _vBufferID;
    GLuint _iBufferID;

    GLenum _mode;
    unsigned int _isize;
    unsigned int _vsize;
    std::vector<Vertex> _varray;
    std::vector<int> _iarray;
};

}// namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLESPHERICALGRID___H__
