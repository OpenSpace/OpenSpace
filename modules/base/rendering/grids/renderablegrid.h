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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLEGRID___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLEGRID___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/ivec2property.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/rendering/labelscomponent.h>
#include <ghoul/opengl/ghoul_gl.h>

namespace ghoul::opengl { class ProgramObject; }
namespace openspace::documentation { struct Documentation; }

namespace openspace {

class RenderableGrid : public Renderable {
public:
    explicit RenderableGrid(const ghoul::Dictionary& dictionary);

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

protected:
    struct Vertex {
        double location[3];
    };

    ghoul::opengl::ProgramObject* _gridProgram = nullptr;

    properties::Vec3Property _color;
    properties::Vec3Property _highlightColor;
    // @TODO (abock, 2021-01-28)  This was a UVec2Property before, but it wasn't supported
    // be the codegen.  As soon as it does, this should be changed back
    properties::IVec2Property _segments;
    properties::IVec2Property _highlightRate;
    properties::FloatProperty _lineWidth;
    properties::FloatProperty _highlightLineWidth;
    properties::Vec2Property _size;

    bool _gridIsDirty = true;

    GLuint _vaoID = 0;
    GLuint _vBufferID = 0;
    GLuint _highlightVaoID = 0;
    GLuint _highlightVBufferID = 0;

    GLenum _mode = GL_LINES;
    std::vector<Vertex> _varray;
    std::vector<Vertex> _highlightArray;

    // Labels
    bool _hasLabels = false;
    std::unique_ptr<LabelsComponent> _labels;
};

}// namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLEGRID___H__
