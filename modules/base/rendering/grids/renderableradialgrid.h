/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLERADIALGRID___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLERADIALGRID___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/ivec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/rendering/helper.h>
#include <ghoul/opengl/ghoul_gl.h>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace::documentation { struct Documentation; }

namespace openspace {

class RenderableRadialGrid : public Renderable {
public:
    RenderableRadialGrid(const ghoul::Dictionary& dictionary);
    ~RenderableRadialGrid() = default;

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

protected:
    struct GeometryData {
        GeometryData(GLenum renderMode);
        GeometryData(GeometryData&& other) noexcept;
        GeometryData& operator=(const GeometryData& other) = delete;
        GeometryData& operator=(GeometryData&& other) noexcept;
        ~GeometryData();

        void update();
        void render();

        std::vector<rendering::helper::VertexXYZ> varray;
        GLuint vao = 0;
        GLuint vbo = 0;
        GLenum mode = GL_LINE_STRIP;
    };

    ghoul::opengl::ProgramObject* _gridProgram;

    properties::Vec3Property _gridColor;
    properties::IVec2Property _gridSegments;
    properties::IntProperty _circleSegments;
    properties::FloatProperty _lineWidth;
    properties::FloatProperty _maxRadius;
    properties::FloatProperty _minRadius;

    bool _gridIsDirty = true;

    std::vector<GeometryData> _circles;
    GeometryData _lines{GL_LINES};
};

}// namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLERADIALGRID___H__
