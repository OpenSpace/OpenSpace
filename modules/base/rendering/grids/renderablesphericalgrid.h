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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLESPHERICALGRID___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLESPHERICALGRID___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/rendering/labelscomponent.h>
#include <ghoul/opengl/ghoul_gl.h>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

namespace documentation { struct Documentation; }

class RenderableSphericalGrid : public Renderable {
public:
    explicit RenderableSphericalGrid(const ghoul::Dictionary& dictionary);
    ~RenderableSphericalGrid() override = default;

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

protected:
    struct Vertex {
        float location[3];
    };

    ghoul::opengl::ProgramObject* _gridProgram;

    properties::Vec3Property _color;
    properties::IntProperty _longSegments;
    properties::IntProperty _latSegments;
    properties::FloatProperty _lineWidth;

    GLuint _vaoID = 0;
    GLuint _vBufferID = 0;
    GLuint _iBufferID = 0;

    bool _gridIsDirty = true;

    // Labels
    bool _hasLabels = false;
    std::unique_ptr<LabelsComponent> _labels;
};

}// namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLESPHERICALGRID___H__
