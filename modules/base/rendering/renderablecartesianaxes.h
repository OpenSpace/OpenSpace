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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLECARTESIANAXES___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLECARTESIANAXES___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/matrix/dmat4property.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <ghoul/opengl/ghoul_gl.h>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace::documentation { struct Documentation; }

namespace openspace {

class RenderableCartesianAxes : public Renderable {
public:
    explicit RenderableCartesianAxes(const ghoul::Dictionary& dictionary);
    ~RenderableCartesianAxes() override = default;

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;

    static documentation::Documentation Documentation();

protected:
    struct Vertex {
        float location[3];
    };

    ghoul::opengl::ProgramObject* _program;

    properties::Vec3Property _xColor;
    properties::Vec3Property _yColor;
    properties::Vec3Property _zColor;

    GLuint _vaoId = 0;
    GLuint _vBufferId = 0;
    GLuint _iBufferId = 0;
};

}// namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLECARTESIANAXES___H__
