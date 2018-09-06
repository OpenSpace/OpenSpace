/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_TOUCH___TOUCH_MARKER___H__
#define __OPENSPACE_MODULE_TOUCH___TOUCH_MARKER___H__

#include <modules/touch/include/tuioear.h>

#include <ghoul/opengl/ghoul_gl.h>
#include <openspace/rendering/renderable.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>

#include <ghoul/glm.h>
#include <ghoul/opengl/uniformcache.h>

#include <memory>
#include <vector>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

class TouchMarker : public properties::PropertyOwner {
public:
    TouchMarker();
    virtual ~TouchMarker();

    void initialize();
    void deinitialize();

    void render(const std::vector<TUIO::TuioCursor>& list);

private:
    void createVertexList(const std::vector<TUIO::TuioCursor>& list);

    properties::BoolProperty _visible;
    properties::FloatProperty _radiusSize;
    properties::FloatProperty _transparency;
    properties::FloatProperty _thickness;
    properties::Vec3Property _color;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
    UniformCache(radius, transparency, thickness, color) _uniformCache;

    std::vector<GLfloat> _vertexData;
    GLuint _quad = 0;
    GLuint _vertexPositionBuffer = 0;
};

} // openspace namespace

#endif // __OPENSPACE_MODULE_TOUCH___TOUCH_MARKER___H__
