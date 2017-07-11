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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___POINTGLOBE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___POINTGLOBE___H__

#include <openspace/rendering/renderable.h>
#include <openspace/properties/scalarproperty.h>

#include <ghoul/opengl/ghoul_gl.h>

namespace ghoul { namespace opengl {
class ProgramObject;
} }

namespace openspace {
namespace globebrowsing {

class RenderableGlobe;

class PointGlobe : public Renderable {
public:
    PointGlobe(const RenderableGlobe& owner);
    virtual ~PointGlobe();

    bool initialize() override;
    bool deinitialize() override;
    bool isReady() const override;

    void render(const RenderData& data) override;
    
private:
    const RenderableGlobe& _owner;
    std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;

    GLuint _vertexBufferID;
    GLuint _vaoID;

    properties::FloatProperty _intensityClamp;
    properties::FloatProperty _lightIntensity;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___POINTGLOBE___H__
