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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___POINTGLOBE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___POINTGLOBE___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/scalar/floatproperty.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace::globebrowsing {

class RenderableGlobe;

class PointGlobe : public Renderable {
public:
    PointGlobe(const RenderableGlobe& owner);
    virtual ~PointGlobe();

    void initialize() override;
    void deinitialize() override;
    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;

private:
    const RenderableGlobe& _owner;
    std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;
    UniformCache(lightIntensityClamped, modelView, projection) _uniformCache;

    GLuint _vertexBufferID = 0;
    GLuint _vaoID = 0;

    properties::FloatProperty _intensityClamp;
    properties::FloatProperty _lightIntensity;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___POINTGLOBE___H__
