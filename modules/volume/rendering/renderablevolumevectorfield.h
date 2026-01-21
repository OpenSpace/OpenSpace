/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_MODULE_VOLUME_RENDERABLEVECTORFIELD___H__
#define __OPENSPACE_MODULE_VOLUME_RENDERABLEVECTORFIELD___H__

#include <openspace/rendering/renderable.h>

#include <modules/volume/rawvolume.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/dvec3property.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/uvec3property.h>
#include <ghoul/opengl/uniformcache.h>

namespace openspace::documentation { struct Documentation; }

namespace ghoul::opengl {
    class ProgramObject;
} // namespace ghoul::opengl

namespace openspace::volume {

struct VelocityData {
    float vx;
    float vy;
    float vz;
};

struct ArrowInstance {
    glm::vec3 position;
    glm::vec3 direction;
    float magnitude;
};

class RenderableVectorField : public Renderable {
public:
   explicit RenderableVectorField(const ghoul::Dictionary& dictionary);

   ~RenderableVectorField() override = default;

   void initializeGL() override;
   void deinitializeGL() override;

   bool isReady() const override;


   void render(const RenderData& data, RendererTasks& renderTask) override;
   void update(const UpdateData& data) override;

   static documentation::Documentation Documentation();


private:
    void computeFieldLinesParallel();

    std::unique_ptr<ghoul::opengl::ProgramObject> _program;
    UniformCache(
        modelViewProjection, arrowScale, filterOutOfRange, dataRangeFilter, colorByMag,
        magDomain
    ) _uniformCache;

    properties::Vec2Property _dataRange;
    properties::BoolProperty _filterOutOfRange;
    properties::IntProperty _stride;
    properties::FloatProperty _vectorFieldScale;
    properties::FloatProperty _lineWidth;
    properties::BoolProperty _colorByMagnitude;


    std::shared_ptr<RawVolume<VelocityData>> _volumeData;
    std::vector<ArrowInstance> _instances;

    std::string _sourceFile;
    glm::dvec3 _minDomain;
    glm::dvec3 _maxDomain;
    glm::uvec3 _dimensions;

    glm::vec2 _magnitudeDomain = glm::vec2(
        std::numeric_limits<float>::max(), std::numeric_limits<float>::lowest()
    );

    GLuint _vao = 0;
    GLuint _vectorFieldVbo = 0;
    GLuint _arrowVbo = 0;

    bool _vectorFieldIsDirty = true;

    // Arrow pointed along +X direction
    const std::vector<glm::vec3> _arrowVertices = {
        // shaft
        {0.0f, 0.0f, 0.0f},
        {1.0f, 0.0f, 0.0f},

        // head
        {1.0f, 0.0f, 0.0f},
        {0.8f, 0.1f, 0.0f},

        {1.0f, 0.0f, 0.0f},
        {0.8f,-0.1f, 0.0f}
    };
};


} // namespace openspace::volume

#endif // __OPENSPACE_MODULE_VOLUME_RENDERABLEVECTORFIELD___H__
