/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLETUBE___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLETUBE___H__

#include <openspace/rendering/renderable.h>

#include <openspace/json.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <ghoul/glm.h>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

class LightSource;

namespace documentation { struct Documentation; }

class RenderableTube : public Renderable {
public:
    RenderableTube(const ghoul::Dictionary& dictionary);

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    struct Shading : properties::PropertyOwner {
        Shading();
        properties::BoolProperty enabled;
        properties::FloatProperty ambientIntensity;
        properties::FloatProperty diffuseIntensity;
        properties::FloatProperty specularIntensity;
    };

    struct TimePolygon {
        double timestamp;
        std::vector<glm::dvec3> points;
    };

    void readDataFile();

    void updateTubeData();
    void updateBufferData();

    // Properties
    properties::Vec3Property _color;
    properties::BoolProperty _enableFaceCulling;
    properties::PropertyOwner _lightSourcePropertyOwner;

    UniformCache(modelViewTransform, projectionTransform, normalTransform, color,
        opacity, performShading, nLightSources, lightDirectionsViewSpace,
        lightIntensities) _uniformCache;

    // Buffers for uniform uploading
    std::vector<float> _lightIntensitiesBuffer;
    std::vector<glm::vec3> _lightDirectionsViewSpaceBuffer;

    std::filesystem::path _dataFile;
    std::vector<TimePolygon> _data;
    std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
    Shading _shading;
    std::vector<std::unique_ptr<LightSource>> _lightSources;
    GLuint _vaoId = 0;
    GLuint _vboId = 0;
    GLuint _iboId = 0;
    std::vector<float> _vertexArray;
    std::vector<unsigned int> _indexArray;
    bool _tubeIsDirty = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLETUBE___H__
