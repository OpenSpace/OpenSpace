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
#include <openspace/rendering/transferfunction.h>
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

    struct PolygonVertex {
        GLfloat position[3];
        GLfloat normal[3];
        GLfloat value;
    };

    struct TimePolygonPoint {
        glm::dvec3 coordinate = glm::dvec3(0.0);
        float value = 0.f;
    };

    struct TimePolygon {
        double timestamp = 0.0;
        std::vector<TimePolygonPoint> points;
    };

    void readDataFile();

    void updateTubeData();
    void createSmoothTube(const size_t nPolygons, const size_t nPoints);
    void createLowPolyTube(const size_t nPolygons, const size_t nPoints);
    void updateBufferData();

    // Properties
    properties::StringProperty _transferFunctionPath;
    properties::Vec3Property _color;
    properties::BoolProperty _enableFaceCulling;
    properties::PropertyOwner _lightSourcePropertyOwner;
    Shading _shading;

    properties::BoolProperty _addEdges;
    properties::BoolProperty _drawWireframe;
    properties::FloatProperty _wireLineWidth;
    properties::BoolProperty _useSmoothNormals;

    UniformCache(modelViewTransform, projectionTransform, normalTransform, color,
        opacity, hasTransferFunction, transferFunction, performShading, nLightSources,
        lightDirectionsViewSpace, lightIntensities, ambientIntensity, diffuseIntensity,
        specularIntensity) _uniformCache;

    std::vector<float> _lightIntensitiesBuffer;
    std::vector<glm::vec3> _lightDirectionsViewSpaceBuffer;
    std::vector<std::unique_ptr<LightSource>> _lightSources;

    std::filesystem::path _dataFile;
    std::vector<TimePolygon> _data;
    bool _tubeIsDirty = false;
    bool _hasTransferFunction = false;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
    GLuint _vaoId = 0;
    GLuint _vboId = 0;
    GLuint _iboId = 0;

    std::vector<PolygonVertex> _verticies;
    std::vector<unsigned int> _indicies;
    std::shared_ptr<openspace::TransferFunction> _transferFunction;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLETUBE___H__
