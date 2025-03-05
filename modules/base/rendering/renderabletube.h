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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLETUBE___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLETUBE___H__

#include <openspace/rendering/renderable.h>

#include <openspace/json.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/rendering/colormappingcomponent.h>
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

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    struct PolygonVertex {
        GLdouble position[3];
        GLuint polyId;
        GLfloat normal[3];
        GLfloat value;
        GLfloat tex[2];
        GLfloat tex_next[2];
    };

    struct TimePolygonPoint {
        glm::dvec3 coordinate = glm::dvec3(0.0);
        glm::vec2 tex = glm::vec2(0.f);
        glm::vec2 tex_next = glm::vec2(0.f);
    };

    struct TimePolygon {
        double timestamp = 0.0;
        glm::dvec3 center = glm::dvec3(0.0);
        std::vector<TimePolygonPoint> points;
        std::filesystem::path texturePath;
    };

    struct FindTimeStruct {
        bool foundPrev = false;
        bool onSlice = false;
        size_t lastPolygonBeforeTime = 0;
        size_t firstPolygonAfterTime = std::numeric_limits<size_t>::max();
    };

    /// Find the index of the currently chosen color parameter in the data
    int currentColorParameterIndex() const;
    int currentColorCutplaneParameterIndex() const;

    void readDataFile();
    void loadSelectedSample();
    void initializeTextures();

    void createTube();
    void createSmoothTube(unsigned int firstSideIndex);
    void createLowPolyTube(unsigned int firstSideIndex);

    void addEdge(int polygonIndex, const TimePolygon const* polygon, int centerIndex,
        bool isCutplane = false, double tInterpolation = -1.0);

    void addSmoothSection(int polygonIndex, const TimePolygon const* polygon,
        bool isFirstPoly, unsigned int vIndex, bool isEnding = false,
        double tInterpolation = -1.0);
    void addLowPolySection(int polygonIndex, const TimePolygon const* polygon,
        const TimePolygon const* nextPolygon, unsigned int& vIndex,
        double tInterpolation = -1.0);

    FindTimeStruct findTime(double time) const;
    void jumpToPrevPolygon() const;
    void jumpToNextPolygon() const;

    void interpolateEnd(double now);
    void creteEnding(double now);
    void createSmoothEnding(const TimePolygon const* prevTimePolygon,
        const TimePolygon const* currentTimePolygon);
    void createLowPolyEnding(const TimePolygon const* prevTimePolygon,
        const TimePolygon const* currentTimePolygon);

    void updateBufferData();
    void updateEndingBufferData();
    void setCommonUniforms(ghoul::opengl::ProgramObject* shader, const RenderData& data);

    struct Shading : properties::PropertyOwner {
        Shading();
        properties::BoolProperty enabled;
        properties::FloatProperty ambientIntensity;
        properties::FloatProperty diffuseIntensity;
        properties::FloatProperty specularIntensity;
    };
    Shading _shading;

    struct ColorSettings : properties::PropertyOwner {
        explicit ColorSettings(const ghoul::Dictionary& dictionary);
        properties::Vec3Property tubeColor;
        std::unique_ptr<ColorMappingComponent> colorMapping;
    };
    ColorSettings _colorSettings;

    struct ColorSettingsCutplane : properties::PropertyOwner {
        explicit ColorSettingsCutplane(const ghoul::Dictionary& dictionary);
        properties::Vec3Property fixedColor;
        std::unique_ptr<ColorMappingComponent> colorMapping;
    };
    ColorSettingsCutplane _colorSettingsCutplane;
    dataloader::Dataset _colorDatasetCutplane;

    properties::PropertyOwner _lightSourcePropertyOwner;
    properties::BoolProperty _addEdges;
    properties::BoolProperty _useSmoothNormals;
    properties::OptionProperty _interpolationMethod;
    properties::BoolProperty _drawWireframe;
    properties::FloatProperty _wireLineWidth;
    properties::BoolProperty _showAllTube;
    properties::BoolProperty _useTubeFade;
    properties::FloatProperty _tubeFadeLength;
    properties::FloatProperty _tubeFadeAmount;
    properties::TriggerProperty _jumpToPrevPolygon;
    properties::TriggerProperty _jumpToNextPolygon;
    properties::StringProperty _selectedSample;
    properties::Vec3Property _sampleColor;
    properties::FloatProperty _sampleLineWidth;
    properties::BoolProperty _enableFaceCulling;

    std::vector<float> _lightIntensitiesBuffer;
    std::vector<glm::vec3> _lightDirectionsViewSpaceBuffer;
    std::vector<std::unique_ptr<LightSource>> _lightSources;

    std::filesystem::path _dataFile;
    std::vector<TimePolygon> _data;
    bool _tubeIsDirty = false;
    size_t _nPolygons = 0;
    size_t _nPoints = 0;
    size_t _nIndiciesToRender = 0;
    size_t _lastPolygonBeforeNow = 0;
    size_t _firstPolygonAfterNow = 0;
    bool _interpolationNeeded = false;
    float _tValue = 0.f;

    dataloader::Dataset _colorDataset;
    bool _hasColorMapFile = false;

    // Shape containers for the whole tube
    GLuint _vaoId = 0;
    GLuint _vboId = 0;
    GLuint _iboId = 0;
    std::vector<PolygonVertex> _verticies;
    std::vector<unsigned int> _indicies;

    // Shape containers for the ending of the tube
    GLuint _vaoIdEnding = 0;
    GLuint _vboIdEnding = 0;
    GLuint _iboIdEnding = 0;
    std::vector<PolygonVertex> _verticiesEnding;
    std::vector<unsigned int> _indiciesEnding;
    std::vector<unsigned int> _indiciesCutplane;

    GLuint _textureArrayId = 0;
    bool _hasTextures = false;
    std::filesystem::path _texturesDirectory;
    std::vector<std::unique_ptr<ghoul::opengl::Texture>> _textures;
    glm::uvec2 _textureResolution = glm::uvec2(0);
    std::filesystem::path _kernelsDirectory;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
    std::unique_ptr<ghoul::opengl::ProgramObject> _shaderCutplane;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLETUBE___H__
