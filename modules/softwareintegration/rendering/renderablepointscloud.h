/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_SOFTWAREINTEGRATION___RENDERABLEPOINTSCLOUD___H__
#define __OPENSPACE_MODULE_SOFTWAREINTEGRATION___RENDERABLEPOINTSCLOUD___H__

#include <optional>

#include <openspace/rendering/renderable.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/properties/vector/ivec3property.h>
#include <openspace/properties/misc/optionproperty.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>

namespace openspace::documentation { struct Documentation; }

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

class SoftwareIntegrationModule;

class RenderablePointsCloud : public Renderable {
public:
    RenderablePointsCloud(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks&) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    void loadPointData(SoftwareIntegrationModule* softwareIntegrationModule);
    void loadColormap(SoftwareIntegrationModule* softwareIntegrationModule);
    void loadColormapAttributeData(SoftwareIntegrationModule* softwareIntegrationModule);
    void loadLinearSizeAttributeData(SoftwareIntegrationModule* softwareIntegrationModule);
    void loadVelocityData(SoftwareIntegrationModule* softwareIntegrationModule);

    bool checkDataStorage();

    void checkColormapMinMax();
    void checkIfColormapCanBeEnabled();
    void checkIfLinearSizeCanBeEnabled();
    void updateVelocityT0();
    void checkIfMotionCanBeEnabled();

    bool shouldLoadPointData(SoftwareIntegrationModule* softwareIntegrationModule);
    bool shouldLoadVelocityData(SoftwareIntegrationModule* softwareIntegrationModule);
    bool shouldLoadColormap(SoftwareIntegrationModule* softwareIntegrationModule);
    bool shouldLoadColormapAttrData(SoftwareIntegrationModule* softwareIntegrationModule);
    bool shouldLoadLinearSizeAttrData(SoftwareIntegrationModule* softwareIntegrationModule);

    std::unique_ptr<ghoul::opengl::ProgramObject> _shaderProgram = nullptr;
    UniformCache(
        color, size, modelMatrix, cameraUp, screenSize,
        cameraViewProjectionMatrix, eyePosition, sizeOption,
        colormapTexture, colormapMin, colormapMax, colormapNanMode,
        colormapNanColor, colormapEnabled, linearSizeMin, linearSizeMax,
        linearSizeEnabled, velocityNanMode, motionEnabled, time
    ) _uniformCache;

    properties::StringProperty _name;
    properties::FloatProperty _size;
    properties::Vec4Property _color;
    properties::StringProperty _pointUnit;

    properties::OptionProperty _sizeOption;
    properties::BoolProperty _linearSizeEnabled;
    properties::FloatProperty _linearSizeMin;
    properties::FloatProperty _linearSizeMax;

    std::unique_ptr<ghoul::opengl::Texture> _colormapTexture;
    properties::BoolProperty _colormapEnabled;
    properties::FloatProperty _colormapMin;
    properties::FloatProperty _colormapMax;
    properties::IntProperty _colormapNanMode;
    properties::Vec4Property _colormapNanColor;

    properties::BoolProperty _motionEnabled;
    properties::StringProperty _velocityDistanceUnit;
    properties::StringProperty _velocityTimeUnit;
    properties::IVec3Property _velocityDateRecorded;
    properties::IntProperty _velocityNanMode;
    
    
    std::optional<std::string> _identifier = std::nullopt;

    bool _pointUnitIsDirty = false;

    bool _hasLoadedColormapAttributeData = false;
    bool _hasLoadedColormap = false;

    bool _hasLoadedLinearSizeAttributeData = false;
    
    double _t0 = 0.0;
    bool _velocityDateIsDirty = false;
    bool _velocityUnitsAreDirty = false;
    bool _hasLoadedVelocityData = false;

    // This is determined by nrAttributes + size for each attribute
    const size_t _nValuesForVAOStride = 8;

    enum class DataSliceKey : size_t {
        Points = 0,
        ColormapAttributes,
        LinearSizeAttributes,
        Velocity
    };
    using DataSlice = std::vector<float>;
    std::unordered_map<DataSliceKey, std::shared_ptr<DataSlice>> _dataSlices;
    std::shared_ptr<DataSlice> getDataSlice(DataSliceKey key);
    enum SizeOption {
        Uniform = 0,
        NonUniform
    };

    struct VBOLayout {
        glm::vec3 position;
        float colormapAttributeData;
    };
    GLuint _vbo = 0;
    GLuint _vao = 0;
};

}// namespace openspace

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___RENDERABLEPOINTSCLOUD___H__
