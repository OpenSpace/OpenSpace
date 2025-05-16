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

#include <modules/softwareintegration/rendering/renderablepointscloud.h>

#include <modules/softwareintegration/softwareintegrationmodule.h>
#include <modules/softwareintegration/simp/simp.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/distanceconversion.h>
#include <openspace/util/timeconversion.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <fstream>


const std::string STRING_NOT_SET = "<string not set>";

namespace {
    constexpr const char* _loggerCat = "PointsCloud";

    constexpr const std::array<const char*, 20> UniformNames = {
        "color", "size", "modelMatrix", "cameraUp", "screenSize",
        "cameraViewProjectionMatrix", "eyePosition", "sizeOption",
        "colormapTexture", "colormapMin", "colormapMax", "colormapNanMode",
        "colormapNanColor", "colormapEnabled", "linearSizeMin", "linearSizeMax",
        "linearSizeEnabled","velocityNanMode", "motionEnabled", "time"
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "The color of the points."
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "The size of the points."
    };

    constexpr openspace::properties::Property::PropertyInfo IdentifierInfo = {
        "Identifier",
        "Identifier",
        "Identifier used as part of key to access data in centralized central storage."
    };
    
    constexpr openspace::properties::Property::PropertyInfo PointUnitInfo = {
        "PointUnit",
        "Point Unit",
        "The distance unit of the point data."
    };

    constexpr openspace::properties::Property::PropertyInfo SizeOptionInfo = {
        "SizeOption",
        "Size option",
        "This value determines how the size of the data points are rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo ColormapMinInfo = {
        "ColormapMin",
        "Colormap min",
        "Minimum value to sample from colormap."
    };

    constexpr openspace::properties::Property::PropertyInfo ColormapMaxInfo = {
        "ColormapMax",
        "Colormap max",
        "Maximum value to sample from colormap."
    };

    constexpr openspace::properties::Property::PropertyInfo ColormapNanModeInfo = {
        "ColormapNanMode",
        "Colormap NaN Mode",
        "How points with NaN value in colormap attribute should be represented."
    };

    constexpr openspace::properties::Property::PropertyInfo ColormapNanColorInfo = {
        "ColormapNanColor",
        "Colormap NaN Color",
        "The color of the points where the colormap scalar is NaN."
    };

    constexpr openspace::properties::Property::PropertyInfo ColormapEnabledInfo = {
        "ColormapEnabled",
        "Colormap enabled",
        "Boolean to determine whether to use colormap or not."
    };

    constexpr openspace::properties::Property::PropertyInfo LinearSizeMinInfo = {
        "LinearSizeMin",
        "Linear size min",
        "Minimum value to use for linear size."
    };

    constexpr openspace::properties::Property::PropertyInfo LinearSizeMaxInfo = {
        "LinearSizeMax",
        "Linear size max",
        "Maximum value to use for linear size."
    };

    constexpr openspace::properties::Property::PropertyInfo LinearSizeEnabledInfo = {
        "LinearSizeEnabled",
        "Linear size enabled",
        "Boolean to determine whether to use linear size or not."
    };

    constexpr openspace::properties::Property::PropertyInfo VelocityDistanceUnitInfo = {
        "VelocityDistanceUnit",
        "Velocity Distance Unit",
        "The distance unit of the velocity data."
    };

    constexpr openspace::properties::Property::PropertyInfo VelocityTimeUnitInfo = {
        "VelocityTimeUnit",
        "Velocity Time Unit",
        "The time unit of the velocity data."
    };
    
    constexpr openspace::properties::Property::PropertyInfo VelocityDateRecordedInfo = {
        "VelocityDateRecorded",
        "Velocity Date Recorded",
        "The date the velocity data was recorded."
    };
    
    constexpr openspace::properties::Property::PropertyInfo VelocityNanModeInfo = {
        "VelocityNanMode",
        "Velocity NaN Mode",
        "How points with NaN value in colormap attribute should be represented."
    };

    constexpr openspace::properties::Property::PropertyInfo MotionEnabledInfo = {
        "MotionEnabled",
        "Motion enabled",
        "Boolean to determine whether to use motion or not."
    };

    constexpr openspace::properties::Property::PropertyInfo NameInfo = {
        "Name",
        "Name",
        "The name of the points cloud"
    };

    struct [[codegen::Dictionary(RenderablePointsCloud)]] Parameters {
        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec4> color;

        // [[codegen::verbatim(SizeInfo.description)]]
        std::optional<float> size;

        // [[codegen::verbatim(IdentifierInfo.description)]]
        std::optional<std::string> identifier;

        // [[codegen::verbatim(PointUnitInfo.description)]]
        std::optional<std::string> pointUnit;

        // [[codegen::verbatim(ColormapMinInfo.description)]]
        std::optional<float> colormapMin;

        // [[codegen::verbatim(ColormapMaxInfo.description)]]
        std::optional<float> colormapMax;

        // [[codegen::verbatim(ColormapNanModeInfo.description)]]
        std::optional<int> colormapNanMode;

        // [[codegen::verbatim(ColormapNanColorInfo.description)]]
        std::optional<glm::vec4> colormapNanColor;

        // [[codegen::verbatim(ColormapEnabledInfo.description)]]
        std::optional<bool> colormapEnabled;

        // [[codegen::verbatim(LinearSizeMinInfo.description)]]
        std::optional<float> linearSizeMin;

        // [[codegen::verbatim(LinearSizeMaxInfo.description)]]
        std::optional<float> linearSizeMax;

        // [[codegen::verbatim(LinearSizeEnabledInfo.description)]]
        std::optional<bool> linearSizeEnabled;

        // [[codegen::verbatim(VelocityDistanceUnitInfo.description)]]
        std::optional<std::string> velocityDistanceUnit;

        // [[codegen::verbatim(VelocityTimeUnitInfo.description)]]
        std::optional<std::string> velocityTimeUnit;

        // [[codegen::verbatim(VelocityDateRecordedInfo.description)]]
        std::optional<glm::ivec3> velocityDateRecorded;

        // [[codegen::verbatim(VelocityNanModeInfo.description)]]
        std::optional<int> velocityNanMode;

        // [[codegen::verbatim(MotionEnabledInfo.description)]]
        std::optional<bool> motionEnabled;
        
        // [[codegen::verbatim(NameInfo.description)]]
        std::optional<std::string> name;

        // VOLATILE: Keep in sync with SizeOption in `./renderablepointscloud.h`
        enum class SizeOption {
            Uniform = 0,
            NonUniform
        };
        // [[codegen::verbatim(SizeOptionInfo.description)]]
        std::optional<SizeOption> sizeOption;
    };
#include "renderablepointscloud_codegen.cpp"
} // namespace

namespace openspace {

using namespace softwareintegration;

documentation::Documentation RenderablePointsCloud::Documentation() {
    return codegen::doc<Parameters>("softwareintegration_renderable_pointscloud");
}

RenderablePointsCloud::RenderablePointsCloud(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _color(ColorInfo, glm::vec4(glm::vec3(0.5f), 1.f), glm::vec4(0.f), glm::vec4(1.f), glm::vec4(.01f))
    , _size(SizeInfo, 1.f, 0.f, 500.f, .1f)
    , _pointUnit(PointUnitInfo, STRING_NOT_SET)
    , _sizeOption(SizeOptionInfo)
    , _colormapMin(ColormapMinInfo)
    , _colormapMax(ColormapMaxInfo)
    , _colormapNanMode(ColormapNanModeInfo)
    , _colormapNanColor(ColormapNanColorInfo, glm::vec4(glm::vec3(0.5f), 1.f), glm::vec4(1.0f), glm::vec4(0.f), glm::vec4(0.f))
    , _colormapEnabled(ColormapEnabledInfo, false)
    , _linearSizeMax(LinearSizeMinInfo)
    , _linearSizeMin(LinearSizeMaxInfo)
    , _linearSizeEnabled(LinearSizeEnabledInfo, false)
    , _velocityDistanceUnit(VelocityDistanceUnitInfo, STRING_NOT_SET)
    , _velocityTimeUnit(VelocityTimeUnitInfo, STRING_NOT_SET)
    , _velocityDateRecorded(VelocityDateRecordedInfo, glm::ivec3{ -1 })
    , _velocityNanMode(VelocityNanModeInfo)
    , _name(NameInfo)
    , _motionEnabled(MotionEnabledInfo, false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _identifier = p.identifier.value();
    
    _name = p.name.value_or(_name);
    _name.setVisibility(properties::Property::Visibility::Hidden);
    addProperty(_name);

    _color = p.color.value_or(_color);
    _color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_color);

    _size = p.size.value_or(_size);
    addProperty(_size);

    _sizeOption.addOptions({
        { SizeOption::Uniform, "Uniform" },
        { SizeOption::NonUniform, "Non uniform" }
    });
    if (p.sizeOption.has_value()) {
        switch (*p.sizeOption) {
            case Parameters::SizeOption::Uniform:
                _sizeOption = SizeOption::Uniform;
                break;
            case Parameters::SizeOption::NonUniform:
                _sizeOption = SizeOption::NonUniform;
                break;
        }
    }
    addProperty(_sizeOption);

    _pointUnit = p.pointUnit.value_or(_pointUnit);
    _pointUnit.setVisibility(properties::Property::Visibility::Hidden);
    _pointUnit.onChange([this] { _pointUnitIsDirty = true; });
    addProperty(_pointUnit);

    // =============== Colormap ===============
    _colormapMin = p.colormapMin.value_or(_colormapMin);
    _colormapMin.setVisibility(properties::Property::Visibility::Hidden);
    _colormapMin.onChange([this] { checkColormapMinMax(); });
    addProperty(_colormapMin);

    _colormapMax = p.colormapMax.value_or(_colormapMax);
    _colormapMax.setVisibility(properties::Property::Visibility::Hidden);
    _colormapMax.onChange([this] { checkColormapMinMax(); });
    addProperty(_colormapMax);

    _colormapNanMode = p.colormapNanMode.value_or(_colormapNanMode);
    _colormapNanMode.setVisibility(properties::Property::Visibility::Hidden);
    addProperty(_colormapNanMode);

    _colormapNanColor = p.colormapNanColor.value_or(_colormapNanColor);
    _colormapNanColor.setVisibility(properties::Property::Visibility::Hidden);
    addProperty(_colormapNanColor);

    _colormapEnabled = p.colormapEnabled.value_or(_colormapEnabled);
    _colormapEnabled.onChange([this] { checkIfColormapCanBeEnabled(); });
    addProperty(_colormapEnabled);

    // =============== Linear size ===============
    _linearSizeEnabled = p.linearSizeEnabled.value_or(_linearSizeEnabled);
    _linearSizeEnabled.onChange([this] { checkIfLinearSizeCanBeEnabled(); });
    addProperty(_linearSizeEnabled);

     auto linearSizeMinMaxChecker = [this] {
        if (_linearSizeMin.value() > _linearSizeMax.value()) {
            auto temp = _linearSizeMin.value();
            _linearSizeMin = _linearSizeMax.value();
            _linearSizeMax = temp;
        }
    };

    _linearSizeMin = p.linearSizeMin.value_or(_linearSizeMin);
    _linearSizeMin.setVisibility(properties::Property::Visibility::Hidden);
    _linearSizeMin.onChange(linearSizeMinMaxChecker);
    addProperty(_linearSizeMin);

    _linearSizeMax = p.linearSizeMax.value_or(_linearSizeMax);
    _linearSizeMax.setVisibility(properties::Property::Visibility::Hidden);
    _linearSizeMax.onChange(linearSizeMinMaxChecker);
    addProperty(_linearSizeMax);

    _velocityDistanceUnit = p.velocityDistanceUnit.value_or(_velocityDistanceUnit);
    _velocityDistanceUnit.setVisibility(properties::Property::Visibility::Hidden);
    _velocityDistanceUnit.onChange([this] { _velocityUnitsAreDirty = true; });
    addProperty(_velocityDistanceUnit);

    _velocityTimeUnit = p.velocityTimeUnit.value_or(_velocityTimeUnit);
    _velocityTimeUnit.setVisibility(properties::Property::Visibility::Hidden);
    _velocityTimeUnit.onChange([this] { _velocityUnitsAreDirty = true; });
    addProperty(_velocityTimeUnit);

    _velocityDateRecorded = p.velocityDateRecorded.value_or(_velocityDateRecorded);
    _velocityDateRecorded.setVisibility(properties::Property::Visibility::Hidden);
    _velocityDateRecorded.onChange([this] { updateVelocityT0(); });
    addProperty(_velocityDateRecorded);

    _velocityNanMode = p.velocityNanMode.value_or(_velocityNanMode);
    _velocityNanMode.setVisibility(properties::Property::Visibility::Hidden);
    addProperty(_velocityNanMode);

    _motionEnabled = p.motionEnabled.value_or(_motionEnabled);
    _motionEnabled.onChange([this] { checkIfMotionCanBeEnabled(); });
    addProperty(_motionEnabled);
}

bool RenderablePointsCloud::isReady() const {
    return _shaderProgram && _identifier.has_value() && _identifier.value() != "";
}

void RenderablePointsCloud::initializeGL() {
    _shaderProgram = global::renderEngine->buildRenderProgram(
        "PointsCloud",
        absPath("${MODULE_SOFTWAREINTEGRATION}/shaders/point_vs.glsl"),
        absPath("${MODULE_SOFTWAREINTEGRATION}/shaders/point_fs.glsl"),
        absPath("${MODULE_SOFTWAREINTEGRATION}/shaders/point_ge.glsl")
    );

    ghoul::opengl::updateUniformLocations(*_shaderProgram, _uniformCache, UniformNames);
}

void RenderablePointsCloud::deinitializeGL() {
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;

    _colormapTexture = nullptr;

    if (_shaderProgram) {
        global::renderEngine->removeRenderProgram(_shaderProgram.get());
        _shaderProgram = nullptr;
    }
}

void RenderablePointsCloud::render(const RenderData& data, RendererTasks&) {
    auto pointDataSlice = getDataSlice(DataSliceKey::Points);
    if (pointDataSlice->empty()) return;

    _shaderProgram->activate();

    auto eyePosition = glm::dvec3{
        glm::inverse(data.camera.combinedViewMatrix()) * glm::dvec4(0.0, 0.0, 0.0, 1.0)
    };
    _shaderProgram->setUniform(_uniformCache.eyePosition, eyePosition);
    // _shaderProgram->setUniform(_uniformCache.cameraPosition, data.camera.positionVec3());

    _shaderProgram->setUniform(
        _uniformCache.cameraUp,
        glm::dvec3(data.camera.lookUpVectorWorldSpace())
    );

    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);
    _shaderProgram->setUniform(_uniformCache.screenSize, glm::ivec2(viewport[2], viewport[3]));

    auto modelMatrix = glm::dmat4{
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), data.modelTransform.scale)
    };
    _shaderProgram->setUniform(_uniformCache.modelMatrix, modelMatrix);

    _shaderProgram->setUniform(
        _uniformCache.cameraViewProjectionMatrix,
        glm::dmat4(
            glm::dmat4(data.camera.projectionMatrix()) * data.camera.combinedViewMatrix()
        )
    );

    ghoul::opengl::TextureUnit colorUnit;
    if (_colormapTexture) {
        colorUnit.activate();
        _colormapTexture->bind();
        _shaderProgram->setUniform(_uniformCache.colormapTexture, colorUnit);
    }
    else {
        // We need to set the uniform to something, or the shader doesn't work
        _shaderProgram->setUniform(_uniformCache.colormapTexture, colorUnit);
    }

    _shaderProgram->setUniform(_uniformCache.colormapMin, _colormapMin);
    _shaderProgram->setUniform(_uniformCache.colormapMax, _colormapMax);
    _shaderProgram->setUniform(_uniformCache.colormapNanMode, _colormapNanMode);
    _shaderProgram->setUniform(_uniformCache.colormapNanColor, _colormapNanColor);
    _shaderProgram->setUniform(_uniformCache.colormapEnabled, _colormapEnabled);

    _shaderProgram->setUniform(_uniformCache.linearSizeMin, _linearSizeMin);
    _shaderProgram->setUniform(_uniformCache.linearSizeMax, _linearSizeMax);
    _shaderProgram->setUniform(_uniformCache.linearSizeEnabled, _linearSizeEnabled);

    _shaderProgram->setUniform(_uniformCache.velocityNanMode, _velocityNanMode);
    _shaderProgram->setUniform(_uniformCache.motionEnabled, _motionEnabled);
    _shaderProgram->setUniform(
        _uniformCache.time,
        static_cast<float>(data.time.j2000Seconds() - _t0)
    );

    _shaderProgram->setUniform(_uniformCache.color, _color);

    _shaderProgram->setUniform(_uniformCache.size, _size);
    _shaderProgram->setUniform(_uniformCache.sizeOption, _sizeOption);

    // Changes GL state:
    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    glDepthMask(false);

    //glEnablei(GL_BLEND, 0);

    //if (_useAdditiveBlending) {
    //    glDepthMask(false);
    //    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    //}
    //else {
    //    // Normal blending, with transparency
    //    glDepthMask(true);
    //    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    //}

    glBindVertexArray(_vao);
    const GLsizei nPoints = static_cast<GLsizei>(pointDataSlice->size() / 3);
    glDrawArrays(GL_POINTS, 0, nPoints);

    glBindVertexArray(0);
    _shaderProgram->deactivate();

    // Restores GL State
    glDepthMask(GL_TRUE);
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
}

void RenderablePointsCloud::update(const UpdateData&) {
    if (_shaderProgram->isDirty()) {
        _shaderProgram->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_shaderProgram, _uniformCache, UniformNames);
    }

    bool updatedDataSlices = checkDataStorage();

    if (updatedDataSlices) {
        if (_vao == 0) {
            glGenVertexArrays(1, &_vao);
            LDEBUG(std::format("Generating Vertex Array id '{}'", _vao));
        }
        if (_vbo == 0) {
            glGenBuffers(1, &_vbo);
            LDEBUG(std::format("Generating Vertex Buffer Object id '{}'", _vbo));
        }

        glBindVertexArray(_vao);
        glBindBuffer(GL_ARRAY_BUFFER, _vbo);

        auto pointDataSlice = getDataSlice(DataSliceKey::Points);
        auto colormapAttrDataSlice = getDataSlice(DataSliceKey::ColormapAttributes);
        auto linearSizeAttrDataSlice = getDataSlice(DataSliceKey::LinearSizeAttributes);
        auto velocityDataSlice = getDataSlice(DataSliceKey::Velocity);

        if (pointDataSlice->empty()) return;

        // ========================== Create resulting data slice and buffer it ==========================
        std::vector<float> bufferData;
        bufferData.reserve(pointDataSlice->size() / 3);

        for(size_t i = 0, j = 0; j < pointDataSlice->size(); ++i, j += 3) {
            bufferData.push_back(pointDataSlice->at(j));
            bufferData.push_back(pointDataSlice->at(j + 1));
            bufferData.push_back(pointDataSlice->at(j + 2));

            if (colormapAttrDataSlice->size() > i) {
                bufferData.push_back(colormapAttrDataSlice->at(i));
            }
            else {
                bufferData.push_back(std::nanf("0"));
            }

            if (linearSizeAttrDataSlice->size() > i) {
                bufferData.push_back(linearSizeAttrDataSlice->at(i));
            }
            else {
                bufferData.push_back(std::nanf("0"));
            }

            if (velocityDataSlice->size() > (j + 2)) {
                bufferData.push_back(velocityDataSlice->at(j));
                bufferData.push_back(velocityDataSlice->at(j + 1));
                bufferData.push_back(velocityDataSlice->at(j + 2));
            }
            else {
                bufferData.push_back(std::nanf("0"));
                bufferData.push_back(std::nanf("0"));
                bufferData.push_back(std::nanf("0"));
            }
        }

        glBufferData(
            GL_ARRAY_BUFFER,
            bufferData.size() * sizeof(GLfloat),
            bufferData.data(),
            GL_STATIC_DRAW
        );
        // ==============================================================================================

        // ========================================= VAO stuff =========================================
        GLsizei stride = static_cast<GLsizei>(sizeof(GLfloat) * _nValuesForVAOStride);
        GLint positionAttribute = _shaderProgram->attributeLocation("in_position");
        glEnableVertexAttribArray(positionAttribute);
        glVertexAttribPointer(
            positionAttribute,
            3,
            GL_FLOAT,
            GL_FALSE,
            stride,
            nullptr
        );

        if (_hasLoadedColormapAttributeData) { 
            GLint colormapScalarsAttribute = _shaderProgram->attributeLocation("in_colormapAttributeScalar");
            glEnableVertexAttribArray(colormapScalarsAttribute);
            glVertexAttribPointer(
                colormapScalarsAttribute,
                1,
                GL_FLOAT,
                GL_FALSE,
                stride,
                reinterpret_cast<void*>(sizeof(GLfloat) * 3)
            );
        }

        if (_hasLoadedLinearSizeAttributeData) { 
            GLint linearSizeAttributeScalar = _shaderProgram->attributeLocation("in_linearSizeAttributeScalar");
            glEnableVertexAttribArray(linearSizeAttributeScalar);
            glVertexAttribPointer(
                linearSizeAttributeScalar,
                1,
                GL_FLOAT,
                GL_FALSE,
                stride,
                reinterpret_cast<void*>(sizeof(GLfloat) * 4)
            );
        }

        if (_hasLoadedVelocityData) { 
            GLint velocityAttribute = _shaderProgram->attributeLocation("in_velocity");
            glEnableVertexAttribArray(velocityAttribute);
            glVertexAttribPointer(
                velocityAttribute,
                3,
                GL_FLOAT,
                GL_FALSE,
                stride,
                reinterpret_cast<void*>(sizeof(GLfloat) * 5)
            );
        }
        // ==============================================================================================

        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);
    }
}

bool RenderablePointsCloud::checkDataStorage() {
    if (!_identifier.has_value()) {
        LERROR("No identifier found in renderable");
        return false;
    }

    bool updatedDataSlices = false;
    auto softwareIntegrationModule = global::moduleEngine->module<SoftwareIntegrationModule>();

    if (shouldLoadPointData(softwareIntegrationModule)) {
        loadPointData(softwareIntegrationModule);
        updatedDataSlices = true;
        _pointUnitIsDirty = false;
    }

    if (shouldLoadColormap(softwareIntegrationModule)) {
        loadColormap(softwareIntegrationModule);
    }

    if (shouldLoadColormapAttrData(softwareIntegrationModule)) {
        loadColormapAttributeData(softwareIntegrationModule);
        updatedDataSlices = true;
    }

    if (shouldLoadLinearSizeAttrData(softwareIntegrationModule)) {
        loadLinearSizeAttributeData(softwareIntegrationModule);
        updatedDataSlices = true;
    }

    if (shouldLoadVelocityData(softwareIntegrationModule)) {
        loadVelocityData(softwareIntegrationModule);
        updatedDataSlices = true;
        _velocityUnitsAreDirty = false;
    }

    return updatedDataSlices;
}

void RenderablePointsCloud::loadPointData(SoftwareIntegrationModule* softwareIntegrationModule) {
    // Fetch point data from module's centralized storage
    std::vector<float> pointData; 
    if (!softwareIntegrationModule->fetchData(_identifier.value(), storage::Key::DataPoints, pointData)) {
        LERROR("There was an issue trying to fetch the point data from the centralized storage.");
        return;
    }

    // Parse unit
    DistanceUnit pointUnit;
    try {
        pointUnit = distanceUnitFromString(_pointUnit.value().c_str());
    }
    catch (const ghoul::MissingCaseException& ) {

        std::string err = std::format(
            "Error when parsing point unit."
            "OpenSpace doesn't support the distance unit.");
        LERROR(err);
        return;
    }

    // Convert to meters if needed
    int nNans = 0;
    if (pointUnit != DistanceUnit::Meter) {
        float toMeters = static_cast<float>(toMeter(pointUnit));
        for (auto& value : pointData) {
            if (isnan(value)) {
                nNans++;
                continue;
            }
            value *= toMeters;
        }
    }
    
    // Assign point data to point data slice
    auto pointDataSlice = getDataSlice(DataSliceKey::Points);
    pointDataSlice->clear();
    pointDataSlice->assign(pointData.begin(), pointData.end());

    softwareIntegrationModule->setDataLoaded(_identifier.value(), storage::Key::DataPoints);
    LINFO(std::format(
        "New point data ({} points) has loaded. {} values are NaN values. "
        "Point's with at least one NaN value are hidden.", 
        (pointData.size() / 3), nNans
    ));

    //compute/create centroid
    std::vector<float> centroid(3);
    for (int i = 0; i < pointData.size(); i += 3) {
        centroid[0] += pointData[i];
        centroid[1] += pointData[i + 1];
        centroid[2] += pointData[i + 2];
    }
    int pointSize = pointData.size();
    centroid[0] /= pointSize / 3;
    centroid[1] /= pointSize / 3;
    centroid[2] /= pointSize / 3;

    LDEBUG(std::format("Centroid '{} {} {}'", centroid[0], centroid[1], centroid[2]));


}

void RenderablePointsCloud::loadVelocityData(SoftwareIntegrationModule* softwareIntegrationModule) {
    // Fetch velocity data from module's centralized storage
    std::vector<float> velocityData;
    if (!softwareIntegrationModule->fetchData(_identifier.value(), storage::Key::VelocityData, velocityData)) {
        LWARNING("There was an issue trying to fetch the velocity data from the centralized storage.");
        return;
    }

    // Check that velocity data is same length as point data
    auto pointDataSlice = getDataSlice(DataSliceKey::Points);
    // if (pointDataSlice->size() != velocityData.size()) {
    //     LWARNING(std::format(
    //         "There is a mismatch in the amount of velocity data ({}) and the amount of points ({})",
    //         velocityData.size() / 3, pointDataSlice->size() / 3
    //     ));
    //     _motionEnabled = false;
    //     return;
    // }

    // Parse units
    DistanceUnit velocityDistanceUnit;
    TimeUnit velocityTimeUnit;
    try {
        velocityDistanceUnit = distanceUnitFromString(_velocityDistanceUnit.value().c_str());
        velocityTimeUnit = timeUnitFromString(_velocityTimeUnit.value().c_str());
    }
    catch (const ghoul::MissingCaseException& ) {
        LERROR(
            "Error when parsing velocity units. OpenSpace doesn't support the velocity unit ."
        );
        return;
    }

    // Units conversion
    bool conversionNeeded = false;

    // Meters multiplier
    float toMeters = 1.0;
    if (velocityDistanceUnit != DistanceUnit::Meter) {
        toMeters = static_cast<float>(toMeter(velocityDistanceUnit));
        conversionNeeded = true;
    }

    // Seconds divider
    float toSeconds = 1.0;
    if (velocityTimeUnit != TimeUnit::Second) {
        toSeconds = static_cast<float>(convertTime(1.0, velocityTimeUnit, TimeUnit::Second));
        conversionNeeded = true;
    }

    // Check for NaN values and convert to m/s if needed
    int nNans = 0;
    for (auto& v : velocityData) {
        if (isnan(v)) {
            nNans++;
            continue;
        }
        if (conversionNeeded) {
            v *= toMeters / toSeconds;
        }
    }

    // Assign velocity data to velocity data slice
    auto velocityDataSlice = getDataSlice(DataSliceKey::Velocity);
    velocityDataSlice->clear();
    velocityDataSlice->assign(velocityData.begin(), velocityData.end());
    
    LINFO(
        std::format(
            "Viewing {} points with velocity ({} points in total). "
            "{} points have NaN-value velocity and are hidden or static.",
            velocityData.size()/3 - nNans/3,
            pointDataSlice->size()/3,
            nNans/3
        )
    );

    _hasLoadedVelocityData = true;
    softwareIntegrationModule->setDataLoaded(_identifier.value(), storage::Key::VelocityData);
    LDEBUG("New velocity data has loaded");
}

void RenderablePointsCloud::loadColormap(SoftwareIntegrationModule* softwareIntegrationModule) {
    // Fetch colormap data from module's centralized storage
    std::vector<float> colormap;
    if (!softwareIntegrationModule->fetchData(_identifier.value(), storage::Key::Colormap, colormap)) {
        LWARNING("There was an issue trying to fetch the colormap data from the centralized storage.");
        return;
    }

    size_t nValues = colormap.size();
    uint8_t* values = new uint8_t[nValues];

    for (size_t i = 0; i < nValues; ++i) {
        values[i] = static_cast<uint8_t>(colormap[i] * 255);
    }

    _colormapTexture = nullptr;
    _colormapTexture = std::make_unique<ghoul::opengl::Texture>(
        values,
        glm::size3_t(nValues / 4, 1, 1),
        GL_TEXTURE_1D,
        ghoul::opengl::Texture::Format::RGBA
    );
    _colormapTexture->uploadTexture();

    _hasLoadedColormap = true;
    LDEBUG("New colormap has loaded");
    softwareIntegrationModule->setDataLoaded(_identifier.value(), storage::Key::Colormap);
}

void RenderablePointsCloud::loadColormapAttributeData(SoftwareIntegrationModule* softwareIntegrationModule) {
    // Fetch colormap attribute data from module's centralized storage
    std::vector<float> colormapAttributeData;
    if (!softwareIntegrationModule->fetchData(_identifier.value(), storage::Key::ColormapAttrData, colormapAttributeData)) {
        LWARNING("There was an issue trying to fetch the colormap attribute data from the centralized storage.");
        return;
    }

    // Check that colormap attribute data is same length as point data
    auto pointDataSlice = getDataSlice(DataSliceKey::Points);
    // if (pointDataSlice->size() / 3 != colormapAttributeData.size()) {
    //     LWARNING(std::format(
    //         "There is a mismatch in the amount of colormap attribute scalars ({}) and the amount of points ({})",
    //         colormapAttributeData.size(), pointDataSlice->size() / 3
    //     ));
    //     _colormapEnabled = false;
    //     return;
    // }

    auto colormapAttributeDataSlice = getDataSlice(DataSliceKey::ColormapAttributes);
    colormapAttributeDataSlice->clear();
    colormapAttributeDataSlice->assign(colormapAttributeData.begin(), colormapAttributeData.end());

    _hasLoadedColormapAttributeData = true;
    softwareIntegrationModule->setDataLoaded(_identifier.value(), storage::Key::ColormapAttrData);
    LDEBUG("New colormap attribute data has loaded");
}

void RenderablePointsCloud::loadLinearSizeAttributeData(SoftwareIntegrationModule* softwareIntegrationModule) {
    // Fetch linear size attribute data from module's centralized storage
    std::vector<float> linearSizeAttributeData;
    if (softwareIntegrationModule->fetchData(_identifier.value(), storage::Key::LinearSizeAttrData, linearSizeAttributeData)) {
        LWARNING("There was an issue trying to fetch the linear size attribute data from the centralized storage.");
        return;
    }

    // Check that linear size attribute data is same length as point data
    auto pointDataSlice = getDataSlice(DataSliceKey::Points);
     //if (pointDataSlice->size() / 3 != linearSizeAttributeData.size()) {
     //    LWARNING(std::format(
     //        "There is a mismatch in the amount of linear size attribute scalars ({}) and the amount of points ({})",
     //        linearSizeAttributeData.size(), pointDataSlice->size() / 3
     //    ));
     //    _linearSizeEnabled = false;
     //    return;
     //}

    auto linearSizeAttributeDataSlice = getDataSlice(DataSliceKey::LinearSizeAttributes);
    linearSizeAttributeDataSlice->clear();
    linearSizeAttributeDataSlice->assign(linearSizeAttributeData.begin(), linearSizeAttributeData.end());

    _hasLoadedLinearSizeAttributeData = true;
    softwareIntegrationModule->setDataLoaded(_identifier.value(), storage::Key::LinearSizeAttrData);
    LDEBUG("New SIZE attribute data has loaded");
}

std::shared_ptr<RenderablePointsCloud::DataSlice> RenderablePointsCloud::getDataSlice(DataSliceKey key) {
    if (!_dataSlices.count(key)) {
        _dataSlices.insert({ key, std::make_shared<DataSlice>() });
    }
    return _dataSlices.at(key);
}

void RenderablePointsCloud::checkIfColormapCanBeEnabled() {
    if (!global::windowDelegate->isMaster()) return;

    // This can happen if the user checks the "ColormapEnabled" checkbox in the GUI
    auto colormapAttributeData = getDataSlice(DataSliceKey::ColormapAttributes);
    if (_colormapEnabled.value()) {
        if ((!_colormapTexture || colormapAttributeData->empty())) {
            if (!_colormapTexture) {
                LINFO("Colormap not loaded. Has it been sent from external software?");
            }
            if (colormapAttributeData->empty()) {
                LINFO("Colormap attribute data not loaded. Has it been sent from external software?");
            }

            global::scriptEngine->queueScript(
                std::format(
                    "openspace.setPropertyValueSingle('Scene.{}.Renderable.ColormapEnabled', {});",
                    _identifier.value(), "false"
                )
            );
        }
    }
}

void RenderablePointsCloud::checkIfLinearSizeCanBeEnabled() {
    if (!global::windowDelegate->isMaster()) return;

    // This can happen if the user checks the "LinearSizeEnabled" checkbox in the GUI
    auto linearSizeAttributeData = getDataSlice(DataSliceKey::LinearSizeAttributes);
    if (_linearSizeEnabled.value() && linearSizeAttributeData->empty()) {
        LINFO("Linear size attribute data not loaded. Has it been sent from external software?");
        global::scriptEngine->queueScript(
            std::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.LinearSizeEnabled', {});",
                _identifier.value(), "false"
            )
        );
    }
}

void RenderablePointsCloud::checkIfMotionCanBeEnabled() {
    if (!global::windowDelegate->isMaster()) return;

    // This can happen if the user checks the "MotionEnabled" checkbox in the GUI
    auto velocityData = getDataSlice(DataSliceKey::Velocity);
    if (_motionEnabled.value() && velocityData->empty()) {
        LINFO("Velocity data not loaded. Has it been sent from external software?");
        global::scriptEngine->queueScript(
            std::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.MotionEnabled', {});",
                _identifier.value(), "false"
            )
        );
    }
}

void RenderablePointsCloud::checkColormapMinMax() {


    float min = _colormapMin;
    float max = _colormapMax;

    if (min > max) {
        float temp = min;
        _colormapMin = max;
        _colormapMax = temp;
    }
}

bool RenderablePointsCloud::shouldLoadPointData(SoftwareIntegrationModule* softwareIntegrationModule) {
    return (
        (
            _pointUnitIsDirty
            || softwareIntegrationModule->isDataDirty(_identifier.value(), storage::Key::DataPoints)
        )
        && _pointUnit.value() != STRING_NOT_SET
    );
}

bool RenderablePointsCloud::shouldLoadVelocityData(SoftwareIntegrationModule* softwareIntegrationModule) {
    return (
        (
            _velocityUnitsAreDirty
            || softwareIntegrationModule->isDataDirty(_identifier.value(), storage::Key::VelocityData)
        ) 
        && _velocityDistanceUnit.value() != STRING_NOT_SET
        && _velocityTimeUnit.value() != STRING_NOT_SET
    );
}

bool RenderablePointsCloud::shouldLoadColormap(SoftwareIntegrationModule* softwareIntegrationModule) {
    return softwareIntegrationModule->isDataDirty(_identifier.value(), storage::Key::Colormap);
}

bool RenderablePointsCloud::shouldLoadColormapAttrData(SoftwareIntegrationModule* softwareIntegrationModule) {
    return softwareIntegrationModule->isDataDirty(_identifier.value(), storage::Key::ColormapAttrData);
}

bool RenderablePointsCloud::shouldLoadLinearSizeAttrData(SoftwareIntegrationModule* softwareIntegrationModule) {
    return softwareIntegrationModule->isDataDirty(_identifier.value(), storage::Key::LinearSizeAttrData);
}

void RenderablePointsCloud::updateVelocityT0() {
    _t0 = Time::convertTime(simp::toDateString(_velocityDateRecorded));
}

} // namespace openspace
