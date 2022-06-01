/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/softwareintegration/utils.h>
#include <modules/softwareintegration/softwareintegrationmodule.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <fstream>

namespace {
    constexpr const char* _loggerCat = "PointsCloud";

    constexpr const std::array<const char*, 16> UniformNames = {
        "color", "opacity", "size", "modelMatrix", "cameraUp", "screenSize",
        "cameraViewProjectionMatrix", "eyePosition", "sizeOption", "colormapTexture",
        "colormapMin", "colormapMax", "colormapEnabled", "linearSizeMin",
        "linearSizeMax", "linearSizeEnabled"
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

    constexpr openspace::properties::Property::PropertyInfo DataInfo = {
        "Data",
        "Data",
        "Data to use for the positions of the points, given in Parsec."
    };

    constexpr openspace::properties::Property::PropertyInfo IdentifierInfo = {
        "Identifier",
        "Identifier",
        "Identifier used as part of key to access data in centralized central storage."
    };

    constexpr openspace::properties::Property::PropertyInfo SizeOptionInfo = {
        "SizeOption",
        "Size option",
        "This value determines how the size of the data points are rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo ColormapMinInfo = {
        "ColormapMin",
        "Colormap min",
        "Minimum value to sample from color map."
    };

    constexpr openspace::properties::Property::PropertyInfo ColormapMaxInfo = {
        "ColormapMax",
        "Colormap max",
        "Maximum value to sample from color map."
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

    struct [[codegen::Dictionary(RenderablePointsCloud)]] Parameters {
        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec4> color;

        // [[codegen::verbatim(SizeInfo.description)]]
        std::optional<float> size;

        // [[codegen::verbatim(DataInfo.description)]]
        std::optional<std::vector<glm::vec3>> data;

        // [[codegen::verbatim(IdentifierInfo.description)]]
        std::optional<std::string> identifier;

        // [[codegen::verbatim(ColormapMinInfo.description)]]
        std::optional<float> colormapMin;

        // [[codegen::verbatim(ColormapMaxInfo.description)]]
        std::optional<float> colormapMax;

        // [[codegen::verbatim(ColormapEnabledInfo.description)]]
        std::optional<bool> colormapEnabled;

        // [[codegen::verbatim(LinearSizeMinInfo.description)]]
        std::optional<float> linearSizeMin;

        // [[codegen::verbatim(LinearSizeMaxInfo.description)]]
        std::optional<float> linearSizeMax;

        // [[codegen::verbatim(LinearSizeEnabledInfo.description)]]
        std::optional<bool> linearSizeEnabled;

        enum class SizeOption : uint32_t {
            Uniform,
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
    , _sizeOption(SizeOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _colormapEnabled(ColormapEnabledInfo, false)
    , _colormapMin(ColormapMinInfo)
    , _colormapMax(ColormapMaxInfo)
    , _linearSizeMax(LinearSizeMinInfo)
    , _linearSizeMin(LinearSizeMaxInfo)
    , _linearSizeEnabled(LinearSizeEnabledInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _color = p.color.value_or(_color);
    _color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_color);

    _colormapEnabled = p.colormapEnabled.value_or(_colormapEnabled);
    _colormapEnabled.onChange([this] { checkIfColormapCanBeEnabled(); });
    addProperty(_colormapEnabled);

    _identifier = p.identifier.value();

    _size = p.size.value_or(_size);
    addProperty(_size);

    _linearSizeEnabled = p.linearSizeEnabled.value_or(_linearSizeEnabled);
    _linearSizeEnabled.onChange([this] { checkIfLinearSizeCanBeEnabled(); });
    addProperty(_linearSizeEnabled);

    addProperty(_opacity);

    _colormapMin = p.colormapMin.value_or(_colormapMin);
    _colormapMin.setVisibility(properties::Property::Visibility::Hidden);
    _colormapMin.onChange([this] { checkColormapMinMax(); });
    addProperty(_colormapMin);

    _colormapMax = p.colormapMax.value_or(_colormapMax);
    _colormapMax.setVisibility(properties::Property::Visibility::Hidden);
    _colormapMax.onChange([this] { checkColormapMinMax(); });
    addProperty(_colormapMax);

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
        // _colormapAttributeData
        // TODO: Set _colormapTextre in shader. A trasnfer function similar to
        // 'bv2rgb' in C:\OpenSpace\SoftwareIntegration\modules\space\shaders\star_fs.glsl
        // should probably be used.

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
    _shaderProgram->setUniform(_uniformCache.colormapEnabled, _colormapEnabled);

    _shaderProgram->setUniform(_uniformCache.linearSizeMin, _linearSizeMin);
    _shaderProgram->setUniform(_uniformCache.linearSizeMax, _linearSizeMax);
    _shaderProgram->setUniform(_uniformCache.linearSizeEnabled, _linearSizeEnabled);

    _shaderProgram->setUniform(_uniformCache.color, _color);

    _shaderProgram->setUniform(_uniformCache.opacity, _opacity);
    _shaderProgram->setUniform(_uniformCache.size, _size);
    _shaderProgram->setUniform(_uniformCache.sizeOption, _sizeOption);

    // Changes GL state:
    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDepthMask(GL_FALSE);

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
            LDEBUG(fmt::format("Generating Vertex Array id '{}'", _vao));
        }
        if (_vbo == 0) {
            glGenBuffers(1, &_vbo);
            LDEBUG(fmt::format("Generating Vertex Buffer Object id '{}'", _vbo));
        }

        glBindVertexArray(_vao);
        glBindBuffer(GL_ARRAY_BUFFER, _vbo);

        auto pointDataSlice = getDataSlice(DataSliceKey::Points);
        auto colormapAttrDataSlice = getDataSlice(DataSliceKey::ColormapAttributes);
        auto linearSizeAttrDataSlice = getDataSlice(DataSliceKey::LinearSizeAttributes);

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
                bufferData.push_back(0.0);
            }

            if (linearSizeAttrDataSlice->size() > i) {
                bufferData.push_back(linearSizeAttrDataSlice->at(i));
            }
            else {
                bufferData.push_back(0.0);
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
        GLsizei stride = static_cast<GLsizei>(sizeof(GLfloat) * 5);
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

    if (softwareIntegrationModule->isDataDirty(_identifier.value(), storage::Key::DataPoints)) {
        loadData(softwareIntegrationModule);
        updatedDataSlices = true;
    }
    
    if (softwareIntegrationModule->isDataDirty(_identifier.value(), storage::Key::Colormap)) {
        loadColormap(softwareIntegrationModule);
    }
    
    if (softwareIntegrationModule->isDataDirty(_identifier.value(), storage::Key::ColormapAttrData)) {
        loadCmapAttributeData(softwareIntegrationModule);
        updatedDataSlices = true;
    }
    
    if (softwareIntegrationModule->isDataDirty(_identifier.value(), storage::Key::LinearSizeAttrData)) {
        loadLinearSizeAttributeData(softwareIntegrationModule);
        updatedDataSlices = true;
    }

    return updatedDataSlices;
}

void RenderablePointsCloud::loadData(SoftwareIntegrationModule* softwareIntegrationModule) {
    // Fetch data from module's centralized storage
    auto fullPointData = softwareIntegrationModule->fetchData(_identifier.value(), storage::Key::DataPoints);

    if (fullPointData.empty()) {
        LWARNING("There was an issue trying to fetch the point data from the centralized storage.");
        return;
    }

    auto pointDataSlice = getDataSlice(DataSliceKey::Points);
    pointDataSlice->clear();
    pointDataSlice->reserve(fullPointData.size() * 3);

    // Create data slice
    auto addPosition = [&](const glm::vec4& pos) {
        for (glm::vec4::length_type j = 0; j < glm::vec4::length() - 1; ++j) {
            pointDataSlice->push_back(pos[j]);
        }
    };

    for (size_t i = 0; i < fullPointData.size(); i += 3) {
        glm::dvec4 transformedPos = {
            fullPointData[i + 0],
            fullPointData[i + 1],
            fullPointData[i + 2],
            1.0
        };
        // W-normalization
        transformedPos /= transformedPos.w;
        transformedPos *= distanceconstants::Parsec;

        addPosition(transformedPos);
    }
}

void RenderablePointsCloud::loadColormap(SoftwareIntegrationModule* softwareIntegrationModule) {
    auto colorMap = softwareIntegrationModule->fetchData(_identifier.value(), storage::Key::Colormap);
    
    if (colorMap.empty()) {
        LWARNING("There was an issue trying to fetch the colormap data from the centralized storage.");
        return;
    }

    size_t nValues = colorMap.size();
    uint8_t* values = new uint8_t[nValues];

    for (size_t i = 0; i < nValues; ++i) {
        values[i] = static_cast<uint8_t>(colorMap[i] * 255);
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
}

void RenderablePointsCloud::loadCmapAttributeData(SoftwareIntegrationModule* softwareIntegrationModule) {
    auto colormapAttributeData = softwareIntegrationModule->fetchData(_identifier.value(), storage::Key::ColormapAttrData);
    
    if (colormapAttributeData.empty()) {
        LWARNING("There was an issue trying to fetch the colormap data from the centralized storage.");
        return;
    }

    auto pointDataSlice = getDataSlice(DataSliceKey::Points);

    if (pointDataSlice->size() / 3 != colormapAttributeData.size()) {
        LWARNING(fmt::format(
            "There is a mismatch in the amount of colormap attribute scalars ({}) and the amount of points ({})",
            colormapAttributeData.size(), pointDataSlice->size() / 3
        ));
        _colormapEnabled = false;
        return;
    }

    auto colormapAttributeDataSlice = getDataSlice(DataSliceKey::ColormapAttributes);
    colormapAttributeDataSlice->clear();
    colormapAttributeDataSlice->reserve(colormapAttributeData.size());

    for (size_t i = 0; i < (pointDataSlice->size() / 3); ++i) {
        colormapAttributeDataSlice->push_back(colormapAttributeData[i]);
    }

    _hasLoadedColormapAttributeData = true;
    LDEBUG("Rerendering colormap attribute data");
}

void RenderablePointsCloud::loadLinearSizeAttributeData(SoftwareIntegrationModule* softwareIntegrationModule) {
    auto linearSizeAttributeData = softwareIntegrationModule->fetchData(_identifier.value(), storage::Key::LinearSizeAttrData);
    
    if (linearSizeAttributeData.empty()) {
        LWARNING("There was an issue trying to fetch the linear size attribute data from the centralized storage.");
        return;
    }

    auto pointDataSlice = getDataSlice(DataSliceKey::Points);

    if (pointDataSlice->size() / 3 != linearSizeAttributeData.size()) {
        LWARNING(fmt::format(
            "There is a mismatch in the amount of linear size attribute scalars ({}) and the amount of points ({})",
            linearSizeAttributeData.size(), pointDataSlice->size() / 3
        ));
        _colormapEnabled = false;
        return;
    }

    auto linearSizeAttributeDataSlice = getDataSlice(DataSliceKey::LinearSizeAttributes);
    linearSizeAttributeDataSlice->clear();
    linearSizeAttributeDataSlice->reserve(linearSizeAttributeData.size());

    for (size_t i = 0; i < (pointDataSlice->size() / 3); ++i) {
        linearSizeAttributeDataSlice->push_back(linearSizeAttributeData[i]);
    }

    _hasLoadedLinearSizeAttributeData = true;
     LDEBUG("Rerendering linear size attribute data");
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
                LINFO("Color map not loaded. Has it been sent from external software?");
            }
            if (colormapAttributeData->empty()) {
                LINFO("Color map attribute data not loaded. Has it been sent from external software?");
            }

            global::scriptEngine->queueScript(
                fmt::format(
                    "openspace.setPropertyValueSingle('Scene.{}.Renderable.ColormapEnabled', {});",
                    _identifier.value(), "false"
                ),
                scripting::ScriptEngine::RemoteScripting::Yes
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
            fmt::format(
                "openspace.setPropertyValueSingle('Scene.{}.Renderable.LinearSizeEnabled', {});",
                _identifier.value(), "false"
            ),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
}

void RenderablePointsCloud::checkColormapMinMax() {
    float min = std::any_cast<float>(_colormapMin.get());
    float max = std::any_cast<float>(_colormapMax.get());

    if (min > max) {
        float temp = min;
        _colormapMin = max;
        _colormapMax = temp;
    }
}

} // namespace openspace
