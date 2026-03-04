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

#include <modules/fieldlines/rendering/renderablefieldlines.h>

#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/opengl/programobject.h>
#include <filesystem>
#include <fstream>
#include <optional>
#include <sstream>
#include <utility>

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "RenderableFieldlines";

    constexpr float DefaultFieldlineStepSize = 0.5f;
    const glm::vec4 DefaultFieldlineColor = glm::vec4(1.f, 0.f, 0.f, 1.f);

    constexpr std::string_view VectorFieldKameleonVariableLorentz = "Lorentz";

    enum class SeedPointSource {
        File = 0,
        Table = 1
    };

    constexpr Property::PropertyInfo StepSizeInfo = {
        "Stepsize",
        "Fieldline step size.",
        "", // @TODO Missing documentation
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo Classification = {
        "Classification",
        "Fieldline classification",
        "", // @TODO Missing documentation
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo FieldlineColorInfo = {
        "FieldlineColor",
        "Fieldline color",
        "", // @TODO Missing documentation
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo SeedPointSourceInfo = {
        "Source",
        "SeedPoint source",
        "", // @TODO Missing documentation
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo SeedPointFileInfo = {
        "SourceFile",
        "SeedPoint file",
        "", // @TODO Missing documentation
        Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableFieldlines)]] Parameters {
        struct Vectorfield {
            enum class VectorfieldType {
                VolumeKameleon
            };
            VectorfieldType type;

            enum class Model {
                BATSRUS
            };
            Model model;

            std::filesystem::path file;
            std::vector<std::string> variables;

        };
        Vectorfield vectorField;

        struct Fieldlines {
            std::optional<float> stepsize;
            std::optional<bool> classification;
            std::optional<glm::vec4> color [[codegen::color()]];
        };
        Fieldlines fieldlines;

        struct SeedPoints {
            enum class SourceType {
                File,
                Table
            };
            std::optional<SourceType> type;
            std::optional<std::filesystem::path> file;

            std::optional<std::vector<glm::vec3>> seedPoints;
        };
        SeedPoints seedPoints;
    };
} // namespace
#include "renderablefieldlines_codegen.cpp"

namespace openspace {

RenderableFieldlines::RenderableFieldlines(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _stepSize(StepSizeInfo, DefaultFieldlineStepSize, 0.f, 10.f)
    , _classification(Classification, true)
    , _fieldlineColor(
        FieldlineColorInfo,
        DefaultFieldlineColor,
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _seedPointSource(SeedPointSourceInfo)
    , _seedPointSourceFile(SeedPointFileInfo)
{
    std::string identifier = dictionary.value<std::string>(SceneGraphNode::KeyIdentifier);
    setIdentifier(identifier);

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _stepSize = p.fieldlines.stepsize.value_or(_stepSize);
    _classification = p.fieldlines.classification.value_or(_classification);
    _fieldlineColor = p.fieldlines.color.value_or(_fieldlineColor);
    _file = p.vectorField.file;
    _variables = p.vectorField.variables;

    // @TODO a non-magic number perhaps ---abock
    setBoundingSphere(250.f * 6371000.f);

    _seedPointSource.addOption(static_cast<int>(SeedPointSource::File), "File");
    _seedPointSource.addOption(static_cast<int>(SeedPointSource::Table), "Lua Table");

    if (p.seedPoints.type.has_value()) {
        switch (*p.seedPoints.type) {
            case Parameters::SeedPoints::SourceType::File:
                _seedPointSource = static_cast<int>(SeedPointSource::File);
                if (!p.seedPoints.file.has_value()) {
                    throw ghoul::RuntimeError("Missing key 'File'");
                }
                _seedPointSourceFile = p.seedPoints.file->string();
                break;
            case Parameters::SeedPoints::SourceType::Table:
                _seedPointSource = static_cast<int>(SeedPointSource::Table);
                break;
        }
    }

    _seedPointsTable = p.seedPoints.seedPoints;

    // @TODO hook up visibility changes ---abock
    _stepSize.onChange([this]() { _fieldLinesAreDirty = true; });
    addProperty(_stepSize);

    addProperty(_classification);

    _fieldlineColor.setViewOption(Property::ViewOptions::Color);
    addProperty(_fieldlineColor);

    auto dirtySeedpoints = [this]() { _seedPointsAreDirty = true; };
    _seedPointSource.onChange(dirtySeedpoints);
    addProperty(_seedPointSource);

    _seedPointSourceFile.onChange(dirtySeedpoints);
    addProperty(_seedPointSourceFile);
}

bool RenderableFieldlines::isReady() const {
    return _program != nullptr;
}

void RenderableFieldlines::initializeGL() {
    _program = global::renderEngine->buildRenderProgram(
        "Fieldline",
        absPath("${MODULE_FIELDLINES}/shaders/fieldline_vs.glsl"),
        absPath("${MODULE_FIELDLINES}/shaders/fieldline_fs.glsl"),
        absPath("${MODULE_FIELDLINES}/shaders/fieldline_gs.glsl")
    );

    glCreateVertexArrays(1, &_vao);

    GLuint vertexLocation = 0;
    glEnableVertexArrayAttrib(_vao, vertexLocation);
    glVertexArrayAttribFormat(_vao, vertexLocation, 3, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(_vao, vertexLocation, 0);

    GLuint colorLocation = 1;
    glEnableVertexArrayAttrib(_vao, colorLocation);
    glVertexArrayAttribFormat(
        _vao,
        colorLocation,
        4,
        GL_FLOAT,
        GL_FALSE,
        offsetof(LinePoint, color)
    );
    glVertexArrayAttribBinding(_vao, colorLocation, 0);
}

void RenderableFieldlines::deinitializeGL() {
    glDeleteVertexArrays(1, &_vao);
    glDeleteBuffers(1, &_vbo);

    if (_program) {
        global::renderEngine->removeRenderProgram(_program.get());
        _program = nullptr;
    }
}

void RenderableFieldlines::render(const RenderData& data, RendererTasks&) {
    _program->activate();
    _program->setUniform("modelViewProjection", data.camera.viewProjectionMatrix());
    _program->setUniform("modelTransform", glm::mat4(1.0));
    _program->setUniform(
        "cameraViewDir",
        glm::vec3(data.camera.viewDirectionWorldSpace())
    );
    glDisable(GL_CULL_FACE);
    _program->setUniform("campos", glm::vec4(data.camera.position(), 1.f));
    _program->setUniform("objpos", glm::vec4(data.modelTransform.translation, 0.f));
    _program->setUniform("camrot", glm::mat4(data.camera.viewRotationMatrix()));
    _program->setUniform("scaling", glm::vec2(1.f, 0.f));

    _program->setUniform("classification", _classification);
    if (!_classification) {
        _program->setUniform("fieldLineColor", _fieldlineColor);
    }

    glBindVertexArray(_vao);
    glMultiDrawArrays(
        GL_LINE_STRIP_ADJACENCY,
        &_lineStart[0],
        &_lineCount[0],
        static_cast<GLsizei>(_lineStart.size())
    );
    glBindVertexArray(0);
    glEnable(GL_CULL_FACE);
    _program->deactivate();
}

void RenderableFieldlines::update(const UpdateData&) {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
    }

    if (_seedPointsAreDirty) {
        _seedPoints.clear();
        switch (_seedPointSource.value()) {
            case static_cast<int>(SeedPointSource::File):
                loadSeedPointsFromFile();
                break;
            case static_cast<int>(SeedPointSource::Table):
                _seedPoints = _seedPointsTable.value_or(_seedPoints);
                break;
        }
        _seedPointsAreDirty = false;
        _fieldLinesAreDirty = true;
    }

    if (_fieldLinesAreDirty) {
        const std::vector<Line>& fieldlines = generateFieldlinesVolumeKameleon();

        if (fieldlines.empty()) {
            return;
        }

        int prevEnd = 0;
        std::vector<LinePoint> vertexData;
        // Arrange data for glMultiDrawArrays
        for (size_t j = 0; j < fieldlines.size(); j++) {
            _lineStart.push_back(prevEnd);
            _lineCount.push_back(static_cast<int>(fieldlines[j].size()));
            prevEnd = prevEnd + static_cast<int>(fieldlines[j].size());
            vertexData.insert(
                vertexData.end(),
                fieldlines[j].begin(),
                fieldlines[j].end()
            );
        }
        LDEBUG(std::format("Number of vertices: {}", vertexData.size()));

        glDeleteBuffers(1, &_vbo);
        glCreateBuffers(1, &_vbo);
        glNamedBufferStorage(
            _vbo,
            vertexData.size() * sizeof(LinePoint),
            &vertexData.front(),
            GL_NONE_BIT
        );
        glVertexArrayVertexBuffer(_vao, 0, _vbo, 0, sizeof(LinePoint));

        _fieldLinesAreDirty = false;
    }
}

void RenderableFieldlines::loadSeedPointsFromFile() {
    LINFO(std::format("Reading seed points from '{}'", _seedPointSourceFile.value()));

    std::ifstream seedFile(_seedPointSourceFile);
    if (!seedFile.good()) {
        LERROR(std::format(
            "Could not open seed points file '{}'", _seedPointSourceFile.value()
        ));
        return;
    }

    std::string line;
    while (ghoul::getline(seedFile, line)) {
        std::stringstream s = std::stringstream(line);

        glm::vec3 point;
        s >> point.x;
        s >> point.y;
        s >> point.z;
        _seedPoints.push_back(std::move(point));
    }
}

std::vector<RenderableFieldlines::Line>
RenderableFieldlines::generateFieldlinesVolumeKameleon()
{
    const bool threeVariables = _variables.size() == 3;
    const bool lorentzForce =
        _variables.size() == 1 && _variables[0] == VectorFieldKameleonVariableLorentz;

    if (!threeVariables && !lorentzForce) {
        LERROR(std::format("Illformed variables: '{}'", _variables));
        return {};
    }

    if (threeVariables) {
        KameleonWrapper kw = KameleonWrapper(_file);
        return kw.classifiedFieldLines(
            _variables[0],
            _variables[1],
            _variables[2],
            _seedPoints,
            _stepSize
        );
    }

    if (lorentzForce) {
        KameleonWrapper kw = KameleonWrapper(_file);
        return kw.lorentzTrajectories(_seedPoints, _fieldlineColor, _stepSize);
    }

    ghoul_assert(false, "Should not reach this");
    return {};
}

} // namespace openspace
