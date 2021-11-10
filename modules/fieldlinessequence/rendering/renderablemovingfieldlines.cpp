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

#include <modules/fieldlinessequence/rendering/renderablemovingfieldlines.h>

#include <modules/fieldlinessequence/fieldlinessequencemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/textureunit.h>


#include <fstream>
#pragma optimize("", off)
namespace {
    constexpr const char* _loggerCat = "RenderableMovingFieldlines";

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
    "LineWidth",
    "Line Width",
    "This value specifies the line width of the fieldlines"
    };
    constexpr openspace::properties::Property::PropertyInfo ColorMethodInfo = {
    "ColorMethod",
    "Color Method",
    "Color lines uniformly or using color tables based on extra quantities like, for "
    "examples, temperature or particle density."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorUniformInfo = {
    "Uniform",
    "Uniform Line Color",
    "The uniform color of lines shown when 'Color Method' is set to 'Uniform'."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorQuantityInfo = {
    "ColorQuantity",
    "Quantity to Color By",
    "Quantity used to color lines if the 'By Quantity' color method is selected."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorMinMaxInfo = {
    "ColorQuantityMinMax",
    "ColorTable Min Value",
    "Value to map to the lowest and highest end of the color table."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorTablePathInfo = {
    "ColorTablePath",
    "Path to Color Table",
    "Color Table/Transfer Function to use for 'By Quantity' coloring."
    };

    struct [[codegen::Dictionary(RenderableMovingFieldlines)]] Parameters {
        // Path to folder containing the input .cdf files
        std::filesystem::path sourceFolder [[codegen::directory()]];
        // Path to file with seed points
        std::filesystem::path seedPointFile;
        // Extra variables such as rho, p or t
        std::optional<std::vector<std::string>> extraVariables;
        // Which variable in CDF file to trace. 'b' is default. b is for magnetic field
        std::optional<std::string> tracingVariable;
        // The number of points on the 'path line' which fieldlines will be moving
        std::optional<int> numberOfPointsOnPathLine;
        // The number of points on the fieldlines that uses the 'path line' vertecis
        // as seed points
        std::optional<int> numberOfPointsOnFieldlines;
        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;
        // [[codegen::verbatim(ColorMethodInfo.description)]]
        std::optional<std::string> colorMethod;
        // List of ranges for which their corresponding parameters values will be 
        // colorized by. Should be entered as {min value, max value} per range
        std::optional<std::vector<glm::vec2>> colorTableRanges;
        // [[codegen::verbatim(ColorQuantityInfo.description)]]
        std::optional<int> colorQuantity;
        // A list of paths to transferfunction .txt files containing color tables
        // used for colorizing the fieldlines according to different parameters
        std::optional<std::vector<std::string>> colorTablePaths;
        // If data sets parameter start_time differ from start of run,
        // elapsed_time_in_seconds might be in relation to start of run.
        // ManuelTimeOffset will be added to trigger time.
        // TODO should not be any need for manual time offset because 
        // we only want one state
        std::optional<double> manualTimeOffset;
    };
#include "renderablemovingfieldlines_codegen.cpp"
} // namespace

namespace openspace {
std::vector<std::string> 
    extractMagnitudeVarsFromStrings(std::vector<std::string> extrVars);

documentation::Documentation RenderableMovingFieldlines::Documentation() {
    return codegen::doc<Parameters>("fieldlinessequence_renderablemovingfieldlines");
}

glm::vec3 lerp(glm::vec3 current, glm::vec3 next, float time) {
    return current * (1.f - time) + next * time;
}

RenderableMovingFieldlines::RenderableMovingFieldlines(
                                                      const ghoul::Dictionary& dictionary) 
    :Renderable(dictionary)
    , _colorGroup({ "Color" })
    , _lineWidth(LineWidthInfo, 1.f, 1.f, 20.f)
    , _colorMethod(ColorMethodInfo, properties::OptionProperty::DisplayType::Radio)
    , _colorUniform(
        ColorUniformInfo,
        glm::vec4(0.3f, 0.57f, 0.75f, 0.5f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _colorQuantity(ColorQuantityInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _colorQuantityMinMax (
        ColorMinMaxInfo,
        glm::vec2(-0.f, 100.f),
        glm::vec2(-5000.f),
        glm::vec2(5000.f)
    )
    , _colorTablePath(ColorTablePathInfo)

{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _sourceFolder = p.sourceFolder;
    if (!std::filesystem::is_directory(_sourceFolder)) {
        LERROR(fmt::format(
            "MovingFieldlines {} is not a valid directory",
            _sourceFolder.string()
        ));
    }
    namespace fs = std::filesystem;
    for (const fs::directory_entry& e : fs::directory_iterator(_sourceFolder)) {
        if (e.is_regular_file() && e.path().extension() == ".cdf") {
            std::string eStr = e.path().string();
            _sourceFiles.push_back(eStr);
        }
    }
    if (_sourceFiles.empty()) {
        throw ghoul::RuntimeError(fmt::format(
            "{} contains no .cdf files",
            _sourceFolder.string()
        ));
    }
    std::sort(_sourceFiles.begin(), _sourceFiles.end());

    _seedFilePath = p.seedPointFile;
    if (!std::filesystem::is_regular_file(_seedFilePath) || 
        _seedFilePath.extension() != ".txt") 
    {
        // TODO: add support for whatever actual seepoint files are being used
        throw ghoul::RuntimeError(fmt::format("SeedPointFile needs to be a .txt file"));
    }
    std::string seedFile = _seedFilePath.string();
    std::ifstream seedFileStream(seedFile);
    if (!seedFileStream.good()) {
        throw ghoul::RuntimeError(fmt::format("Could not read from {}", seedFile));
    }
    std::string line;
    while (std::getline(seedFileStream, line)) {
        std::stringstream ss(line);
        glm::vec3 point;
        ss >> point.x;
        ss >> point.y;
        ss >> point.z;
        _seedPoints.push_back(std::move(point));
    }
    if (_seedPoints.empty()) {
        throw ghoul::RuntimeError(fmt::format(
            "Found no seed points in: {}",
            _seedFilePath
        ));
    }

    _extraVars = p.extraVariables.value_or(_extraVars);
    _manualTimeOffset = p.manualTimeOffset.value_or(_manualTimeOffset);
    _tracingVariable = p.tracingVariable.value_or(_tracingVariable);
    if (p.numberOfPointsOnPathLine.has_value()) {
        //TODO Check if nPointsOnPathLine = 100 if not specified
        if (p.numberOfPointsOnPathLine.value() < 0) {
            LWARNING(fmt::format("Number of points on path line: {}, must be positive "
                "and have therefor been replaced with the default 200 points",
                p.numberOfPointsOnPathLine.value()
            ));
        }
        else {
            _nPointsOnPathLine = static_cast<size_t>(p.numberOfPointsOnPathLine.value());
        }
    }

    if (p.numberOfPointsOnFieldlines.has_value()) {
        //TODO Check if nPointsOnFieldlines = 100 if not specified
        if (p.numberOfPointsOnFieldlines.value() < 0) {
            LWARNING(fmt::format("Number of points on fieldlines: {}, must be positive "
                "and have therefor been replaced with the default 100 points",
                p.numberOfPointsOnFieldlines.value()
            ));
        }
        else {
            _nPointsOnFieldlines = 
                static_cast<size_t>(p.numberOfPointsOnFieldlines.value());
        }
    }

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    _colorTablePaths = p.colorTablePaths.value_or(_colorTablePaths);

    _colorMethod.addOption(static_cast<int>(ColorMethod::Uniform), "Uniform");
    _colorMethod.addOption(static_cast<int>(ColorMethod::ByQuantity), "By Quantity");
    if (p.colorMethod.has_value()) {
        if (p.colorMethod.value() == "Uniform") {
            _colorMethod = static_cast<int>(ColorMethod::Uniform);
        }
        else {
            _colorMethod = static_cast<int>(ColorMethod::ByQuantity);
        }
    }
    else {
        _colorMethod = static_cast<int>(ColorMethod::Uniform);
    }

    if (p.colorTableRanges.has_value()) {
        _colorTableRanges = *p.colorTableRanges;
    }
    else {
        _colorTableRanges.push_back(glm::vec2(0.f, 1.f));
    }

    if (p.colorQuantity.has_value()) {
        _colorMethod = static_cast<int>(ColorMethod::ByQuantity);
        _colorQuantityTemp = *p.colorQuantity;
    }
}

void RenderableMovingFieldlines::initialize() {
    // Set a default color table, just in case the (optional) user defined paths are
    // corrupt or not provided
    _colorTablePaths.push_back(FieldlinesSequenceModule::DefaultTransferFunctionFile);
    _transferFunction = std::make_unique<TransferFunction>(
        absPath(_colorTablePaths[0]).string()
    );

    bool stateSuccuess = getStateFromCdfFiles();
    if (!stateSuccuess) {
        throw ghoul::RuntimeError("Trying to read cdf file failed");
    }
    for (int i = 0; i < _fieldlineState.lineCount().size(); ++i) {
        _timeSinceLastInterpolation.push_back(0.f);
        _pathsVertexIndex.push_back(0.f);
    }
    _renderedLines = _fieldlineState.vertexPositions();
    size_t nExtraQuantities = _fieldlineState.nExtraQuantities();
        
    addPropertySubOwner(_colorGroup);
    _colorUniform.setViewOption(properties::Property::ViewOptions::Color);
    _colorGroup.addProperty(_colorUniform);

    if (nExtraQuantities > 0) {
        _colorQuantity = _colorQuantityTemp;
        _colorQuantityMinMax = _colorTableRanges[_colorQuantity];
        _colorQuantityMinMax.setViewOption(
            properties::Property::ViewOptions::MinMaxRange
        );
        _colorGroup.addProperty(_colorQuantityMinMax);
        _colorGroup.addProperty(_colorMethod);
        _colorGroup.addProperty(_colorQuantity);
        _colorGroup.addProperty(_colorTablePath);

        const std::vector<std::string>& extraNames = _fieldlineState.extraQuantityNames();
        for (int i = 0; i < static_cast<int>(nExtraQuantities); ++i) {
            _colorQuantity.addOption(i, extraNames[i]);
        }

        _colorTableRanges.resize(nExtraQuantities, _colorTableRanges.back());
        _colorTablePaths.resize(nExtraQuantities, _colorTablePaths.back());

        _colorTablePath = _colorTablePaths[0];

        _colorQuantity.onChange([this]() {
            _shouldUpdateColorBuffer = true;
            _colorQuantityMinMax = _colorTableRanges[_colorQuantity];
            _colorTablePath = _colorTablePaths[_colorQuantity];
        });
        _colorTablePath.onChange([this]() {
            _transferFunction->setPath(_colorTablePath);
            _colorTablePaths[_colorQuantity] = _colorTablePath;
        });
        _colorQuantityMinMax.onChange([this]() {
            _colorTableRanges[_colorQuantity] = _colorQuantityMinMax;
        });
    }
}

void RenderableMovingFieldlines::initializeGL() {
    _shaderProgram = global::renderEngine->buildRenderProgram(
        "MovingFieldlines",
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/movingfieldlines_vs.glsl"),
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/movingfieldlines_fs.glsl")
    );

    glGenVertexArrays(1, &_vertexArrayObject);
    glGenBuffers(1, &_vertexPositionBuffer);
    glGenBuffers(1, &_vertexColorBuffer);

    // Needed for additive blending
    setRenderBin(Renderable::RenderBin::Overlay);
}

bool RenderableMovingFieldlines::getStateFromCdfFiles() {
    std::vector<std::string> extraMagVars = extractMagnitudeVarsFromStrings(_extraVars);
    
    // TODO remove placeholder, fix map vs vector
    std::unordered_map<std::string, std::vector<glm::vec3>> seedpointsPlaceholder;
    seedpointsPlaceholder["20000101080000"] = _seedPoints;
    
    namespace fs = std::filesystem;
    bool isSuccessful = false;
    for (const std::filesystem::path entry : _sourceFiles) {
        const std::string& cdfPath = entry.string();
        isSuccessful = fls::convertCdfToMovingFieldlinesState(
            _fieldlineState,
            cdfPath,
            seedpointsPlaceholder,
            _manualTimeOffset,
            _tracingVariable,
            _extraVars,
            extraMagVars,
            _nPointsOnPathLine,
            _nPointsOnFieldlines
        );
    }

    _fieldlineState.addLinesToBeRendered();

    if (isSuccessful) {
        switch (_fieldlineState.model()) {
        case fls::Model::Batsrus:
            _fieldlineState.scalePositions(fls::ReToMeter);
            break;
        default:
            break;
        }
    }

    return isSuccessful;
}

void RenderableMovingFieldlines::deinitializeGL() {
    glDeleteVertexArrays(1, &_vertexArrayObject);
    _vertexArrayObject = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    glDeleteBuffers(1, &_vertexColorBuffer);
    _vertexColorBuffer = 0;

    if (_shaderProgram) {
        global::renderEngine->removeRenderProgram(_shaderProgram.get());
        _shaderProgram = nullptr;
    }
}

bool RenderableMovingFieldlines::isReady() const {
    return _shaderProgram != nullptr;
}

void RenderableMovingFieldlines::render(const RenderData& data, RendererTasks&) {
    _shaderProgram->activate();

    // Calculate Model View MatrixProjection
    const glm::dmat4 rotMat = glm::dmat4(data.modelTransform.rotation);
    const glm::dmat4 modelMat =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        rotMat *
        glm::dmat4(glm::scale(glm::dmat4(1), glm::dvec3(data.modelTransform.scale)));
    const glm::dmat4 modelViewMat = data.camera.combinedViewMatrix() * modelMat;

    _shaderProgram->setUniform("modelViewProjection",
        data.camera.sgctInternal.projectionMatrix() * glm::mat4(modelViewMat));
    _shaderProgram->setUniform("lineColor", _colorUniform);
    _shaderProgram->setUniform("colorMethod", _colorMethod);

    if (_colorMethod == static_cast<int>(ColorMethod::ByQuantity)) {
        ghoul::opengl::TextureUnit textureUnit;
        textureUnit.activate();
        _transferFunction->bind(); // Calls update internally
        _shaderProgram->setUniform("colorTable", textureUnit);
        _shaderProgram->setUniform("colorTableRange", _colorTableRanges[_colorQuantity]);
    }

    glBindVertexArray(_vertexArrayObject);
#ifndef __APPLE__
    glLineWidth(_lineWidth);
#else
    glLineWidth(1.f);
#endif

    glMultiDrawArrays(
        GL_LINE_STRIP,
        _fieldlineState.lineStart().data(),
        _fieldlineState.lineCount().data(),
        static_cast<GLsizei>(_fieldlineState.lineStart().size())
    );

    glBindVertexArray(0);
    _shaderProgram->deactivate();

    // Restores OpenGL Rendering State
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
}

void RenderableMovingFieldlines::update(const UpdateData& data) {
    if (_shaderProgram->isDirty()) {
        _shaderProgram->rebuildFromFile();
    }

    const double previousTime = data.previousFrameTime.j2000Seconds();
    const double currentTime = data.time.j2000Seconds();
    const double dt = currentTime - previousTime;
    if (abs(dt) > DBL_EPSILON) {
        moveLines(dt);
    }

    updateVertexPositionBuffer();
    if (_fieldlineState.nExtraQuantities() > 0) {
        updateVertexColorBuffer();
    }
}

void RenderableMovingFieldlines::moveLines(const double dt) {
    bool forward = std::signbit(dt) ? false : true;

    std::vector<FieldlinesState::PathLine> allPathLines = _fieldlineState.allPathLines();
    size_t nDrawnLines = _fieldlineState.lineCount().size();
    
    // for each path line
    for (size_t i = 0; i < nDrawnLines; ++i) {
        bool atStart = false;
        bool atEnd = false;
        _timeSinceLastInterpolation[i] += float(dt);
        GLint lineStart = _fieldlineState.lineStart()[i];
        GLsizei nVertices = _fieldlineState.lineCount()[i];

        if (forward) {
            std::vector<FieldlinesState::Fieldline>::iterator fieldlineIt = 
                allPathLines[i].fieldlines.begin();
            std::advance(fieldlineIt, _pathsVertexIndex[i]);
            FieldlinesState::Fieldline& currentFieldline = *fieldlineIt;
            FieldlinesState::Fieldline& nextFieldline = *(fieldlineIt + 1);

            // if we past the next vertex on pathline, advance paths vertex index 
            // and reset this paths time since last interpolation
            if (_timeSinceLastInterpolation[i] > currentFieldline.timeToNextFieldline) {
                // -2 is becaue: -1 because size = index+1, -1 for second to last line
                if (_pathsVertexIndex[i] != allPathLines[i].fieldlines.size()-2) {
                    _timeSinceLastInterpolation[i] -= 
                        currentFieldline.timeToNextFieldline;
                    ++(_pathsVertexIndex[i]);
                    ++fieldlineIt;
                    currentFieldline = *fieldlineIt;
                    nextFieldline = *(fieldlineIt + 1);
                }
                // if we reach end of path line. 
                else {
                    // TAODO: see why/if this is needed: set to previous timeSince?
                    //FieldlinesState::Fieldline& previousFieldline = *(fieldlineIt - 1);
                    //_timeSinceLastInterpolation[i] =
                    //    previousFieldline.timeToNextFieldline;
                    int fieldlineVertex = 0;
                    // j = rendered line vertex index
                    for (int j = lineStart; j < nVertices + lineStart; ++j) {
                        // set the rendered lines vertex positions
                        _renderedLines[j] =
                            // to be = to current fieldlines vertex
                            currentFieldline.vertecies[fieldlineVertex].position;
                        ++fieldlineVertex;
                    }
                    atEnd = true;
                    break;
                }
            }

            if (atEnd) continue;

            // if there is a topology change to the next fieldline: no lerping
            if (nextFieldline.topology != currentFieldline.topology) {
                ++_pathsVertexIndex[i];
                ++fieldlineIt;
                currentFieldline = *fieldlineIt;
                nextFieldline = *(fieldlineIt + 1);
                _timeSinceLastInterpolation[i] = 0.f;

                int fieldlineVertex = 0;
                // j = rendered line vertex index
                for (int j = lineStart; j < nVertices + lineStart; ++j) {
                    _renderedLines[j] =
                        currentFieldline.vertecies[fieldlineVertex].position;
                    ++fieldlineVertex;
                }
            }
            else {
                float timeDifference = _timeSinceLastInterpolation[i] /
                    currentFieldline.timeToNextFieldline;
                int fieldlineVertex = 0;
                // j = rendered line vertex index
                for (int j = lineStart; j < nVertices + lineStart; ++j) {
                    glm::vec3 currentPosition = 
                        currentFieldline.vertecies[fieldlineVertex].position;
                    glm::vec3 nextPosition =
                        nextFieldline.vertecies[fieldlineVertex].position;
                    _renderedLines[j] =
                        lerp(currentPosition, nextPosition, timeDifference);
                    ++fieldlineVertex;
                }
            }
        }
        //backwards
        else {
            std::vector<FieldlinesState::Fieldline>::iterator fieldlineIt =
                allPathLines[i].fieldlines.begin();
            std::advance(fieldlineIt, _pathsVertexIndex[i]);
            FieldlinesState::Fieldline& currentFieldline = *fieldlineIt;
            // if backing past prev fieldline
            if (_timeSinceLastInterpolation[i] < FLT_EPSILON) {

                //if we're back to start
                if (_pathsVertexIndex[i] > 1) {
                    _timeSinceLastInterpolation[i] +=
                        currentFieldline.timeToNextFieldline;
                    --_pathsVertexIndex[i];
                    --fieldlineIt;
                    currentFieldline = *fieldlineIt;
                }
                else {
                    int fieldlineVertex = 0;
                    for (int j = lineStart; j < nVertices + lineStart; ++j) {
                        _renderedLines[j] =
                            currentFieldline.vertecies[fieldlineVertex].position;
                        ++fieldlineVertex;
                    }
                    atStart = true;
                }
            }

            if (atStart) continue;
            FieldlinesState::Fieldline& nextFieldline = *(fieldlineIt - 1);

            //if different topology
            if (nextFieldline.topology != currentFieldline.topology) {
                --_pathsVertexIndex[i];
                _timeSinceLastInterpolation[i] = currentFieldline.timeToNextFieldline;
                int fieldlineVertex = 0;
                // j = rendered line vertex index
                // for (int j = lineStart; j < lineCount + lineStart; ++j) {
                for (int j = 0; j < nVertices; ++j) {
                    _renderedLines[lineStart + j] =
                        currentFieldline.vertecies[fieldlineVertex].position;
                    ++fieldlineVertex;
                }
            }
            else {
                float timeDifference = _timeSinceLastInterpolation[i] /
                    nextFieldline.timeToNextFieldline;
                int fieldlineVertex = 0;
                // j = rendered line vertex index
                for (int j = lineStart; j < nVertices + lineStart; ++j) {
                    glm::vec3 currentPosition =
                        currentFieldline.vertecies[fieldlineVertex].position;
                    glm::vec3 nextPosition =
                        nextFieldline.vertecies[fieldlineVertex].position;
                    _renderedLines[j] =
                        lerp(nextPosition, currentPosition, timeDifference);
                    ++fieldlineVertex;
                }
            }





        }



    }
}

void RenderableMovingFieldlines::updateVertexPositionBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

    const std::vector<glm::vec3>& vertPos = _renderedLines;

    glBufferData(
        GL_ARRAY_BUFFER,
        vertPos.size() * sizeof(glm::vec3),
        vertPos.data(),
        GL_STATIC_DRAW
    );

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableMovingFieldlines::updateVertexColorBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexColorBuffer);

    bool isSuccessful;
    const std::vector<float>& quantities = _fieldlineState.extraQuantity(
        _colorQuantity,
        isSuccessful
    );

    if (isSuccessful) {
        glBufferData(
            GL_ARRAY_BUFFER,
            quantities.size() * sizeof(float),
            quantities.data(),
            GL_STATIC_DRAW
        );

        glEnableVertexAttribArray(1);
        glVertexAttribPointer(1, 1, GL_FLOAT, GL_FALSE, 0, 0);

        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);
    }
}

} // namespace openspace
