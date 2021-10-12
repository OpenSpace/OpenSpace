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

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/openglstatecache.h>

#include <fstream>

namespace {
    constexpr const char* _loggerCat = "RenderableMovingFieldlines";

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
    "LineWidth",
    "Line Width",
    "This value specifies the line width of the fieldlines"
    };

    struct [[codegen::Dictionary(RenderableMovingFieldlines)]] Parameters {
        // Path to folder containing the input .cdf files
        std::filesystem::path sourceFolder [[codegen::directory()]];
        // Path to file with seed points
        std::filesystem::path seedPointFile;
        // Extra variables such as rho, p or t
        std::optional<std::vector<std::string>> extraVariables;
        // Which variable in CDF file to trace. b is default for fieldline
        std::optional<std::string> tracingVariable;
        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;
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

RenderableMovingFieldlines::RenderableMovingFieldlines(
                                                      const ghoul::Dictionary& dictionary) 
    :Renderable(dictionary)
    , _lineWidth(LineWidthInfo, 1.f, 1.f, 20.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _sourceFolder = p.sourceFolder;
    if (!std::filesystem::is_directory(_sourceFolder)) {
        LERROR(fmt::format(
            "MovingFieldlines {} is not a valid directory",
            _sourceFolder.string()
        ));
    }
    std::vector<std::string> sourceFiles;
    namespace fs = std::filesystem;
    for (const fs::directory_entry& e : fs::directory_iterator(_sourceFolder)) {
        if (e.is_regular_file() && e.path().extension() == ".cdf") {
            std::string eStr = e.path().string();
            sourceFiles.push_back(eStr);
        }
    }
    if (sourceFiles.empty()) {
        throw ghoul::RuntimeError(fmt::format(
            "{} contains no .cdf files",
            _sourceFolder.string()
        ));
    }
    std::sort(sourceFiles.begin(), sourceFiles.end());

    _seedFilePath = p.seedPointFile;
    if (!std::filesystem::is_regular_file(_seedFilePath) || 
        _seedFilePath.extension() != ".txt") 
    {
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
    _lineWidth = p.lineWidth.value_or(_lineWidth);


}

void RenderableMovingFieldlines::initialize() {
    bool statesSuccuess = getStatesFromCdfFiles();

}

void RenderableMovingFieldlines::initializeGL() {
    _shaderProgram = global::renderEngine->buildRenderProgram(
        "MovingFieldlines",
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_vs.glsl"),
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_fs.glsl")
    );

    glGenVertexArrays(1, &_vertexArrayObject);
    glGenBuffers(1, &_vertexPositionBuffer);
    glGenBuffers(1, &_vertexColorBuffer);

    // Needed for additive blending
    setRenderBin(Renderable::RenderBin::Overlay);
}

bool RenderableMovingFieldlines::getStatesFromCdfFiles() {
    std::vector<std::string> extraMagVars = extractMagnitudeVarsFromStrings(_extraVars);
    
    // TODO remove placeholder, fix map vs vector
    std::unordered_map<std::string, std::vector<glm::vec3>> seedpointsPlaceholder;
    seedpointsPlaceholder["20000101080000"] = _seedPoints;
    
    namespace fs = std::filesystem;
    for (const std::filesystem::path entry : _sourceFiles) {
        const std::string& cdfPath = entry.string();
        bool isSuccessful = fls::convertCdfToFieldlinesState(
            _fieldlineState,
            cdfPath,
            seedpointsPlaceholder,
            _manualTimeOffset,
            _tracingVariable,
            _extraVars,
            extraMagVars
        );
    }

    return true;
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
    return true;
}

void RenderableMovingFieldlines::render(const RenderData& data, RendererTasks&) {
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
}

} // namespace openspace
