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
#include <openspace/util/updatestructures.h>
#include <fstream>

namespace {
    constexpr const char* _loggerCat = "RenderableMovingFieldlines";

    struct [[codegen::Dictionary(RenderableMovingFieldlines)]] Parameters {
        // Path to folder containing the input .cdf files
        std::filesystem::path sourceFolder [[codegen::directory()]];
        // Path to file with seed points
        std::filesystem::path seedPintFile;
        // Extra variables such as rho, p or t
        std::optional<std::vector<std::string>> extraVariables;
        // Which variable in CDF file to trace. b is default for fieldline
        std::optional<std::string> tracingVariable;
        // If data sets parameter start_time differ from start of run,
        // elapsed_time_in_seconds might be in relation to start of run.
        // ManuelTimeOffset will be added to trigger time.
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

    _seedFilePath = p.seedPintFile;
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


}

void RenderableMovingFieldlines::initialize() {
    bool statesSuccuess = getStatesFromCdfFiles();

}

void RenderableMovingFieldlines::initializeGL() {

}

bool RenderableMovingFieldlines::getStatesFromCdfFiles() {
    std::vector<std::string> extraMagVars = extractMagnitudeVarsFromStrings(_extraVars);
    std::unordered_map<std::string, std::vector<glm::vec3>> placeholder;
    placeholder["1"] = _seedPoints;
    namespace fs = std::filesystem;
    for (const fs::directory_entry entry : fs::directory_iterator(_sourceFolder)) {
        const std::string& cdfPath = entry.path().string();
        FieldlinesState newState;
        bool isSuccessful = fls::convertCdfToFieldlinesState(
            newState,
            cdfPath,
            placeholder,
            _manualTimeOffset,
            _tracingVariable,
            _extraVars,
            extraMagVars
        );
    }
    return true;
}

//std::vector<std::string>
//    extractMagnitudeVarsFromStrings(std::vector<std::string> extrVars)
//{
//    std::vector<std::string> extraMagVars;
//    for (int i = 0; i < static_cast<int>(extrVars.size()); i++) {
//        const std::string& str = extrVars[i];
//        // Check if string is in the format specified for magnitude variables
//        if (str.substr(0, 2) == "|(" && str.substr(str.size() - 2, 2) == ")|") {
//            std::istringstream ss(str.substr(2, str.size() - 4));
//            std::string magVar;
//            size_t counter = 0;
//            while (std::getline(ss, magVar, ',')) {
//                magVar.erase(
//                    std::remove_if(
//                        magVar.begin(),
//                        magVar.end(),
//                        ::isspace
//                    ),
//                    magVar.end()
//                );
//                extraMagVars.push_back(magVar);
//                counter++;
//                if (counter == 3) {
//                    break;
//                }
//            }
//            if (counter != 3 && counter > 0) {
//                extraMagVars.erase(extraMagVars.end() - counter, extraMagVars.end());
//            }
//            extrVars.erase(extrVars.begin() + i);
//            i--;
//        }
//    }
//    return extraMagVars;
//}

void RenderableMovingFieldlines::deinitializeGL() {

}

bool RenderableMovingFieldlines::isReady() const {
    return true;
}

void RenderableMovingFieldlines::render(const RenderData& data, RendererTasks&) {

}

void RenderableMovingFieldlines::update(const UpdateData& data) {

}

} // namespace openspace
