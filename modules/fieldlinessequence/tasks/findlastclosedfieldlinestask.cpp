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

#include <modules/fieldlinessequence/tasks/findlastclosedfieldlinestask.h>
#include <modules/fieldlinessequence/util/fieldlinesstate.h>
#include <modules/fieldlinessequence/util/kameleonfieldlinehelper.h>
#include <modules/kameleon/include/kameleonhelper.h>
#include <modules/kameleon/ext/kameleon/src/ccmc/Tracer.h>



#include <openspace/documentation/verifier.h>
#include <ghoul/logging/logmanager.h>
#include <numbers>

#include <optional>
namespace {
    constexpr std::string_view _loggerCat = "FindLastClosedFieldlinesTask";

    struct [[codegen::Dictionary(FindLastClosedFieldlinesTask)]] Parameters {
        // The folder to the cdf files to extract data from
        std::filesystem::path input [[codegen::directory()]];
        // The name of the kameleon variable to use for tracing, like b for magnetic
        // or u for velocity or even electric?
        std::optional<std::string> tracingVar;
        // number of points to work with
        std::optional<int> numberOfPointsOnBoundary;
        // this will determine how accurate to the boundary it will get
        // every iteration the seedpoint will move closer to the boundary,
        // the distance it moves will be halfed each iteration until that
        // distance is less than this threshold.
        std::optional<float> threshold;
        // The folder to write the files to
        std::filesystem::path output [[codegen::directory()]];
    };
#include "findlastclosedfieldlinestask_codegen.cpp"
}

namespace openspace {

documentation::Documentation FindLastClosedFieldlinesTask::Documentation() {
    return codegen::doc<Parameters>("find_last_closed_fieldlines_task");
}

FindLastClosedFieldlinesTask::FindLastClosedFieldlinesTask(
                                                      const ghoul::Dictionary& dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _outputFolder = p.output;
    if (&_outputFolder.string().back() != "/") {
        _outputFolder += "/";
    }

    _numberOfPointsOnBoundary =
        p.numberOfPointsOnBoundary.value_or(_numberOfPointsOnBoundary);
    _tracingVar = p.tracingVar.value_or(_tracingVar);
    _threshold = p.threshold.value_or(_threshold);
    _inputPath = p.input;
    if (!std::filesystem::is_directory(_inputPath)) {
        LERROR(std::format(
            "FindLastClosedFieldlinesTask: {} is not a valid directory",
            _inputPath
        ));
    }
    namespace fsm = std::filesystem;
    for (const fsm::directory_entry& e : fsm::directory_iterator(_inputPath)) {
        if (e.path().extension() == ".cdf") {
            std::filesystem::path ePath = e.path();
            _sourceFiles.push_back(ePath);
        }
    }
    LINFO(std::format("\nFinished initializing"));

}

FindLastClosedFieldlinesTask::~FindLastClosedFieldlinesTask() {

}

std::string FindLastClosedFieldlinesTask::description() {
    return "FindLastClosedFieldlinesTask";
}

void FindLastClosedFieldlinesTask::perform(
    const Task::ProgressCallback& progressCallback)
{
    for (const std::filesystem::path& cdfPath : _sourceFiles) {
        std::unique_ptr<ccmc::Kameleon> kameleon =
            kameleonHelper::createKameleonObject(cdfPath.string());
        ccmc::Tracer tracer(kameleon.get());


        FieldlinesState state;
        //for (ccmc::Fieldline& fieldline : fieldlines) {
        //    std::vector<ccmc::Point3f> vertices = fieldline.getPositions();
        //    std::vector<glm::vec3> positions;
        //    for (int i = 0; vertices.size(); ++i) {
        //        positions[i] = {
        //            vertices[i].component1,
        //            vertices[i].component2,
        //            vertices[i].component3
        //        };
        //    }
        //    state.addLine(positions);
        //    //addLine(fieldline, state);   // Add the fieldline to the OpenSpace state
        //    //addExtraQuantities(fieldline, state);  // Add extra quantities to the fieldline
        //}

        LINFO(std::format("Model name: {}", kameleon->getModelName()));
        std::vector<std::string> variableNames;
        std::vector<std::string> magVariableNames;
        long nVariables = kameleon->getNumberOfVariables();
        for (long i = 0; i < nVariables; ++i) {
            std::string name = kameleon->getVariableName(i);
            if (name.size() <= 3) {
                if (name.back() == 'x' && i+2 < nVariables) {
                    magVariableNames.push_back(name);
                    magVariableNames.push_back(kameleon->getVariableName(i + 1));
                    magVariableNames.push_back(kameleon->getVariableName(i + 2));
                    LINFO(std::format(
                        "Magnitude variable name : {}, {}, {}",
                        name,
                        kameleon->getVariableName(i + 1),
                        kameleon->getVariableName(i + 2)
                    ));
                    i += 2;
                }
                else {
                    variableNames.push_back(name);
                    LINFO(std::format("Variable name : {}", name));
                }
            }
        }
        std::vector<ccmc::Fieldline> fieldlines =
            tracer.getLastClosedFieldlines(_numberOfPointsOnBoundary, 1, 300);

        fls::addExtraQuantities(&*kameleon, variableNames, magVariableNames, state);
        std::string fileName = cdfPath.stem().string() + "_lastClosedFieldlines";
        state.saveStateToJson(_outputFolder.string() + fileName);
    }










    //LINFO(std::format("Reached perform function"));
    ////find positions of initial set of seedpoints
    //std::vector<glm::vec3> listOfSeedPoints = initialCircleOfPoints();
    //// set threshold
    //float progressCallbackValue = 0.0f;

    //std::vector<std::string> extraVars;
    //std::vector<std::string> extraMagVars;

    ////for each file
    //for (const std::string& cdfPath : _sourceFiles) {
    //    progressCallbackValue += (1/ listOfSeedPoints.size());
    //    //////////for each seedpoint
    //    //trace
    //    FieldlinesState newState;
    //    bool isSuccessful = fls::traceFromListOfPoints(
    //        newState,
    //        cdfPath,
    //        listOfSeedPoints,
    //        0.0, //_manualTimeOffset
    //        _tracingVar,
    //        extraVars, // _extraVars
    //        extraMagVars // _extraMagVars
    //    );

    //    newState.lineStart();
    //    const std::vector<GLint>& lineStarts = newState.lineCount();
    //    const std::vector<GLsizei>& lineCounts = newState.lineCount();

    //    LINFO(std::format("Find first and last point on line"));
    //    for (int i = 0; i < lineStarts.size(); ++i) {
    //        progressCallback(progressCallbackValue);
    //        size_t firstIndex = lineStarts[i];
    //        size_t lastIndex = firstIndex + lineCounts[i];
    //        glm::vec3 firstPos = newState.vertexPositions()[firstIndex];
    //        glm::vec3 lastPos = newState.vertexPositions()[lastIndex];
    //        LINFO(std::format("First Pos: {}", firstPos));
    //        LINFO(std::format("Last Pos: {}", lastPos));

    //        
    //    }




    //        //check if endpoints are close to earth and determain closed vs not closed

    //        //run iteration:

    //            //move seedpoint, if closed further, else closer to earth
    //            //reduce movement distance
    //        //step when movement distance is less than threshold

    //    // save list of points in .txt file with approriate name correlating with cdf file used
    //    // save traced fieldlines in either osfls or json
    //}

    progressCallback(1.0f);
}

std::vector<glm::vec3> FindLastClosedFieldlinesTask::initialCircleOfPoints() {
    std::vector<glm::vec3> list;
    float angleDeg = 360.0 / _numberOfPointsOnBoundary;
    float angleRad = static_cast<float>(angleDeg * std::numbers::pi / 180.0);

    for (int i = 0; i<_numberOfPointsOnBoundary; ++i) {
        glm::vec3 posSphere;
        posSphere.x = 10.0;
        posSphere.y = angleRad * i;
        posSphere.z = 0.0;

        glm::vec3 posCartesian;
        posCartesian.x = posSphere.x * cos(posSphere.y);
        posCartesian.y = posSphere.x * sin(posSphere.y);
        posCartesian.z = 0.0;
        LINFO(std::format("\n {}, {} ", posCartesian.x, posCartesian.y));

        list.push_back(posCartesian);
    }
    return list;
}

} //namespace openspace
