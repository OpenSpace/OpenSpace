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
        tracer.setInnerBoundary(1.5f);

        FieldlinesState state;
        const std::string& modelname = kameleon->getModelName();
        LINFO(std::format("Model name: {}", modelname));
        state.setModel(fls::stringToModel(modelname));
        state.setTriggerTime(kameleonHelper::getTime(kameleon.get(),0.0));

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
            tracer.getLastClosedFieldlines(_numberOfPointsOnBoundary, 1, 5.1, 300);

        for (ccmc::Fieldline line : fieldlines) {
            std::vector<glm::vec3> vertices;
            const std::vector<ccmc::Point3f>& positions = line.getPositions();
            for (const ccmc::Point3f& p : positions) {
                vertices.emplace_back(p.component1, p.component2, p.component3);
            }
            state.addLine(vertices);
        }

        fls::addExtraQuantities(&*kameleon, variableNames, magVariableNames, state);
        switch (state.model()) {
        case fls::Model::Batsrus:
            state.scalePositions(fls::ReToMeter);
            break;
        case fls::Model::Enlil:
            state.convertLatLonToCartesian(fls::AuToMeter);
            break;
        default:
            break;
        }
        std::string fileName = cdfPath.stem().string() + "_lastClosedFieldlines";
        state.saveStateToJson(_outputFolder.string() + fileName);
        state.saveStateToOsfls(_outputFolder.string() + fileName);
    }

    progressCallback(1.0f);
}

} //namespace openspace
