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

#include <modules/neoviz/tasks/impactcorridortask.h>

#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/filesystem.h>
#include <filesystem>

namespace {
    struct [[codegen::Dictionary(ImpactCorridorTask)]] Parameters {
        // The CDF file to extract data from
        std::filesystem::path input;

        // The HTML file to write documentation to
        std::string output [[codegen::annotation("A valid filepath")]];
    };
#include "impactcorridortask_codegen.cpp"
} // namespace

namespace openspace::neoviz {

documentation::Documentation ImpactCorridorTask::documentation() {
    return codegen::doc<Parameters>("neoviz_documentation_task");
}

std::string ImpactCorridorTask::description() {
    return "Return the NEOviz impact corridor map image for the given input";
}

ImpactCorridorTask::ImpactCorridorTask(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _inputPath = absPath(p.input);
    _outputPath = absPath(p.output);
}

void ImpactCorridorTask::perform(const Task::ProgressCallback & progressCallback) {

}

} // namespace openspace::neoviz
