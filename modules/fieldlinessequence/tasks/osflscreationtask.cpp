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

#include <modules/fieldlinessequence/tasks/osflscreationtask.h>
#include <openspace/documentation/documentation.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <optional>

namespace {
    constexpr const char* _loggerCat = "OsflsCreationTask";
    struct [[codegen::Dictionary(OsflsCreationTask)]] Parameters {
        // 'b' for fieldline, 'u' for velocity line or 'u_perp_b' for flow line
        std::string tracingVariable;

        // directory where cdf files are located
        std::filesystem::path inputDirectory;
        
        // filepath to .txt file containing seedpoints
        std::filesystem::path inputSeedpoints;

        // directory for result / output of task run
        std::filesystem::path outputDirectory;

        // Also outputs in JSON format if set to true. False by default
        std::optional<bool> jsonOutput;
    };
#include "osflscreationtask_codegen.cpp"
} // namespace

namespace openspace::fls {

documentation::Documentation OsflsCreationTask::documentation() {
    return codegen::doc<parameters>("osfls_creation_task");
}

OsflsCreationTask::OsflsCreationTask(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _tracingVariable;
    _inputDirectory;
    _inputSeedpoints;
    _outputDirectory;
    _jsonOutput = p.jsonOutput.value_or(_jsonOutput);
}

std::string OsflsCreationTask::description() {
    return fmt::format(
        "Create osfls-files (or osfls and Json)-files by tracing, with the kameleon 
        "tracer, the vector field described in cdf-files, from the input seed points."
    );
}

void OsflsCreationTask::perform(const Task::ProgressCallback& progressCallback) {

}

} // namespace openspace::fls
