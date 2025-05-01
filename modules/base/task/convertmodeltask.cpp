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

#include <modules/base/task/convertmodeltask.h>

#include <ghoul/io/model/modelgeometry.h>
#include <ghoul/io/model/modelreaderassimp.h>

namespace {
    // This task converts a 3D model format from a format that is natively supported both
    // by OpenSpace and common 3D modelling tools and converts it into an OpenSpace
    // proprietary format that can be loaded more efficiently, but more important can be
    // distributed without violating terms of service for various 3D model hosting
    // websites.
    // 
    // The resulting output file can be used everywhere in OpenSpace in place of the
    // source material.
    // 
    // The list of supported files can be found here:
    // https://github.com/assimp/assimp/blob/master/doc/Fileformats.md
    struct [[codegen::Dictionary(ConvertModelTask)]] Parameters {
        // The path to the source file
        std::filesystem::path inputFilePath;
        // The path to the output file
        std::filesystem::path outputFilePath [[codegen::mustexist(false)]];
    };
#include "convertmodeltask_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation ConvertModelTask::Documentation() {
    return codegen::doc<Parameters>("base_convert_model_task");
}

ConvertModelTask::ConvertModelTask(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _inFilePath = p.inputFilePath;
    _outFilePath = p.outputFilePath;
}

std::string ConvertModelTask::description() {
    return "This task converts a 3D model format from a format that is natively "
        "supported both by OpenSpace and common 3D modelling tools and converts it into "
        "an OpenSpace proprietary format that can be loaded more efficiently, but more "
        "important can be distributed without violating terms of service for various 3D "
        "model hosting websites";
}

void ConvertModelTask::perform(const Task::ProgressCallback&) {
    ghoul::io::ModelReaderAssimp reader;

    std::unique_ptr<ghoul::modelgeometry::ModelGeometry> geometry =
        reader.loadModel(_inFilePath, false, true);
    geometry->saveToCacheFile(_outFilePath);
}

} // namespace openspace
