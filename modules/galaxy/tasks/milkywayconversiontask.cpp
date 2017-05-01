/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/galaxy/tasks/milkywayconversiontask.h>

#include <modules/volume/textureslicevolumereader.h>
#include <modules/volume/rawvolumewriter.h>
#include <modules/volume/volumesampler.h>

#include <openspace/documentation/documentation.h>

#include <ghoul/misc/dictionary.h>

namespace {
    const char* KeyInFilenamePrefix = "InFilenamePrefix";
    const char* KeyInFilenameSuffix = "InFilenameSuffix";
    const char* KeyInFirstIndex = "InFirstIndex";
    const char* KeyInNSlices = "InNSlices";
    const char* KeyOutFilename = "OutFilename";
    const char* KeyOutDimensions = "OutDimensions";
} // namespace 

namespace openspace {
    
MilkywayConversionTask::MilkywayConversionTask(const ghoul::Dictionary& dictionary) {
    std::string inFilenamePrefix;
    if (dictionary.getValue(KeyInFilenamePrefix, inFilenamePrefix)) {
        _inFilenamePrefix = inFilenamePrefix;
    }

    std::string inFilenameSuffix;
    if (dictionary.getValue(KeyInFilenameSuffix, inFilenameSuffix)) {
        _inFilenameSuffix = inFilenameSuffix;
    }

    size_t inFirstIndex;
    if (dictionary.getValue(KeyInFirstIndex, inFirstIndex)) {
        _inFirstIndex = inFirstIndex;
    }

    size_t inNSlices;
    if (dictionary.getValue(KeyInNSlices, inNSlices)) {
        _inNSlices = inNSlices;
    }

    std::string outFilename;
    if (dictionary.getValue(KeyOutFilename, outFilename)) {
        _outFilename = outFilename;
    }

    glm::ivec3 outDimensions;
    if (dictionary.getValue(KeyOutDimensions, outDimensions)) {
        _outDimensions = outDimensions;
    }
}

MilkywayConversionTask::~MilkywayConversionTask() {}

std::string MilkywayConversionTask::description()
{
    return std::string();
}

void MilkywayConversionTask::perform(const Task::ProgressCallback& progressCallback) {
    std::vector<std::string> filenames;
    for (int i = 0; i < _inNSlices; i++) {
        filenames.push_back(_inFilenamePrefix + std::to_string(i + _inFirstIndex) + _inFilenameSuffix);
    }
    
    TextureSliceVolumeReader<glm::tvec4<GLfloat>> sliceReader(filenames, _inNSlices, 10);
    sliceReader.initialize();

    RawVolumeWriter<glm::tvec4<GLfloat>> rawWriter(_outFilename);
    rawWriter.setDimensions(_outDimensions);

    glm::vec3 resolutionRatio =
        static_cast<glm::vec3>(sliceReader.dimensions()) / static_cast<glm::vec3>(rawWriter.dimensions());

    VolumeSampler<TextureSliceVolumeReader<glm::tvec4<GLfloat>>> sampler(sliceReader, resolutionRatio);
    std::function<glm::tvec4<GLfloat>(glm::ivec3)> sampleFunction = [&](glm::ivec3 outCoord) {
        glm::vec3 inCoord = ((glm::vec3(outCoord) + glm::vec3(0.5)) * resolutionRatio) - glm::vec3(0.5);
        glm::tvec4<GLfloat> value = sampler.sample(inCoord);
        return value;
    };

    rawWriter.write(sampleFunction, progressCallback);
}

documentation::Documentation MilkywayConversionTask::documentation()
{
    return documentation::Documentation();
}

}
