/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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
#include <modules/volume/textureslicevolumereader.h>

#include <modules/volume/rawvolumewriter.h>
#include <openspace/documentation/documentation.h>

#include <ghoul/misc/dictionary.h>

namespace {
    constexpr std::string_view KeyInFilenamePrefix = "InFilenamePrefix";
    constexpr std::string_view KeyInFilenameSuffix = "InFilenameSuffix";
    constexpr std::string_view KeyInFirstIndex = "InFirstIndex";
    constexpr std::string_view KeyInNSlices = "InNSlices";
    constexpr std::string_view KeyOutFilename = "OutFilename";
    constexpr std::string_view KeyOutDimensions = "OutDimensions";
} // namespace

namespace openspace {

MilkywayConversionTask::MilkywayConversionTask(const ghoul::Dictionary& dictionary)
    : _inFirstIndex(0)
    , _inNSlices(0)
{
    if (dictionary.hasKey(KeyInFilenamePrefix)) {
        _inFilenamePrefix = dictionary.value<std::string>(KeyInFilenamePrefix);
    }
    if (dictionary.hasKey(KeyInFilenameSuffix)) {
        _inFilenameSuffix = dictionary.value<std::string>(KeyInFilenameSuffix);
    }
    if (dictionary.hasKey(KeyInFirstIndex)) {
        _inFirstIndex = static_cast<size_t>(dictionary.value<int>(KeyInFirstIndex));
    }
    if (dictionary.hasKey(KeyInNSlices)) {
        _inNSlices = static_cast<size_t>(dictionary.value<int>(KeyInNSlices));
    }
    if (dictionary.hasKey(KeyOutFilename)) {
        _outFilename = dictionary.value<std::string>(KeyOutFilename);
    }
    if (dictionary.hasKey(KeyOutDimensions)) {
        _outDimensions = dictionary.value<glm::ivec3>(KeyOutDimensions);
    }
}

std::string MilkywayConversionTask::description() {
    return std::string();
}

void MilkywayConversionTask::perform(const Task::ProgressCallback& onProgress) {
    using namespace openspace::volume;

    std::vector<std::string> filenames;
    for (size_t i = 0; i < _inNSlices; i++) {
        filenames.push_back(
            _inFilenamePrefix + std::to_string(i + _inFirstIndex) + _inFilenameSuffix
        );
    }

    TextureSliceVolumeReader<glm::tvec4<GLfloat>> sliceReader(filenames, _inNSlices, 10);
    sliceReader.initialize();

    RawVolumeWriter<glm::tvec4<GLfloat>> rawWriter(_outFilename);
    rawWriter.setDimensions(_outDimensions);

    const glm::vec3 resolutionRatio = static_cast<glm::vec3>(sliceReader.dimensions()) /
                                      static_cast<glm::vec3>(rawWriter.dimensions());

    VolumeSampler<TextureSliceVolumeReader<glm::tvec4<GLfloat>>> sampler(
        &sliceReader,
        resolutionRatio
    );
    std::function<glm::tvec4<GLfloat>(glm::ivec3)> sampleFunction =
        [resolutionRatio, sampler](glm::ivec3 outCoord) {
            const glm::vec3 inCoord = ((glm::vec3(outCoord) + glm::vec3(0.5f)) *
                                      resolutionRatio) - glm::vec3(0.5f);
            const glm::tvec4<GLfloat> value = sampler.sample(inCoord);
            return value;
        };

    rawWriter.write(sampleFunction, onProgress);
}

documentation::Documentation MilkywayConversionTask::Documentation() {
    return {
        "MilkywayConversionTask",
        "galaxy_milkywayconversiontask"
    };
}

} // namespace openspace
