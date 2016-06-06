#include <apps/DataConverter/milkywayconversiontask.h>
#include <modules/volume/textureslicevolumereader.h>
#include <modules/volume/rawvolumewriter.h>
#include <modules/volume/volumesampler.h>

namespace openspace {
namespace dataconverter {
    
MilkyWayConversionTask::MilkyWayConversionTask(
    const std::string& inFilenamePrefix,
    const std::string& inFilenameSuffix,
    size_t inFirstIndex,
    size_t inNSlices, 
    const std::string& outFilename,
    const glm::ivec3& outDimensions)
    : _inFilenamePrefix(inFilenamePrefix)
    , _inFilenameSuffix(inFilenameSuffix)
    , _inFirstIndex(inFirstIndex)
    , _inNSlices(inNSlices)
    , _outFilename(outFilename)
    , _outDimensions(outDimensions) {}

    
void MilkyWayConversionTask::perform(const std::function<void(float)>& onProgress) {
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

    rawWriter.write(sampleFunction, onProgress);
}

}
}
