#ifndef __MILKYWAYCONVERSIONTASK_H__
#define __MILKYWAYCONVERSIONTASK_H__

#include <apps/DataConverter/conversiontask.h>
#include <string>
#include <glm/glm.hpp>
#include <functional>
#include <modules/volume/textureslicevolumereader.h>
#include <modules/volume/rawvolumewriter.h>


namespace openspace {
namespace dataconverter {

/**
 * Converts a set of exr image slices to a raw volume
 * with floating point RGBA data (32 bit per channel).
 */
class MilkyWayConversionTask : public ConversionTask {
public:
    MilkyWayConversionTask(const std::string& inFilenamePrefix,
                           const std::string& inFilenameSuffix,
                           size_t inFirstIndex,
                           size_t inNSlices, 
                           const std::string& outFilename,
                           const glm::ivec3& outDimensions);
    
    void perform(const std::function<void(float)>& onProgress) override;
private:
    std::string _inFilenamePrefix;
    std::string _inFilenameSuffix;
    size_t _inFirstIndex;
    size_t _inNSlices;
    std::string _outFilename;
    glm::ivec3 _outDimensions;
};

}
}

#endif
