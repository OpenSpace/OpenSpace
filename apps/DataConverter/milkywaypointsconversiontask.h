#ifndef __MILKYWAYPOINTSCONVERSIONTASK_H__
#define __MILKYWAYPOINTSCONVERSIONTASK_H__

#include <apps/DataConverter/conversiontask.h>
#include <string>
#include <glm/glm.hpp>
#include <functional>
#include <modules/volume/textureslicevolumereader.h>
#include <modules/volume/rawvolumewriter.h>


namespace openspace {
namespace dataconverter {

/**
 * Converts ascii based point data
 * int64_t n
 * (float x, float y, float z, float r, float g, float b) * n
 * to a binary (floating point) representation with the same layout.
 */
class MilkyWayPointsConversionTask : public ConversionTask {
public:
    MilkyWayPointsConversionTask(const std::string& inFilename,
                                 const std::string& outFilename);
    
    void perform(const std::function<void(float)>& onProgress) override;
private:
    std::string _inFilename;    
    std::string _outFilename;
};

}
}

#endif
