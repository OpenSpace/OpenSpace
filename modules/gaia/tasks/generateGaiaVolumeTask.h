
#ifndef __OPENSPACE_MODULE_SPACE___GENERATEGAIAVOLUMETASK___H___
#define __OPENSPACE_MODULE_SPACE___GENERATEGAIAVOLUMETASK___H___

#include <openspace/util/task.h>
#include <string>
#include <filesystem>
#include <ghoul/glm.h>
#include <map>

namespace openspace {
namespace gaiavolume {

struct GaiaVolumeDataLayout {
    //Each voxel should contain all the stars that reside in that perticular voxel
    //It would store all the data for every star 
    //then we can use a similiar approach as in the gaia renderable to access the different 
    //headers when needed, this solution should be able to support choosing any 
    //column as the x and y axis 
    double firstValue = 0.0;
    double secondValue = 0.0;
    int nStars = 0;
};
    
class GenerateGaiaVolumeTask : public Task {
public:
    GenerateGaiaVolumeTask(const ghoul::Dictionary& dictionary); 
    ~GenerateGaiaVolumeTask() override = default;

    std::string description() override;
    void perform(const Task::ProgressCallback& onProgress) override;
    static documentation::Documentation Documentation();

private:
    std::filesystem::path  _inputFilePath;
    std::filesystem::path _rawVolumeOutputPath;
    std::filesystem::path  _dictionaryOutputPath;
    std::string _time;

    glm::uvec3 _dimensions = glm::uvec3(0);
    glm::vec3 _lowerDomainBound = glm::vec3(0.f);
    glm::vec3 _upperDomainBound = glm::vec3(0.f);

    std::map<std::string, size_t> _fileHeaders;


};

} // namespace gaiavolume
} // namespace openspace
#endif // !__OPENSPACE_MODULE_SPACE___GENERATEGAIAVOLUMETASK___H___

