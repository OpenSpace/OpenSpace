
#ifndef __OPENSPACE_MODULE_SPACE___GENERATEGAIAVOLUMETASK___H___
#define __OPENSPACE_MODULE_SPACE___GENERATEGAIAVOLUMETASK___H___

#include <openspace/util/task.h>
#include <string>
#include <filesystem>
#include <ghoul/glm.h>
#include <map>
#include <vector>

namespace openspace::gaiavolume {

//Limits are specified as [minimum, maximum], values are initialized in reverse. So that 
//we can use std::min/std::max functions without having to worry about uninitialized values. 
struct Limits {
    std::pair<double, double> avg{ std::numeric_limits<double>::max(), std::numeric_limits<double>::lowest() };
    std::pair<float, float> max{ std::numeric_limits<float>::max(), std::numeric_limits<float>::lowest() };
    std::pair<float, float> min{ std::numeric_limits<float>::max(), std::numeric_limits<float>::lowest() };
};

struct VoxelDataLayout {
    double avgData = 0.0; //Average is double if we have very large values summed before computing the average.
    float minData{ std::numeric_limits<float>::max() };
    float maxData{ std::numeric_limits<float>::lowest() };
};

struct GaiaVolumeDataLayout {
    //Each voxel contain data about the stars that reside in that perticular voxel
    //It stores the combined voxeldata for all the stars and not 1 perticular star's values.
    std::vector<VoxelDataLayout> data{};
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

    std::map<std::string, int> _fileHeaders;
};

} // namespace openspace::gaiavolume
#endif // !__OPENSPACE_MODULE_SPACE___GENERATEGAIAVOLUMETASK___H___

