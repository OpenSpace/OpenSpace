#include <modules/gaia/tasks/generateGaiaVolumeTask.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/misc/csvreader.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <vector>
#include <string>
#include <modules/volume/rawvolume.h>
#include <modules/volume/rawvolumewriter.h>
#include <modules/volume/rawvolumemetadata.h>
#include <modules/volume/volumegridtype.h>
#include <openspace/util/time.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#define _USE_MATH_DEFINES
#include <math.h>

        //// The Lua function used to compute the cell values
        //std::string valueFunction [[codegen::annotation("A Lua expression that returns a "
        //    "function taking three numbers as arguments (x, y, z) and returning a "
        //    "number")]];

namespace {

    constexpr openspace::properties::Property::PropertyInfo FilterOptionInfo = {
        "FilterOption",
        "Filter Option",
        "This option determines which type of filter to apply to the voxel data."
        "Gaussian applies applies a gaussian distribution with size 5x5 of each star value to nearby voxels"
        "RadiusFallOff applies a normal distribution fall off with radii 1",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr std::string_view _loggerCat = "GenerateGaiaVolumeTask";

    struct [[codegen::Dictionary(GenerateRawVolumeTask)]] Parameters {

        // The raw volume file to import data from in csv format
        std::string dataInputPath [[codegen::annotation("A valid filepath")]];

        // The raw volume file to export data to
        std::string rawVolumeOutput [[codegen::annotation("A valid filepath")]];

        // The lua dictionary file to export metadata to
        std::string dictionaryOutput [[codegen::annotation("A valid filepath")]];

        // The timestamp that is written to the metadata of this volume
        std::string time;

        // A vector representing the number of cells in each dimension
        glm::ivec3 dimensions;

        //Apply filtering
        std::optional<bool> applyFilter;

        std::optional<bool> normalizeData;

        enum class [[codegen::map(openspace::gaiavolume::FilterOption)]] FilterType {
            None,
            Gaussian,
            RadiusFallOff,
        };

        std::optional<FilterType> filterOption;
    };
#include "generategaiavolumetask_codegen.cpp"
} // namespace

namespace openspace::gaiavolume {

documentation::Documentation GenerateGaiaVolumeTask::Documentation() {
    return codegen::doc<Parameters>("generate_gaia_volume_task");
}

GenerateGaiaVolumeTask::GenerateGaiaVolumeTask(const ghoul::Dictionary& dictionary) 
    //_filterOption{FilterOptionInfo}
{
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _inputFilePath = absPath(p.dataInputPath);
    _rawVolumeOutputPath = absPath(p.rawVolumeOutput);
    _dictionaryOutputPath = absPath(p.dictionaryOutput);
    _dimensions = p.dimensions;
    _time = p.time;
    _lowerDomainBound = glm::vec3(std::numeric_limits<float>::max());
    _upperDomainBound = glm::vec3(std::numeric_limits<float>::min());
    _applyFilter = p.applyFilter.value_or(false);
    _normalize = p.normalizeData.value_or(true);

    if (p.filterOption.has_value())
        _filterOption = codegen::map<gaiavolume::FilterOption>(*p.filterOption);

    //_filterOption = p.filterOption.value_or(gaiavolume::FilterOption::RadiusFallOff);

    //if (p.shaderOption.has_value()) {
        //_shaderOption = codegen::map<gaia::ShaderOption>(*p.shaderOption);
}

std::string GenerateGaiaVolumeTask::description()
{
    return std::string();
}

float computeGaussianValue(float sigma, int x) {
    return 1.0f / (std::sqrt(2.0f * static_cast<float>(M_PI) * sigma * sigma)) * std::exp(-(static_cast<float>(x * x)) / (2.0f * sigma * sigma));
}
void GenerateGaiaVolumeTask::perform(const Task::ProgressCallback& onProgress)
{
    int nStarsRead = 0;
    //Open file , read and store content of file in local data structure
    std::vector<std::vector<std::string>> dataTable = ghoul::loadCSVFile(_inputFilePath.string(), true);
    std::vector<std::vector<float>> data;

    onProgress(0.2f);
    if (dataTable.empty()) {
        LERROR(fmt::format(
            "Error loading CSV data in file '{}'", _inputFilePath.string()
        ));
        return;
    }
    std::vector<std::string> headers = std::move(dataTable[0]);

    int idx{ 0 };
    for (const auto& s : headers) {
        _fileHeaders[s] = idx++;
    }
    // Create 3D Gaussian Kernel
    const float sigma{ 0.54f };
    const int size{ 5 / 2 };
    float kernel[5][5][5];
    float sum = 0.0;

    if (_applyFilter) {
        for (int i{ -size }; i <= size; i++) {
            float x = computeGaussianValue(sigma, i);

            for (int j{ -size }; j <= size; j++) {
                float y = computeGaussianValue(sigma, j);

                for (int k{ -size }; k <= size; k++) {
                    float z = computeGaussianValue(sigma, k);
                    float weight = x * y * z;
                    kernel[i + size][j + size][k + size] = weight;

                    sum += weight;
                }
            }
        }
    }
    
    //find min/max x,y,z of position - domain bounds of the volume
    for (auto it{ dataTable.begin() + 1 }; it != dataTable.end(); it++)
    {
        const std::vector<std::string>& row = *it;
        //Convert row of data from string values to floats
        std::vector<float> starValues{};
        std::transform(row.begin(), row.end(), std::back_inserter(starValues),
            [](const std::string& s) {
                if (s.size() == 0)
                    return std::nanf("");

                return std::stof(s); //string to float
            });

        //Get the bounding box of the dataset
        glm::vec3 pos{ starValues[0], starValues[1], starValues[2] }; //TODO get correct values with _fileheaders 
        _lowerDomainBound = glm::vec3{
            std::min(pos.x, _lowerDomainBound.x),
            std::min(pos.y, _lowerDomainBound.y),
            std::min(pos.z, _lowerDomainBound.z) };

        _upperDomainBound = glm::vec3{
            std::max(pos.x, _upperDomainBound.x),
            std::max(pos.y, _upperDomainBound.y),
            std::max(pos.z, _upperDomainBound.z) };

        data.push_back(std::move(starValues));
    }

    onProgress(0.5f);

    //Parameter limits are used to normalize the data between 0 and 1 to be able to use them as coordinates in a texture. 
    //The idea is that doing the normalization here saves time during rendering.
    //We store the minimum and maximum for each of the render mode we have respectively, i.e., average, min/max value.
    std::vector<Limits> parameterLimits{ headers.size() };
    volume::RawVolume<GaiaVolumeDataLayout> rawVolume(_dimensions);

    //Initialize all volumes, necessary to do this so that we can use the guassian blur on 
    //surrounding voxels without overwriting data, if we dont apply filter the voxels are initialized lazily
    //if (_applyFilter) {
    //    for (int i{ 0 }; i < rawVolume.nCells(); i++) {
    //        GaiaVolumeDataLayout voxel = rawVolume.get(i);
    //        voxel.data.assign(headers.size(), VoxelDataLayout{});
    //        rawVolume.set(i, voxel);
    //    }
    //}
    onProgress(0.6f);
    int maxStarsInACell = 0;
    //For every star find the nearest voxel i,j,k that should contain that stars values
    for (std::vector<float> const& star : data) {
        glm::vec3 pos{ star[0], star[1], star[2] };
        //Normalizes the position between 0 and 1 in each x,y,z dimension
        glm::vec3 normalizedPos{ (pos - _lowerDomainBound) / (_upperDomainBound - _lowerDomainBound) };

        int const i = static_cast<int>(std::min(
            static_cast<unsigned int>(std::floor(normalizedPos.x * _dimensions.x)), _dimensions.x - 1));
        int const j = static_cast<int>(std::min(
            static_cast<unsigned int>(std::floor(normalizedPos.y * _dimensions.y)), _dimensions.y - 1));
        int const k = static_cast<int>(std::min(
            static_cast<unsigned int>(std::floor(normalizedPos.z * _dimensions.z)), _dimensions.z - 1));

        //bool applyFilter = false;
        if (_applyFilter) {
            //bool gaussianFilter = false;

            if (_filterOption == FilterOption::Gaussian) {
                //Spread the values to voxels surrounding i,j,k using the 3D gaussian filter kernel
                for (int x{ -size }; x <= size; x++) {
                    for (int y{ -size }; y <= size; y++) {
                        for (int z{ -size }; z <= size; z++) {
                            //Get the indices of the neighbours
                            unsigned int u = x + i;
                            unsigned int v = y + j;
                            unsigned int w = z + k;
                            //Make sure its not out of bounds of the volume.
                            if (u < 0 || v < 0 || w < 0 ||
                                u >= _dimensions.x || v >= _dimensions.y || w >= _dimensions.z)
                                continue;

                            glm::uvec3 voxelCell{ u, v, w };
                            GaiaVolumeDataLayout voxel = rawVolume.get(voxelCell);
                            float weight = kernel[x + size][y + size][z + size];

                            if (!voxel.containData()) {
                                voxel.data.assign(headers.size(), VoxelDataLayout{});
                            }

                            //Loop the data and set each value multiplied with the weight
                            int counter{ 0 };
                            for (VoxelDataLayout& parameter : voxel.data) {
                                float value = star[counter];
                                //Store the x,y,z position as absolute value, i.e., distance to the origin
                                //TODO use the file headers to find these position if we decide to keep this
                                if (counter < 3)
                                    value = std::abs(value);

                                if (std::isnan(value)) {
                                    if (!voxel.containData()) { //If this is the first time writing to the voxel
                                        parameter.isNaNData = true;
                                    }
                                }
                                else {
                                    if (counter >= 3)
                                        value *= weight;

                                    parameter.avgData += value;
                                    parameter.minData = std::min(parameter.minData, value); //Store the smallest value of all stars
                                    parameter.maxData = std::max(parameter.maxData, value); //Store the largest value of all stars
                                    parameter.isNaNData = false;
                                    parameter.nStars += 1;
                                }
                                ++counter;
                            }

                            if (!voxel.containData()) {
                                voxel.has_data = GaiaVolumeDataLayout::HAS_DATA::yes;
                            }
                            //Only increase the counter for the voxel we are at, ignore neighbours.
                            //We should probably increase the voxel nstars to get correct scaling when computing the averages.
                            //if(x == y == z == 0) {
                            //++voxel.nStars;
                            //}

                            rawVolume.set(voxelCell, voxel);
                        }
                    }
                }
            }
            else if(_filterOption == FilterOption::RadiusFallOff) {
                int radius = 4;
                float sigma = 1.0;
                //Spread the values to voxels surrounding i,j,k using a distance based method and normal distribution function
                for (int x{ -radius }; x <= radius; x++) {
                    for (int y{ -radius }; y <= radius; y++) {
                        for (int z{ -radius }; z <= radius; z++) {
                            //Get the indices of the neighbours
                            unsigned int u = x + i;
                            unsigned int v = y + j;
                            unsigned int w = z + k;
                            //Make sure its not out of bounds of the volume.
                            if (u < 0 || v < 0 || w < 0 ||
                                u >= _dimensions.x || v >= _dimensions.y || w >= _dimensions.z)
                                continue;

                            float distance = std::sqrt(x * x + y * y + z * z);
                            if (distance > 1)
                                continue;

                            glm::uvec3 voxelCell{ u, v, w };
                            GaiaVolumeDataLayout voxel = rawVolume.get(voxelCell);
                            //The weight is scaled with the distance to the cell we are currently at.
                            float weight = std::exp(-distance / (2.0f * sigma * sigma));

                            if (!voxel.containData()) {
                                voxel.data.assign(headers.size(), VoxelDataLayout{});
                            }

                            //Loop the data and set each value multiplied with the weight
                            int counter{ 0 };
                            for (VoxelDataLayout& parameter : voxel.data) {
                                float value = star[counter];
                                //Store the x,y,z position as absolute value, i.e., distance to the origin
                                //TODO use the file headers to find these position if we decide to keep this
                                if (counter < 3)
                                    value = std::abs(value);
                                
                                if (std::isnan(value)) {
                                    if (!voxel.containData()) { //If this is the first time writing to the voxel
                                        parameter.isNaNData = true;
                                    }
                                }
                                else {
                                    if (counter >= 3)
                                        value *= weight;

                                    parameter.avgData += value;
                                    parameter.minData = std::min(parameter.minData, value); //Store the smallest value of all stars
                                    parameter.maxData = std::max(parameter.maxData, value); //Store the largest value of all stars
                                    parameter.isNaNData = false;
                                    parameter.nStars += 1;
                                }
                                ++counter;
                            }

                            if (!voxel.containData()) {
                                voxel.has_data = GaiaVolumeDataLayout::HAS_DATA::yes;
                            }
                            //Only increase the counter for the voxel we are at, ignore neighbours.
                            //We should probably increase the voxel nstars to get correct scaling when computing the averages.
                            //if(x == y == z == 0) {
                            //++voxel.nStars;
                            //}

                            rawVolume.set(voxelCell, voxel);
                        }
                    }
                }
            }
            else {
                LERROR("Did not specify a filter method. Use FilterOption in .asset file, either 'Gaussian' or 'RadiusFallOff'");
                return;
            }
        }
        else {
            glm::uvec3 voxelCell{ i,j,k };

            GaiaVolumeDataLayout voxel = rawVolume.get(voxelCell);
            voxel.data.assign(star.size(), VoxelDataLayout{});
            //Add all star data to the voxel data
            int counter{ 0 };
            for (VoxelDataLayout& parameter : voxel.data) {
                float value = star[counter];
                //Store the x,y,z position as absolute value, i.e., distance to the origin
                //TODO use the file headers to find these position if we decide to keep this
                if (counter < 3)
                    value = std::abs(value);
                //If the value is NaN we do not want to add to avgData because float + nan => nan
                //However, if this was the first time writing to the voxel we set a flag saying this data is invalid
                //this is so that we don't include the float::min and float::max data when computing the globla min and max for each parameter
                if (std::isnan(value)) {
                    if (!voxel.containData()) { //If this is the first time writing to the voxel
                        parameter.isNaNData = true;
                    }
                }
                else {
                    parameter.avgData += value;
                    parameter.minData = std::min(parameter.minData, value); //Store the smallest value of all stars
                    parameter.maxData = std::max(parameter.maxData, value); //Store the largest value of all stars
                    parameter.isNaNData = false;
                    parameter.nStars += 1;
                    maxStarsInACell = std::max(parameter.nStars, maxStarsInACell);
                }
                ++counter;

            }
            //++voxel.nStars;
            voxel.has_data = GaiaVolumeDataLayout::HAS_DATA::yes;

            rawVolume.set(voxelCell, voxel);
        }
    }
    onProgress(0.8f);

    //Compute the average of each star, and find the global minimum and maximum limits for each column
    rawVolume.forEachVoxel([&](glm::uvec3 const& cell, GaiaVolumeDataLayout const& cellData) {
        if (!cellData.containData())
            return; 
        GaiaVolumeDataLayout avgData{ cellData };

        int index{ 0 };
        for (VoxelDataLayout& parameter : avgData.data) {
            if (!parameter.isNaNData) {
                parameter.avgData = cellData.data[index].avgData / static_cast<double>(parameter.nStars);

                //Update the minimum and maximum limits for each parameter. 
                Limits& limits{ parameterLimits[index] };
                limits.avg.first = std::min(limits.avg.first, parameter.avgData);
                limits.avg.second = std::max(limits.avg.second, parameter.avgData);

                limits.min.first = std::min(limits.min.first, cellData.data[index].minData);
                limits.min.second = std::max(limits.min.second, cellData.data[index].minData);

                limits.max.first = std::min(limits.max.first, cellData.data[index].maxData);
                limits.max.second = std::max(limits.max.second, cellData.data[index].maxData);
            }
            ++index;
        }
        
        rawVolume.set(cell, avgData);
    });

    //for (const Limits& limit : parameterLimits) {
    //    std::cout << "avg min: " << limit.avg.first << " max : " << limit.avg.second << '\n'
    //        <<
    //}
    int ctr = 0;
    std::vector<std::pair<float, float>> comparison;
    //Normalize the data to [0,1]
    if (_normalize) {
        rawVolume.forEachVoxel([&](glm::uvec3 const& cell, GaiaVolumeDataLayout const& cellData) {
            if (!cellData.containData())
                return;

            //This is just plain stupid but cellData is const and we need to modify it
            GaiaVolumeDataLayout voxel = rawVolume.get(cell);

            int index{ 0 };
            //(value - min) / (max - min) for each parameter column
            for (VoxelDataLayout& parameter : voxel.data) {
                if (!parameter.isNaNData) {
                    float avg = parameter.avgData;
                    parameter.avgData = ((parameter.avgData - parameterLimits[index].avg.first) / (parameterLimits[index].avg.second - parameterLimits[index].avg.first));
                    parameter.minData = ((parameter.minData - parameterLimits[index].min.first) / (parameterLimits[index].min.second - parameterLimits[index].min.first));
                    parameter.avgData = ((parameter.maxData - parameterLimits[index].max.first) / (parameterLimits[index].max.second - parameterLimits[index].max.first));

                    if (index == 3 && ctr < 10) {
                        comparison.push_back({ avg, parameter.avgData });
                        ++ctr;
                        //LDEBUG("Voxel: {}: nel before: {}, after {}", ctr, avg, parameter.avgData);
                    }
                }
                ++index;
            }
            rawVolume.set(cell, voxel);
        });
    }

    //Write data to file
    std::filesystem::path const directory = _rawVolumeOutputPath.parent_path();
    if (!std::filesystem::is_directory(directory)) {
        std::filesystem::create_directories(directory);
    }

    volume::RawVolumeWriter<GaiaVolumeDataLayout> writer(_rawVolumeOutputPath.string());
    writer.write(rawVolume);
    onProgress(0.9f);
    
    //Write metadata to file
    volume::RawVolumeMetadata metadata;
    metadata.time = Time::convertTime(_time);
    metadata.dimensions = _dimensions;
    metadata.hasDomainUnit = true;
    metadata.domainUnit = "kParsec";
    metadata.hasValueUnit = true;
    metadata.valueUnit = "K";
    metadata.gridType = volume::VolumeGridType::Cartesian;
    metadata.hasDomainBounds = true;
    metadata.lowerDomainBound = _lowerDomainBound;
    metadata.upperDomainBound = _upperDomainBound;
    metadata.hasValueRange = true;
    metadata.hasfileheaders = true;
    metadata.fileheaders = std::move(_fileHeaders);
    
    ghoul::Dictionary outputDictionary = metadata.dictionary();
    std::string metadataString = ghoul::formatLua(outputDictionary);

    std::fstream f(_dictionaryOutputPath, std::ios::out);
    f << "return " << metadataString;
    f.close();
    onProgress(1.0f);

    for (int i = 0; i < headers.size(); i++)
    {
        Limits const& limit = parameterLimits[i];
        LDEBUG(fmt::format("Header {}: avg: min/max {},{}, min: min/max {},{}, max: min/max {},{}",
            headers[i], limit.avg.first, limit.avg.second, limit.min.first,
            limit.min.second, limit.max.first, limit.max.second)
        );
    }
    LDEBUG(fmt::format("Max stars in one cell: {}", maxStarsInACell));

    LDEBUG(fmt::format("number of data points read {}", dataTable.size()));
    ctr = 0;
    for (auto c : comparison) {
        LDEBUG(fmt::format("Voxel: {}: nel before: {} after: {}", ctr, c.first, c.second));
        ++ctr;
    }

    //LERROR("Nstars read: " + std::to_string(nStarsRead));
}

}// namespace 
