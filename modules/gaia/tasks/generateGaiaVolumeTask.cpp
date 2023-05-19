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



        //// The Lua function used to compute the cell values
        //std::string valueFunction [[codegen::annotation("A Lua expression that returns a "
        //    "function taking three numbers as arguments (x, y, z) and returning a "
        //    "number")]];

namespace {
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
    };
#include "generategaiavolumetask_codegen.cpp"
} // namespace

namespace openspace::gaiavolume {

documentation::Documentation GenerateGaiaVolumeTask::Documentation() {
    return codegen::doc<Parameters>("generate_gaia_volume_task");
}

GenerateGaiaVolumeTask::GenerateGaiaVolumeTask(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _inputFilePath = absPath(p.dataInputPath);
    _rawVolumeOutputPath = absPath(p.rawVolumeOutput);
    _dictionaryOutputPath = absPath(p.dictionaryOutput);
    _dimensions = p.dimensions;
    _time = p.time;
    _lowerDomainBound = glm::vec3(std::numeric_limits<float>::max());
    _upperDomainBound = glm::vec3(std::numeric_limits<float>::min());
}

std::string GenerateGaiaVolumeTask::description()
{
    return std::string();
}

void GenerateGaiaVolumeTask::perform(const Task::ProgressCallback& onProgress)
{
    int nStarsRead = 0;
    //Open file , read and store content of file in data structure
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

    //Starting with just one float value in the volume
    double minFirstValue{ std::numeric_limits<double>::max() };
    double maxFirstValue{ std::numeric_limits<double>::min() };

    double minSecondValue{ std::numeric_limits<double>::max() };
    double maxSecondValue{ std::numeric_limits<double>::min() };

    //find min/max x,y,z of position - domain bounds of the volume
    for (auto it{ dataTable.begin() + 1 }; it != dataTable.end(); it++)
    {
        const std::vector<std::string>& row = *it;
        //Convert row of data from string values to floats
        std::vector<float> starValues{};
        std::transform(row.begin(), row.end(), std::back_inserter(starValues),
            [](const std::string& s) {
                if (s.size() == 0)
                    return 0.0f;

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

        float value = starValues[_fileHeaders["fe_h"]];

        data.push_back(std::move(starValues));
    }
    onProgress(0.5f);

    volume::RawVolume<GaiaVolumeDataLayout> rawVolume(_dimensions);

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

        glm::uvec3 voxelCell{ i,j,k };

        GaiaVolumeDataLayout voxel = rawVolume.get(voxelCell);
        double value{ star[_fileHeaders["fe_h"]] };
        double radius{ std::abs(star[_fileHeaders["pz"]])};

        voxel.firstValue += value;
        voxel.secondValue += radius;
        ++voxel.nStars;

        rawVolume.set(voxelCell, voxel);
    }
    onProgress(0.8f);

    //Take the average of every voxel
    rawVolume.forEachVoxel([&](glm::uvec3 const& cell, GaiaVolumeDataLayout const& cellData) {
        if (cellData.nStars == 0)
            return; 
        GaiaVolumeDataLayout avgData;
        avgData.firstValue = cellData.firstValue / cellData.nStars;
        avgData.secondValue = cellData.secondValue / cellData.nStars;
        avgData.nStars = cellData.nStars;
        rawVolume.set(cell, avgData);

        minFirstValue = std::min(minFirstValue, avgData.firstValue);
        maxFirstValue = std::max(maxFirstValue, avgData.firstValue);

        minSecondValue = std::min(minSecondValue, avgData.secondValue);
        maxSecondValue = std::max(maxSecondValue, avgData.secondValue);

        nStarsRead += avgData.nStars;
        //LERROR("Stars: " + std::to_string(avgData.nStars));
    });

    //Bring the data values in the range 0.2 - 1.0
    rawVolume.forEachVoxel([&](glm::uvec3 const& cell, GaiaVolumeDataLayout const& cellData) {
        if (cellData.nStars == 0)
            return;

        GaiaVolumeDataLayout voxel = rawVolume.get(cell);
        float newMin = 0.2f;
        float newMax = 1.0f;
        float newRange = newMax - newMin;
        float oldRange = maxFirstValue - minFirstValue;
        voxel.firstValue = (((voxel.firstValue - minFirstValue) * newRange) / oldRange + newMin);

        oldRange = maxSecondValue - minSecondValue;
        voxel.secondValue = (((voxel.secondValue - minFirstValue) * newRange) / oldRange + newMin);


        rawVolume.set(cell, voxel);


        //rawVolume.set(cell, avgData);

        //LERROR("Stars: " + std::to_string(avgData.nStars));
        });

    /*int size = 10;
    for(int i = 0; i < size; ++i){
        for (int j = 0; j < size; ++j) {
            for (int k = 0; k < size; ++k) {
                GaiaVolumeDataLayout voxel;
                voxel.firstValue = 0.75;
                voxel.nStars = 1;
                rawVolume.set(glm::uvec3(i, j, k), voxel);
            }
        }
    }*/

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
    metadata.minValue = minFirstValue;
    metadata.maxValue = maxFirstValue;

    ghoul::Dictionary outputDictionary = metadata.dictionary();
    std::string metadataString = ghoul::formatLua(outputDictionary);

    std::fstream f(_dictionaryOutputPath, std::ios::out);
    f << "return " << metadataString;
    f.close();
    onProgress(1.0f);

    LERROR("Nstars read: " + std::to_string(nStarsRead));


}

}// namespace 
