/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/volume/tasks/generaterawvolumetask.h>

#include <modules/volume/rawvolume.h>
#include <modules/volume/rawvolumemetadata.h>
#include <modules/volume/rawvolumewriter.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <ghoul/misc/defer.h>
#include <filesystem>
#include <fstream>

namespace {
    struct [[codegen::Dictionary(GenerateRawVolumeTask)]] Parameters {
        // The Lua function used to compute the cell values
        std::optional<std::string> valueFunction [[codegen::annotation("A Lua expression that returns a "
            "function taking three numbers as arguments (x, y, z) and returning a "
            "number")]];

        // The path to the SPECK file that contains information about the volume
        // object being rendered
        std::optional<std::string> file;

        // The raw volume file to export data to
        std::string rawVolumeOutput [[codegen::annotation("A valid filepath")]];

        // The lua dictionary file to export metadata to
        std::string dictionaryOutput [[codegen::annotation("A valid filepath")]];

        // The timestamp that is written to the metadata of this volume
        std::string time;

        // A vector representing the number of cells in each dimension
        glm::ivec3 dimensions;

        // A vector representing the lower bound of the domain
        glm::dvec3 lowerDomainBound;

        // A vector representing the upper bound of the domain
        glm::dvec3 upperDomainBound;
    };
#include "generaterawvolumetask_codegen.cpp"
} // namespace

namespace openspace::volume {

documentation::Documentation GenerateRawVolumeTask::Documentation() {
    return codegen::doc<Parameters>("generate_raw_volume_task");
}

GenerateRawVolumeTask::GenerateRawVolumeTask(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _rawVolumeOutputPath = absPath(p.rawVolumeOutput);
    _dictionaryOutputPath = absPath(p.dictionaryOutput);
    _dimensions = p.dimensions;
    _time = p.time;
    _lowerDomainBound = p.lowerDomainBound;
    _upperDomainBound = p.upperDomainBound;

    if (p.file.has_value()) {
        _file = absPath(*p.file).string();
    }
    if (p.valueFunction.has_value()) {
        _valueFunctionLua = *p.valueFunction;
    }
    _hasFile = p.file.has_value();
    _hasFunction = p.valueFunction.has_value();
}

std::string GenerateRawVolumeTask::description() {
    return fmt::format(
        "Generate a raw volume with dimenstions: ({}, {}, {}). For each cell, set the "
        "value by evaluating the file `{}` or lua function: `{}`, with three arguments (x, y, z) "
        "ranging from ({}, {}, {}) to ({}, {}, {}). Write raw volume data into {} and "
        "dictionary with metadata to {}",
        _dimensions.x, _dimensions.y, _dimensions.z, _file, _valueFunctionLua,
        _lowerDomainBound.x, _lowerDomainBound.y, _lowerDomainBound.z,
        _upperDomainBound.x, _upperDomainBound.y, _upperDomainBound.z,
        _rawVolumeOutputPath, _dictionaryOutputPath
    );
}

void GenerateRawVolumeTask::perform(const Task::ProgressCallback& progressCallback) {
    // Spice kernel is required for time conversions.
    // Todo: Make this dependency less hard coded.
    SpiceManager::KernelHandle kernel = SpiceManager::ref().loadKernel(
        absPath("${DATA}/assets/spice/naif0012.tls").string()
    );

    defer {
        SpiceManager::ref().unloadKernel(kernel);
    };

    volume::RawVolume<float> rawVolume(_dimensions);
    progressCallback(0.1f);

    float minVal = std::numeric_limits<float>::max();
    float maxVal = std::numeric_limits<float>::min();

    glm::vec3 domainSize = _upperDomainBound - _lowerDomainBound;

    if (!_hasFile && !_hasFunction) {
        LINFOC("GenerateRawVolumeTask", "Either a data file or a function is required to generate this volume task!");
    }
    else if (_hasFile) {
        // _dataset = speck::data::loadFileWithCache(_speckFile);
        std::ifstream data(_file);
        if (!data) {
            LINFOC("GenerateRawVolumeTask", "Error opening output file");
        };

        // Save volume values in 1D vector and get index of each coordinates (x,y,z) by:
        // index = x + dim_x*y + dim_x*dim_y*z
        // coord_value = vector[ index ]
        std::vector<float> flatten_volume_vector;
        std::string line;

        while (std::getline(data, line)) {
            flatten_volume_vector.push_back(std::stof(line));
        };


        rawVolume.forEachVoxel([&](glm::uvec3 cell, float) {
            int index = cell.x + (_dimensions.x * cell.y) + (_dimensions.x * _dimensions.y * cell.z);
            float value = flatten_volume_vector[index];
            rawVolume.set(cell, value);

            minVal = std::min(minVal, value);
            maxVal = std::max(maxVal, value);
        });

    }
    else{

        ghoul::lua::LuaState state;
        ghoul::lua::runScript(state, _valueFunctionLua);

        #if (defined(NDEBUG) || defined(DEBUG))
            ghoul::lua::verifyStackSize(state, 1);
        #endif

        int functionReference = luaL_ref(state, LUA_REGISTRYINDEX);

        #if (defined(NDEBUG) || defined(DEBUG))
            ghoul::lua::verifyStackSize(state, 0);
        #endif

        rawVolume.forEachVoxel([&](glm::uvec3 cell, float) {
            glm::vec3 coord = _lowerDomainBound +
                glm::vec3(cell) / glm::vec3(_dimensions) * domainSize;

        #if (defined(NDEBUG) || defined(DEBUG))
                ghoul::lua::verifyStackSize(state, 0);
        #endif
            lua_rawgeti(state, LUA_REGISTRYINDEX, functionReference);

            lua_pushnumber(state, coord.x);
            lua_pushnumber(state, coord.y);
            lua_pushnumber(state, coord.z);

        #if (defined(NDEBUG) || defined(DEBUG))
                ghoul::lua::verifyStackSize(state, 4);
        #endif

            if (lua_pcall(state, 3, 1, 0) != LUA_OK) {
                return;
            }

            float value = static_cast<float>(luaL_checknumber(state, 1));
            lua_pop(state, 1);
            rawVolume.set(cell, value);

            minVal = std::min(minVal, value);
            maxVal = std::max(maxVal, value);
        });

        luaL_unref(state, LUA_REGISTRYINDEX, functionReference);

    }

    const std::filesystem::path directory = _rawVolumeOutputPath.parent_path();
    if (!std::filesystem::is_directory(directory)) {
        std::filesystem::create_directories(directory);
    }

    volume::RawVolumeWriter<float> writer(_rawVolumeOutputPath.string());
    writer.write(rawVolume);

    progressCallback(0.9f);

    RawVolumeMetadata metadata;
    metadata.time = Time::convertTime(_time);
    metadata.dimensions = _dimensions;
    metadata.hasDomainUnit = true;
    metadata.domainUnit = "m";
    metadata.hasValueUnit = true;
    metadata.valueUnit = "K";
    metadata.gridType = VolumeGridType::Cartesian;
    metadata.hasDomainBounds = true;
    metadata.lowerDomainBound = _lowerDomainBound;
    metadata.upperDomainBound = _upperDomainBound;
    metadata.hasValueRange = true;
    metadata.minValue = minVal;
    metadata.maxValue = maxVal;

    ghoul::Dictionary outputDictionary = metadata.dictionary();
    std::string metadataString = ghoul::formatLua(outputDictionary);

    std::fstream f(_dictionaryOutputPath, std::ios::out);
    f << "return " << metadataString;
    f.close();

    progressCallback(1.0f);
}

} // namespace openspace::volume
