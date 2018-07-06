/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <ghoul/misc/defer.h>

#include <fstream>

namespace {
    constexpr const char* KeyRawVolumeOutput = "RawVolumeOutput";
    constexpr const char* KeyDictionaryOutput = "DictionaryOutput";
    constexpr const char* KeyDimensions = "Dimensions";
    constexpr const char* KeyTime = "Time";
    constexpr const char* KeyValueFunction = "ValueFunction";
    constexpr const char* KeyLowerDomainBound = "LowerDomainBound";
    constexpr const char* KeyUpperDomainBound = "UpperDomainBound";

    constexpr const char* KeyMinValue = "MinValue";
    constexpr const char* KeyMaxValue = "MaxValue";
} // namespace

namespace openspace {
namespace volume {

GenerateRawVolumeTask::GenerateRawVolumeTask(const ghoul::Dictionary& dictionary)
{
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "GenerateRawVolumeTask"
    );

    _rawVolumeOutputPath = absPath(dictionary.value<std::string>(KeyRawVolumeOutput));
    _dictionaryOutputPath = absPath(dictionary.value<std::string>(KeyDictionaryOutput));
    _dimensions = glm::uvec3(dictionary.value<glm::vec3>(KeyDimensions));
    _time = dictionary.value<std::string>(KeyTime);
    _valueFunctionLua = dictionary.value<std::string>(KeyValueFunction);
    _lowerDomainBound = dictionary.value<glm::vec3>(KeyLowerDomainBound);
    _upperDomainBound = dictionary.value<glm::vec3>(KeyUpperDomainBound);
}

std::string GenerateRawVolumeTask::description() {
    return "Generate a raw volume with dimenstions: (" +
        std::to_string(_dimensions.x) + ", " +
        std::to_string(_dimensions.y) + ", " +
        std::to_string(_dimensions.z) + "). " +
        "For each cell, set the value by evaluating the lua function: " +
        "`" + _valueFunctionLua + "`, with three arguments (x, y, z) ranging from " +
        "(" + std::to_string(_lowerDomainBound.x) + ", "
        + std::to_string(_lowerDomainBound.y) + ", " +
        std::to_string(_lowerDomainBound.z) + ") to (" +
        std::to_string(_upperDomainBound.x) + ", " +
        std::to_string(_upperDomainBound.y) + ", " +
        std::to_string(_upperDomainBound.z) + ")." +
        "Write raw volume data into " + _rawVolumeOutputPath +
        " and dictionary with metadata to " + _dictionaryOutputPath;
}

void GenerateRawVolumeTask::perform(const Task::ProgressCallback& progressCallback) {
    // Spice kernel is required for time conversions.
    // Todo: Make this dependency less hard coded.
    SpiceManager::KernelHandle kernel =
        SpiceManager::ref().loadKernel(absPath("${DATA}/assets/spice/naif0012.tls"));

    defer {
        SpiceManager::ref().unloadKernel(kernel);
    };

    volume::RawVolume<float> rawVolume(_dimensions);
    progressCallback(0.1f);

    ghoul::lua::LuaState state;
    ghoul::lua::runScript(state, _valueFunctionLua);

    ghoul::lua::verifyStackSize(state, 1);

    int functionReference = luaL_ref(state, LUA_REGISTRYINDEX);

    ghoul::lua::verifyStackSize(state, 0);

    glm::vec3 domainSize = _upperDomainBound - _lowerDomainBound;


    float minVal = std::numeric_limits<float>::max();
    float maxVal = std::numeric_limits<float>::min();

    rawVolume.forEachVoxel([&](glm::uvec3 cell, float) {
        glm::vec3 coord = _lowerDomainBound +
            glm::vec3(cell) / glm::vec3(_dimensions) * domainSize;

        ghoul::lua::verifyStackSize(state, 0);
        lua_rawgeti(state, LUA_REGISTRYINDEX, functionReference);

        lua_pushnumber(state, coord.x);
        lua_pushnumber(state, coord.y);
        lua_pushnumber(state, coord.z);

        ghoul::lua::verifyStackSize(state, 4);

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

    ghoul::filesystem::File file(_rawVolumeOutputPath);
    const std::string directory = file.directoryName();
    if (!FileSys.directoryExists(directory)) {
        FileSys.createDirectory(directory, ghoul::filesystem::FileSystem::Recursive::Yes);
    }

    volume::RawVolumeWriter<float> writer(_rawVolumeOutputPath);
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
    ghoul::DictionaryLuaFormatter formatter;
    std::string metadataString = formatter.format(outputDictionary);

    std::fstream f(_dictionaryOutputPath, std::ios::out);
    f << "return " << metadataString;
    f.close();

    progressCallback(1.0f);
}

documentation::Documentation GenerateRawVolumeTask::documentation() {
    using namespace documentation;
    return {
        "GenerateRawVolumeTask",
        "generate_raw_volume_task",
        {
            {
                "Type",
                new StringEqualVerifier("GenerateRawVolumeTask"),
                Optional::No,
                "The type of this task",
            },
            {
                KeyValueFunction,
                new StringAnnotationVerifier("A lua expression that returns a function "
                "taking three numbers as arguments (x, y, z) and returning a number."),
                Optional::No,
                "The lua function used to compute the cell values",
            },
            {
                KeyRawVolumeOutput,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "The raw volume file to export data to",
            },
            {
                KeyDictionaryOutput,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "The lua dictionary file to export metadata to",
            },
            {
                KeyDimensions,
                new DoubleVector3Verifier,
                Optional::No,
                "A vector representing the number of cells in each dimension",
            },
            {
                KeyLowerDomainBound,
                new DoubleVector3Verifier,
                Optional::No,
                "A vector representing the lower bound of the domain"
            },
            {
                KeyUpperDomainBound,
                new DoubleVector3Verifier,
                Optional::No,
                "A vector representing the upper bound of the domain"
            }
        }
    };
}

} // namespace volume
} // namespace openspace
