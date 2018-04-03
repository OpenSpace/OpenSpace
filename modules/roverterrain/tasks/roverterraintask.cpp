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

#include <modules/roverterrain/tasks/roverterraintask.h>

#include <modules/roverterrain/filehandler/txtreader.h>
#include <modules/roverterrain/generation/collapsedmeshgeneration.h>

#include <ghoul/logging/logmanager.h>

#include <openspace/openspace.h>
#include <openspace/documentation/core_registration.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/util/synchronizationwatcher.h>
#include <openspace/scripting/scriptengine.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <string>
#include <chrono>
#include <iostream>
#include <filesystem>

#include <boost/filesystem.hpp>

namespace {
    const char* KeyAsset = "Asset";
    const std::string _loggerCat = "RoverTerrainTask";
} // namespace

namespace openspace {
RoverTerrainTask::RoverTerrainTask(const ghoul::Dictionary& dictionary) {
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "RoverTerrainTask"
        );
    std::string sourcePath = dictionary.value<std::string>(KeyAsset);

    std::replace(sourcePath.begin(), sourcePath.end(), '/', '\\');

    _asset = sourcePath;

    std::string outputPath = dictionary.value<std::string>("OutputPath");

    std::replace(outputPath.begin(), outputPath.end(), '/', '\\');
    
    _outputPath = outputPath;
}
std::string RoverTerrainTask::description() {
    return "Rover asset " + _asset;
}

void RoverTerrainTask::perform(const Task::ProgressCallback& progressCallback) {
    progressCallback(0.0f);
    std::string txtPath = _asset + "binaries.txt";
    std::vector<std::string> filenames;
    TxtReader::read(txtPath, filenames);
    ghoul::Dictionary dic;
    dic.setValue("_asset", _asset);
    dic.setValue("_outputPath", _outputPath);
    int k = 0;
    for (auto& p : filenames) {
        dic.setValue("binaryfile" + std::to_string(k), p);
        k++;
    }

    CollapsedMeshGeneration::generateMeshFromBinaries(dic);
}

documentation::Documentation RoverTerrainTask::documentation() {
    using namespace documentation;
    return {
        "RoverTerrainTask",
        "rover_terrain_task",
    {
        {
            "Type",
            new StringEqualVerifier("RoverTerrainTask"),
            Optional::No,
            "The type of this task"
        },
            {
                KeyAsset,
                new StringAnnotationVerifier("A file path to an asset"),
                Optional::No,
                "The asset file to sync"
            }
    }
    };
}
}
