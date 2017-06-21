/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/gpufieldlines/util/gpufieldlinesmanager.h>

#include <openspace/util/time.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
// #include <ghoul/misc/assert.h>

#include <algorithm>
#include <fstream>
#include <memory>

#include <ccmc/Kameleon.h>

#include <modules/gpufieldlines/util/gpufieldlinesstate.h>

namespace {
    const std::string _loggerCat = "GpuFieldlinesManager";
    using RawPath = ghoul::filesystem::FileSystem::RawPath;
    using FileSystem = ghoul::filesystem::FileSystem;
    using Sort = ghoul::filesystem::Directory::Sort;


}

namespace openspace {

GpuFieldlinesManager::GpuFieldlinesManager() {}

GpuFieldlinesManager::~GpuFieldlinesManager() {}

bool GpuFieldlinesManager::getSeedPointsFromFile(
        const std::string& path, std::vector<glm::vec3>& outVec) {

    std::string seedPointsSourceFile;
    seedPointsSourceFile = absPath(path);

    if ( !FileSys.fileExists(seedPointsSourceFile, RawPath::Yes) ) {
        LERROR("The file '" << seedPointsSourceFile << "' could not be found!");
        return false;
    }

    std::ifstream seedFile(FileSys.relativePath(seedPointsSourceFile));
    if (!seedFile.good()) {
        LERROR("Could not open seed points file '" << seedPointsSourceFile << "'");
        return false;
    }

    LINFO("Reading seed points from file '" << seedPointsSourceFile << "'");
    std::string line;
    glm::vec3 point;
    while (std::getline(seedFile, line)) {
        std::stringstream s(line);
        s >> point.x;
        s >> point.y;
        s >> point.z;
        outVec.push_back(std::move(point));
    }
    return true;
}

bool GpuFieldlinesManager::getCdfFilePaths(
        const std::string& pathToCdfDirectory,
        std::vector<std::string>& outCdfFilePaths) {

    std::string absFolderPath;
    absFolderPath = absPath(pathToCdfDirectory);

    if ( !FileSys.directoryExists(absFolderPath) ) {
        LERROR("The folder '" << absFolderPath << "' could not be found!");
        return false;
    }

    // Get absolute path to
    ghoul::filesystem::Directory cdfDirectory(absFolderPath, RawPath::Yes);
    outCdfFilePaths = cdfDirectory.read(FileSystem::Recursive::Yes, Sort::Yes);

    outCdfFilePaths.erase(std::remove_if(
            outCdfFilePaths.begin(), outCdfFilePaths.end(), [](std::string s) {
                    std::string sub = s.substr(s.length()-4, 4);
                    std::transform(sub.begin(), sub.end(), sub.begin(), ::tolower);
                    return sub != ".cdf";
                }), outCdfFilePaths.end());

    return true;
}

bool GpuFieldlinesManager::getGpuFieldlinesState(
        const std::string& pathToCdfFile,
        const std::string& tracingVariable,
        const std::vector<glm::vec3>& inSeedPoints,
        const int& maxIterations,
        const bool shouldResample,
        const int& numResamples,
        const int& resamplingOption,
        std::vector<double>& startTimes,
        GpuFieldlinesState& outGpuFieldlinesStates) {


    //TODO PREALLOCATE outFieldlinesState._vertexPositions if resampling!!!!!!!!!!
    std::unique_ptr<ccmc::Kameleon> kameleon = std::make_unique<ccmc::Kameleon>();
    long kamStatus = kameleon->open(pathToCdfFile);

    if (kamStatus != ccmc::FileReader::OK) {
        return false;
    }
    // TODO: check model
    // == "enlil"; // TODO, specify in Lua and confirm?

    LDEBUG("Successfully created a Kameleon Object from file: " << pathToCdfFile);

    double startTime = getTime(kameleon.get());
    startTimes.push_back(startTime);
    LDEBUG("State will start at " << startTime << " (J2000 Time)");

    bool status;
    // TODO: check status
    status = traceGpuFieldlines(kameleon.get(),
                             tracingVariable,
                             inSeedPoints,maxIterations,
                             shouldResample,
                             numResamples,
                             resamplingOption,
                             outGpuFieldlinesStates);
    return status;
}

bool GpuFieldlinesManager::traceGpuFieldlines(
        ccmc::Kameleon* kameleon,
        const std::string& tracingVariable,
        const std::vector<glm::vec3>& inSeedPoints,
        const int& maxIterations,
        const bool shouldResample,
        const int& numResamples,
        const int& resamplingOption,
        GpuFieldlinesState& outGpuFieldlinesStates) {

    const std::string model = kameleon->getModelName();
    kameleon->loadVariable(tracingVariable);

    ccmc::Tracer tracer(kameleon);

    tracer.setMaxIterations(maxIterations);

    if (model == "batsrus") {
        tracer.setInnerBoundary(1.1f); // TODO specify in Lua
    } else if (model == "enlil") {
        tracer.setInnerBoundary(0.11f); // TODO specify in Lua
    } else {
        LERROR("OpenSpace's fieldlines  currently only supports the " <<
                "BATSRUS and ENLIL models");
        return false;
    }

    // tracer.setUseRegionOfInterest(true);
    // tracer.setRegionOfInterest(ccmc::Point3f(-20.f, -5.f, -5.f), ccmc::Point3f(20.f, 5.f, 5.f));

    int lineStart = 0;
    for (glm::vec3 seedPoint : inSeedPoints) {
        // A ccmc::Fieldline contains much more info than we need here,

        // but might be sneeded in future.
        ccmc::Fieldline ccmcFieldline = tracer.bidirectionalTrace(tracingVariable,
                                                                  seedPoint.x,
                                                                  seedPoint.y,
                                                                  seedPoint.z);

        if (!addLineToState(ccmcFieldline,
                            model,
                            shouldResample,
                            numResamples,
                            resamplingOption,
                            lineStart,
                            outGpuFieldlinesStates)) {
            return false;
        }
    }

    // TODO check that everything worked out?
    return true;
}

bool GpuFieldlinesManager::addLineToState(ccmc::Fieldline& ccmcFieldline,
                                               const std::string& model,
                                               const bool shouldResample,
                                               const int& numResamples,
                                               const int& resamplingOption,
                                               int& lineStart,
                                               GpuFieldlinesState& outGpuFieldlinesStates) {
    int lineCount = 0;

    if (shouldResample) {
        outGpuFieldlinesStates._vertexPositions.reserve(numResamples);
        // ResamplingOption = 1, 2 and 3 uses CCMC's built in resampling which requires
        // the ccmc::Fieldline variable. Resamples before conversion to glm::vec3!
        // Other options requires the transformation to the correct coordinate system first
        if (resamplingOption < 4) {
            if (resamplingOption < 1) {
                LERROR("Not a valid resampling option! Lowest resampling option = 1.");
                return false;
            }

            if (model == "enlil") {
                LWARNING("CCMC's fieldline resampling doesn't account for spherical " <<
                         "coordinates. Consider selecting Resampling Option 4 instead!");
            }
            resampleCcmcFieldline(numResamples, resamplingOption, ccmcFieldline);
        }

        lineCount = numResamples;
    } else {
        lineCount = static_cast<int>(ccmcFieldline.size());
    }

    outGpuFieldlinesStates._lineStart.push_back(lineStart);
    outGpuFieldlinesStates._lineCount.push_back(lineCount);
    const float R_E_TO_METER = 6371000.f; // Earth radius
    const float R_S_TO_METER = 695700000.f; // Sun radius
    const float A_U_TO_METER = 149597871000.f; // Astronomical Units
    const float DEG_TO_RAD = 3.14159265359f / 180.f;

    const std::vector<ccmc::Point3f> positions = ccmcFieldline.getPositions();
    // TODO FIX ALL OF THIS.. works but is ugly code
    // Add line points to GpuFieldlinesState. Also resample if ResamplingOption == 4
    if (model == "batsrus") {
        // Scale all values
        if (shouldResample && resamplingOption == 4) {
            std::vector<glm::vec3> tempVec;
            std::transform(positions.begin(), positions.end(),
                    std::back_inserter(tempVec),
                            [&R_E_TO_METER](const ccmc::Point3f& p) {
                                return glm::vec3(p.component1 * R_E_TO_METER,
                                                 p.component2 * R_E_TO_METER,
                                                 p.component3 * R_E_TO_METER);
                            });

            int seedIndex = ccmcFieldline.getStartIndex();
            centerSeedPointResampling(numResamples, seedIndex, tempVec,
                    outGpuFieldlinesStates._vertexPositions);
        } else {
            std::transform(positions.begin(), positions.end(),
                    std::back_inserter(outGpuFieldlinesStates._vertexPositions),
                            [&R_E_TO_METER](const ccmc::Point3f& p) {
                                return glm::vec3(p.component1 * R_E_TO_METER,
                                                 p.component2 * R_E_TO_METER,
                                                 p.component3 * R_E_TO_METER);
                            });
        }
    } else if (model == "enlil") {
        if (shouldResample && resamplingOption == 4) {
            std::vector<glm::vec3> tempVec;
            std::transform(positions.begin(), positions.end(),
                    std::back_inserter(tempVec),
                            [&A_U_TO_METER, &DEG_TO_RAD](const ccmc::Point3f& p) {
                                float r         = A_U_TO_METER * p.component1;
                                float lat_rad   = DEG_TO_RAD   * p.component2;
                                float lon_rad   = DEG_TO_RAD   * p.component3;
                                float r_cosLat  = r * cos(lat_rad);
                                return glm::vec3(r_cosLat * cos(lon_rad),
                                                 r_cosLat * sin(lon_rad),
                                                 r * sin(lat_rad));
                            });

            int seedIndex = ccmcFieldline.getStartIndex();
            centerSeedPointResampling(numResamples, seedIndex, tempVec,
                    outGpuFieldlinesStates._vertexPositions);
        } else {
            std::transform(positions.begin(), positions.end(),
                    std::back_inserter(outGpuFieldlinesStates._vertexPositions),
                            [&A_U_TO_METER, &DEG_TO_RAD](const ccmc::Point3f& p) {
                                float r         = A_U_TO_METER * p.component1;
                                float lat_rad   = DEG_TO_RAD   * p.component2;
                                float lon_rad   = DEG_TO_RAD   * p.component3;
                                float r_cosLat  = r * cos(lat_rad);
                                return glm::vec3(r_cosLat * cos(lon_rad),
                                                 r_cosLat * sin(lon_rad),
                                                 r * sin(lat_rad));
                            });
        }
    } else {
        LERROR("OpenSpace's fieldlines  currently only supports the " <<
                "BATSRUS and ENLIL models");
        return false;
    }

    // if (shouldResample && resamplingOption > 3 ) {
    //     if (resamplingOption == 4) {
    //         LDEBUG("Option 4: Fieldline idx 0 = " << positions[0].component1 * R_E_TO_METER<< ", " << positions[0].component2 * R_E_TO_METER<< ", " << positions[0].component3* R_E_TO_METER );
    //         int seedIndex = ccmcFieldline.getStartIndex();
    //         centerSeedPointResampling(lineStart,
    //                                   static_cast<int>(ccmcFieldline.size()),
    //                                   numResamples,
    //                                   seedIndex,
    //                                   outGpuFieldlinesStates, ccmcFieldline);
    //         LDEBUG("Option 4: \tvertexPositions idx " << lineStart << " = " << outGpuFieldlinesStates._vertexPositions[lineStart].x << ", " << outGpuFieldlinesStates._vertexPositions[lineStart].y << ", " << outGpuFieldlinesStates._vertexPositions[lineStart].z );

    //     } else {
    //         LERROR( "Not a valid resampling option!" <<
    //                 "Only 4 different resampling options are implemented!");
    //         return false;
    //     }
    // }

    lineStart += lineCount;
    return true;
}

// Already traced
void GpuFieldlinesManager::resampleCcmcFieldline(const int& numResamples,
                                                      const int& resamplingOption,
                                                      ccmc::Fieldline& line) {

    int numPoints = line.size();//line.getStartIndex();
    if (numPoints == numResamples || numPoints <= 0) {
        return; // Nothing is needed to be done
    }

    // Resample with the built in functionality in ccmd::Fieldline
    switch (resamplingOption) {
        case 1: {
            line.getLength(numPoints);
            line = line.interpolate(resamplingOption, numResamples);
            break;
        } case 2: {
            // todo: getIntegral?
            line.integrate();
            line = line.interpolate(resamplingOption, numResamples);
            break;
        } case 3: {
            // todo getSomething?
            line = line.interpolate(resamplingOption, numResamples);
            break;
        } default: {
            break;
        }
    }
}

void GpuFieldlinesManager::centerSeedPointResampling(
                                                const int& numResamples,
                                                int& seedPointIdx,
                                                const std::vector<glm::vec3>& line,
                                                std::vector<glm::vec3>& outPositions) {

    if (seedPointIdx != 0) {
        seedPointIdx -= 1; // For some reason ccmc::Fieldline.getStartIndex() is one off
    }

    int numPoints = static_cast<int>(line.size());
    if (numPoints < 1) {
        return;
    }

    const int preSeed  = seedPointIdx;
    const int postSeed = numPoints - seedPointIdx - 1;
    const int n = numResamples / 2; // Final number of samples on either side of seedPoint

    auto seedIterator = (line.begin() + seedPointIdx);
    auto origLineIter = line.begin();

    if (preSeed == 0) {
        // insert 'n' + 1 copies of seedPoint
        for (int i = 0 ; i < n ; ++i) {
            outPositions.push_back(*seedIterator);
        }
    } else {
        const int dif = n - preSeed; // number of points to insert before seedPoint
        const int pointsPerSegment = dif / preSeed; // minimum number of points to add per segment
        const int additionalPoints = dif % preSeed; // total number of additional points

        // For each existing line segment, add necessary number of points
        for (int i = 0; i < preSeed; ++i, ++origLineIter) {
            outPositions.push_back(*origLineIter);

            int numPointsToInsert = pointsPerSegment;
            if (i < additionalPoints) {
                numPointsToInsert += 1;
            }

            const glm::vec3 offset = ( (*(origLineIter+1)) - (*origLineIter) ) *
                                     (1.f / static_cast<float>(numPointsToInsert + 1));

            for (int k = 1; k < numPointsToInsert + 1; ++k) {
                outPositions.push_back(*origLineIter + offset * static_cast<float>(k));
            }
        }
    }

    if (postSeed == 0) {
        // insert 'n'copies of seedPoint
        for (int i = 0 ; i < n + 1 ; ++i) {
            outPositions.push_back(*seedIterator);
        }
    } else {
        const int dif = n - postSeed;
        const int pointsPerSegment = dif / postSeed;
        const int additionalPoints = dif % postSeed;

        for (int i = 0 ; i < postSeed; ++i, ++origLineIter) {
            outPositions.push_back(*origLineIter);

            int numPointsToInsert = pointsPerSegment;
            if (i >= postSeed - additionalPoints) {
                numPointsToInsert += 1;
            }

            const glm::vec3 offset = ( (*(origLineIter+1)) - (*origLineIter) ) *
                                     (1.f / static_cast<float>(numPointsToInsert + 1));

            for (int k = 1; k < numPointsToInsert + 1; ++k) {
                outPositions.push_back(*origLineIter + offset * static_cast<float>(k));
            }
        }
        // Push end point to state vector
        outPositions.push_back(*origLineIter);
    }
    // TODO assertion that we've added the correct number of points and that the
    // seed point is still centered
}


// This funciton resamples the line that's already added to the current state's
// vertex vector. It leaves the already added points in their positions but subdivides
// the segments in-between.
// void GpuFieldlinesManager::centerSeedPointResampling(
//             const int& lineStartIdx,
//             const int& numPoints,
//             const int& numResamples,
//             int& seedPointIdx,
//             GpuFieldlinesState& outGpuFieldlinesStates,
//             ccmc::Fieldline& fl) {

//     // At this stage all points are already stored in outGpuFieldlinesState._vertexPositions
//     // This might make the code a bit difficult to grasp..
//     // TODO: resample at the same time as addding the points to the state to make code more readable

//     // vertexIter point to the first point of the new line that's just been added
//     std::vector<glm::vec3>::iterator vertexIterStart =
//             outGpuFieldlinesStates._vertexPositions.begin() + lineStartIdx;
//     std::vector<glm::vec3>::iterator vertexIterEnd =
//             outGpuFieldlinesStates._vertexPositions.end() - 1;
//     std::vector<glm::vec3>::iterator vertexIterEnd2 =
//             outGpuFieldlinesStates._vertexPositions.begin() + numPoints;
//     std::vector<glm::vec3>::iterator vertexIter =
//             outGpuFieldlinesStates._vertexPositions.begin() + lineStartIdx;

//     if (seedPointIdx != 0) {
//         seedPointIdx -= 1; // For some reason ccmc::Fieldline.getStartIndex() is one off
//     }

//     const int preSeed  = seedPointIdx;
//     const int postSeed = numPoints - seedPointIdx - 1;
//     const int n = numResamples / 2; // Final number of samples on either side of seedPoint

//     if (preSeed == 0) {
//         // TODO insert 'n' copies of seedPoint
//         LERROR("PRE IS IMPLEMENTED ALREADY!!!!!!!!!!");
//         for (int i = 0 ; i < n ; ++i) {
//             outGpuFieldlinesStates._vertexPositions.insert(vertexIter, *vertexIter);
//         }
//     } else {
//         const int dif = n - preSeed; // number of points to insert before seedPoint
//         const int pointsPerSegment = dif / preSeed; // minimum number of points to add per segment
//         const int additionalPoints = dif % preSeed; // total number of additional points

//         // For each existing line segment, add necessary number of points
//         for (int i = 0; i < preSeed; ++i, ++vertexIter) {

//             int numPointsToInsert = pointsPerSegment;
//             if (i < additionalPoints) {
//                 numPointsToInsert += 1;
//             }

//             const glm::vec3 offset = ( (*(vertexIter+1)) - (*vertexIter) ) *
//                                      (1.f / static_cast<float>(numPointsToInsert + 1));

//             for (float k = 1.0; k < numPointsToInsert + 1.f; k += 1.f) {
//                 outGpuFieldlinesStates._vertexPositions.insert(vertexIter + 1,
//                                                             *vertexIter + offset * k);
//             }
//         }
//     }

//     if (postSeed == 0) {
//         // TODO insert 'n'copies of seedPoint
//         LERROR("POST IS ALREADY IMPLEMENTED!!!!!!!!!!");
//         for (int i = 0 ; i < n ; ++i) {
//             outGpuFieldlinesStates._vertexPositions.insert(vertexIter, *vertexIter);
//         }
//     } else {
//         const int dif = n - postSeed;
//         const int pointsPerSegment = dif / postSeed;
//         const int additionalPoints = dif % postSeed;
//         // Insert before seedPoint
//         // For each value insert
//         for (int i = 0 ; i < postSeed; ++i, ++vertexIter) {
//             // Copy/insert existing point to new line

//             int numPointsToInsert = pointsPerSegment;
//             if (i > postSeed - additionalPoints) {
//                 numPointsToInsert += 1;
//             }

//             const glm::vec3 offset = ( (*(vertexIter+1)) - (*vertexIter) ) *
//                                      (1.f / static_cast<float>(numPointsToInsert + 1));

//             for (float k = 1.0; k < numPointsToInsert + 1.f; k += 1.f) {
//                 outGpuFieldlinesStates._vertexPositions.insert(vertexIter + 1,
//                                                             *vertexIter + offset * k);
//                 vertexIter = vertexIterStart + i;
//             }
//         }
//     }
//     // TODO assertion that we've added the correct number of points and that the
//     // seed point is still centered
// }



double GpuFieldlinesManager::getTime(ccmc::Kameleon* kameleon) {
    // Inspiration from 'void KameleonInterpolator::setEphemTime()' which doesn't seem to
    // exist in the version of Kameleon that is included in OpenSpace. Alterations
    // done to fit here.

        std::string seqStartStr;
        double seqStartDbl;
        if (kameleon->doesAttributeExist("start_time")){
            seqStartStr =
                    kameleon->getGlobalAttribute("start_time").getAttributeString();
        } else if (kameleon->doesAttributeExist("tim_crstart_cal")) {
            seqStartStr =
                    kameleon->getGlobalAttribute("tim_crstart_cal").getAttributeString();
        } else {
            LWARNING("No starting time attribute could be found in the .cdf file.\n\t" <<
                    "Starting time is set to 01.JAN.2000 12:00.");
            seqStartDbl = 0.0;
        }

        if (seqStartStr.length() == 19){
            seqStartStr += ".000Z";
        }

        if (seqStartStr.length() == 24){
            seqStartDbl =
                    Time::convertTime(
                            seqStartStr.substr(0, seqStartStr.length() - 2));
        }

        double stateStartOffset;

        if (kameleon->doesAttributeExist("elapsed_time_in_seconds")) {
            stateStartOffset = static_cast<double>(
                    kameleon->getGlobalAttribute(
                            "elapsed_time_in_seconds").getAttributeFloat());
        } else if (kameleon->doesAttributeExist("time_physical_time")) {
            stateStartOffset = static_cast<double>(
                    kameleon->getGlobalAttribute(
                            "time_physical_time").getAttributeFloat());
        } else {
            stateStartOffset = 0.0;
            LWARNING("No time offset attribute could be found in the .cdf file.\n\t" <<
                     "The current state starts the same time as the !");
        }

    return seqStartDbl + stateStartOffset;;
}

} // namsepace openspace
