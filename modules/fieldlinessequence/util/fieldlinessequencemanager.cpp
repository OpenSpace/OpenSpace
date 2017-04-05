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

#include <modules/fieldlinessequence/util/fieldlinessequencemanager.h>

#include <openspace/util/time.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
// #include <ghoul/misc/assert.h>

#include <algorithm>
#include <fstream>
#include <memory>

#include <ccmc/Kameleon.h>

#include <modules/fieldlinessequence/util/fieldlinesstate.h>

namespace {
    const std::string _loggerCat = "FieldlinesSequenceManager";
    using RawPath = ghoul::filesystem::FileSystem::RawPath;
    using FileSystem = ghoul::filesystem::FileSystem;
    using Sort = ghoul::filesystem::Directory::Sort;

    const float R_E_TO_METER = 6371000.f; // Earth radius
    const float R_S_TO_METER = 695700000.f; // Sun radius
    const float A_U_TO_METER = 149597871000.f; // Astronomical Units
    // const float A_U_TO_KM    = 149597871.f; // Astronomical Units
    const float DEG_TO_RAD   = 3.14159265359f / 180.f;
}

namespace openspace {

FieldlinesSequenceManager::FieldlinesSequenceManager() {}

FieldlinesSequenceManager::~FieldlinesSequenceManager() {}

bool FieldlinesSequenceManager::getSeedPointsFromFile(
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

bool FieldlinesSequenceManager::getCdfFilePaths(
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

bool FieldlinesSequenceManager::getFieldlinesState(
        const std::string& pathToCdfFile,
        const std::string& tracingVariable,
        const std::vector<glm::vec3>& inSeedPoints,
        const int& maxIterations,
        const bool shouldResample,
        const int& numResamples,
        const int& resamplingOption,
        std::vector<double>& startTimes,
        FieldlinesState& outFieldlinesStates) {


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
    status = traceFieldlines(kameleon.get(),
                             tracingVariable,
                             inSeedPoints,maxIterations,
                             shouldResample,
                             numResamples,
                             resamplingOption,
                             outFieldlinesStates);
    return status;
}

bool FieldlinesSequenceManager::traceFieldlines(
        ccmc::Kameleon* kameleon,
        const std::string& tracingVariable,
        const std::vector<glm::vec3>& inSeedPoints,
        const int& maxIterations,
        const bool shouldResample,
        const int& numResamples,
        const int& resamplingOption,
        FieldlinesState& outFieldlinesStates) {

    const std::string model = kameleon->getModelName();
    kameleon->loadVariable(tracingVariable);

    ccmc::Tracer tracer(kameleon);

    tracer.setMaxIterations(maxIterations);

    if (model == "batsrus") {
        tracer.setInnerBoundary(1.1f); // TODO specify in Lua
    } else if (model == "enlil") {
        tracer.setInnerBoundary(0.11f); // TODO specify in Lua
    } else {
        LERROR("OpenSpace's fieldlines sequence currently only supports the " <<
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
                            outFieldlinesStates)) {
            return false;
        }
    }

    // TODO check that everything worked out?
    return true;
}

bool FieldlinesSequenceManager::addLineToState(ccmc::Fieldline& ccmcFieldline,
                                               const std::string& model,
                                               const bool shouldResample,
                                               const int& numResamples,
                                               const int& resamplingOption,
                                               int& lineStart,
                                               FieldlinesState& outFieldlinesStates) {
    int lineCount = 0;

    if (shouldResample) {
        outFieldlinesStates._vertexPositions.reserve(numResamples);
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

    outFieldlinesStates._lineStart.push_back(lineStart);
    outFieldlinesStates._lineCount.push_back(lineCount);

    const std::vector<ccmc::Point3f> positions = ccmcFieldline.getPositions();
    // TODO FIX ALL OF THIS.. works but is ugly code
    // Add line points to FieldlinesState. Also resample if ResamplingOption == 4
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
                    outFieldlinesStates._vertexPositions);
        } else {
            std::transform(positions.begin(), positions.end(),
                    std::back_inserter(outFieldlinesStates._vertexPositions),
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
                    outFieldlinesStates._vertexPositions);
        } else {
            std::transform(positions.begin(), positions.end(),
                    std::back_inserter(outFieldlinesStates._vertexPositions),
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
        LERROR("OpenSpace's fieldlines sequence currently only supports the " <<
                "BATSRUS and ENLIL models");
        return false;
    }


    lineStart += lineCount;
    return true;
}

// Already traced
void FieldlinesSequenceManager::resampleCcmcFieldline(const int& numResamples,
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

void FieldlinesSequenceManager::centerSeedPointResampling(
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

double FieldlinesSequenceManager::getTime(ccmc::Kameleon* kameleon) {
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
                    Time::ref().convertTime(
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
                     "The current state starts the same time as the sequence!");
        }

    return seqStartDbl + stateStartOffset;;
}

// TODO find a smarter way
// This is not very elegant but works for now
void FieldlinesSequenceManager::setQuickMorphBooleans(std::vector<FieldlinesState>& states,
                                                      const int& pointsPerCurve,
                                                      const float& threshold) {
    int numLines = states[0]._lineStart.size();
    // for each state
    for (int s = 0; s < states.size()-1; ++s) {
        // for each line (each state has the same number of lines)
        for (int l = 0; l < numLines; ++l) {
            int startIdx = states[s]._lineStart[l];
            // int startIdx = static_cast<int>(state[s]._lineStart[l]);
            int endIdx = startIdx + pointsPerCurve - 1;

            // if distance between end points are larger than 'threshold' then quick morph
            // (QM) should be used
            bool shouldFirstHalfQM = (glm::length(states[s  ]._vertexPositions[startIdx] -
                                 states[s+1]._vertexPositions[startIdx] ) > threshold);

            bool shouldSecondHalfQM = (glm::length(states[s  ]._vertexPositions[endIdx] -
                                 states[s+1]._vertexPositions[endIdx] ) > threshold);


            // Should the first half of the line use "quick morph" (unstable) or not?
            states[s]._quickMorph.insert(states[s]._quickMorph.end(), pointsPerCurve/2,
                                         shouldFirstHalfQM ? 1.f : 0.f);

            // Should the second half of the line use "quick morph" (unstable) or not?
            // Line always contains an odd number of points.. hence pointsPerCurve/2+1
            states[s]._quickMorph.insert(states[s]._quickMorph.end(), pointsPerCurve/2 +1,
                                         shouldSecondHalfQM ? 1.f : 0.f);

        }
    }
}

} // namsepace openspace
