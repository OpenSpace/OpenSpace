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
    const float A_U_TO_METER = 149597870700.f; // Astronomical Units
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
        std::vector<std::string>& colorizingFloatVars,
        std::vector<double>& startTimes,
        FieldlinesState& outFieldlinesStates) {

    // --------------------- CREATE KAMELEON OBJECT AND DEPENDENCIES ---------------------
    std::unique_ptr<ccmc::Kameleon> kameleon = std::make_unique<ccmc::Kameleon>();
    long kamStatus = kameleon->open(pathToCdfFile);

    if (kamStatus != ccmc::FileReader::OK) {
        LERROR("Failed to create a Kameleon Object from file: " << pathToCdfFile);
        return false;
    }

    const std::string model = kameleon->getModelName();
    outFieldlinesStates._modelName = model;
    bool status = kameleon->loadVariable(tracingVariable);
    if (!status) {
        LERROR("FAILED TO LOAD TRACING VARIABLE: " << tracingVariable);
        return false;
    }


    // ----------------- CHECK CDF MODEL AND SETUP VARIABLES ACCORDINGLY -----------------
    float scalingFactor;
    float innerBoundaryLimit;
    bool convertToCartesian = false;

    if (model == "batsrus") {
        innerBoundaryLimit = 2.5f; // TODO specify in Lua
        scalingFactor = R_E_TO_METER;

    } else if (model == "enlil") {
        innerBoundaryLimit = 0.11f; // TODO specify in Lua
        scalingFactor = A_U_TO_METER;
        convertToCartesian = true;
        // tracer.setDn(0.2f);

    } else {
        LERROR("OpenSpace's fieldlines sequence currently only supports the " <<
                "BATSRUS and ENLIL models. No support for " << model << "!" );
        return false;
    }

    // --- DETERMINE HOW AND WHEN TO CONVERT FROM CCMC::FIELDLINE IN MODEL COORDINATES ---
    // ---            TO GLM::VEC3 IN PROPERLY SCALED CARTESIAN COORDINATES            ---
    // ResamplingOption = 1, 2 and 3 uses CCMC's built in resampling which requires
    // the ccmc::Fieldline variable. Resample before conversion to glm::vec3! Other
    // options (4) requires the transformation to the correct coordinate system first
    bool preConversionResampling = false;
    bool postConversionResampling = false;

    if (shouldResample) {
        // TODO Make this check less hardcoded
        // Check if resamplingOption is valid
        if (resamplingOption < 5 && resamplingOption > 0) {

            outFieldlinesStates._vertexPositions.reserve(numResamples);
            if (resamplingOption < 4) {
                preConversionResampling = true;
                if (model == "enlil") {
                    LWARNING("CCMC's fieldline resampling doesn't account for spherical "
                       << "coordinates. Consider selecting Resampling Option 4 instead!");
                }
            } else { // resamplingOption == 4
                postConversionResampling = true;
            }

        } else {
            LERROR("NOT A VALID RESAMPLING OPTION! Only 1, 2, 3 & 4 are valid options!");
            return 0;
        }
    }

    // ---- DETERMINE WETHER OR NOT TO SAMPLE EXTRA QUANTITIES AT FIELDLINE VERTICES ----
    // ----------------- IF SO LOAD THEM, ELSE DELETE STRING FROM VECTOR -----------------
    bool sampleExtraQuantities = false;
    for (std::string str : colorizingFloatVars) {
        status =  kameleon->loadVariable(str);
        if (!status) {
            LWARNING("FAILED TO LOAD EXTRA VARIABLE: '" << str << "'. Ignoring it!");
            colorizingFloatVars.erase(std::remove(colorizingFloatVars.begin(),
                                                  colorizingFloatVars.end(), str),
                                                  colorizingFloatVars.end());
        } else {
            sampleExtraQuantities = true;
        }
    }

    // ------ LOOP THROUGH THE SEED POINTS, TRACE AND CONVERT TO THE DESIRED FORMAT ------
    // TODO CREATE MORE VECTORS
    std::vector<float> xtraVarVec;
    int lineStart = 0;
    for (glm::vec3 seedPoint : inSeedPoints) {
        //--------------------------------------------------------------------------//
        // We have to create a new tracer (or actually a new interpolator) for each //
        // new line, otherwise some issues occur                                    //
        //--------------------------------------------------------------------------//

        // IMPORTANT!: Remember to delete interpolator if creating it here!
        ccmc::Interpolator* interpolator = kameleon->createNewInterpolator();
        ccmc::Tracer tracer(kameleon.get(), interpolator);
        // ccmc::Tracer tracer(kameleon.get());
        tracer.setMaxIterations(maxIterations);
        tracer.setInnerBoundary(innerBoundaryLimit); // TODO specify in Lua

        // A ccmc::Fieldline contains much more info than we need here,
        // but might be needed in future.
        ccmc::Fieldline ccmcFieldline = tracer.bidirectionalTrace(tracingVariable,
                                                                  seedPoint.x,
                                                                  seedPoint.y,
                                                                  seedPoint.z);

        outFieldlinesStates._lineStart.push_back(lineStart);
        int lineCount = 0;

        if (preConversionResampling) {
            resampleCcmcFieldline(numResamples, resamplingOption, ccmcFieldline);
        }

        const std::vector<ccmc::Point3f> positions = ccmcFieldline.getPositions();

        if ( (!preConversionResampling && !postConversionResampling)
                || preConversionResampling ) {

            lineCount = positions.size();

            for (ccmc::Point3f p : positions) {
                glm::vec3 gPos = glm::vec3(p.component1, p.component2, p.component3);
                if (sampleExtraQuantities) {
                    // LDEBUG("TODO: SAMPLE EXTRA PROPERTIES FOR COLORIZING LINES AT gPos");
                    xtraVarVec.push_back(interpolator->interpolate(colorizingFloatVars[0], gPos.x, gPos.y, gPos.z));
                }
                if (convertToCartesian) {
                    // LDEBUG("TODO: CONVERT gPos TO CARTESIAN");
                    convertLatLonToCartesian(gPos);
                }
                // LDEBUG("TODO: SCALE AND STORE gPos IN STATE");
                outFieldlinesStates._vertexPositions.push_back(gPos * scalingFactor);
            }

        } else /*if (postConversionResampling)*/ {
            std::vector<glm::vec3> glmPositions;
            for (ccmc::Point3f p : positions) {
                glm::vec3 gPos = glm::vec3(p.component1, p.component2, p.component3);
                if (convertToCartesian) {
                    // LDEBUG("TODO: CONVERT glmPositions TO CARTESIAN");
                    convertLatLonToCartesian(gPos);
                }
                glmPositions.push_back(gPos);
            }

            int seedIndex = ccmcFieldline.getStartIndex();

            std::vector<glm::vec3> tmpvec;
            centerSeedPointResampling(numResamples, seedIndex, glmPositions, tmpvec);

            if (convertToCartesian) {
                // LDEBUG("TODO: SCALE AND STORE tmpvec IN STATE");
                for (glm::vec3 p : tmpvec) {
                    outFieldlinesStates._vertexPositions.push_back(p * scalingFactor);
                }
                if (sampleExtraQuantities) {
                    LDEBUG("TODO: CONVERT glmPositions BACK TO SPHERICAL (lon lat)");
                    LDEBUG("TODO: SAMPLE EXTRA PROPERTIES FOR COLORIZING LINES AT gPos");
                }
            } else {
                if (sampleExtraQuantities) {
                    LDEBUG("TODO: SAMPLE EXTRA PROPERTIES FOR COLORIZING LINES AT gPos");
                }
                // LDEBUG("TODO: SCALE AND STORE glmPositions IN STATE");
                for (glm::vec3 p : tmpvec) {
                    outFieldlinesStates._vertexPositions.push_back(p * scalingFactor);
                }
            }

            lineCount = tmpvec.size();
        }
        outFieldlinesStates._lineCount.push_back(lineCount);
        lineStart += lineCount;
        delete interpolator;
    }
    if (sampleExtraQuantities) {
        outFieldlinesStates._extraVariables.push_back(xtraVarVec);
    }

    // ------------------------ MAKE SURE STATE HAS A START TIME ------------------------
    double startTime = getTime(kameleon.get());
    startTimes.push_back(startTime);
    LDEBUG("State will start at " << startTime << " (J2000 Time)");

    return status;
}

// Already traced
void FieldlinesSequenceManager::resampleCcmcFieldline(const int& numResamples,
                                                      const int& resamplingOption,
                                                      ccmc::Fieldline& line) {

    int numPoints = line.size();//line.getStartIndex();
    if (numPoints == numResamples || numPoints <= 0) {
        return; // Nothing is needed to be done
    }

    // auto test = line.getPosition(0);
    // auto testCoord = test.getCoordinates();

    // if (testCoord == 0) {
    //     LDEBUG("TODO: CONVERT EACH POINT TO CARTESIAN");

    //     const std::vector<ccmc::Point3f> positions = line.getPositions();
    //     const std::vector<float> values = line.getData();

    //     ccmc::Fieldline newLine;
    //     int i = 0;
    //     for (ccmc::Point3f p : positions) {
    //         // if (p.component3 > 360.f || p.component3 < 0.f) {
    //         //     LERROR("third component is out of domain");
    //         // }
    //         // if (p.component2 > 90.f || p.component2 < -90.f) {
    //         //     LERROR("second component is out of domain");
    //         // }
    //         // ccmc::Point3f cartPoint = p.getCartesian();
    //         glm::vec3 pn = glm::vec3(p.component1, p.component2, p.component3);
    //         convertLatLonToCartesian(pn);
    //         ccmc::Point3f cartPoint(pn.x,pn.y,pn.z);
    //         // cartPoint.setCoordinates(ccmc::Point3f::CARTESIAN);
    //         newLine.insertPointData(cartPoint, values[i]);
    //         ++i;
    //     }
    //     line = newLine;
    // } else {
    //     LERROR("THIS SHUOLDNT BE REACHED NOW!");
    // }

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

//bool FieldlinesSequenceManager::addVelocityMagnitudesToState(
//            const ccmc::Kameleon* kameleon,
//            const std::vector<glm::vec3>& samplePositions,
//            FieldlinesState& outFieldlinesStates) {
//
//    outFieldlinesStates._velocityMagnitudes.reserve(samplePositions.size());
//
//    
//
//    return false;
//}

// Converts spherical coordinates expressed in (r, lat, lon) to cartesian
// where lat belongs to interval [-90,90] and lon to [0,360]
void FieldlinesSequenceManager::convertLatLonToCartesian(glm::vec3& p) {
    float r         = p.x;
    float lat_rad   = glm::radians(p.y);
    float lon_rad   = glm::radians(p.z);
    float r_cosLat  = r * cos(lat_rad);

    p = glm::vec3(r_cosLat * cos(lon_rad),
                  r_cosLat * sin(lon_rad),
                  r * sin(lat_rad));
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
