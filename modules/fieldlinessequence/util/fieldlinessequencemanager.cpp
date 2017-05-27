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

#include <openspace/util/spicemanager.h>
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
#include <modules/fieldlinessequence/ext/nlohmann/json/json.hpp>

// JSON PARSER IS FROM THE ISWA MODULE
// #include <modules/iswa/ext/json/json.hpp>

namespace {
    const std::string _loggerCat = "FieldlinesSequenceManager";
    using RawPath = ghoul::filesystem::FileSystem::RawPath;
    using FileSystem = ghoul::filesystem::FileSystem;
    using Sort = ghoul::filesystem::Directory::Sort;

    const float R_E_TO_METER = 6371000.f; // Earth radius
    const float R_S_TO_METER = 695700000.f; // Sun radius
    const float A_U_TO_METER = 149597870700.f; // Astronomical Units

    // For conversion from pressure[nPa]/density[amu/cm^3] to temperature in Kelvin [K]
    // const float NPA_PER_AMU_PER_CM3_TO_K = 1.f; // <-- * [nPa]/[amu/cm^3] => K
    const float NPA_PER_AMU_PER_CM3_TO_K = 72429735.6984f; // <-- * [nPa]/[amu/cm^3] => K
    const std::string TEMPERATURE_P_OVER_RHO = "T = p/rho";

    using json = nlohmann::json;

} // namespace (empty)

// json specific
namespace nlohmann {
    // Tell json library how to handle glm::vec3
    template <>
    struct adl_serializer<glm::vec3> {
        static void to_json(json& j, const glm::vec3& vec3) {
            j = { vec3.x, vec3.y, vec3.z};
        }

        // TODO: try this: static void from_json(const std::array<float,3>, glm::vec3& vec3) {
        static void from_json(const json& j, glm::vec3& vec3) {
            vec3 = glm::vec3(j[0], j[1], j[2]);
        }
    };
} // namespace nlohmann

namespace openspace {

FieldlinesSequenceManager::FieldlinesSequenceManager() {}

FieldlinesSequenceManager::~FieldlinesSequenceManager() {}

std::string FieldlinesSequenceManager::timeToString(double time, bool pathSafe /* = false*/) {
    std::string datetime = SpiceManager::ref().dateFromEphemerisTime(time);
    std::string month = datetime.substr(5, 3);

    std::string MM = "";
    if      (month == "JAN") MM = "01";
    else if (month == "FEB") MM = "02";
    else if (month == "MAR") MM = "03";
    else if (month == "APR") MM = "04";
    else if (month == "MAY") MM = "05";
    else if (month == "JUN") MM = "06";
    else if (month == "JUL") MM = "07";
    else if (month == "AUG") MM = "08";
    else if (month == "SEP") MM = "09";
    else if (month == "OCT") MM = "10";
    else if (month == "NOV") MM = "11";
    else if (month == "DEC") MM = "12";
    else ghoul_assert(false, "Bad month");

    datetime.replace(4, 5, "-" + MM + "-");
    if (pathSafe) {
        datetime.replace(13, 1, "-");
        datetime.replace(16, 1, "-");
        datetime.replace(19, 1, "-");
    }
    return datetime;
}

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

    // TODO: Will this actually ever be triggered even if file is empty? Check!
    if (outVec.size() == 0) {
        return false;
    }

    return true;
}


// Todo move to other util file
bool FieldlinesSequenceManager::getAllFilePathsOfType(
        const std::string& pathToDirectory,
        const std::string& fileExtension,
        std::vector<std::string>& outFilePaths) {

    std::string absFolderPath;
    absFolderPath = absPath(pathToDirectory);

    if ( !FileSys.directoryExists(absFolderPath) ) {
        LERROR("The folder '" << absFolderPath << "' could not be found!");
        return false;
    }

    std::string extension;

    // Add dot if included fileExtension doesn't start with one
    if (fileExtension[0] == '.') {
        extension = fileExtension;
    } else {
        extension = "." + fileExtension;
    }

    int extLength = static_cast<int>(extension.length());

    // Get absolute path to
    ghoul::filesystem::Directory dir(absFolderPath, RawPath::Yes);
    outFilePaths = dir.read(FileSystem::Recursive::Yes, Sort::Yes);

    outFilePaths.erase(std::remove_if(
            outFilePaths.begin(), outFilePaths.end(), [extLength, extension](std::string s) {
                    std::string sub = s.substr(s.length() - extLength, extLength);
                    std::transform(sub.begin(), sub.end(), sub.begin(), ::tolower);
                    return sub != extension;
                }), outFilePaths.end());

    return true;
}

void FieldlinesSequenceManager::saveFieldlinesStatesToJson(const std::string& directoryPath,
                                                           const std::string& filePrefix,
                                                           const std::vector<FieldlinesState>& states) {
    std::string absFolder = absPath(directoryPath);

    for (auto state : states) {
        std::string prefix = filePrefix + timeToString(state._triggerTime);
        saveFieldlinesStateAsJson(state, absFolder, true, prefix, false);
    }
}

void FieldlinesSequenceManager::saveFieldlinesStatesToBinaries(const std::string& directoryPath,
                                                               const std::string& filePrefix,
                                                               const std::vector<FieldlinesState>& states) {
    std::string absPrefix = absPath(directoryPath) + filePrefix;
    std::string extension = ".osfls"; // (O)pen(S)pace(F)ield(L)ine(S)tate

    for (auto state : states) {
        std::string timeString = timeToString(state._triggerTime);
        state.saveStateToBinaryFile(absPrefix + timeString + extension);
    }
}

// TODO - MOVE ALL JSON HANDLING TO ITS OWN FILE!!!!!!!
bool FieldlinesSequenceManager::saveFieldlinesStateAsJson(const FieldlinesState& state,
                                                          const std::string& directoryPath,
                                                          const bool isAbsPath,
                                                          const std::string& prefix    /* = ""   */,
                                                          const bool stateDataInName   /* = true */,
                                                          const std::string& separator /* = "_"  */,
                                                          const int& prettyIndentation /* = 1    */) {

    size_t numExtras = state._extraVariables.size();
    size_t numLines = state._lineStart.size();
    size_t numPoints = state._vertexPositions.size();

    std::string fileName;

    // if prefix is empty -> force the use of stateData in filename!
    if (stateDataInName || prefix == "") {
        fileName = prefix + separator + state._modelName;
        if (state._triggerTime >= 0) {
            // _triggerTime has been initialized -> convert to UT time string
            std::string timeString = SpiceManager::ref().dateFromEphemerisTime(state._triggerTime);
            // replace specified chars with '-'
            std::replace_if(timeString.begin(), timeString.end(), [](char c){ return (c == ' ' ||
                                                                                      c == '.' ||
                                                                                      c == ':');}, '-');
            fileName += separator + timeString;
        }

        fileName += separator + std::to_string(numLines) + "Lines";
        fileName += separator + std::to_string(numPoints) + "Points";

        if (numExtras > 0) {
            std::string s = "";
            if (numExtras > 1) {
                s = "s";
            }

            fileName += separator + std::to_string(numExtras);
            if (state._extraVariableNames.size() == numExtras) {
                fileName += "ColorVar" + s;
            } else {
                fileName += "UnknownColorVar" + s;
            }
        }
    } else {
        fileName = prefix;
    }

    fileName += ".json";

    // Create main json object
    json jfile = {
        // Create meta data
        {"0. meta", {
            {"0. _triggerTime", state._triggerTime},
            {"1. _modelName", state._modelName},
            {"2. numLines", numLines},
            {"3. numPoints", numPoints},
            // push_back number of extraVars
            {"4. numExtras", numExtras}
        }}
    };

    // add entire arrays
    jfile["1. _lineStart"] = state._lineStart;
    jfile["2. _lineCount"] = state._lineCount;
    jfile["3. _vertexPositions"] = state._vertexPositions;

    // TODO this needs to be checked if actually correct!
    json jExtras;
    for (size_t i = 0; i < numExtras; ++i) {
       std::string key = std::to_string(i);
       if (state._extraVariableNames.size() == numExtras) {
           key += ". " + state._extraVariableNames[i];
       }
       jExtras[key] = state._extraVariables[i];
    }

    jfile["4. colorizingVariables"] = jExtras;

    // TODO: VALIDATE THAT FOLDER EXISTS FIRST?
    std::string fullPath;
    if (!isAbsPath) {
        fullPath = absPath(directoryPath);
    }
    fullPath += fileName;
    std::ofstream ofs(fullPath);
    if (!ofs.is_open()) {
        LERROR("FAILED TO OPEN FILE TO WRITE TO");
        return false;
    } else {
        LINFO("Saved fieldline state to: " << fullPath );
    }

    ofs << std::setw(prettyIndentation) << jfile << std::endl;

    return true;
}

bool FieldlinesSequenceManager::getFieldlinesStateFromBinary(
        const std::string& pathToBinaryFile, FieldlinesState& outState) {

    std::ifstream ifs(pathToBinaryFile, std::ifstream::binary);
    if (!ifs.is_open()) {
        LERROR("Couldn't open file: " << pathToBinaryFile);
        return false;
    }

    int binFileVersion;
    ifs.read( reinterpret_cast<char*>(&binFileVersion), sizeof(int));

    switch (binFileVersion) {
        case 0 : {
                LDEBUG("BINARY FILE - VERSION 0");
            }
            break;
        default : {
                LERROR("VERSION OF BINARY FILE NOT RECOGNISED!");
            }
    }

    ifs.read( reinterpret_cast<char*>(&outState._triggerTime), sizeof(double));
    FieldlinesState::Model model;
    ifs.read( reinterpret_cast<char*>(&model), sizeof(int));
    outState.setModel(model);
    ifs.read( reinterpret_cast<char*>(&outState._isMorphable), sizeof(bool));
    size_t numLines;
    ifs.read( reinterpret_cast<char*>(&numLines), sizeof(size_t));
    size_t numPoints;
    ifs.read( reinterpret_cast<char*>(&numPoints), sizeof(size_t));
    size_t numExtras;
    ifs.read( reinterpret_cast<char*>(&numExtras), sizeof(size_t));
    size_t byteSizeAllNames;
    ifs.read( reinterpret_cast<char*>(&byteSizeAllNames), sizeof(size_t));

    // TODO: figure out how to do this with reserve instead of resize! resize is about 1,4 times slower
    // outState._lineStart.reserve(numLines);
    outState._lineStart.resize(numLines);
    ifs.read( reinterpret_cast<char*>(outState._lineStart.data()), sizeof(GLint)*numLines);

    // outState._lineCount.reserve(numLines);
    outState._lineCount.resize(numLines);
    ifs.read( reinterpret_cast<char*>(outState._lineCount.data()), sizeof(GLsizei)*numLines);

    // outState._vertexPositions.reserve(numPoints);
    outState._vertexPositions.resize(numPoints);
    ifs.read( reinterpret_cast<char*>(outState._vertexPositions.data()), sizeof(glm::vec3)*numPoints);

    // outState._extraVariables.reserve(numExtras);
    outState._extraVariables.resize(numExtras);
    for (std::vector<float>& vec : outState._extraVariables) {
        //vec.reserve(numPoints);
        vec.resize(numPoints);
        ifs.read( reinterpret_cast<char*>(vec.data()), sizeof(float) * numPoints);
    }

    // outState._extraVariableNames.reserve(numExtras);
    outState._extraVariableNames.resize(numExtras);
    std::string allNamesInOne;
    char* s = new char[byteSizeAllNames];
    ifs.read(s, byteSizeAllNames);
    allNamesInOne.assign(s,byteSizeAllNames);
    delete s;

    size_t offset = 0;
    for (size_t i = 0; i < numExtras; ++i) {
        auto endOfVarName = allNamesInOne.find('\0', offset);
        endOfVarName -= offset;
        std::string varName = allNamesInOne.substr(offset, endOfVarName);
        offset += varName.size() + 1;
        outState._extraVariableNames.emplace_back(varName);
    }

    return true;
}

/** READ PRECALCULATED FIELDLINE STATES FROM .JSON FILES AND STORE AS A FIELDLINE STATE
 *
 *
 */
bool FieldlinesSequenceManager::getFieldlinesState(
        const std::string& pathToJsonFile,
        const bool shouldResample, //does const bool& make sense?
        const int& numResamples,
        const int& resamplingOption,
        FieldlinesState& outFieldlinesState) {



    std::ifstream ifs(pathToJsonFile);
    if (!ifs.is_open()) {
        LERROR("FAILED TO OPEN FILE: " << pathToJsonFile);
        return false;
    }
    json jfile;// = json::parse(ifs);
    ifs >> jfile;

    size_t numLines = jfile.size();

    // if the JSON file contains the key "0. meta" it is in fieldline state format
    if (jfile.find("0. meta") != jfile.end()) {
        // check my format

        // Objects in jfile
            //jfile["0. meta"];
            //jfile["1. _lineStart"];
            //jfile["2. _lineCount"];
            //jfile["3. _vertexPositions"];
            //jfile["4. colorizingVariables"];

        // jfile["0. meta"] has the following keys and corresponding value types
            // "0. _triggerTime", double
            // "1. _modelName"  , std::string
            // "2. numLines"    , size_t,
            // "3. numPoints"   , size_t,
            // "4. numExtras"   , size_t
        outFieldlinesState._triggerTime = jfile["0. meta"]["0. _triggerTime"];
        outFieldlinesState._modelName   = jfile["0. meta"]["1. _modelName"];
        size_t numLines                 = jfile["0. meta"]["2. numLines"];
        size_t numPoints                = jfile["0. meta"]["3. numPoints"];
        size_t numExtras                = jfile["0. meta"]["4. numExtras"];

        // jfile["1. _lineStart"] stores an array of 'GLuint's
        // jfile["2. _lineCount"] stores an array of 'GLsizei's
        std::vector<GLint> lS   = jfile["1. _lineStart"];
        std::vector<GLsizei> lC = jfile["2. _lineCount"];

        outFieldlinesState._lineStart = std::move(lS);
        outFieldlinesState._lineCount = std::move(lC);

        // outFieldlinesState._lineStart = jfile["1. _lineStart"];
        // outFieldlinesState._lineCount = jfile["2. _lineCount"];

        // jfile["3. _vertexPositions"] stores an array of 'glm::vec3's
        for (auto it = jfile["3. _vertexPositions"].begin(); it != jfile["3. _vertexPositions"].end(); ++it) {
            outFieldlinesState._vertexPositions.push_back(glm::vec3(it.value()[0], it.value()[1], it.value()[2]));
        }

        // jfile["4. colorizingVariables"] stores arrays of 'float's and each array's key describes what unit it holds
        for (auto it = jfile["4. colorizingVariables"].begin(); it != jfile["4. colorizingVariables"].end(); ++it) {
            outFieldlinesState._extraVariableNames.push_back(it.key());
            outFieldlinesState._extraVariables.push_back(it.value());
        }

    } else {
        // in the json file I have gotten from CCMC, fieldline data is structured like this:
        // {
        //   "0": {
        //     "y": [value0, value1, value2],
        //     "x": [value0, value1, value2],
        //     "z": [value0, value1, value2],
        //     "arclength": [value0, value1, value2],
        //     "topology": "solar_wind"
        //   },
        //   "1": {
        //     "y": [value0, value1, value2],
        //     "x": [value0, value1, value2],
        //     "z": [value0, value1, value2],
        //     "arclength": [value0, value1, value2],
        //     "topology": "closed"
        //   }
        // }
        //
        // where "0" and "1" contains data for one fieldline each
        //

        // TODO, this should be included either in the JSON file or in LUA (.mod)
        // when specifying which folders to get JSON files from
        const std::string model = "batsrus";
        outFieldlinesState._modelName = model;
        outFieldlinesState._model = outFieldlinesState.Model::batsrus;

        // Check if the json file contains a trigger time
        if (jfile.find("_triggerTime") != jfile.end()) {
          // there is an entry for key "trigger_time"
            outFieldlinesState._triggerTime = jfile["_triggerTime"];
        } else {
            // LWARNING("JSON FILE DOESN'T CONTAIN A TRIGGER TIME. USING DEFAULT: July 12th 2012 at 00:00:00:000");
            // startTimes.push_back(395323200.0);

            LWARNING("JSON FILE DOESN'T CONTAIN A TRIGGER TIME. USING DEFAULT: March 15th 2015 at 00:00:00:000");
            outFieldlinesState._triggerTime = 479649600.0;
        }

        // iterate through each line
        for (json::iterator fieldlineIt = jfile.begin(); fieldlineIt != jfile.end(); ++fieldlineIt) {
            json fieldline = *fieldlineIt;

            int status = fieldline.count("x") + fieldline.count("y") + fieldline.count("z");

            if (status != 3) {
                LERROR("Couldn't find the needed variables in JSON object!");
                return false;
            }

            std::vector<float> xVars = fieldline["x"];
            std::vector<float> yVars = fieldline["y"];
            std::vector<float> zVars = fieldline["z"];

            size_t pointCount = xVars.size();
            size_t lineStart = outFieldlinesState._vertexPositions.size();

            // TODO: assert!
            // ghoul_assert(length(x) == length(y) == length(z), "ERRORORORORORO");

            auto xIt = xVars.begin(),
                 yIt = yVars.begin(),
                 zIt = zVars.begin();

            for ( ; xIt != xVars.end(); ++xIt, ++yIt, ++zIt) {
                outFieldlinesState._vertexPositions.push_back(R_E_TO_METER * glm::vec3(*xIt,*yIt,*zIt));
            }
            // TODO: assert (pointCount == std::distance(xVars.begin(), xIt))
            outFieldlinesState._lineCount.push_back(static_cast<GLsizei>(pointCount));
            outFieldlinesState._lineStart.push_back(static_cast<GLint>(lineStart));
        }
    }

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
        std::vector<std::string>& colorizingMagVars,
        FieldlinesState& outFieldlinesState) {

    // ----------------------------- CREATE KAMELEON OBJECT -----------------------------
    std::unique_ptr<ccmc::Kameleon> kameleon = std::make_unique<ccmc::Kameleon>();
    long kamStatus = kameleon->open(pathToCdfFile);

    if (kamStatus != ccmc::FileReader::OK) {
        LERROR("Failed to create a Kameleon Object from file: " << pathToCdfFile);
        return false;
    }

    const std::string model = kameleon->getModelName();
    outFieldlinesState._modelName = model;
    outFieldlinesState._model = outFieldlinesState.Model::batsrus;
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
        outFieldlinesState._model = outFieldlinesState.Model::batsrus;
        innerBoundaryLimit = 2.5f; // TODO specify in Lua
        scalingFactor = R_E_TO_METER;

    } else if (model == "enlil") {
        outFieldlinesState._model = outFieldlinesState.Model::enlil;
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

            outFieldlinesState._vertexPositions.reserve(numResamples);
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
    for (int i = 0; i < static_cast<int>(colorizingFloatVars.size()); i++) {
        std::string str = colorizingFloatVars[i];
        status = kameleon->doesVariableExist(str) && kameleon->loadVariable(str);
        // TODO this is not good looking code.. needs refactoring
        // BATSRUS doesn't contain variable T for temperature but it can be calculated
        // using density and pressure!
        if (!status &&
           (str == TEMPERATURE_P_OVER_RHO || (str == "T" && model == "batsrus" ))) {
                std::string p = "p", r = "rho";
                status = kameleon->doesVariableExist(p) && kameleon->loadVariable(p)
                      && kameleon->doesVariableExist(r) && kameleon->loadVariable(r);
                colorizingFloatVars[i] = str = TEMPERATURE_P_OVER_RHO;
                LWARNING("BATSRUS doesn't contain variable T for temperature. Trying to "
                        << "calculate it using the ideal gas law instead: "
                        << "T = pressure/density");
        }
        if (!status) {
            LWARNING("FAILED TO LOAD COLOR VARIABLE: '" << str << "'. Ignoring it!");
            colorizingFloatVars.erase(std::remove(colorizingFloatVars.begin(),
                                                  colorizingFloatVars.end(), str),
                                                  colorizingFloatVars.end());
            --i;
        } else {
            sampleExtraQuantities = true;
            LDEBUG("Color depending on variable " << str << " is allowed!");
            outFieldlinesState._extraVariableNames.push_back(str);
        }
    }

    if (colorizingMagVars.size() % 3 == 0) {
        for (int i = 0; i < static_cast<int>(colorizingMagVars.size()); i += 3) {
            std::string str1 = colorizingMagVars[i];
            std::string str2 = colorizingMagVars[i+1];
            std::string str3 = colorizingMagVars[i+2];
            status = kameleon->doesVariableExist(str1) &&
                     kameleon->doesVariableExist(str2) &&
                     kameleon->doesVariableExist(str3) &&
                     kameleon->loadVariable(str1) &&
                     kameleon->loadVariable(str2) &&
                     kameleon->loadVariable(str3);
            if (!status) {
                LWARNING("FAILED TO LOAD AT LEAST ONE OF THE MAGNITUDE VARIABLES: "
                        << str1 << ", " << str2 <<  " & " << str3
                        << ". Removing ability to colorize by corresponding magnitude!");
                colorizingMagVars.erase(colorizingMagVars.begin() + i,
                                        colorizingMagVars.begin() + i + 3);
                i -= 3;
            } else {
                sampleExtraQuantities = true;
                LDEBUG("Color depending on magnitude of variables "
                        << str1 << ", " << str2 <<  " & " << str3 << " is allowed!");
                outFieldlinesState._extraVariableNames.push_back("Magnitude of ("
                                                                + str1 + ", "
                                                                + str2 + ", "
                                                                + str3 + ")");
            }
        }
    } else {
        LWARNING("Wrong number of variables provided for magnitude colorization. "
                << "Expects multiple of 3, but " << colorizingMagVars.size()
                << " are provided! E.g to colorize by velocity provide : "
                << "ux, uy, uz (for BATSRUS) or ur, utheta, uphi (for ENLIL!)\n\t"
                << "To allow coloring by multiple magnitudes make sure to provide "
                << "variables in blocks of three! E.g: {'ux','uy','uz','jx','jy','jz'}");
    }

    int numValidFloatQuantities = colorizingFloatVars.size();
    int numValidMagnitudeQuantities = colorizingMagVars.size() / 3;

    std::vector<std::vector<float>> colorizingVariables;
    colorizingVariables.resize(numValidFloatQuantities + numValidMagnitudeQuantities);

    // ------ LOOP THROUGH THE SEED POINTS, TRACE AND CONVERT TO THE DESIRED FORMAT ------
    // TODO CREATE MORE VECTORS
    // std::vector<float> xtraVarVec;
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

        outFieldlinesState._lineStart.push_back(lineStart);
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
                    for (int i = 0; i < numValidFloatQuantities; i++) {
                        float val;
                        if (colorizingFloatVars[i] == TEMPERATURE_P_OVER_RHO) {
                            val = interpolator->interpolate("p", gPos.x, gPos.y, gPos.z);
                            val *= NPA_PER_AMU_PER_CM3_TO_K;
                            val /= interpolator->interpolate("rho", gPos.x, gPos.y, gPos.z);
                        } else {
                            val = interpolator->interpolate(colorizingFloatVars[i],
                                                          gPos.x, gPos.y, gPos.z);
                        }
                        colorizingVariables[i].push_back(val);
                    }
                    for (int i = 0; i < numValidMagnitudeQuantities; ++i) {
                        int firstIdx = i*3;

                        float xVal = interpolator->interpolate(
                                colorizingMagVars[firstIdx], gPos.x, gPos.y, gPos.z);

                        float yVal = interpolator->interpolate(
                                colorizingMagVars[firstIdx+1], gPos.x, gPos.y, gPos.z);

                        float zVal = interpolator->interpolate(
                                colorizingMagVars[firstIdx+2], gPos.x, gPos.y, gPos.z);

                        colorizingVariables[i + numValidFloatQuantities].push_back(
                                sqrt(xVal*xVal + yVal*yVal + zVal*zVal));
                    }
                }
                if (convertToCartesian) {
                    // LDEBUG("TODO: CONVERT gPos TO CARTESIAN");
                    convertLatLonToCartesian(gPos);
                }
                // LDEBUG("TODO: SCALE AND STORE gPos IN STATE");
                outFieldlinesState._vertexPositions.push_back(gPos * scalingFactor);
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
                    outFieldlinesState._vertexPositions.push_back(p * scalingFactor);
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
                    outFieldlinesState._vertexPositions.push_back(p * scalingFactor);
                }
            }

            lineCount = tmpvec.size();
        }
        outFieldlinesState._lineCount.push_back(lineCount);
        lineStart += lineCount;
        delete interpolator;
    }
    if (sampleExtraQuantities) {
        outFieldlinesState._extraVariables = std::move(colorizingVariables);
    }

    // ------------------------ MAKE SURE STATE HAS A START TIME ------------------------
    double startTime = getTime(kameleon.get());
    outFieldlinesState._triggerTime = startTime;
    LDEBUG("State will start at " << timeToString(startTime));

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
//            FieldlinesState& outFieldlinesState) {
//
//    outFieldlinesState._velocityMagnitudes.reserve(samplePositions.size());
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
        } else if (kameleon->doesAttributeExist("tim_rundate_cal")) {
            seqStartStr =
                    kameleon->getGlobalAttribute("tim_rundate_cal").getAttributeString();
            if (seqStartStr.length() < 19 && kameleon->doesAttributeExist("tim_crstart_cal")) {
                seqStartStr =
                     kameleon->getGlobalAttribute("tim_crstart_cal").getAttributeString();
            }
        } else if (kameleon->doesAttributeExist("tim_obsdate_cal")) {
            seqStartStr =
                    kameleon->getGlobalAttribute("tim_obsdate_cal").getAttributeString();
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
        } else {
            LWARNING("No starting time attribute could be found in the .cdf file.\n\t" <<
                "Starting time is set to 01.JAN.2000 12:00.");
            seqStartDbl = 0.0;
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

    return seqStartDbl + stateStartOffset;
}

// TODO find a smarter way
// This is not very elegant but works for now
void FieldlinesSequenceManager::setQuickMorphBooleans(std::vector<FieldlinesState>& states,
                                                      const int& pointsPerCurve,
                                                      const float& threshold) {
    unsigned int numLines = states[0]._lineStart.size();
    // for each state
    for (unsigned int s = 0; s < states.size()-1; ++s) {
        // for each line (each state has the same number of lines)
        for (unsigned int l = 0; l < numLines; ++l) {
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
