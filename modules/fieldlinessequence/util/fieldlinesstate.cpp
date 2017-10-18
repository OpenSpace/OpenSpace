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

#include <modules/fieldlinessequence/util/fieldlinesstate.h>

#include <ext/json/json.hpp>

#include <openspace/util/time.h>

#include <ghoul/logging/logmanager.h>

#include <fstream>

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
    #include <ccmc/Kameleon.h>
    #include <ccmc/KameleonInterpolator.h>
    #include <modules/kameleon/include/kameleonhelper.h>
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

namespace {
    std::string _loggerCat = "FieldlinesState";
    const int CurrentVersion = 0;

    const std::string TAsPOverRho = "T = p/rho";
    const std::string JParallelB  = "Current: mag(J||B)";
    const float ToKelvin = 72429735.6984f; // <-- [nPa]/[amu/cm^3] * ToKelvin => Temperature in Kelvin

    using json = nlohmann::json;
}

namespace openspace {

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
/**
 * Traces and adds line vertices to state. (Also sets the simulation model variable: _model!)
 * Vertices may need to be scaled to meters & converted from spherical into cartesian coordinates.
 * Note that extraQuantities will NOT be set!
 */
bool FieldlinesState::addLinesFromKameleon(ccmc::Kameleon* kameleon,
                                          const std::vector<glm::vec3>& seedPoints,
                                          const std::string tracingVar) {

    _model = fls::stringToModel(kameleon->getModelName());

    float innerBoundaryLimit;

    switch (_model) {
        case fls::Model::Batsrus :
            innerBoundaryLimit = 2.5f;  // TODO specify in Lua?
            break;
        case fls::Model::Enlil :
            innerBoundaryLimit = 0.11f; // TODO specify in Lua?
            break;
        default:
            LERROR("OpenSpace's fieldlines sequence currently only supports CDFs from" <<
                "the BATSRUS and ENLIL models!" );
            return false;
    }

    // --------------------------- LOAD TRACING VARIABLE ---------------------------- //
    if (!kameleon->loadVariable(tracingVar)) {
        LERROR("FAILED TO LOAD TRACING VARIABLE: " << tracingVar);
        return false;
    }

    LINFO("TRACING FIELD LINES!");
    // - LOOP THROUGH THE SEED POINTS, TRACE LINES AND CONVERT TO THE DESIRED FORMAT - //
    size_t lineStart = 0;
    for (glm::vec3 seed : seedPoints) {
        //--------------------------------------------------------------------------//
        // We have to create a new tracer (or actually a new interpolator) for each //
        // new line, otherwise some issues occur                                    //
        //--------------------------------------------------------------------------//
        std::unique_ptr<ccmc::Interpolator> interpolator =
                std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);
        ccmc::Tracer tracer(kameleon, interpolator.get());
        tracer.setInnerBoundary(innerBoundaryLimit); // TODO specify in Lua?
        ccmc::Fieldline ccmcFieldline = tracer.bidirectionalTrace(tracingVar,
                                                                  seed.x,
                                                                  seed.y,
                                                                  seed.z);
        const std::vector<ccmc::Point3f>& positions = ccmcFieldline.getPositions();

        _lineStart.push_back(lineStart);
        const size_t nLinePoints = positions.size();
        _lineCount.push_back(static_cast<GLsizei>(nLinePoints));
        lineStart += static_cast<GLint>(nLinePoints);

        for (const ccmc::Point3f& p : positions) {
            _vertexPositions.emplace_back(
                    glm::vec3(p.component1, p.component2, p.component3));
        }
    }

    return _vertexPositions.size() > 0;
}
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
void FieldlinesState::loadExtrasIntoKameleon(ccmc::Kameleon* kameleon,
                                             std::vector<std::string>& xtraScalarVars,
                                             std::vector<std::string>& xtraMagVars) {
    // Load the existing SCALAR variables into kameleon.
    // Remove non-existing variables from vector
    for (int i = 0; i < xtraScalarVars.size(); i++) {
        std::string& str = xtraScalarVars[i];
        bool isSuccesful = kameleon->doesVariableExist(str) && kameleon->loadVariable(str);
        if (!isSuccesful &&
                (_model == fls::Model::Batsrus && (str == TAsPOverRho || str == "T" ))) {
            LDEBUG("BATSRUS doesn't contain variable T for temperature. Trying to "
                   << "calculate it using the ideal gas law: T = pressure/density");
            const std::string p = "p", r = "rho";
            isSuccesful = kameleon->doesVariableExist(p) && kameleon->loadVariable(p)
                       && kameleon->doesVariableExist(r) && kameleon->loadVariable(r);
            str = TAsPOverRho;
        }
        if (!isSuccesful) {
            LWARNING("FAILED TO LOAD EXTRA VARIABLE: '" << str << "'. Ignoring it!");
            xtraScalarVars.erase(xtraScalarVars.begin() + i);
            --i;
        } else {
            _extraQuantityNames.push_back(str);
        }
    }

    // Load the existing magnitude variables (should be provided in multiple of 3)
    // into kameleon. Remove non-existing variables from vector
    if (xtraMagVars.size() % 3 == 0) {
        for (int i = 0; i < static_cast<int>(xtraMagVars.size()); i += 3) {
            std::string s1 = xtraMagVars[i];
            std::string s2 = xtraMagVars[i+1];
            std::string s3 = xtraMagVars[i+2];
            bool isSuccesful = kameleon->doesVariableExist(s1) &&
                               kameleon->doesVariableExist(s2) &&
                               kameleon->doesVariableExist(s3) &&
                               kameleon->loadVariable(s1) &&
                               kameleon->loadVariable(s2) &&
                               kameleon->loadVariable(s3);
            std::string name = "Magnitude of (" + s1 + ", "+ s2 + ", "+ s3 + ")";
            if (isSuccesful && _model == fls::Model::Batsrus && s1 == "jx" && s2 == "jy"
                    && s3 == "jz") {
                // CCMC isn't really interested in the magnitude of current, but by the
                // magnitude of the part of the current's vector that is parallel to the
                // magnetic field => ensure that the magnetic variables are loaded
                isSuccesful =  kameleon->doesVariableExist("bx") &&
                               kameleon->doesVariableExist("by") &&
                               kameleon->doesVariableExist("bz") &&
                               kameleon->loadVariable("bx") &&
                               kameleon->loadVariable("by") &&
                               kameleon->loadVariable("bz");
                name = JParallelB;
            }
            if (!isSuccesful) {
                LWARNING("FAILED TO LOAD AT LEAST ONE OF THE MAGNITUDE VARIABLES: "
                        << s1 << ", " << s2 <<  " & " << s3
                        << ". Removing ability to store corresponding magnitude!");
                xtraMagVars.erase(xtraMagVars.begin() + i, xtraMagVars.begin() + i + 3);
                i -= 3;
            } else {
                _extraQuantityNames.push_back(name);
            }
        }
    } else {
        // WRONG NUMBER OF MAGNITUDE VARIABLES.. REMOVE ALL!
        xtraMagVars.clear();
        LWARNING("Wrong number of variables provided for storing magnitudes. "
                << "Expects multiple of 3 but " << xtraMagVars.size()
                << " are provided");
    }
}
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
/**
 * Loops through _vertexPositions and extracts corresponding 'extraQuantities' att each
 * position from the kameleon object using a ccmc::interpolator.
 * Note that the positions MUST be unaltered (NOT scaled NOR converted to different
 * coordinate system)!
 *
 * @param kameleon raw pointer to an already opened Kameleon object
 * @param xtraScalarVars vector of strings. Strings should be names of a scalar quantities
 * to load into _extraQuantites; such as: "T" for temperature or "rho" for density.
 * @param xtraMagVars vector of strings. Size must be multiple of 3. Strings should be
 * names of the components needed to calculate magnitude. E.g. {"ux", "uy", "uz"} will
 * calculate: sqrt(ux*ux + uy*uy + uz*uz). Magnitude will be stored in _extraQuantities
 */
void FieldlinesState::addExtraQuantities(ccmc::Kameleon* kameleon,
                                         std::vector<std::string>& xtraScalarVars,
                                         std::vector<std::string>& xtraMagVars) {

    loadExtrasIntoKameleon(kameleon, xtraScalarVars, xtraMagVars);

    const size_t nXtraScalars = xtraScalarVars.size();
    const size_t nXtraMagnitudes = xtraMagVars.size() / 3;

    _extraQuantities.resize(nXtraScalars + nXtraMagnitudes);

    std::unique_ptr<ccmc::Interpolator> interpolator =
        std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);

    // ------ Extract all the extraQuantities from kameleon and store in state! ------ //
    for (const glm::vec3& p : _vertexPositions) {
        // Load the scalars!
        for (size_t i = 0; i < nXtraScalars; i++) {
            float val;
            if (xtraScalarVars[i] == TAsPOverRho) {
                val = interpolator->interpolate("p", p.x, p.y, p.z);
                val *= ToKelvin;
                val /= interpolator->interpolate("rho", p.x, p.y, p.z);
            } else {
                val = interpolator->interpolate(xtraScalarVars[i], p.x, p.y, p.z);

                // When measuring density in ENLIL CCMC multiply by the radius^2
                if (xtraScalarVars[i] == "rho" && _model == fls::Model::Enlil) {
                    val *= std::pow(p.x * fls::AuToMeter, 2.0f);
                }
            }
            _extraQuantities[i].push_back(val);
        }
        // Calculate and store the magnitudes!
        for (size_t i = 0; i < nXtraMagnitudes; ++i) {
            const size_t idx = i*3;

            const float x = interpolator->interpolate(xtraMagVars[idx]  , p.x, p.y, p.z);
            const float y = interpolator->interpolate(xtraMagVars[idx+1], p.x, p.y, p.z);
            const float z = interpolator->interpolate(xtraMagVars[idx+2], p.x, p.y, p.z);
            float val;
            // When looking at the current's magnitude in Batsrus, CCMC staff are
            // only interested in the magnitude parallel to the magnetic field
            if (_extraQuantityNames[nXtraScalars + i] == JParallelB) {
                const glm::vec3 normMagnetic =  glm::normalize(glm::vec3(
                        interpolator->interpolate("bx", p.x, p.y, p.z),
                        interpolator->interpolate("by", p.x, p.y, p.z),
                        interpolator->interpolate("bz", p.x, p.y, p.z)));
                // Magnitude of the part of the current vector that's parallel to
                // the magnetic field vector!
                val = glm::dot(glm::vec3(x,y,z), normMagnetic);

            } else {
                val = std::sqrt(x*x + y*y + z*z);
            }
            _extraQuantities[i + nXtraScalars].push_back(val);
        }
    }
}
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
/**
* Converts all glm::vec3 in _vertexPositions from spherical (radius, latitude, longitude)
* coordinates into cartesian coordinates. The longitude and latitude coordinates are
* expected to be in degrees. scale is an optional scaling factor.
*/
void FieldlinesState::convertLatLonToCartesian(const float scale /* = 1.f */) {
    for (glm::vec3& p : _vertexPositions) {

        const float r = p.x * scale;
        const float lat = glm::radians(p.y);
        const float lon = glm::radians(p.z);
        const float rCosLat = r * cos(lat);

        p = glm::vec3(rCosLat * cos(lon), rCosLat* sin(lon), r * sin(lat));
    }
}
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
void FieldlinesState::scalePositions(const float scale) {
    for (glm::vec3& p : _vertexPositions) {
        p *= scale;
    }
}
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

bool FieldlinesState::loadStateFromOsfls(const std::string& pathToOsflsFile) {
    std::ifstream ifs(pathToOsflsFile, std::ifstream::binary);
    if (!ifs.is_open()) {
        LERRORC("FieldlinesState", "Couldn't open file: " + pathToOsflsFile);
        return false;
    }

    int binFileVersion;
    ifs.read( reinterpret_cast<char*>(&binFileVersion), sizeof(int));

    switch (binFileVersion) {
        case 0 : {
                // No need to put everything in this scope now, as only version 0 exists!
            }
            break;
        default :
             LERRORC("FieldlinesState","VERSION OF BINARY FILE WAS NOT RECOGNISED!");
             return false;
    }

    // Define tmp variables to store meta data in
    size_t nLines;
    size_t nPoints;
    size_t nExtras;
    size_t byteSizeAllNames;

    // Read single value variables
    ifs.read( reinterpret_cast<char*>(&_triggerTime),     sizeof(double));
    ifs.read( reinterpret_cast<char*>(&_model),           sizeof(int));
    ifs.read( reinterpret_cast<char*>(&_isMorphable),     sizeof(bool));
    ifs.read( reinterpret_cast<char*>(&nLines),           sizeof(size_t));
    ifs.read( reinterpret_cast<char*>(&nPoints),          sizeof(size_t));
    ifs.read( reinterpret_cast<char*>(&nExtras),          sizeof(size_t));
    ifs.read( reinterpret_cast<char*>(&byteSizeAllNames), sizeof(size_t));

    // RESERVE/RESIZE vectors
    // TODO: Do this without initializing values? Resize is slower than just using reserve, due to initialization of all values
    _lineStart.resize(nLines);
    _lineCount.resize(nLines);
    _vertexPositions.resize(nPoints);
    _extraQuantities.resize(nExtras);
    _extraQuantityNames.reserve(nExtras);

    // Read vertex position data
    ifs.read( reinterpret_cast<char*>(_lineStart.data()),       sizeof(GLint)*nLines);
    ifs.read( reinterpret_cast<char*>(_lineCount.data()),       sizeof(GLsizei)*nLines);
    ifs.read( reinterpret_cast<char*>(_vertexPositions.data()), sizeof(glm::vec3)*nPoints);

    // Read all extra quantities
    for (std::vector<float>& vec : _extraQuantities) {
        vec.resize(nPoints);
        ifs.read( reinterpret_cast<char*>(vec.data()), sizeof(float) * nPoints);
    }

    // Read all extra quantities' names. Stored as multiple c-strings
    std::string allNamesInOne;
    char* s = new char[byteSizeAllNames];
    ifs.read(s, byteSizeAllNames);
    allNamesInOne.assign(s, byteSizeAllNames);
    delete[] s;

    size_t offset = 0;
    for (size_t i = 0; i < nExtras; ++i) {
        auto endOfVarName = allNamesInOne.find('\0', offset);
        endOfVarName -= offset;
        std::string varName = allNamesInOne.substr(offset, endOfVarName);
        offset += varName.size() + 1;
        _extraQuantityNames.push_back(varName);
    }

    return true;
}

bool FieldlinesState::loadStateFromJson(const std::string& pathToJsonFile,
                                        const fls::Model Model,
                                        const float coordToMeters = 1.f) {

    // --------------------- ENSURE FILE IS VALID, THEN PARSE IT --------------------- //
    std::ifstream ifs(pathToJsonFile);

    if (!ifs.is_open()) {
        LERROR("FAILED TO OPEN FILE: " << pathToJsonFile);
        return false;
    }

    json jFile;
    ifs >> jFile;
    // -------------------------------------------------------------------------------- //

    _model = Model;

    const std::string sData  = "data";
    const std::string sTrace = "trace";

    // ----- EXTRACT THE EXTRA QUANTITY NAMES & TRIGGER TIME (same for all lines) ----- //
    {
        const json jTmp = *jFile.begin(); // First field line in the file
        _triggerTime = Time::convertTime(jTmp["time"]);

        const std::string sColumns = "columns";
        auto variableNameVec = jTmp[sTrace][sColumns];
        const size_t nVariables = variableNameVec.size();
        const size_t nPosComponents = 3; // x,y,z

        if (nVariables < nPosComponents) {
            LERROR(pathToJsonFile + ": Each field '" + sColumns +
                    "' must contain the variables: 'x', 'y' and 'z' (order is important).");
            return false;
        }

        for (size_t i = nPosComponents ; i < nVariables ; i++) {
            _extraQuantityNames.push_back(variableNameVec[i]);
        }
    }

    const size_t nExtras = _extraQuantityNames.size();
    _extraQuantities.resize(nExtras);

    size_t lineStartIdx = 0;
    // Loop through all fieldlines
    for (json::iterator lineIter = jFile.begin(); lineIter != jFile.end(); ++lineIter) {
        // The 'data' field in the 'trace' variable contains all vertex positions and the
        // extra quantities. Each element is an array related to one vertex point.
        const std::vector<std::vector<float>> jData = (*lineIter)[sTrace][sData];
        const size_t nPoints = jData.size();

        for (size_t j = 0; j < nPoints; ++j) {
            const std::vector<float>& variables = jData[j];

            // Expects the x, y and z variables to be stored first!
            const size_t xIdx = 0, yIdx = 1, zIdx = 2;
            _vertexPositions.push_back(coordToMeters * glm::vec3(variables[xIdx],
                                                                 variables[yIdx],
                                                                 variables[zIdx]));

            // Add the extra quantites. Stored in the same array as the x,y,z variables.
            // Hence index of the first extra quantity = 3
            for (size_t xtraIdx = 3, k = 0 ; k < nExtras; ++k, ++xtraIdx) {
                _extraQuantities[k].push_back(variables[xtraIdx]);
            }
        }
        _lineCount.push_back(static_cast<GLsizei>(nPoints));
        _lineStart.push_back(static_cast<GLsizei>(lineStartIdx));
        lineStartIdx += nPoints;
    }
    return true;
}

/**
 * @param absPath must be the path to the file (incl. filename but excl. extension!)
 * Directory must exist! File is created (or overwritten if already existing).
 * File is structured like this: (for version 0)
 *  0. int                    - version number of binary state file! (in case something needs to be altered in the future, then increase CurrentVersion)
 *  1. double                 - _triggerTime
 *  2. int                    - _model
 *  3. bool                   - _isMorphable
 *  4. size_t                 - Number of lines in the state  == _lineStart.size()       == _lineCount.size()
 *  5. size_t                 - Total number of vertex points == _vertexPositions.size() == _extraQuantities[i].size()
 *  6. size_t                 - Number of extra quantites     == _extraQuantities.size() == _extraQuantityNames.size()
 *  7. site_t                 - Number of total bytes that ALL _extraQuantityNames consists of (Each such name is stored as a c_str which means it ends with the null char '\0' )
 *  7. std::vector<GLint>     - _lineStart
 *  8. std::vector<GLsizei>   - _lineCount
 *  9. std::vector<glm::vec3> - _vertexPositions
 * 10. std::vector<float>     - _extraQuantities
 * 11. array of c_str         - Strings naming the extra quantities (elements of _extraQuantityNames). Each string ends with null char '\0'
 */
void FieldlinesState::saveStateToOsfls(const std::string& absPath) {
    // ------------------------------- Create the file ------------------------------- //
    std::string pathSafeTimeString = Time(_triggerTime).ISO8601();
    pathSafeTimeString.replace(13, 1, "-");
    pathSafeTimeString.replace(16, 1, "-");
    pathSafeTimeString.replace(19, 1, "-");
    const std::string fileName = pathSafeTimeString + ".osfls";

    std::ofstream ofs(absPath + fileName, std::ofstream::binary | std::ofstream::trunc);
    if (!ofs.is_open()) {
        LERROR("Failed to save state to binary file: " << absPath << fileName);
        return;
    }

    // --------- Add each string of _extraQuantityNames into one long string --------- //
    std::string allExtraQuantityNamesInOne = "";
    for (std::string str : _extraQuantityNames) {
        allExtraQuantityNamesInOne += str + '\0'; // Add the null char '\0' for easier reading
    }

    const size_t nLines       = _lineStart.size();
    const size_t nPoints      = _vertexPositions.size();
    const size_t nExtras      = _extraQuantities.size();
    const size_t nStringBytes = allExtraQuantityNamesInOne.size();

    //------------------------------ WRITE EVERYTHING TO FILE ------------------------------
    // WHICH VERSION OF BINARY FIELDLINES STATE FILE - IN CASE STRUCTURE CHANGES IN THE FUTURE
    ofs.write( (char*)(&CurrentVersion), sizeof( int ) );

    //-------------------- WRITE META DATA FOR STATE --------------------------------
    ofs.write( reinterpret_cast<char*>(&_triggerTime), sizeof( _triggerTime ) );
    ofs.write( reinterpret_cast<char*>(&_model),       sizeof( int ) );
    ofs.write( reinterpret_cast<char*>(&_isMorphable), sizeof( bool ) );

    ofs.write( reinterpret_cast<const char*>(&nLines),       sizeof( size_t ) );
    ofs.write( reinterpret_cast<const char*>(&nPoints),      sizeof( size_t ) );
    ofs.write( reinterpret_cast<const char*>(&nExtras),      sizeof( size_t ) );
    ofs.write( reinterpret_cast<const char*>(&nStringBytes), sizeof( size_t ) );

    //---------------------- WRITE ALL ARRAYS OF DATA --------------------------------
    ofs.write( reinterpret_cast<char*>(_lineStart.data()),       sizeof(GLint) * nLines);
    ofs.write( reinterpret_cast<char*>(_lineCount.data()),       sizeof(GLsizei) * nLines);
    ofs.write( reinterpret_cast<char*>(_vertexPositions.data()), sizeof(glm::vec3) * nPoints);
    // Write the data for each vector in _extraQuantities
    for (std::vector<float>& vec : _extraQuantities) {
        ofs.write( reinterpret_cast<char*>(vec.data()),  sizeof(float) * nPoints);
    }
    ofs.write( allExtraQuantityNamesInOne.c_str(), nStringBytes);
}

// TODO: This should probably be rewritten, but this is the way the files were structured by CCMC
// Structure of File! NO TRAILING COMMAS ALLOWED!
// Additional info can be stored within each line as the code only extracts the keys it needs (time, trace & data)
// The key/name of each line ("0" & "1" in the example below) is arbitrary
// {
//     "0":{
//         "time": "YYYY-MM-DDTHH:MM:SS.XXX",
//         "trace": {
//             "columns": ["x","y","z","s","temperature","rho","j_para"],
//             "data": [[8.694,127.853,115.304,0.0,0.047,9.249,-5e-10],...,[8.698,127.253,114.768,0.800,0.0,9.244,-5e-10]]
//         },
//     },
//     "1":{
//         "time": "YYYY-MM-DDTHH:MM:SS.XXX
//         "trace": {
//             "columns": ["x","y","z","s","temperature","rho","j_para"],
//             "data": [[8.694,127.853,115.304,0.0,0.047,9.249,-5e-10],...,[8.698,127.253,114.768,0.800,0.0,9.244,-5e-10]]
//         },
//     }
// }
void FieldlinesState::saveStateToJson(const std::string& absPath) {
    // Create the file
    const std::string ext = ".json";
    std::ofstream ofs(absPath + ext, std::ofstream::trunc);
    if (!ofs.is_open()) {
        LERROR("Failed to save state to json file at location: " << absPath << ext);
        return;
    }
    LINFO("Saving fieldline state to: " << absPath << ext );

    json jColumns = {"x", "y", "z"};
    for (std::string s : _extraQuantityNames) {
        jColumns.push_back(s);
    }

    json jFile;

    const std::string timeStr = Time(_triggerTime).ISO8601();
    const size_t nLines       = _lineStart.size();
    const size_t nPoints      = _vertexPositions.size();
    const size_t nExtras      = _extraQuantities.size();

    size_t pointIndex = 0;
    for (size_t lineIndex = 0; lineIndex < nLines; lineIndex++) {
        json jData = json::array();
        for (size_t i = 0; i < _lineCount[lineIndex]; i++, pointIndex++) {
            const glm::vec3 pos = _vertexPositions[pointIndex];
            json jDataElement = {pos.x, pos.y, pos.z};

            for (size_t extraIndex = 0; extraIndex < nExtras; extraIndex++) {
                jDataElement.push_back(_extraQuantities[extraIndex][pointIndex]);
            }
            jData.push_back(jDataElement);
        }

        jFile[std::to_string(lineIndex)] = {
            {"time", timeStr},
            {"trace", {
                {"columns", jColumns},
                {"data", jData}
            }}
        };
    }

    //------------------------------ WRITE EVERYTHING TO FILE ------------------------------
    const int indentationSpaces = 2;
    ofs << std::setw(indentationSpaces) << jFile << std::endl;

    LINFO("Saved fieldline state to: " << absPath << ext );
}

// Returns one of the extra quantity vectors, _extraQuantities[index].
// If index is out of scope an empty vector is returned and the referenced bool will be false.
const std::vector<float>&  FieldlinesState::extraQuantity(const size_t index,
                                                          bool& isSuccessful) const {
    if (index < _extraQuantities.size()) {
        isSuccessful = true;
        return _extraQuantities[index];
    }
    isSuccessful = false;
    // return empty vector which goes out of scope hence unusable!
    return std::vector<float>();
}

} // namespace openspace
