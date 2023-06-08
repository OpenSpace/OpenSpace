/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <openspace/json.h>
#include <openspace/util/time.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <fstream>
#include <iomanip>

namespace {
    constexpr std::string_view _loggerCat = "FieldlinesState";
    constexpr int CurrentVersion = 0;
    using json = nlohmann::json;
} // namespace

namespace openspace {

/**
 * Converts all glm::vec3 in _vertexPositions from spherical (radius, latitude, longitude)
 * coordinates into cartesian coordinates. The longitude and latitude coordinates are
 * expected to be in degrees. scale is an optional scaling factor.
 */
void FieldlinesState::convertLatLonToCartesian(float scale) {
    for (glm::vec3& p : _vertexPositions) {
        const float r = p.x * scale;
        const float lat = glm::radians(p.y);
        const float lon = glm::radians(p.z);
        const float rCosLat = r * cos(lat);

        p = glm::vec3(rCosLat * cos(lon), rCosLat* sin(lon), r * sin(lat));
    }
}

void FieldlinesState::scalePositions(float scale) {
    for (glm::vec3& p : _vertexPositions) {
        p *= scale;
    }
}

bool FieldlinesState::loadStateFromOsfls(const std::string& pathToOsflsFile) {
    std::ifstream ifs(pathToOsflsFile, std::ifstream::binary);
    if (!ifs.is_open()) {
        LERROR("Couldn't open file: " + pathToOsflsFile);
        return false;
    }

    int binFileVersion;
    ifs.read(reinterpret_cast<char*>(&binFileVersion), sizeof(int));

    switch (binFileVersion) {
        case 0:
            // No need to put everything in this scope now, as only version 0 exists!
            break;
        default:
            LERROR("VERSION OF BINARY FILE WAS NOT RECOGNIZED");
            return false;
    }

    // Define tmp variables to store meta data in
    size_t nLines;
    size_t nPoints;
    size_t nExtras;
    size_t byteSizeAllNames;

    // Read single value variables
    ifs.read(reinterpret_cast<char*>(&_triggerTime), sizeof(double));
    ifs.read(reinterpret_cast<char*>(&_model), sizeof(int32_t));
    ifs.read(reinterpret_cast<char*>(&_isMorphable), sizeof(bool));
    ifs.read(reinterpret_cast<char*>(&nLines), sizeof(uint64_t));
    ifs.read(reinterpret_cast<char*>(&nPoints), sizeof(uint64_t));
    ifs.read(reinterpret_cast<char*>(&nExtras), sizeof(uint64_t));
    ifs.read(reinterpret_cast<char*>(&byteSizeAllNames), sizeof(uint64_t));

    _lineStart.resize(nLines);
    _lineCount.resize(nLines);
    _vertexPositions.resize(nPoints);
    _extraQuantities.resize(nExtras);
    _extraQuantityNames.resize(nExtras);

    // Read vertex position data
    ifs.read(reinterpret_cast<char*>(_lineStart.data()), sizeof(int32_t) * nLines);
    ifs.read(reinterpret_cast<char*>(_lineCount.data()), sizeof(uint32_t) * nLines);
    ifs.read(
        reinterpret_cast<char*>(_vertexPositions.data()),
        3 * sizeof(float) * nPoints
    );

    // Read all extra quantities
    for (std::vector<float>& vec : _extraQuantities) {
        vec.resize(nPoints);
        ifs.read(reinterpret_cast<char*>(vec.data()), sizeof(float) * nPoints);
    }

    // Read all extra quantities' names. Stored as multiple c-strings
    std::string allNamesInOne;
    std::vector<char> buffer(byteSizeAllNames);
    ifs.read(buffer.data(), byteSizeAllNames);
    allNamesInOne.assign(buffer.data(), byteSizeAllNames);

    size_t offset = 0;
    for (size_t i = 0; i < nExtras; ++i) {
        auto endOfVarName = allNamesInOne.find('\0', offset);
        endOfVarName -= offset;
        const std::string varName = allNamesInOne.substr(offset, endOfVarName);
        offset += varName.size() + 1;
        _extraQuantityNames[i] = varName;
    }

    return true;
}

bool FieldlinesState::loadStateFromJson(const std::string& pathToJsonFile,
                                        fls::Model Model, float coordToMeters)
{
    // --------------------- ENSURE FILE IS VALID, THEN PARSE IT --------------------- //
    std::ifstream ifs(pathToJsonFile);

    if (!ifs.is_open()) {
        LERROR(fmt::format("FAILED TO OPEN FILE: {}", pathToJsonFile));
        return false;
    }

    json jFile;
    ifs >> jFile;
    // -------------------------------------------------------------------------------- //

    _model = Model;

    const char* sData  = "data";
    const char* sTrace = "trace";

    // ----- EXTRACT THE EXTRA QUANTITY NAMES & TRIGGER TIME (same for all lines) ----- //
    {
        const char* sTime = "time";
        const json& jTmp = *(jFile.begin()); // First field line in the file
        _triggerTime = Time::convertTime(jTmp[sTime]);

        const char* sColumns = "columns";
        const json::value_type& variableNameVec = jTmp[sTrace][sColumns];
        const size_t nVariables = variableNameVec.size();
        const size_t nPosComponents = 3; // x,y,z

        if (nVariables < nPosComponents) {
            LERROR(
                pathToJsonFile + ": Each field '" + sColumns +
                "' must contain the variables: 'x', 'y' and 'z' (order is important)"
            );
            return false;
        }

        for (size_t i = nPosComponents ; i < nVariables ; ++i) {
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
        const std::vector<std::vector<float>>& jData = (*lineIter)[sTrace][sData];
        const size_t nPoints = jData.size();

        for (size_t j = 0; j < nPoints; ++j) {
            const std::vector<float>& variables = jData[j];

            // Expects the x, y and z variables to be stored first!
            const size_t xIdx = 0;
            const size_t yIdx = 1;
            const size_t zIdx = 2;
            _vertexPositions.push_back(
                coordToMeters * glm::vec3(
                    variables[xIdx],
                    variables[yIdx],
                    variables[zIdx]
                )
            );

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
 * \param absPath must be the path to the file (incl. filename but excl. extension!)
 * Directory must exist! File is created (or overwritten if already existing).
 * File is structured like this: (for version 0)
 *  0. int                    - version number of binary state file! (in case something
 *                              needs to be altered in the future, then increase
 *                              CurrentVersion)
 *  1. double                 - _triggerTime
 *  2. int                    - _model
 *  3. bool                   - _isMorphable
 *  4. size_t                 - Number of lines in the state  == _lineStart.size()
 *                                                            == _lineCount.size()
 *  5. size_t                 - Total number of vertex points == _vertexPositions.size()
 *                                                           == _extraQuantities[i].size()
 *  6. size_t                 - Number of extra quantites     == _extraQuantities.size()
 *                                                           == _extraQuantityNames.size()
 *  7. site_t                 - Number of total bytes that ALL _extraQuantityNames
 *                              consists of (Each such name is stored as a c_str which
 *                              means it ends with the null char '\0' )
 *  7. std::vector<GLint>     - _lineStart
 *  8. std::vector<GLsizei>   - _lineCount
 *  9. std::vector<glm::vec3> - _vertexPositions
 * 10. std::vector<float>     - _extraQuantities
 * 11. array of c_str         - Strings naming the extra quantities (elements of
 *                              _extraQuantityNames). Each string ends with null char '\0'
 */
void FieldlinesState::saveStateToOsfls(const std::string& absPath) {
    // ------------------------------- Create the file ------------------------------- //
    std::string pathSafeTimeString = std::string(Time(_triggerTime).ISO8601());
    pathSafeTimeString.replace(13, 1, "-");
    pathSafeTimeString.replace(16, 1, "-");
    pathSafeTimeString.replace(19, 1, "-");
    const std::string& fileName = pathSafeTimeString + ".osfls";

    std::ofstream ofs(absPath + fileName, std::ofstream::binary | std::ofstream::trunc);
    if (!ofs.is_open()) {
        LERROR(fmt::format(
            "Failed to save state to binary file: {}{}", absPath, fileName
        ));
        return;
    }

    // --------- Add each string of _extraQuantityNames into one long string --------- //
    std::string allExtraQuantityNamesInOne = "";
    for (const std::string& str : _extraQuantityNames) {
        allExtraQuantityNamesInOne += str + '\0'; // Add null char '\0' for easier reading
    }

    const size_t nLines = _lineStart.size();
    const size_t nPoints = _vertexPositions.size();
    const size_t nExtras = _extraQuantities.size();
    const size_t nStringBytes = allExtraQuantityNamesInOne.size();

    //----------------------------- WRITE EVERYTHING TO FILE -----------------------------
    // VERSION OF BINARY FIELDLINES STATE FILE - IN CASE STRUCTURE CHANGES IN THE FUTURE
    ofs.write(reinterpret_cast<const char*>(&CurrentVersion), sizeof(int));

    //-------------------- WRITE META DATA FOR STATE --------------------------------
    ofs.write(reinterpret_cast<const char*>(&_triggerTime), sizeof(_triggerTime));
    ofs.write(reinterpret_cast<const char*>(&_model), sizeof(int32_t));
    ofs.write(reinterpret_cast<const char*>(&_isMorphable), sizeof(bool));

    ofs.write(reinterpret_cast<const char*>(&nLines), sizeof(uint64_t));
    ofs.write(reinterpret_cast<const char*>(&nPoints), sizeof(uint64_t));
    ofs.write(reinterpret_cast<const char*>(&nExtras), sizeof(uint64_t));
    ofs.write(reinterpret_cast<const char*>(&nStringBytes), sizeof(uint64_t));

    //---------------------- WRITE ALL ARRAYS OF DATA --------------------------------
    ofs.write(reinterpret_cast<char*>(_lineStart.data()), sizeof(int32_t) * nLines);
    ofs.write(reinterpret_cast<char*>(_lineCount.data()), sizeof(uint32_t) * nLines);
    ofs.write(
        reinterpret_cast<char*>(_vertexPositions.data()),
        3 * sizeof(float) * nPoints
    );
    // Write the data for each vector in _extraQuantities
    for (std::vector<float>& vec : _extraQuantities) {
        ofs.write(reinterpret_cast<char*>(vec.data()), sizeof(float) * nPoints);
    }
    ofs.write(allExtraQuantityNamesInOne.c_str(), nStringBytes);
}

// TODO: This should probably be rewritten, but this is the way the files were structured
// by CCMC
// Structure of File! NO TRAILING COMMAS ALLOWED!
// Additional info can be stored within each line as the code only extracts the keys it
// needs (time, trace & data)
// The key/name of each line ("0" & "1" in the example below) is arbitrary
// {
//     "0":{
//         "time": "YYYY-MM-DDTHH:MM:SS.XXX",
//         "trace": {
//             "columns": ["x","y","z","s","temperature","rho","j_para"],
//             "data": [[8.694,127.853,115.304,0.0,0.047,9.249,-5e-10],...,
//                     [8.698,127.253,114.768,0.800,0.0,9.244,-5e-10]]
//         },
//     },
//     "1":{
//         "time": "YYYY-MM-DDTHH:MM:SS.XXX
//         "trace": {
//             "columns": ["x","y","z","s","temperature","rho","j_para"],
//             "data": [[8.694,127.853,115.304,0.0,0.047,9.249,-5e-10],...,
//                     [8.698,127.253,114.768,0.800,0.0,9.244,-5e-10]]
//         },
//     }
// }
void FieldlinesState::saveStateToJson(const std::string& absPath) {
    // Create the file
    const char* ext = ".json";
    std::ofstream ofs(absPath + ext, std::ofstream::trunc);
    if (!ofs.is_open()) {
        LERROR(fmt::format(
            "Failed to save state to json file at location: {}{}", absPath, ext
        ));
        return;
    }
    LINFO(fmt::format("Saving fieldline state to: {}{}", absPath, ext));

    json jColumns = { "x", "y", "z" };
    for (const std::string& s : _extraQuantityNames) {
        jColumns.push_back(s);
    }

    json jFile;

    std::string_view timeStr = Time(_triggerTime).ISO8601();
    const size_t nLines = _lineStart.size();
    // const size_t nPoints      = _vertexPositions.size();
    const size_t nExtras = _extraQuantities.size();

    size_t pointIndex = 0;
    for (size_t lineIndex = 0; lineIndex < nLines; ++lineIndex) {
        json jData = json::array();
        for (GLsizei i = 0; i < _lineCount[lineIndex]; ++i, ++pointIndex) {
            const glm::vec3 pos = _vertexPositions[pointIndex];
            json jDataElement = { pos.x, pos.y, pos.z };

            for (size_t extraIndex = 0; extraIndex < nExtras; ++extraIndex) {
                jDataElement.push_back(_extraQuantities[extraIndex][pointIndex]);
            }
            jData.push_back(jDataElement);
        }

        jFile[std::to_string(lineIndex)] = {
            { "time", timeStr },
            { "trace", {
                { "columns", jColumns },
                { "data", jData }
            }}
        };
    }

    //----------------------------- WRITE EVERYTHING TO FILE -----------------------------
    const int indentationSpaces = 2;
    ofs << std::setw(indentationSpaces) << jFile << std::endl;

    LINFO(fmt::format("Saved fieldline state to: {}{}", absPath, ext));
}

void FieldlinesState::setModel(fls::Model m) {
    _model = m;
}

void FieldlinesState::setTriggerTime(double t) {
    _triggerTime = t;
}

// Returns one of the extra quantity vectors, _extraQuantities[index].
// If index is out of scope an empty vector is returned and the referenced bool is false.
std::vector<float> FieldlinesState::extraQuantity(size_t index, bool& isSuccessful) const
{
    if (index < _extraQuantities.size()) {
        isSuccessful = true;
        return _extraQuantities[index];
    }
    else {
        isSuccessful = false;
        LERROR("Provided Index was out of scope");
        return {};
    }
}

// Moves the points in @param line over to _vertexPositions and updates
// _lineStart & _lineCount accordingly.
void FieldlinesState::addLine(std::vector<glm::vec3>& line) {
    const size_t nNewPoints = line.size();
    const size_t nOldPoints = _vertexPositions.size();
    _lineStart.push_back(static_cast<GLint>(nOldPoints));
    _lineCount.push_back(static_cast<GLsizei>(nNewPoints));
    _vertexPositions.reserve(nOldPoints + nNewPoints);

    std::copy(line.begin(), line.end(), std::back_inserter(_vertexPositions));
}

void FieldlinesState::addLinesToBeRendered() {
    // i for each originally given seed point
    // the 0 is for only the first fieldline on that pathline
    // because at startup we only want to render one line at the position of the first
    // fieldlines position.
    for (int i = 0; i < _allPathLines.size(); ++i) {
        std::vector<glm::vec3> line;
        for (glm::vec3 v : _allPathLines[i].keyFrames[0].vertices) {
            line.push_back(v);
        }
        addLine(line);
    }
}

double FieldlinesState::daysideDeathTime(size_t index) {
    size_t nPathlineVertices = _allMatchingFieldlines[index].pathLines.first.line.size();
    size_t reconnectionIndex = nPathlineVertices / 2;

    double time = 0;
    for (size_t i = reconnectionIndex; i < nPathlineVertices; ++i) {
        if (_allMatchingFieldlines[index].pathLines.first.keyFrames[i].topology !=
            _allMatchingFieldlines[index].pathLines.first.keyFrames[i + 1].topology)
        {
            break;
        }

    }

    return time;
}

/**
* Adds the first keyframe from each pathline as a fieldline to be rendered.
* Added in the vector _vertexPositions
*/
void FieldlinesState::initializeRenderedMatchingFieldlines() {

    //deleteBadMatchingFieldlines();

    for (MatchingFieldlines mf : _allMatchingFieldlines) {
        addLine(mf.pathLines.first.keyFrames[0].vertices);
        addLine(mf.pathLines.second.keyFrames[0].vertices);
    }
}

// move all bad to the end and then we delete
// hard coded, this circulates 21 vertices around the first reconnection
void FieldlinesState::deleteBadMatchingFieldlines() {

    std::vector<size_t> indicesToRemove;
    size_t nMatchingFieldlines = _allMatchingFieldlines.size() - 1;
    for (size_t j = 0; j < nMatchingFieldlines; ++j) {
        int totalTopologyChanges = 0;
        for (size_t i = 190; i < 211; ++i) {
            if (_allMatchingFieldlines[j].pathLines.first.keyFrames[i].topology !=
                _allMatchingFieldlines[j].pathLines.first.keyFrames[i + 1].topology) {
                ++totalTopologyChanges;
            }
        }

        if (totalTopologyChanges > 2) {
            --j;
            --nMatchingFieldlines;
            std::swap(_allMatchingFieldlines[j], _allMatchingFieldlines[nMatchingFieldlines]);
            continue;
        }
        totalTopologyChanges = 0;

        for (size_t i = 190; i < 211; ++i) {
            if (_allMatchingFieldlines[j].pathLines.second.keyFrames[i].topology !=
                _allMatchingFieldlines[j].pathLines.second.keyFrames[i + 1].topology) {
                ++totalTopologyChanges;
            }
        }

        if (totalTopologyChanges > 2) {
            --j;
            --nMatchingFieldlines;
            std::swap(_allMatchingFieldlines[j], _allMatchingFieldlines[nMatchingFieldlines]);
        }
    }

    _allMatchingFieldlines.resize(nMatchingFieldlines - 1);
}

glm::vec3 FieldlinesState::criticalPoint(size_t i) {
    size_t afterReconnection =
        _allMatchingFieldlines[i].pathLines.first.line.size() / 2;
    size_t beforeReconnection = afterReconnection - 1;

    glm::vec3 criticalPoint =
        (_allMatchingFieldlines[i].pathLines.first.line[beforeReconnection] +
        _allMatchingFieldlines[i].pathLines.first.line[afterReconnection] +
        _allMatchingFieldlines[i].pathLines.second.line[beforeReconnection] +
        _allMatchingFieldlines[i].pathLines.second.line[afterReconnection]) / 4.0f;

    return fls::ReToMeter*criticalPoint;
}

void FieldlinesState::appendToExtra(size_t idx, float val) {
    _extraQuantities[idx].push_back(val);
}

void FieldlinesState::addPathLine(const std::vector<glm::vec3> line, const int i) {
    PathLine pl;
    pl.line = line;
    _allPathLines.push_back(pl);
}

void FieldlinesState::addMatchingPathLines(const std::vector<glm::vec3>&& pathLine1,
    size_t reconPathLine1,
    const std::vector<glm::vec3>&& pathLine2,
    size_t reconPathLine2,
    const double time)

{
    MatchingFieldlines m;
    PathLine pl1, pl2;

    pl1.line = pathLine1;
    pl1.daysideReconnectionStart = reconPathLine1;
    pl2.line = pathLine2;
    pl2.daysideReconnectionStart = reconPathLine2;

    pl1.birthTime = time;
    pl2.birthTime = time;

    m.pathLines = std::make_pair(pl1, pl2);

    _allMatchingFieldlines.push_back(m);
}

void FieldlinesState::addMatchingKeyFrames(
    const std::vector<glm::vec3>&& keyFrame1, const std::vector<glm::vec3>&& keyFrame2,
    const double time1, const double time2, const std::vector<float>&& length1, const std::vector<float>&& length2,
    size_t matchingFieldlinesId) {

    Fieldline f1, f2;

    // convert vertices from RE to meters and place in key frame objects
    for (size_t i = 0; i < keyFrame1.size(); ++i) {
        glm::vec3 v1, v2;
        v1 = keyFrame1[i] * fls::ReToMeter;
        v2 = keyFrame2[i] * fls::ReToMeter;
        f1.vertices.push_back(v1);
        f2.vertices.push_back(v2);

        //// compute accumilated length over the fieldline
        //if (i == 0) {
        //    f1.lengths.push_back(0.0f);
        //    f2.lengths.push_back(0.0f);
        //}
        //if (i > 0) {
        //    f1.lengths.push_back(f1.lengths[i-1] + glm::distance(f1.vertices[i], f1.vertices[i-1]));
        //    f2.lengths.push_back(f2.lengths[i-1] + glm::distance(f2.vertices[i], f2.vertices[i-1]));
        //}
    }

    f1.timeToNextKeyFrame = time1;
    f2.timeToNextKeyFrame = time2;

    //f1.lengths = length1;
    //f2.lengths = length2;

    // Elon: check if even correct. Probably will need both front and back to be < 1.5f
    // to be considered closed. 1.5 is just a number from thin air
    if (glm::length(keyFrame1.front()) < 1.5f && glm::length(keyFrame1.back()) < 1.5f) {
        f1.topology = Fieldline::Topology::Closed;
    }
    else if (glm::length(keyFrame1.back()) < 1.5f || glm::length(keyFrame1.front()) < 1.5f) {
        f1.topology = Fieldline::Topology::Open;
    }
    else {
        f1.topology = Fieldline::Topology::Imf;
    }

    if (glm::length(keyFrame2.front()) < 1.5f && glm::length(keyFrame2.back()) < 1.5f) {
        f2.topology = Fieldline::Topology::Closed;
    }
    else if (glm::length(keyFrame2.back()) < 1.5f || glm::length(keyFrame2.front()) < 1.5f) {
        f2.topology = Fieldline::Topology::Open;
    }
    else {
        f2.topology = Fieldline::Topology::Imf;
    }

    _allMatchingFieldlines[matchingFieldlinesId].pathLines.first.keyFrames.push_back(f1);
    _allMatchingFieldlines[matchingFieldlinesId].pathLines.second.keyFrames.push_back(f2);
}

void FieldlinesState::addFieldLine(const std::vector<glm::vec3> fieldline,
                                   const double time, const int pathLineIndex)
{
    Fieldline f;
    for (glm::vec3 pos : fieldline) {
        glm::vec3 v;
        v = pos * fls::ReToMeter;
        f.vertices.push_back(v);
    }

    f.timeToNextKeyFrame = time;

    // Elon: check if even correct. Probably will need both front and back to be < 1.5f
    // to be considered closed. 1.5 is just a number from thin air
    if (glm::length(fieldline.front()) < 1.5f && glm::length(fieldline.back()) < 1.5f) {
        f.topology = Fieldline::Topology::Closed;
    }
    else if (glm::length(fieldline.back()) < 1.5f || glm::length(fieldline.front()) < 1.5f) {
        f.topology = Fieldline::Topology::Open;
    }
    else {
        f.topology = Fieldline::Topology::Imf;
    }

    _allPathLines[pathLineIndex].keyFrames.push_back(f);
}

void FieldlinesState::setExtraQuantityNames(std::vector<std::string> names) {
    _extraQuantityNames = std::move(names);
    _extraQuantities.resize(_extraQuantityNames.size());
}

void FieldlinesState::setDeathTimes(double time1, double time2, size_t index) {
    _allMatchingFieldlines[index].pathLines.first.deathTime = time1;
    _allMatchingFieldlines[index].pathLines.second.deathTime = time2;
}

const std::vector<std::vector<float>>& FieldlinesState::extraQuantities() const {
    return _extraQuantities;
}

const std::vector<std::string>& FieldlinesState::extraQuantityNames() const {
    return _extraQuantityNames;
}

const std::vector<GLsizei>& FieldlinesState::lineCount() const {
    return _lineCount;
}

const std::vector<GLint>& FieldlinesState::lineStart() const {
    return _lineStart;
}

const std::vector<FieldlinesState::PathLine>& FieldlinesState::allPathLines() const {
    return _allPathLines;
}

const std::vector<FieldlinesState::MatchingFieldlines>& FieldlinesState::getAllMatchingFieldlines() const {
    return _allMatchingFieldlines;
}

fls::Model FieldlinesState::FieldlinesState::model() const {
    return _model;
}

size_t FieldlinesState::nExtraQuantities() const {
    return _extraQuantities.size();
}

double FieldlinesState::triggerTime() const {
    return _triggerTime;
}

const std::vector<glm::vec3>& FieldlinesState::vertexPositions() const {
    return _vertexPositions;
}

} // namespace openspace
