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

#include <modules/fieldlinessequence/util/fieldlineutils.hpp>

#include <ghoul/logging/logmanager.h>

#include <fstream>

namespace {
    std::string _loggerCat = "FieldlineState";

    const int CURRENT_VERSION = 0;
}

namespace openspace {

FieldlinesState::FieldlinesState(size_t numLines) {
    _lineStart.reserve(numLines);
    _lineCount.reserve(numLines);
}

void FieldlinesState::reserveSize(size_t size) {
    _vertexPositions.reserve(size);
    _vertexColors.reserve(size);
}

/**
 * @param absFilePath must be the entire path to the file to create/overwrite (INCLUDING
 * file extension!)
 * Directory must exist. File is created.
 * File is structured like this: Version 0
 *  0. int                      - version number of binary state file! (in case something needs to be altered in the future, then increase CURRENT_VERSION)
 *  1. double                   - _triggerTime
 *  2. int                      - _model
 *  3. bool                     - _isMorphable
 *  4. size_t                   - _lineStart.size() == _lineCount.size()
 *  5. size_t                   - _vertexPositions.size()
 *  6. size_t                   - _extraVariables.size() == _extraVariableNames.size()
 *  7. site_t                   - number of total bytes that ALL colorVariableNames consists of (Each such name is stored as a c_str which means it ends with the null char '\0' )
 *  7. std::vector<GLint>       - _lineStart
 *  8. std::vector<GLsizei>     - _lineCount
 *  9. std::vector<glm::vec3>   - _vertexPositions
 * 10. std::vector<float>       - _extraVariables
 * 11. array of c_str           - strings nameing the extra variables. Each string end with null char '\0'
 * 12. size_t                   - sizeof(_quickMorph) - Might not exist! (Might exist if _isMorphable is true)
 * 13. std::vector<GLfloat>     - _quickMorph         - Might not exist!
 */
void FieldlinesState::saveStateToBinaryFile(const std::string& absFilePath) {
    // Create the file
    std::ofstream ofs(absFilePath, std::ofstream::binary | std::ofstream::trunc);
    if (!ofs.is_open()) {
        LERROR("Failed to save state to binary file at location: " << absFilePath);
        return;
    }

    size_t numLines         = _lineStart.size();
    size_t numPoints        = _vertexPositions.size();
    size_t numExtras        = _extraVariables.size();
    std::string allExtraVarNamesInOne = "";
    for (std::string str : _extraVariableNames) {
        allExtraVarNamesInOne += str + '\0'; // Add the null char '\0' for easier reading
    }
    size_t numStringBytes = allExtraVarNamesInOne.size();


    //------------------------------ WRITE EVERYTHING TO FILE ------------------------------
    // WHICH VERSION OF BINARY FIELDLINES STATE FILE - IN CASE STRUCTURE CHANGES IN THE FUTURE
    ofs.write( (char*)(&CURRENT_VERSION), sizeof( int ) );

    //-------------------- WRITE META DATA FOR STATE --------------------------------
    ofs.write( reinterpret_cast<char*>(&_triggerTime),      sizeof( _triggerTime ) );
    ofs.write( reinterpret_cast<char*>(&_model),            sizeof( int ) );
    ofs.write( reinterpret_cast<char*>(&_isMorphable),      sizeof( bool ) ); // Handle bool as a byte

    ofs.write( reinterpret_cast<char*>(&numLines),          sizeof( size_t ) );
    ofs.write( reinterpret_cast<char*>(&numPoints),         sizeof( size_t ) );
    ofs.write( reinterpret_cast<char*>(&numExtras),         sizeof( size_t ) );
    ofs.write( reinterpret_cast<char*>(&numStringBytes),    sizeof( size_t ) );

    //---------------------- WRITE ALL ARRAYS OF DATA --------------------------------
    ofs.write( reinterpret_cast<char*>(_lineStart.data()),       sizeof(GLint) *  numLines);
    ofs.write( reinterpret_cast<char*>(_lineCount.data()),       sizeof(GLsizei) * numLines);
    ofs.write( reinterpret_cast<char*>(_vertexPositions.data()), sizeof(glm::vec3) * numPoints);
    // Write the data for each vector in _extraVariables
    for (std::vector<float>& vec : _extraVariables) {
        ofs.write( reinterpret_cast<char*>(vec.data()),  sizeof(float) * numPoints);
    }
    ofs.write( allExtraVarNamesInOne.c_str(), numStringBytes);
}

// TEMPORARY.. only pre event to downscale how many points each state stores!!
// Should be provided as option from LUA and incorporated into the main saveState function!
// Saves only every n'th point to the binary file!
void FieldlinesState::saveStateSubsetToBinaryFile(const std::string& absFilePath,
                                                  const size_t& numPointsToSkipEachStep) {
    // Create the file
    std::ofstream ofs(absFilePath, std::ofstream::binary | std::ofstream::trunc);
    if (!ofs.is_open()) {
        LERROR("Failed to save state to binary file at location: " << absFilePath);
        return;
    }

    size_t numLines         = _lineStart.size();
    size_t numPoints        = 0;
    std::vector<GLint> lineStart;
    std::vector<GLsizei> lineCount;

    size_t numExtras        = _extraVariables.size();
    std::string allExtraVarNamesInOne = "";
    for (std::string str : _extraVariableNames) {
        allExtraVarNamesInOne += str + '\0'; // Add the null char '\0' for easier reading
    }
    size_t numStringBytes = allExtraVarNamesInOne.size();

    std::vector<glm::vec3>  vertexPositionsSubset;
    // vertexPositionsSubset.reserve(numPoints);
    std::vector<std::vector<float>> extraVariablesSubset;
    extraVariablesSubset.resize(numExtras);

    GLint newStart = 0;
    for (size_t i = 0; i < numLines; ++i) {
        auto tmpStart = _lineStart[i];
        auto tmpCount = _lineCount[i];
        GLsizei newCount = 0;
        GLint oldIndex = tmpStart;
        // Add every nth point in line
        for (size_t j = 0; j < tmpCount; j += numPointsToSkipEachStep) {
            vertexPositionsSubset.push_back(_vertexPositions[oldIndex]);
            newCount++;
            numPoints++;
            for (size_t k = 0; k < numExtras ; ++k) {
                extraVariablesSubset[k].push_back(_extraVariables[k][oldIndex]);
            }
            oldIndex += numPointsToSkipEachStep;
        }
        if (newCount > 0) {
            lineCount.push_back(newCount);
            lineStart.push_back(newStart);
            newStart += newCount;
        }
    }

    //------------------------------ WRITE EVERYTHING TO FILE ------------------------------
    // WHICH VERSION OF BINARY FIELDLINES STATE FILE - IN CASE STRUCTURE CHANGES IN THE FUTURE
    ofs.write( (char*)(&CURRENT_VERSION), sizeof( int ) );

    //-------------------- WRITE META DATA FOR STATE --------------------------------
    ofs.write( reinterpret_cast<char*>(&_triggerTime),      sizeof( _triggerTime ) );
    ofs.write( reinterpret_cast<char*>(&_model),            sizeof( int ) );
    ofs.write( reinterpret_cast<char*>(&_isMorphable),      sizeof( bool ) ); // Handle bool as a byte

    ofs.write( reinterpret_cast<char*>(&numLines),          sizeof( size_t ) );
    ofs.write( reinterpret_cast<char*>(&numPoints),         sizeof( size_t ) );
    ofs.write( reinterpret_cast<char*>(&numExtras),         sizeof( size_t ) );
    ofs.write( reinterpret_cast<char*>(&numStringBytes),    sizeof( size_t ) );

    //---------------------- WRITE ALL ARRAYS OF DATA --------------------------------
    ofs.write( reinterpret_cast<char*>(lineStart.data()),       sizeof(GLint) *  numLines);
    ofs.write( reinterpret_cast<char*>(lineCount.data()),       sizeof(GLsizei) * numLines);
    ofs.write( reinterpret_cast<char*>(vertexPositionsSubset.data()), sizeof(glm::vec3) * numPoints);
    // Write the data for each vector in _extraVariables
    for (std::vector<float>& vec : extraVariablesSubset) {
        ofs.write( reinterpret_cast<char*>(vec.data()),  sizeof(float) * numPoints);
    }
    ofs.write( allExtraVarNamesInOne.c_str(), numStringBytes);
}

void FieldlinesState::setModel(const Model& modelNumber) {
    switch (modelNumber) {
        case batsrus : {
                _model = modelNumber;
                _modelName = "batsrus";
            }
            break;
        case enlil : {
                _model = modelNumber;
                _modelName = "enlil";
            }
            break;
        case pfss : {
                _model = modelNumber;
                _modelName = "pfss";
            }
            break;
        default :
            LERROR("Unrecognised model!");
            break;
    }
}

// TODO: Topology should be stored ONCE PER LINE, not once per vertex as it is now!!!!!!!
// Calculates topology of each line within the state and adds a new extraVariables vector
void FieldlinesState::calculateTopologies(const float& radius) {
    size_t arrayIdx = _extraVariables.size();
    _extraVariables.resize(arrayIdx + 1);
    _extraVariableNames.push_back("topology");
    for (size_t idx = 0; idx < _lineStart.size(); ++idx) {

        size_t startVertIdx = _lineStart[idx];
        size_t endVertIdx = startVertIdx + _lineCount[idx] - 1;

        int topology = getFieldlineTopology(_model, _vertexPositions[startVertIdx],
                _vertexPositions[endVertIdx], radius);

        _extraVariables[arrayIdx].insert(_extraVariables[arrayIdx].end(),
                                         static_cast<size_t>(_lineCount[idx]),
                                         static_cast<float>(topology));
    }
}

} // namespace openspace
