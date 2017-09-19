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

#include <ghoul/logging/logmanager.h>

#include <fstream>

namespace openspace {

FieldlinesState::FieldlinesState() {}
FieldlinesState::FieldlinesState(const std::string& PATH_TO_OSFLS_FILE, bool& loadSucessful) {
    loadSucessful = loadStateFromOsfls(PATH_TO_OSFLS_FILE);
}

bool FieldlinesState::loadStateFromOsfls(const std::string& PATH_TO_OSFLS_FILE) {
    std::ifstream ifs(PATH_TO_OSFLS_FILE, std::ifstream::binary);
    if (!ifs.is_open()) {
        LERRORC("FieldlinesState", "Couldn't open file: " + PATH_TO_OSFLS_FILE);
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
    size_t numLines;
    size_t numPoints;
    size_t numExtras;
    size_t byteSizeAllNames;

    // Read single value variables
    ifs.read( reinterpret_cast<char*>(&_triggerTime),     sizeof(double));
    ifs.read( reinterpret_cast<char*>(&_model),           sizeof(int));
    ifs.read( reinterpret_cast<char*>(&_isMorphable),     sizeof(bool));
    ifs.read( reinterpret_cast<char*>(&numLines),         sizeof(size_t));
    ifs.read( reinterpret_cast<char*>(&numPoints),        sizeof(size_t));
    ifs.read( reinterpret_cast<char*>(&numExtras),        sizeof(size_t));
    ifs.read( reinterpret_cast<char*>(&byteSizeAllNames), sizeof(size_t));

    // RESERVE/RESIZE vectors
    // TODO: Do this without initializing values? Resize is slower than just using reserve, due to initialization of all values
    _lineStart.resize(numLines);
    _lineCount.resize(numLines);
    _vertexPositions.resize(numPoints);
    _extraVariables.resize(numExtras);
    _extraVariableNames.reserve(numExtras);

    // Read vertex position data
    ifs.read( reinterpret_cast<char*>(_lineStart.data()), sizeof(GLint)*numLines);
    ifs.read( reinterpret_cast<char*>(_lineCount.data()), sizeof(GLsizei)*numLines);
    ifs.read( reinterpret_cast<char*>(_vertexPositions.data()), sizeof(glm::vec3)*numPoints);

    // Read all extra variables
    for (std::vector<float>& vec : _extraVariables) {
        vec.resize(numPoints);
        ifs.read( reinterpret_cast<char*>(vec.data()), sizeof(float) * numPoints);
    }

    // Read all extra variables' names. Stored as multiple c-strings
    std::string allNamesInOne;
    char* s = new char[byteSizeAllNames];
    ifs.read(s, byteSizeAllNames);
    allNamesInOne.assign(s, byteSizeAllNames);
    delete[] s;

    size_t offset = 0;
    for (size_t i = 0; i < numExtras; ++i) {
        auto endOfVarName = allNamesInOne.find('\0', offset);
        endOfVarName -= offset;
        std::string varName = allNamesInOne.substr(offset, endOfVarName);
        offset += varName.size() + 1;
        _extraVariableNames.push_back(varName);
    }

    return true;
}

} // namespace openspace
