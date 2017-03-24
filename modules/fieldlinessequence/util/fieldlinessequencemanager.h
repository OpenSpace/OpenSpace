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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___FIELDLINESSEQUENCEMANAGER___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___FIELDLINESSEQUENCEMANAGER___H__

#include <ghoul/designpattern/singleton.h>

#include <ghoul/opengl/ghoul_gl.h> // TODO: FORWARD DECLARE glm::vec3 instead?
#include <ghoul/glm.h>

#include <vector>

// #ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
// #include <ccmc/Kameleon.h>
// #endif

// #include <openspace/properties/selectionproperty.h>
// #include <openspace/rendering/renderable.h>
// #include <openspace/util/spicemanager.h>
// #include <openspace/util/time.h>

// #include <modules/fieldlinessequence/util/fieldlinesstate.h>

namespace openspace {
// Forward declarations
class FieldlinesState;

class FieldlinesSequenceManager : public ghoul::Singleton<FieldlinesSequenceManager> /*, public properties::PropertyOwner */ {
    friend class ghoul::Singleton<FieldlinesSequenceManager>;

public:
    FieldlinesSequenceManager();
    ~FieldlinesSequenceManager();

    bool getSeedPointsFromFile(const std::string& path, std::vector<glm::vec3>& outVec);

    bool getCdfFilePaths(const std::string& pathToCdfDirectory,
                         std::vector<std::string>& outCdfFilePaths);

    bool traceFieldlinesState(const std::string& pathToCdfFile,
                              const std::string& tracingVariable,
                              const std::vector<glm::vec3>& inSeedPoints,
                              FieldlinesState& outFieldlinesStates);

    // bool traceFieldlines(const std::string& pathToCdfDirectory,
    //                      const std::vector<glm::vec3>& inSeedPoints,
    //                      std::vector<FieldlinesState>& outFieldlinesStates);

private:

};

} //namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___FIELDLINESSEQUENCEMANAGER___H__
