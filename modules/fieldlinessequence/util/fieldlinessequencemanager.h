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

// forward declarations
namespace ccmc {
    class Kameleon;
    class Fieldline;
}

namespace openspace {
// Forward declarations
class FieldlinesState;

class FieldlinesSequenceManager : public ghoul::Singleton<FieldlinesSequenceManager> {
    friend class ghoul::Singleton<FieldlinesSequenceManager>;

public:
    FieldlinesSequenceManager();
    ~FieldlinesSequenceManager();

    bool getSeedPointsFromFile(const std::string& path, std::vector<glm::vec3>& outVec);

    bool getAllFilePathsOfType(const std::string& pathToDirectory,
                               const std::string& fileExtension,
                               std::vector<std::string>& outFilePaths);

    void saveFieldlinesStatesToJson(const std::string& directoryPath,
                                    const std::string& filePrefix,
                                    const std::vector<FieldlinesState>& states);

    void saveFieldlinesStatesToBinaries(const std::string& directoryPath,
                                        const std::string& filePrefix,
                                        const std::vector<FieldlinesState>& states);

    bool saveFieldlinesStateAsJson(const FieldlinesState& state,
                                   const std::string& directoryPath,
                                   const bool isAbsPath,
                                   const std::string& prefix    = "",
                                   const bool stateDataInName   = true, // if false prefix is needed
                                   const std::string& separator = "_",
                                   const int& prettyIndentation = 1);

    bool getFieldlinesStateFromBinary(const std::string& pathToBinaryFile,
                                      FieldlinesState& outState);

    bool getFieldlinesState/*Json*/(const std::string& pathToJsonFile,
                            const bool shouldResample, //does const bool& make sense?
                            const int& numResamples,
                            const int& resamplingOption,
                            FieldlinesState& outFieldlinesStates);

    bool getFieldlinesState/*Cdf*/(const std::string& pathToCdfFile,
                            const std::string& tracingVariable,
                            const std::vector<glm::vec3>& inSeedPoints,
                            const int& maxIterations,
                            const bool shouldResample, //does const bool& make sense?
                            const int& numResamples,
                            const int& resamplingOption,
                            std::vector<std::string>& colorizingFloatVars,
                            std::vector<std::string>& colorizingMagnitudeVars,
                            FieldlinesState& outFieldlinesStates);

    void setQuickMorphBooleans(std::vector<FieldlinesState>& states,
                               const int& pointsPerCurve,
                               const float& threshold);

    // TODO move somewhere else
    std::string timeToString(double time, bool pathSafe = false);

private:

    void resampleCcmcFieldline( const int& numResamples,
                                const int& resamplingOption,
                                ccmc::Fieldline& line);

    void centerSeedPointResampling( const int& numResamples,
                                    int& seedPointIdx,
                                    const std::vector<glm::vec3>& line,
                                    std::vector<glm::vec3>& outPositions);

    void ccmcFieldlineToDesiredFormat(const int& numResamples,
                                      int& seedPointIdx,
                                      const std::vector<glm::vec3>& line,
                                      std::vector<glm::vec3>& outPositions);

    double getTime(ccmc::Kameleon* kameleon);

    void convertLatLonToCartesian(glm::vec3& p);
};

} //namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___FIELDLINESSEQUENCEMANAGER___H__
