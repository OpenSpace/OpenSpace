/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___FIELDLINESSTATE___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___FIELDLINESSTATE___H__

#include <modules/fieldlinessequence/util/commons.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>

#include <string>
#include <vector>

namespace openspace {


class FieldlinesState {
public:
    void convertLatLonToCartesian(float scale = 1.f);
    void scalePositions(float scale);

    void scaleflowline(float scale);

    void scaleFieldlines(float scale);

    void addClosedIndices(std::vector<int> close);

    void addOpenIndices(std::vector<int> open);

    void computeTimes();

    bool loadStateFromOsfls(const std::string& pathToOsflsFile);
    void saveStateToOsfls(const std::string& pathToOsflsFile);

    bool loadStateFromJson(const std::string& pathToJsonFile, fls::Model model,
        float coordToMeters);
    void saveStateToJson(const std::string& pathToJsonFile);

    const std::vector<std::vector<float>>& extraQuantities() const;
    const std::vector<std::string>& extraQuantityNames() const;
    const std::vector<GLsizei>& lineCount() const;
    const std::vector<GLint>& lineStart() const;

    fls::Model model() const;
    size_t nExtraQuantities() const;
    double triggerTime() const;

    const std::vector< std::vector<int> > closed() const;

    const std::vector< std::vector<int> > open() const;

    const std::vector<glm::vec3>& vertexPositions() const;

    const std::vector<std::vector<glm::vec3>>& vertexPath() const;

    const std::vector < std::vector<float>>& vertexVelocities() const;

    const std::vector < std::vector<float>>& vertexTimes() const;

    const std::vector < std::vector<std::vector<glm::vec3>>>& fieldLines() const;

    // Special getter. Returns extraQuantities[index].
    std::vector<float> extraQuantity(size_t index, bool& isSuccesful) const;

    void setModel(fls::Model m);
    void setTriggerTime(double t);
    void setExtraQuantityNames(std::vector<std::string> names);

    void addLine(std::vector<glm::vec3>& line);
    void appendToExtra(size_t idx, float val);

    void moveLine( double dt);

    void addPath(std::vector<glm::vec3> path);

    void addFieldLines(std::vector<std::vector<glm::vec3>> fieldLine);

    void addVelocities(std::vector<float> path);


private:
    bool _isMorphable = false;
    double _triggerTime = -1.0;
    fls::Model _model;

    bool atEnd = false;
    bool atStart = false;

    std::vector<std::vector<float>> _extraQuantities;
    std::vector<std::string> _extraQuantityNames;
    std::vector<GLsizei> _lineCount;
    std::vector<GLint> _lineStart;
    int _numberOfLines = 0;
    std::vector<glm::vec3> _vertexPositions;

    std::vector<size_t> _vertexIndex;
    std::vector<float> _timeSinceLastInterpolation;
    std::vector<std::vector<glm::vec3>> _vertexPath;
    std::vector<std::vector<std::vector<glm::vec3>>> _fieldLines;
    std::vector<std::vector<float>> _vertexVelocities;
    std::vector<std::vector<float>> _vertexTimes;
    std::vector<std::vector<int>> _open;
    std::vector<std::vector<int>> _closed;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___FIELDLINESSTATE___H__
