/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
    struct Fieldline {
        enum class Topology {
            Closed = 0,
            Open,
            Imf
        };
        Topology topology;
        std::vector<glm::vec3> vertices;
        double timeToNextKeyFrame;
        std::vector<float> lengths;
    };

    struct PathLine {
        std::vector<Fieldline> keyFrames;
        std::vector<glm::vec3> line;

        // Defines the time each fieldline is visible.
        // Path lines are visible even outside birth- and death time.
        double birthTime;   // Number of seconds from simulation start to when fieldline starts
        double deathTime;   // Number of seconds from simulation start to when fieldline ends
        // index where dayside reconnection happnens on the PathLine
        size_t daysideReconnectionStart;
        //std::vector<glm::vec3>::const_iterator daysideReconnectionStart;
        //float lifetime; // for when multiple cdf-files come into play ?
    };

    struct MatchingFieldlines {
        std::pair<PathLine, PathLine> pathLines;
    };

    void convertLatLonToCartesian(float scale = 1.f);
    void scalePositions(float scale);

    bool loadStateFromOsfls(const std::string& pathToOsflsFile);
    void saveStateToOsfls(const std::string& pathToOsflsFile);

    bool loadStateFromJson(const std::string& pathToJsonFile, fls::Model model,
        float coordToMeters);
    void saveStateToJson(const std::string& pathToJsonFile);

    const std::vector<std::vector<float>>& extraQuantities() const;
    const std::vector<std::string>& extraQuantityNames() const;
    const std::vector<GLsizei>& lineCount() const;
    const std::vector<GLint>& lineStart() const;
    const std::vector<PathLine>& allPathLines() const;
    const std::vector<MatchingFieldlines>& getAllMatchingFieldlines() const; //allmmafhing

    fls::Model model() const;
    size_t nExtraQuantities() const;
    double triggerTime() const;
    const std::vector<glm::vec3>& vertexPositions() const;

    // Special getter. Returns extraQuantities[index].
    std::vector<float> extraQuantity(size_t index, bool& isSuccesful) const;

    void setModel(fls::Model m);
    void setTriggerTime(double t);
    void setExtraQuantityNames(std::vector<std::string> names);
    void setDeathTimes(double time1, double time2, size_t index);

    void addLinesToBeRendered();
    void addLine(std::vector<glm::vec3>& line);
    void appendToExtra(size_t idx, float val);

    void addPathLine(const std::vector<glm::vec3>, const int i);
    void addFieldLine(const std::vector<glm::vec3> fieldLines, const double time, const int i);
    void addMatchingPathLines(const std::vector<glm::vec3>&& pathLine1,
        size_t reconPathLine1,
        const std::vector<glm::vec3>&& pathLine2,
        size_t reconPathLine2,
        const double time);
    //void addMatchingPathLines(const std::vector<glm::vec3>&& pathLine1,
    //    std::vector<glm::vec3>::const_iterator reconPathLine1,
    //    const std::vector<glm::vec3>&& pathLine2,
    //    std::vector<glm::vec3>::const_iterator reconPathLine2);
    void addMatchingKeyFrames(
        const std::vector<glm::vec3>&& keyFrame1, const std::vector<glm::vec3>&& keyFrame2, 
        const double time1, const double time2, 
        const std::vector<float>&& length1, const std::vector<float>&& length2,
        size_t matchingFieldlinesId);

    void initializeRenderedMatchingFieldlines();

private:
    bool _isMorphable = false;
    double _triggerTime = -1.0;
    fls::Model _model;

    std::vector<std::vector<float>> _extraQuantities;
    std::vector<std::string> _extraQuantityNames;

    // information about all matching path lines with associated key frames
    std::vector<MatchingFieldlines> _allMatchingFieldlines;

    /**
    * The format with _lineStart, _lineCount and _vertexPositions is used to
    * update the OpenGL vertex position buffer in the renderable.
    */

    // The index where each fieldline first vertex is in _vertexPositions
    std::vector<GLint> _lineStart;
    // The number of vertices for each fieldline in _vertexPositions
    std::vector<GLsizei> _lineCount;
    // Vertices for all rendered fieldlines
    std::vector<glm::vec3> _vertexPositions;

    std::vector<PathLine> _allPathLines;    // replaces _fieldLinesPerPath
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___FIELDLINESSTATE___H__
