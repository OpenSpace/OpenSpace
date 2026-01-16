/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_CORE___MESSAGESTRUCTURES___H__
#define __OPENSPACE_CORE___MESSAGESTRUCTURES___H__

#include <ghoul/glm.h>
#include <cstdint>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <string>
#include <vector>

namespace openspace::datamessagestructures {

enum class Type : uint32_t {
    CameraData = 0,
    TimelineData,
    ScriptData
};

struct CameraKeyframe {
    CameraKeyframe() = default;
    explicit CameraKeyframe(const std::vector<char>& buffer);
    CameraKeyframe(glm::dvec3 pos, glm::dquat rot, std::string focusNode,
        bool followNodeRot, float scale);

    void serialize(std::vector<char>& buffer) const;
    size_t deserialize(const std::vector<char>& buffer, size_t offset = 0);

    void write(std::ostream& out) const;
    void write(std::stringstream& out) const;

    void read(std::istream* in);
    void read(std::istringstream& iss);

    glm::dvec3 _position = glm::dvec3(0.0);
    glm::dquat _rotation = glm::dquat(1.0, 0.0, 0.0, 0.0);
    bool _followNodeRotation = false;
    std::string _focusNode;
    float _scale = 0.f;

    double _timestamp = 0.0;
};

struct TimeKeyframe {
    TimeKeyframe() = default;
    TimeKeyframe(const std::vector<char>& buffer);

    void serialize(std::vector<char>& buffer) const;
    size_t deserialize(const std::vector<char>& buffer, size_t offset = 0);

    void write(std::ostream* out) const;
    void write(std::stringstream& out) const;

    void read(std::istream* in);
    void read(std::istringstream& iss);

    double _time = 0.0;
    double _dt = 0.0;
    bool _paused = false;
    bool _requiresTimeJump = false;
    double _timestamp = 0.0;
};

struct TimeTimeline {
    TimeTimeline() = default;
    TimeTimeline(const std::vector<char>& buffer);

    void serialize(std::vector<char>& buffer) const;
    size_t deserialize(const std::vector<char>& buffer, size_t offset = 0);

    void write(std::ostream* out) const;
    void read(std::istream* in);

    bool _clear = true;
    std::vector<TimeKeyframe> _keyframes;
};

struct ScriptMessage {
    ScriptMessage() = default;

    void serialize(std::vector<char>& buffer) const;
    void deserialize(const std::vector<char>& buffer);

    void write(std::ostream* out) const;
    void write(unsigned char* buf, size_t& idx, std::ofstream& file) const;
    void write(std::stringstream& ss) const;

    void read(std::istream* in);
    void read(std::istringstream& iss);

    std::string _script;
    double _timestamp = 0.0;
};

} // namespace openspace::messagestructures

#endif // __OPENSPACE_CORE___MESSAGESTRUCTURES___H__
