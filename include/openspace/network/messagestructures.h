/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <ghoul/fmt.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <algorithm>
#include <cstring>
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
    CameraKeyframe(const std::vector<char>& buffer) {
        deserialize(buffer);
    }
    CameraKeyframe(glm::dvec3 pos, glm::dquat rot, std::string focusNode,
                   bool followNodeRot, float scale)
        : _position(pos)
        , _rotation(rot)
        , _followNodeRotation(followNodeRot)
        , _focusNode(focusNode)
        , _scale(scale)
    {}

    glm::dvec3 _position = glm::dvec3(0.0);
    glm::dquat _rotation = glm::dquat(1.0, 0.0, 0.0, 0.0);
    bool _followNodeRotation = false;
    std::string _focusNode;
    float _scale = 0.f;

    double _timestamp = 0.0;

    void serialize(std::vector<char>& buffer) const {
        // Add position
        buffer.insert(
            buffer.end(),
            reinterpret_cast<const char*>(&_position),
            reinterpret_cast<const char*>(&_position) + sizeof(_position)
        );

        // Add orientation
        buffer.insert(
            buffer.end(),
            reinterpret_cast<const char*>(&_rotation),
            reinterpret_cast<const char*>(&_rotation) + sizeof(_rotation)
        );

        // Follow focus node rotation?
        buffer.insert(
            buffer.end(),
            reinterpret_cast<const char*>(&_followNodeRotation),
            reinterpret_cast<const char*>(&_followNodeRotation) +
                sizeof(_followNodeRotation)
        );

        uint32_t nodeNameLength = static_cast<uint32_t>(_focusNode.size());

        // Add focus node
        buffer.insert(
            buffer.end(),
            reinterpret_cast<const char*>(&nodeNameLength),
            reinterpret_cast<const char*>(&nodeNameLength) + sizeof(uint32_t)
        );
        buffer.insert(
            buffer.end(),
            _focusNode.data(),
            _focusNode.data() + nodeNameLength
        );

        buffer.insert(
            buffer.end(),
            reinterpret_cast<const char*>(&_scale),
            reinterpret_cast<const char*>(&_scale) + sizeof(_scale)
        );

        // Add timestamp
        buffer.insert(
            buffer.end(),
            reinterpret_cast<const char*>(&_timestamp),
            reinterpret_cast<const char*>(&_timestamp) + sizeof(_timestamp)
        );
    }

    size_t deserialize(const std::vector<char>& buffer, size_t offset = 0) {
        int size = 0;

        // Position
        size = sizeof(_position);
        std::memcpy(glm::value_ptr(_position), buffer.data() + offset, size);
        offset += size;

        // Orientation
        size = sizeof(_rotation);
        std::memcpy(glm::value_ptr(_rotation), buffer.data() + offset, size);
        offset += size;

        // Follow focus node rotation?
        size = sizeof(_followNodeRotation);
        std::memcpy(&_followNodeRotation, buffer.data() + offset, size);
        offset += size;

        // Focus node
        int nodeNameLength;
        size = sizeof(int);
        std::memcpy(&nodeNameLength, buffer.data() + offset, size);
        offset += size;
        size = nodeNameLength;
        _focusNode = std::string(buffer.data() + offset, buffer.data() + offset + size);
        offset += size;

        // Scale
        size = sizeof(_scale);
        std::memcpy(&_scale, buffer.data() + offset, size);
        offset += size;

        // Timestamp
        size = sizeof(_timestamp);
        std::memcpy(&_timestamp, buffer.data() + offset, size);
        offset += size;

        return offset;
    }

    void write(std::ostream& out) const {
        out.write(
            reinterpret_cast<const char*>(glm::value_ptr(_position)),
            sizeof(_position)
        );
        out.write(
            reinterpret_cast<const char*>(glm::value_ptr(_rotation)),
            sizeof(_rotation)
        );

        // Write follow focus node rotation?
        out.write(
            reinterpret_cast<const char*>(&_followNodeRotation),
            sizeof(_followNodeRotation)
        );

        int nodeNameLength = static_cast<int>(_focusNode.size());

        // Write focus node
        out.write(reinterpret_cast<const char*>(&nodeNameLength), sizeof(nodeNameLength));
        out.write(_focusNode.c_str(), _focusNode.size());

        // Write scale
        out.write(reinterpret_cast<const char*>(&_scale), sizeof(_scale));

        // Write timestamp
        out.write(reinterpret_cast<const char*>(&_timestamp), sizeof(_timestamp));
    }

    void write(std::stringstream& out) const {
        // Add camera position
        out << std::fixed << std::setprecision(7) << _position.x << ' '
            << std::fixed << std::setprecision(7) << _position.y << ' '
            << std::fixed << std::setprecision(7) << _position.z << ' ';
        // Add camera rotation
        out << std::fixed << std::setprecision(7) << _rotation.x << ' '
            << std::fixed << std::setprecision(7) << _rotation.y << ' '
            << std::fixed << std::setprecision(7) << _rotation.z << ' '
            << std::fixed << std::setprecision(7) << _rotation.w << ' ';
        out << std::fixed
            << std::setprecision(std::numeric_limits<double>::max_digits10)
            << _scale << ' ';
        if (_followNodeRotation) {
            out << "F ";
        }
        else {
            out << "- ";
        }
        out << _focusNode;
    }

    void read(std::istream* in) {
        // Read position
        in->read(reinterpret_cast<char*>(&_position), sizeof(_position));

        // Read orientation
        in->read(reinterpret_cast<char*>(&_rotation), sizeof(_rotation));

        // Read follow focus node rotation
        unsigned char b;
        in->read(reinterpret_cast<char*>(&b), sizeof(unsigned char));
        _followNodeRotation = (b == 1);

        // Read focus node
        int nodeNameLength = static_cast<int>(_focusNode.size());
        in->read(reinterpret_cast<char*>(&nodeNameLength), sizeof(nodeNameLength));
        std::vector<char> temp(static_cast<size_t>(nodeNameLength) + 1);
        in->read(temp.data(), nodeNameLength);

        temp[nodeNameLength] = '\0';
        _focusNode = temp.data();

        // Read scale
        in->read(reinterpret_cast<char*>(&_scale), sizeof(_scale));

        // Read timestamp
        in->read(reinterpret_cast<char*>(&_timestamp), sizeof(_timestamp));
    }

    void read(std::istringstream& iss) {
        std::string rotationFollowing;

        iss >> _position.x
            >> _position.y
            >> _position.z
            >> _rotation.x
            >> _rotation.y
            >> _rotation.z
            >> _rotation.w
            >> _scale
            >> rotationFollowing
            >> _focusNode;
        _followNodeRotation = (rotationFollowing == "F");
    }
};

struct TimeKeyframe {
    TimeKeyframe() = default;
    TimeKeyframe(const std::vector<char>& buffer) {
        deserialize(buffer);
    }

    double _time = 0.0;
    double _dt = 0.0;
    bool _paused = false;
    bool _requiresTimeJump = false;
    double _timestamp = 0.0;

    void serialize(std::vector<char>& buffer) const {
        buffer.insert(
            buffer.end(),
            reinterpret_cast<const char*>(this),
            reinterpret_cast<const char*>(this) + sizeof(TimeKeyframe)
        );
    }

    size_t deserialize(const std::vector<char>& buffer, size_t offset = 0) {
        *this = *reinterpret_cast<const TimeKeyframe*>(buffer.data() + offset);
        offset += sizeof(TimeKeyframe);
        return offset;
    }

    void write(std::ostream* out) const {
        out->write(reinterpret_cast<const char*>(this), sizeof(TimeKeyframe));
    }

    void write(std::stringstream& out) const {
        out << ' ' << _dt;
        if (_paused) {
            out << " P";
        }
        else {
            out << " R";
        }
        if (_requiresTimeJump) {
            out << " J";
        }
        else {
            out << " -";
        }
    }

    void read(std::istream* in) {
        in->read(reinterpret_cast<char*>(this), sizeof(TimeKeyframe));
    }

    void read(std::istringstream& iss) {
        std::string paused, jump;

        iss >> _dt
            >> paused
            >> jump;
        _paused = (paused == "P");
        _requiresTimeJump = (jump == "J");
    }
};

struct TimeTimeline {
    TimeTimeline() = default;
    TimeTimeline(const std::vector<char>& buffer) {
        deserialize(buffer);
    }

    bool _clear = true;
    std::vector<TimeKeyframe> _keyframes;

    void serialize(std::vector<char>& buffer) const {
        buffer.insert(
            buffer.end(),
            reinterpret_cast<const char*>(&_clear),
            reinterpret_cast<const char*>(&_clear) + sizeof(bool)
        );

        int64_t nKeyframes = _keyframes.size();
        buffer.insert(
            buffer.end(),
            reinterpret_cast<const char*>(&nKeyframes),
            reinterpret_cast<const char*>(&nKeyframes) + sizeof(int64_t)
        );
        for (const TimeKeyframe& k : _keyframes) {
            k.serialize(buffer);
        }
    }

    size_t deserialize(const std::vector<char>& buffer, size_t offset = 0) {
        int size = 0;

        size = sizeof(_clear);
        std::memcpy(&_clear, buffer.data() + offset, size);
        offset += size;

        int64_t nKeyframes = _keyframes.size();
        size = sizeof(nKeyframes);
        std::memcpy(&nKeyframes, buffer.data() + offset, size);
        offset += size;

        _keyframes.resize(nKeyframes);
        for (TimeKeyframe& k : _keyframes) {
            offset = k.deserialize(buffer, offset);
        }
        return offset;
    }

    void write(std::ostream* out) const {
        out->write(reinterpret_cast<const char*>(&_clear), sizeof(bool));

        int64_t nKeyframes = _keyframes.size();
        out->write(reinterpret_cast<const char*>(&nKeyframes), sizeof(int64_t));
        for (const TimeKeyframe& k : _keyframes) {
            k.write(out);
        }
    }

    void read(std::istream* in) {
        in->read(reinterpret_cast<char*>(&_clear), sizeof(bool));

        int64_t nKeyframes = _keyframes.size();
        in->read(reinterpret_cast<char*>(&nKeyframes), sizeof(int64_t));
        for (TimeKeyframe& k : _keyframes) {
            k.read(in);
        }
    }
};

struct ScriptMessage {
    ScriptMessage() = default;
    ScriptMessage(const std::vector<char>& buffer) {
        deserialize(buffer);
    }
    virtual ~ScriptMessage() {}

    std::string _script;
    double _timestamp = 0.0;

    void serialize(std::vector<char>& buffer) const {
        uint32_t strLen = static_cast<uint32_t>(_script.size());

        const char* p = reinterpret_cast<const char*>(&strLen);
        buffer.insert(buffer.end(), p, p + sizeof(uint32_t));

        buffer.insert(buffer.end(), _script.begin(), _script.end());
    }

    void deserialize(const std::vector<char>& buffer) {
        const char* p = buffer.data();
        const uint32_t len = *reinterpret_cast<const uint32_t*>(p);

        if (buffer.size() != (sizeof(uint32_t) + len)) {
            LERRORC(
                "ParallelPeer",
                std::format(
                    "Received buffer with wrong size. Expected {} got {}",
                    len, buffer.size()
                )
            );
            return;
        }

        // We can skip over the first uint32_t that encoded the length
        _script.assign(buffer.begin() + sizeof(uint32_t), buffer.end());
    }

    void write(std::ostream* out) const {
        out->write(_script.c_str(), _script.size());
    }

    void write(unsigned char* buf, size_t& idx, std::ofstream& file) const {
        size_t strLen = _script.size();
        size_t writeSize_bytes = sizeof(size_t);

        unsigned char const *p = reinterpret_cast<unsigned char const*>(&strLen);
        memcpy((buf + idx), p, writeSize_bytes);
        idx += static_cast<unsigned int>(writeSize_bytes);

        memcpy((buf + idx), _script.c_str(), _script.size());
        idx += static_cast<unsigned int>(strLen);
        file.write(reinterpret_cast<char*>(buf), idx);
        //Write directly to file because some scripts can be very long
        file.write(_script.c_str(), _script.size());
    }

    void write(std::stringstream& ss) const {
        unsigned int numLinesInScript = static_cast<unsigned int>(
            std::count(_script.begin(), _script.end(), '\n')
        );
        ss << ' ' << (numLinesInScript + 1) << ' ';
        ss << _script;
    }

    virtual void read(std::istream* in) {
        uint32_t strLen;
        //Read string length from file
        in->read(reinterpret_cast<char*>(&strLen), sizeof(strLen));
        //Read back full string
        std::vector<char> temp(strLen + 1);
        in->read(temp.data(), strLen);
        temp[strLen] = '\0';

        _script.erase();
        _script = temp.data();
    }

    void read(std::istringstream& iss) {
        int numScriptLines;
        iss >> numScriptLines;
        if (numScriptLines < 0) {
            numScriptLines = 0;
        }
        std::string tmpReadbackScript;
        _script.erase();
        for (int i = 0; i < numScriptLines; i++) {
            std::getline(iss, tmpReadbackScript);
            size_t start = tmpReadbackScript.find_first_not_of(" ");
            tmpReadbackScript = tmpReadbackScript.substr(start);
            _script.append(tmpReadbackScript);
            if (i < (numScriptLines - 1)) {
                _script.append("\n");
            }
        }
    }
};

} // namespace openspace::messagestructures

#endif // __OPENSPACE_CORE___MESSAGESTRUCTURES___H__
