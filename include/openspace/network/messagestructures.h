/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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
#include <cstring>
#include <string>
#include <vector>
#include <fstream>

namespace openspace::datamessagestructures {

enum class Type : uint32_t {
    CameraData = 0,
    TimelineData,
    ScriptData
};

struct CameraKeyframe {
    CameraKeyframe() {}
    CameraKeyframe(const std::vector<char> &buffer) {
        deserialize(buffer);
    }

    glm::dvec3 _position;
    glm::dquat _rotation;
    bool _followNodeRotation;
    std::string _focusNode;
    float _scale;

    double _timestamp;

    void serialize(std::vector<char> &buffer) const {
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

        int nodeNameLength = static_cast<int>(_focusNode.size());

        // Add focus node
        buffer.insert(
            buffer.end(),
            reinterpret_cast<const char*>(&nodeNameLength),
            reinterpret_cast<const char*>(&nodeNameLength) + sizeof(nodeNameLength)
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
    };

    size_t deserialize(const std::vector<char> &buffer, size_t offset = 0) {
        int size = 0;

        // Position
        size = sizeof(_position);
        memcpy(&_position, buffer.data() + offset, size);
        offset += size;

        // Orientation
        size = sizeof(_rotation);
        memcpy(&_rotation, buffer.data() + offset, size);
        offset += size;

        // Follow focus node rotation?
        size = sizeof(_followNodeRotation);
        memcpy(&_followNodeRotation, buffer.data() + offset, size);
        offset += size;

        // Focus node
        int nodeNameLength;
        size = sizeof(int);
        memcpy(&nodeNameLength, buffer.data() + offset, size);
        offset += size;
        size = nodeNameLength;
        _focusNode = std::string(buffer.data() + offset, buffer.data() + offset + size);
        offset += size;

        // Scale
        size = sizeof(_scale);
        memcpy(&_scale, buffer.data() + offset, size);
        offset += size;

        // Timestamp
        size = sizeof(_timestamp);
        memcpy(&_timestamp, buffer.data() + offset, size);
        offset += size;

        return offset;
    };

    void write(std::ostream& out) const {
        // Write position
        out.write(
            reinterpret_cast<const char*>(&_position),
            sizeof(_position)
        );

        // Write orientation
        out.write(
            reinterpret_cast<const char*>(&_rotation),
            sizeof(_rotation)
        );

        // Write follow focus node rotation?
        out.write(
            reinterpret_cast<const char*>(&_followNodeRotation),
            sizeof(_followNodeRotation)
        );

        int nodeNameLength = static_cast<int>(_focusNode.size());

        // Write focus node
        out.write(
            reinterpret_cast<const char*>(&nodeNameLength),
            sizeof(nodeNameLength)
        );
        out.write(
            _focusNode.c_str(),
            _focusNode.size()
        );

        //Write scale
        out.write(
            reinterpret_cast<const char*>(&_scale),
            sizeof(_scale)
        );

        // Write timestamp
        out.write(
            reinterpret_cast<const char*>(&_timestamp),
            sizeof(_timestamp)
        );
    };

    void read(std::istream* in) {
        // Read position
        in->read(
            reinterpret_cast<char*>(&_position),
            sizeof(_position)
        );

        // Read orientation
        in->read(
            reinterpret_cast<char*>(&_rotation),
            sizeof(_rotation)
        );

        // Read follow focus node rotation
        unsigned char b;
        in->read(
            reinterpret_cast<char*>(&b),
            sizeof(unsigned char)
        );
        _followNodeRotation = (b == 1);

        // Read focus node
        int nodeNameLength = static_cast<int>(_focusNode.size());
        in->read(
            reinterpret_cast<char*>(&nodeNameLength),
            sizeof(nodeNameLength)
        );
        std::vector<char> temp(nodeNameLength + 1);
        in->read(temp.data(), nodeNameLength);

        temp[nodeNameLength] = '\0';
        _focusNode = temp.data();

        // Read scale
        in->read(
            reinterpret_cast<char*>(&_scale),
            sizeof(_scale)
        );

        // Read timestamp
        in->read(
            reinterpret_cast<char*>(&_timestamp),
            sizeof(_timestamp)
        );
    };

    void read(std::istringstream* iss) {
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
    };
};

struct TimeKeyframe {
    TimeKeyframe() {}
    TimeKeyframe(const std::vector<char> &buffer) {
        deserialize(buffer);
    }

    double _time;
    double _dt;
    bool _paused;
    bool _requiresTimeJump;
    double _timestamp;

    void serialize(std::vector<char> &buffer) const {
        buffer.insert(
            buffer.end(),
            reinterpret_cast<const char*>(this),
            reinterpret_cast<const char*>(this) + sizeof(TimeKeyframe)
        );
    };

    size_t deserialize(const std::vector<char> &buffer, size_t offset = 0){
        *this = *reinterpret_cast<const TimeKeyframe*>(buffer.data() + offset);
        offset += sizeof(TimeKeyframe);
        return offset;
    };

    void write(std::ostream* out) const {
        out->write(
            reinterpret_cast<const char*>(this),
            sizeof(TimeKeyframe)
        );
    };

    void read(std::istream* in) {
        in->read(
            reinterpret_cast<char*>(this),
            sizeof(TimeKeyframe)
        );
    };
};

struct TimeTimeline {
    TimeTimeline() {}
    TimeTimeline(const std::vector<char> &buffer) {
        deserialize(buffer);
    }

    bool _clear = true;
    std::vector<TimeKeyframe> _keyframes;

    void serialize(std::vector<char> &buffer) const {
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
        for (const auto& k : _keyframes) {
            k.serialize(buffer);
        }
    };

    size_t deserialize(const std::vector<char> &buffer, size_t offset = 0) {
        int size = 0;

        size = sizeof(_clear);
        memcpy(&_clear, buffer.data() + offset, size);
        offset += size;

        int64_t nKeyframes = _keyframes.size();
        size = sizeof(nKeyframes);
        memcpy(&nKeyframes, buffer.data() + offset, size);
        offset += size;

        _keyframes.resize(nKeyframes);
        for (auto& k : _keyframes) {
            offset = k.deserialize(buffer, offset);
        }
        return offset;
    };

    void write(std::ostream* out) const {
        out->write(
            reinterpret_cast<const char*>(&_clear),
            sizeof(bool)
        );

        int64_t nKeyframes = _keyframes.size();
        out->write(
            reinterpret_cast<const char*>(&nKeyframes),
            sizeof(int64_t)
        );
        for (const auto& k : _keyframes) {
            k.write(out);
        }
    };

    void read(std::istream* in) {
        in->read(
            reinterpret_cast<char*>(&_clear),
            sizeof(bool)
        );

        int64_t nKeyframes = _keyframes.size();
        in->read(
            reinterpret_cast<char*>(&nKeyframes),
            sizeof(int64_t)
        );
        for (auto& k : _keyframes) {
            k.read(in);
        }
    };
};

struct ScriptMessage {
    ScriptMessage() {}
    ScriptMessage(const std::vector<char> &buffer) {
        deserialize(buffer);
    }

    std::string _script;
    double _timestamp;

    void serialize(std::vector<char> &buffer) const {
        buffer.insert(buffer.end(), _script.begin(), _script.end());
    };

    void deserialize(const std::vector<char> &buffer) {
        _script.assign(buffer.begin(), buffer.end());
    };

    void write(std::ostream* out) const {
        out->write(_script.c_str(), _script.size());
    };

    void read(std::istream* in) {
        size_t strLen;
        //Read string length from file
        in->read(reinterpret_cast<char*>(&strLen), sizeof(strLen));
        //Read back full string
        std::vector<char> temp(strLen + 1);
        in->read(temp.data(), strLen);
        temp[strLen] = '\0';

        _script.erase();
        _script = temp.data();
    };
};

} // namespace openspace::messagestructures

#endif // __OPENSPACE_CORE___MESSAGESTRUCTURES___H__
