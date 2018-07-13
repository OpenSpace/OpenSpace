/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
        for (const auto k : _keyframes) {
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
};

struct ScriptMessage {
    ScriptMessage() {}
    ScriptMessage(const std::vector<char> &buffer) {
        deserialize(buffer);
    }

    std::string _script;

    void serialize(std::vector<char> &buffer) const {
        buffer.insert(buffer.end(), _script.begin(), _script.end());
    };

    void deserialize(const std::vector<char> &buffer) {
        _script.assign(buffer.begin(), buffer.end());
    };
};

} // namespace openspace::messagestructures

#endif // __OPENSPACE_CORE___MESSAGESTRUCTURES___H__
