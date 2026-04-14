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

#include <openspace/network/messagestructures.h>

#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/stringhelper.h>
#include <algorithm>
#include <cstring>

namespace openspace::datamessagestructures {

CameraKeyframe::CameraKeyframe(const std::vector<char>& buffer) {
    deserialize(buffer);
}

CameraKeyframe::CameraKeyframe(glm::dvec3 pos, glm::dquat rot, std::string focusNode,
                               bool followNodeRot, float scale)
    : _position(pos)
    , _rotation(rot)
    , _followNodeRotation(followNodeRot)
    , _focusNode(focusNode)
    , _scale(scale)
{}

void CameraKeyframe::serialize(std::vector<char>& buffer) const {
    buffer.insert(
        buffer.end(),
        reinterpret_cast<const char*>(&_position),
        reinterpret_cast<const char*>(&_position) + sizeof(_position)
    );

    buffer.insert(
        buffer.end(),
        reinterpret_cast<const char*>(&_rotation),
        reinterpret_cast<const char*>(&_rotation) + sizeof(_rotation)
    );

    buffer.insert(
        buffer.end(),
        reinterpret_cast<const char*>(&_followNodeRotation),
        reinterpret_cast<const char*>(&_followNodeRotation) +
        sizeof(_followNodeRotation)
    );

    uint32_t nodeNameLength = static_cast<uint32_t>(_focusNode.size());
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

    buffer.insert(
        buffer.end(),
        reinterpret_cast<const char*>(&_timestamp),
        reinterpret_cast<const char*>(&_timestamp) + sizeof(_timestamp)
    );
}

size_t CameraKeyframe::deserialize(const std::vector<char>& buffer, size_t offset) {
    int size = 0;

    size = sizeof(_position);
    std::memcpy(glm::value_ptr(_position), buffer.data() + offset, size);
    offset += size;

    size = sizeof(_rotation);
    std::memcpy(glm::value_ptr(_rotation), buffer.data() + offset, size);
    offset += size;

    size = sizeof(_followNodeRotation);
    std::memcpy(&_followNodeRotation, buffer.data() + offset, size);
    offset += size;

    int nodeNameLength;
    size = sizeof(int);
    std::memcpy(&nodeNameLength, buffer.data() + offset, size);
    offset += size;
    size = nodeNameLength;
    _focusNode = std::string(buffer.data() + offset, buffer.data() + offset + size);
    offset += size;

    size = sizeof(_scale);
    std::memcpy(&_scale, buffer.data() + offset, size);
    offset += size;

    size = sizeof(_timestamp);
    std::memcpy(&_timestamp, buffer.data() + offset, size);
    offset += size;

    return offset;
}

TimeKeyframe::TimeKeyframe(const std::vector<char>& buffer) {
    deserialize(buffer);
}

void TimeKeyframe::serialize(std::vector<char>& buffer) const {
    buffer.insert(
        buffer.end(),
        reinterpret_cast<const char*>(this),
        reinterpret_cast<const char*>(this) + sizeof(TimeKeyframe)
    );
}

size_t TimeKeyframe::deserialize(const std::vector<char>& buffer, size_t offset) {
    *this = *reinterpret_cast<const TimeKeyframe*>(buffer.data() + offset);
    offset += sizeof(TimeKeyframe);
    return offset;
}

TimeTimeline::TimeTimeline(const std::vector<char>& buffer) {
    deserialize(buffer);
}

void TimeTimeline::serialize(std::vector<char>& buffer) const {
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

size_t TimeTimeline::deserialize(const std::vector<char>& buffer, size_t offset) {
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

void ScriptMessage::serialize(std::vector<char>& buffer) const {
    uint32_t strLen = static_cast<uint32_t>(_script.size());

    const char* p = reinterpret_cast<const char*>(&strLen);
    buffer.insert(buffer.end(), p, p + sizeof(uint32_t));

    buffer.insert(buffer.end(), _script.begin(), _script.end());
}

void ScriptMessage::deserialize(const std::vector<char>& buffer) {
    const char* p = buffer.data();
    const uint32_t len = *reinterpret_cast<const uint32_t*>(p);

    if (buffer.size() != (sizeof(uint32_t) + len)) {
        LERRORC(
            "ScriptMessage",
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

} // namespace openspace::datamessagestructures
