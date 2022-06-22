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

#include <modules/softwareintegration/utils.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionaryluaformatter.h>

#include <iomanip>

namespace {
    constexpr const char* _loggerCat = "SoftwareIntegrationMessageFormat";
} // namespace

namespace openspace::softwareintegration {


namespace storage {

// Anonymous namespace
namespace {
    
    const std::unordered_map<std::string, Key> _keyStringFromKey{
        { "DataPoints", Key::DataPoints },
        { "VelocityData", Key::VelocityData },
        { "Colormap", Key::Colormap },
        { "ColormapAttributeData", Key::ColormapAttrData },
        { "LinearSizeAttributeData", Key::LinearSizeAttrData },
    };

} // namespace

bool hasStorageKey(const std::string& key) {
    return _keyStringFromKey.count(key) > 0;
}

Key getStorageKey(const std::string& key) {
    if (hasStorageKey(key)) {
        return _keyStringFromKey.at(key);
    }

    return Key::Unknown;
}

std::string getStorageKeyString(const Key key) {
    auto it = std::find_if(
        _keyStringFromKey.begin(),
        _keyStringFromKey.end(),
        [key](const std::pair<const std::string, Key>& p) {
            return key == p.second;
        }
    );
    if (it == _keyStringFromKey.end()) return "";
    return it->first;
}

} // namespace storage

namespace simp {

// Anonymous namespace
namespace {

const std::unordered_map<std::string, MessageType> _messageTypeFromSIMPType {
    { "CONN", MessageType::Connection },
    { "DATA", MessageType::Data },
    { "RSGN", MessageType::RemoveSceneGraphNode },
};

const std::unordered_map<std::string, DataKey> _dataTypeFromString{
    { "pos.x", DataKey::X },
    { "pos.y", DataKey::Y },
    { "pos.z", DataKey::Z },
    { "pos.unit", DataKey::PointUnit },
    { "vel.u", DataKey::U },
    { "vel.v", DataKey::V },
    { "vel.w", DataKey::W },
    { "vel.unit.dist", DataKey::VelocityDistanceUnit },
    { "vel.unit.time", DataKey::VelocityTimeUnit },
    { "vel.nan.mode", DataKey::VelocityNanMode },
    { "vel.enable", DataKey::VelocityEnabled },
    { "col.r", DataKey::Red },
    { "col.g", DataKey::Green },
    { "col.b", DataKey::Blue },
    { "col.a", DataKey::Alpha },
    { "cmap.enable", DataKey::ColormapEnabled },
    { "cmap.r", DataKey::ColormapReds },
    { "cmap.g", DataKey::ColormapGreens },
    { "cmap.b", DataKey::ColormapBlues },
    { "cmap.a", DataKey::ColormapAlphas },
    { "cmap.min", DataKey::ColormapMin },
    { "cmap.max", DataKey::ColormapMax },
    { "cmap.nan.r", DataKey::ColormapNanR },
    { "cmap.nan.g", DataKey::ColormapNanG },
    { "cmap.nan.b", DataKey::ColormapNanB },
    { "cmap.nan.a", DataKey::ColormapNanA },
    { "cmap.nan.mode", DataKey::ColormapNanMode },
    { "cmap.attr", DataKey::ColormapAttributeData },
    { "size.val", DataKey::FixedSize },
    { "lsize.enabled", DataKey::LinearSizeEnabled },
    { "lsize.min", DataKey::LinearSizeMin },
    { "lsize.max", DataKey::LinearSizeMax },
    { "lsize.attr", DataKey::LinearSizeAttributeData },
    { "vis.val", DataKey::Visibility },
};

const std::unordered_map<std::string, ColormapNanRenderMode> _colormapNanRenderModeFromString {
    { "Hide", ColormapNanRenderMode::Hide },
    { "FixedColor", ColormapNanRenderMode::FixedColor },
};

const std::unordered_map<std::string, VelocityNanRenderMode> _velocityNanRenderModeFromString {
    { "Hide", VelocityNanRenderMode::Hide },
    { "Static", VelocityNanRenderMode::Static },
};

// glm::vec4 readSingleColor(const std::vector<std::byte>& message, size_t& offset) {
//     if (message[offset] != '[') {
//         throw SimpError(
//             tools::ErrorCode::Generic,
//             fmt::format("Expected to read '[', got {} in 'readColor'", message[offset])
//         );
//     }
//     ++offset;

//     float r = readFloat32Value(message, offset);
//     float g = readFloat32Value(message, offset);
//     float b = readFloat32Value(message, offset);
//     float a = readFloat32Value(message, offset);

//     if (message[offset] != ']') {
//         throw SimpError(
//             tools::ErrorCode::Generic,
//             fmt::format("Expected to read ']', got {} in 'readColor'", message[offset])
//         );
//     }
//     ++offset;

//     return { r, g, b, a };
// }

void checkOffset(const std::vector<std::byte>& message, size_t offset) {
    if (offset >= message.size()) {
        throw SimpError(
            tools::ErrorCode::OffsetLargerThanMessageSize,
            "Offset is larger than length of message..."
        );
    }
}

void checkOffset(const std::vector<std::byte>& message, std::vector<size_t>& offsets) {
    for (size_t offset : offsets) {
        checkOffset(message, offset);
    }
}

int32_t readInt32Value(const std::vector<std::byte>& message, size_t& offset) {
    std::vector<size_t> offsetsToCheck{ offset, offset + 3 };
    checkOffset(message, offsetsToCheck);
    int32_t value;

    try {
        // Read 4 bytes
        std::memcpy(&value, message.data() + offset, 4);
    }
    catch(std::exception &err) {
        throw SimpError(
            fmt::format("Error when trying to parse an integer at offset {}", err.what())
        );
    }

    offset += 4;
    return networkToHostEndian(value);
}

bool readBoolValue(const std::vector<std::byte>& message, size_t& offset) {
    checkOffset(message, offset);
    bool value;

    try {
        // Read 1 byte
        std::memcpy(&value, message.data() + offset, 1);
    }
    catch(std::exception &err) {
        throw SimpError(
            fmt::format("Error when trying to parse a bool at offset {}", err.what())
        );
    }

    offset += 1;
    return value;
}

/**
 * Assumption: float is 4 bytes
 * Maybe add a check in beginning?
 */
float readFloat32Value(const std::vector<std::byte>& message, size_t& offset) {
    std::vector<size_t> offsetsToCheck{ offset, offset + 3 };
    checkOffset(message, offsetsToCheck);
    float value;

    try {
        // Read 4 bytes
        std::memcpy(&value, message.data() + offset, 4);
    }
    catch(std::exception &err) {
        throw SimpError(
            fmt::format("Error when trying to parse a float at offset = {}", err.what())
        );
    }
    offset += 4;
    return networkToHostEndian(value);
}

size_t findEndOfString(const std::vector<std::byte>& message, size_t offset) {
    checkOffset(message, offset);

    auto delimIt = std::find(message.begin() + offset, message.end(), DELIM_BYTES);
    auto delimOffset = std::distance(message.begin(), delimIt);
    while (message[delimOffset - 1] == std::byte{ '\\' }) {
        delimIt = std::find(message.begin() + delimOffset + 1, message.end(), DELIM_BYTES);
        delimOffset = std::distance(message.begin(), delimIt);
    }

    if (delimIt == message.end()) {
        throw SimpError(
            tools::ErrorCode::ReachedEndBeforeSeparator,
            "Message reached end before delimiter character"
        );
    }

    checkOffset(message, delimOffset); // Sanity check

    return delimOffset;
}

std::string readString(const std::vector<std::byte>& message, size_t& offset) {
    checkOffset(message, offset);

    size_t offsetAtEndOfString = findEndOfString(message, offset);

    std::vector<std::byte> stringByteBuffer{ message.begin() + offset, message.begin() + offsetAtEndOfString };
    std::string value{ reinterpret_cast<const char *>(stringByteBuffer.data()), stringByteBuffer.size() };

    offset = offsetAtEndOfString + 1; // +1 because we need to skip delimiter char in next value read
    return value;
}

}  // namespace

SimpError::SimpError(const tools::ErrorCode _errorCode, const std::string& msg)
    : errorCode{errorCode}, ghoul::RuntimeError(fmt::format("{}: Error Code: {} - {}", "SIMP error", static_cast<uint32_t>(_errorCode), msg), "Software Integration Messaging Protocol error")
{}

SimpError::SimpError(const std::string& msg)
    : errorCode{tools::ErrorCode::Generic}, ghoul::RuntimeError(fmt::format("{}: Error Code: {} - {}", "SIMP error", static_cast<uint32_t>(errorCode), msg), "Software Integration Messaging Protocol error")
{}

MessageType getMessageType(const std::string& type) {
    if (_messageTypeFromSIMPType.count(type) == 0) return MessageType::Unknown;
    return _messageTypeFromSIMPType.at(type);
}

std::string getStringFromMessageType(const MessageType& type) {
    auto it = std::find_if(
        _messageTypeFromSIMPType.begin(),
        _messageTypeFromSIMPType.end(),
        [type](const std::pair<const std::string, MessageType>& p) {
            return type == p.second;
        }
    );
    if (it == _messageTypeFromSIMPType.end()) return "UNKN";
    return it->first;
}

bool hasDataKey(const std::string& key) {
    return _dataTypeFromString.count(key) > 0;
}

DataKey getDataKey(const std::string& key) {
    if (hasDataKey(key)) {
        return _dataTypeFromString.at(key);
    }
    return DataKey::Unknown;
}

std::string getStringFromDataKey(const DataKey& key) {
    auto it = std::find_if(
        _dataTypeFromString.begin(),
        _dataTypeFromString.end(),
        [key](const std::pair<const std::string, DataKey>& p) {
            return key == p.second;
        }
    );
    if (it == _dataTypeFromString.end()) return "";
    return it->first;
}

ColormapNanRenderMode getColormapNanRenderMode(const std::string& type) {
    if (_colormapNanRenderModeFromString.count(type) == 0) return ColormapNanRenderMode::Unknown;
    return _colormapNanRenderModeFromString.at(type);
}

VelocityNanRenderMode getVelocityNanRenderMode(const std::string& type) {
    if (_velocityNanRenderModeFromString.count(type) == 0) return VelocityNanRenderMode::Unknown;
    return _velocityNanRenderModeFromString.at(type);
}

std::string formatLengthOfSubject(size_t lengthOfSubject) {
    // Format length of subject to always be 15 digits
    std::ostringstream os;
    os << std::setfill('0') << std::setw(15) << lengthOfSubject;
    return os.str();
}

bool readColorChannel(
    const std::vector<std::byte>& message,
    size_t& offset,
    const DataKey& dataKey,
    glm::vec4& color,
    const glm::vec4::length_type& channel
) {
    float newChannelValue;
    try {
        simp::readValue(message, offset, newChannelValue);
    }
    catch (const simp::SimpError& err) {
        LERROR(fmt::format(
            "Error when parsing float in {} message: {}",
            simp::getStringFromDataKey(dataKey), err.message
        ));
        return false;
    }
    color[channel] = newChannelValue;

    return true;
}

void readValue(const std::vector<std::byte>& message, size_t& offset, float& value) {
    value = readFloat32Value(message, offset);
}

void readValue(const std::vector<std::byte>& message, size_t& offset, int32_t& value) {
    value = readInt32Value(message, offset);
}

void readValue(const std::vector<std::byte>& message, size_t& offset, std::string& value) {
    value = readString(message, offset);
}

void readValue(const std::vector<std::byte>& message, size_t& offset, bool& value) {
    // Bool is only one byte in SIMP => no need to convert endianness
    value = readBoolValue(message, offset);
}

void toByteBuffer(std::vector<std::byte>& byteBuffer, const size_t& offset, float value) {
    byteBuffer.resize(byteBuffer.size() +  4);
    auto valueInNetworkByteorder = hostToNetworkEndian(value);
    std::memcpy(byteBuffer.data() + offset, &valueInNetworkByteorder, 4);
}
void toByteBuffer(std::vector<std::byte>& byteBuffer, size_t& offset, float value) {
    toByteBuffer(byteBuffer, static_cast<const size_t&>(offset), value);
    offset += 4;
}

void toByteBuffer(std::vector<std::byte>& byteBuffer, const size_t& offset, int32_t value) {
    byteBuffer.resize(byteBuffer.size() +  4);
    auto valueInNetworkByteorder = hostToNetworkEndian(value);
    std::memcpy(byteBuffer.data() + offset, &valueInNetworkByteorder, 4);
}
void toByteBuffer(std::vector<std::byte>& byteBuffer, size_t& offset, int32_t value) {
    toByteBuffer(byteBuffer, static_cast<const size_t&>(offset), value);
    offset += 4;
}

void toByteBuffer(std::vector<std::byte>& byteBuffer, const size_t& offset, bool value) {
    byteBuffer.resize(byteBuffer.size() +  1);
    // Bool is only one byte in SIMP => no need to convert endianness (byteorder)
    std::memcpy(byteBuffer.data() + offset, &value, 1);
}
void toByteBuffer(std::vector<std::byte>& byteBuffer, size_t& offset, bool value) {
    toByteBuffer(byteBuffer, static_cast<const size_t&>(offset), value);
    offset += 1;
}

void toByteBuffer(std::vector<std::byte>& byteBuffer, const size_t& offset, const std::string& value) {
    byteBuffer.resize(byteBuffer.size() + (value.size() * sizeof(char)));
    // Don't want to convert byteorder of strings, since we want to 
    std::memcpy(byteBuffer.data() + offset, value.data(), value.size() * sizeof(char));
}
void toByteBuffer(std::vector<std::byte>& byteBuffer, size_t& offset, const std::string& value) {
    toByteBuffer(byteBuffer, static_cast<const size_t&>(offset), value);
    offset += value.size() * sizeof(char);
}

void toByteBuffer(std::vector<std::byte>& byteBuffer, const size_t& offset, const std::vector<std::byte>& value) {
    byteBuffer.resize(byteBuffer.size() + value.size());
    std::memcpy(byteBuffer.data() + offset, value.data(), value.size());
}
void toByteBuffer(std::vector<std::byte>& byteBuffer, size_t& offset, const std::vector<std::byte>& value) {
    toByteBuffer(byteBuffer, static_cast<const size_t&>(offset), value);
    offset += value.size();
}


} // namespace simp

} // namespace openspace::softwareintegration
