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

namespace openspace {

namespace softwareintegration {

namespace storage {
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

SimpError::SimpError(const tools::ErrorCode _errorCode, const std::string& msg)
    : errorCode{errorCode}, ghoul::RuntimeError(fmt::format("{}: Error Code: {} - {}", "SIMP error", static_cast<uint32_t>(_errorCode), msg), "Software Integration Messaging Protocol error")
{}

bool tools::isEndOfCurrentValue(const std::vector<char>& message, size_t offset) {
    if (offset >= message.size()) {
        throw SimpError(
            tools::ErrorCode::OffsetLargerThanMessageSize,
            "Unexpectedly reached the end of the message..."
        );
    }

    if (message.size() > 0 && offset == message.size() - 1 && message[offset] != SEP) {
        throw SimpError(
            tools::ErrorCode::ReachedEndBeforeSeparator,
            "Reached end of message before reading separator character..."
        );
    }

    return offset != 0 && message[offset] == SEP && message[offset - 1] != '\\';
}

MessageType getMessageType(const std::string& type) {
    if (tools::_messageTypeFromSIMPType.count(type) == 0) return MessageType::Unknown;
    return tools::_messageTypeFromSIMPType.at(type);
}

std::string getSIMPType(const MessageType& type) {
    auto it = std::find_if(
        tools::_messageTypeFromSIMPType.begin(),
        tools::_messageTypeFromSIMPType.end(),
        [type](const std::pair<const std::string, MessageType>& p) {
            return type == p.second;
        }
    );
    if (it == tools::_messageTypeFromSIMPType.end()) return "UNKN";
    return it->first;
}

CmapNaNMode getCmapNaNMode(const std::string& type) {
    if (tools::_cmapNaNModeFromString.count(type) == 0) return CmapNaNMode::Unknown;
    return tools::_cmapNaNModeFromString.at(type);
}

std::string formatLengthOfSubject(size_t lengthOfSubject) {
    // Format length of subject to always be 15 digits
    std::ostringstream os;
    os << std::setfill('0') << std::setw(15) << lengthOfSubject;
    return os.str();
}

std::string formatUpdateMessage(MessageType messageType,
                                std::string_view identifier,
                                std::string_view value)
{
    std::string subject = fmt::format(
        "{}{}{}{}", identifier, SEP, value, SEP
    );

    const std::string lengthOfSubject = formatLengthOfSubject(subject.length());

    return fmt::format("{}{}{}{}", ProtocolVersion, getSIMPType(messageType), lengthOfSubject, subject);
}

std::string formatConnectionMessage(std::string_view software) {
    std::string subject = fmt::format(
        "{}{}", software, SEP
    );

    const std::string lengthOfSubject = formatLengthOfSubject(subject.length());

    return fmt::format("{}{}{}{}", ProtocolVersion, getSIMPType(MessageType::Connection), lengthOfSubject, subject);
}

std::string formatColorMessage(std::string_view identifier, glm::vec4 color) {
    std::ostringstream value_stream;
    value_stream << "[" << floatToHex(color.r) << SEP << floatToHex(color.g) << SEP
                << floatToHex(color.b) << SEP << floatToHex(color.a) << SEP << "]";

    return formatUpdateMessage(MessageType::Color, identifier, value_stream.str());
}

std::string formatPointDataCallbackMessage(std::string_view identifier) {
    std::string subject = fmt::format(
        "{}{}", identifier, SEP
    );

    const std::string lengthOfSubject = formatLengthOfSubject(subject.length());

    return fmt::format("{}{}{}{}", ProtocolVersion, getSIMPType(MessageType::PointData), lengthOfSubject, subject);
}

std::string floatToHex(const float f) {
    const uint32_t *int_representation = reinterpret_cast<const uint32_t *>(&f);
    std::ostringstream stream;
    stream << "0x" << std::setfill ('0') << std::setw(sizeof(float)*2) << std::hex << *int_representation;

    return stream.str();
}

float hexToFloat(const std::string& f) {
    uint32_t int_value = static_cast<uint32_t>(std::stoul(f, nullptr, 16));
    std::ostringstream stream;
    stream << std::dec << int_value;
    return *reinterpret_cast<float*>(&int_value);
}

int readIntValue(const std::vector<char>& message, size_t& offset) {
    std::string string_value;
    int value;
    bool isHex = false;

    while (!tools::isEndOfCurrentValue(message, offset)) {
        char c = message[offset];
        if (c == 'x' || c == 'X') isHex = true;
        string_value.push_back(c);
        offset++;
    }

    try {
        value = std::stoi(string_value, nullptr, isHex ? 16 : 10);
    }
    catch(std::exception &err) {
        throw SimpError(
            tools::ErrorCode::Generic,
            fmt::format("Error when trying to parse the integer {}: {}", string_value, err.what())
        );
    }

    ++offset;
    return value;
}

float readFloatValue(const std::vector<char>& message, size_t& offset) {
    std::string string_value;
    float value;

    while (!tools::isEndOfCurrentValue(message, offset)) {
        string_value.push_back(message[offset]);
        offset++;
    }

    try {
        value = hexToFloat(string_value);
    }
    catch(std::exception &err) {
        throw SimpError(
            tools::ErrorCode::Generic,
            fmt::format("Error when trying to parse the float {}: {}", string_value, err.what())
        );
    }

    ++offset;
    return value;
}

void readColormap(
    const std::vector<char>& message, size_t& offset, size_t nColors, std::vector<float>& colorMap
) {
    colorMap.reserve(nColors * 4);
    while (message[offset] != SEP) {
        glm::vec4 color = tools::readSingleColor(message, offset);
        
        // Colormap should be stored in a sequential vector 
        // of floats for syncing between nodes and when 
        // loaded to as a texture in the shader.
        colorMap.push_back(color[0]);
        colorMap.push_back(color[1]);
        colorMap.push_back(color[2]);
        colorMap.push_back(color[3]);
    }
    
    offset++;
}

glm::vec4 tools::readSingleColor(const std::vector<char>& message, size_t& offset) {
    if (message[offset] != '[') {
        throw SimpError(
            tools::ErrorCode::Generic,
            fmt::format("Expected to read '[', got {} in 'readColor'", message[offset])
        );
    }
    ++offset;

    float r = readFloatValue(message, offset);
    float g = readFloatValue(message, offset);
    float b = readFloatValue(message, offset);
    float a = readFloatValue(message, offset);

    if (message[offset] != ']') {
        throw SimpError(
            tools::ErrorCode::Generic,
            fmt::format("Expected to read ']', got {} in 'readColor'", message[offset])
        );
    }
    ++offset;

    return { r, g, b, a };
}

glm::vec4 readColor(const std::vector<char>& message, size_t& offset) {
    glm::vec4 color = tools::readSingleColor(message, offset);
    ++offset;
    return color;
}

std::string readString(const std::vector<char>& message, size_t& offset) {
    std::string value;
    while (!tools::isEndOfCurrentValue(message, offset)) {
        value.push_back(message[offset]);
        ++offset;
    }

    ++offset;
    return value;
}

void readPointData(
    const std::vector<char>& message,
    size_t& offset,
    size_t nPoints,
    size_t dimensionality,
    std::vector<float>& pointData
) {
    pointData.reserve(nPoints * dimensionality);

    while (!tools::isEndOfCurrentValue(message, offset)) {
        if (message[offset] != '[') {
            throw SimpError(
                tools::ErrorCode::Generic,
                fmt::format("Expected to read '[', got {} in 'readPointData'", message[offset])
            );
        }
        ++offset;

        for (int i = 0; i < dimensionality; ++i) {
            pointData.push_back(readFloatValue(message, offset));
        }

        if (message[offset] != ']') {
            throw SimpError(
                tools::ErrorCode::Generic,
                fmt::format("Expected to read ']', got {} in 'readPointData'", message[offset])
            );
        }
        ++offset;
    }

    ++offset;
}

} // namespace simp

} // namespace softwareintegration

} // namespace openspace
