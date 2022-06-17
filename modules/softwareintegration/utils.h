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

#ifndef __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SIMP___H__
#define __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SIMP___H__

#include <unordered_map>

namespace openspace::softwareintegration {

namespace storage {

enum class Key : uint8_t {
    DataPoints = 0,
    VelocityData,
    Colormap,
    ColormapAttrData,
    LinearSizeAttrData,
    Unknown
};

Key getStorageKey(const std::string& key);

std::string getStorageKeyString(const Key key);

bool hasStorageKey(const std::string& key);

} // namespace storage

namespace simp {

const std::string ProtocolVersion = "1.9";

const char SEP = ';';

enum class MessageType : uint32_t {
    Connection = 0,
    PointData,
    VelocityData,
    RemoveSceneGraphNode,
    Color,
    Colormap,
    AttributeData,
    Opacity,
    FixedSize,
    LinearSize,
    Visibility,
    Unknown
};

enum class ColormapNaNRenderMode : uint8_t {
    Hide = 0,
    FixedColor,
    Unknown
};

enum class VelocityNaNRenderMode : uint8_t {
    Hide = 0,
    Static,
    Unknown
};

namespace tools {

enum class ErrorCode : uint32_t {
    ReachedEndBeforeSeparator = 0,
    OffsetLargerThanMessageSize,
    InvalidDimensionality,
    Generic,
};

}  // namespace tools

class SimpError : public ghoul::RuntimeError {
public:
    tools::ErrorCode errorCode;
    explicit SimpError(const std::string& msg);
    explicit SimpError(const tools::ErrorCode _errorCode, const std::string& msg);
};

MessageType getMessageType(const std::string& type);

std::string getSIMPType(const MessageType& type);

ColormapNaNRenderMode getColormapNaNRenderMode(const std::string& type);

VelocityNaNRenderMode getVelocityNaNRenderMode(const std::string& type);

std::string formatLengthOfSubject(size_t lengthOfSubject);

std::string formatUpdateMessage(MessageType messageType, std::string_view identifier, std::string_view value);

std::string formatConnectionMessage(std::string_view value);

std::string formatColorMessage(std::string_view identifier, glm::vec4 color);

std::string formatPointDataCallbackMessage(std::string_view identifier);

std::string floatToHex(const float f);

float hexToFloat(const std::string& f);

float readFloatValue(const std::vector<char>& message, size_t& offset);

int readIntValue(const std::vector<char>& message, size_t& offset);

std::string readString(const std::vector<char>& message, size_t& offset);

glm::vec4 readColor(const std::vector<char>& message, size_t& offset);

void readPointData(
    const std::vector<char>& message,
    size_t& offset,
    size_t nPoints,
    size_t dimensionality,
    std::vector<float>& pointData
);

void readColormap(
    const std::vector<char>& message,
    size_t& offset,
    size_t nColors,
    std::vector<float>& colorMap
);

} // namespace simp

} // namespace openspace::softwareintegration

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SIMP___H__
