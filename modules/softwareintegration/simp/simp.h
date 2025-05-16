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

namespace openspace::softwareintegration::simp {

namespace {

const uint8_t _version_major = 1;
const uint8_t _version_minor = 9;
const uint8_t _version_patch = 1;

} // namespace

const std::string protocolVersion = std::format("{}.{}.{}", _version_major, _version_minor, _version_patch);

const char DELIM = ';';
const std::byte DELIM_BYTES{ DELIM };

enum class MessageType : uint8_t {
    Connection = 0,
    Data,
    RemoveSceneGraphNode,
    Unknown
};

enum class DataKey : uint16_t {
    // Point
    X = 0,
    Y,
    Z,
    PointUnit,
    // Velocity
    U,
    V,
    W,
    VelocityDistanceUnit,
    VelocityTimeUnit,
    VelocityDayRecorded,
    VelocityMonthRecorded,
    VelocityYearRecorded,
    VelocityNanMode,
    VelocityEnabled,
    // Color
    Red,
    Green,
    Blue,
    Alpha,
    ColormapEnabled,
    // Colormap
    ColormapReds,
    ColormapGreens,
    ColormapBlues,
    ColormapAlphas,
    ColormapMin,
    ColormapMax,
    ColormapNanR,
    ColormapNanG,
    ColormapNanB,
    ColormapNanA,
    ColormapNanMode,
    ColormapAttributeData,
    // Fixed size
    FixedSize,
    LinearSizeEnabled,
    // Linear size
    LinearSizeMin,
    LinearSizeMax,
    LinearSizeAttributeData,
    // Visibility
    Visibility,
    Unknown
};

enum class ColormapNanRenderMode {
    Hide = 0,
    FixedColor,
    Unknown
};

enum class VelocityNanRenderMode {
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

std::string getStringFromMessageType(const MessageType& type);

DataKey getDataKey(const std::string& type);

std::string getStringFromDataKey(const DataKey& type);

ColormapNanRenderMode getColormapNanRenderMode(const std::string& type);

VelocityNanRenderMode getVelocityNanRenderMode(const std::string& type);

std::string formatLengthOfSubject(size_t lengthOfSubject);

std::string yearIntToString(int32_t yearAsInt);
std::string monthIntToString(int32_t monthAsInt);
std::string dayIntToString(int32_t dayAsInt);
std::string toDateString(glm::ivec3 dateVec);

bool readColorChannel(
    const std::vector<std::byte>& message,
    size_t& offset,
    const DataKey& dataKey,
    glm::vec4& color,
    const glm::vec4::length_type& channel
);

bool readDateValue(
    const std::vector<std::byte>& message,
    size_t& offset,
    const DataKey& dataKey,
    glm::ivec3& date,
    const glm::ivec3::length_type& timeUnit
); 

template <typename T>
T networkToHostEndian(T value);

template <typename T>
T hostToNetworkEndian(T value);

void readValue(const std::vector<std::byte>& message, size_t& offset, float& value);
void readValue(const std::vector<std::byte>& message, size_t& offset, int32_t& value);
void readValue(const std::vector<std::byte>& message, size_t& offset, bool& value);
void readValue(const std::vector<std::byte>& message, size_t& offset, std::string& value);

void toByteBuffer(std::vector<std::byte>& byteBuffer, const size_t& offset, float value);
void toByteBuffer(std::vector<std::byte>& byteBuffer, size_t& offset, float value);
void toByteBuffer(std::vector<std::byte>& byteBuffer, const size_t& offset, int32_t value);
void toByteBuffer(std::vector<std::byte>& byteBuffer, size_t& offset, int32_t value);
void toByteBuffer(std::vector<std::byte>& byteBuffer, const size_t& offset, bool value);
void toByteBuffer(std::vector<std::byte>& byteBuffer, size_t& offset, bool value);
void toByteBuffer(std::vector<std::byte>& byteBuffer, const size_t& offset, const std::string& value);
void toByteBuffer(std::vector<std::byte>& byteBuffer, size_t& offset, const std::string& value);
void toByteBuffer(std::vector<std::byte>& byteBuffer, const size_t& offset, const std::vector<std::byte>& value);
void toByteBuffer(std::vector<std::byte>& byteBuffer, size_t& offset, const std::vector<std::byte>& value);

} // namespace openspace::softwareintegration::simp

#include "simp.inl"

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SIMP___H__
