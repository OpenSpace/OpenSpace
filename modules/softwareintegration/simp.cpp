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

#include <modules/softwareintegration/simp.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionaryluaformatter.h>

#include <iomanip>

namespace {
    constexpr const char* _loggerCat = "SoftwareIntegrationMessageFormat";
} // namespace

namespace openspace {

namespace simp {

SimpError::SimpError(const ErrorCode _errorCode, const std::string& msg)
    : errorCode{errorCode}, ghoul::RuntimeError(fmt::format("{}: Error Code: {} - {}", "SIMP error", static_cast<uint32_t>(_errorCode), msg), "Software Integration Messaging Protocol error")
{}

bool isEndOfCurrentValue(const std::vector<char>& message, size_t offset) {
    if (offset >= message.size()) {
        throw SimpError(
            ErrorCode::OffsetLargerThanMessageSize,
            "Unexpectedly reached the end of the message..."
        );
    }

    if (message.size() > 0 && offset == message.size() - 1 && message[offset] != SEP) {
        throw SimpError(
            ErrorCode::ReachedEndBeforeSeparator,
            "Reached end of message before reading separator character..."
        );
    }

    return offset != 0 && message[offset] == SEP && message[offset - 1] != '\\';
}

MessageType getMessageType(const std::string& type) {
    if (_messageTypeFromSIMPType.count(type) == 0) return MessageType::Unknown;
    return _messageTypeFromSIMPType.at(type);
}

std::string getSIMPType(const MessageType& type){
    for (auto [key, messageType] : _messageTypeFromSIMPType) {
        if (messageType == type) return key;
    }
    return "";
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
    const int lengthOfIdentifier = static_cast<int>(identifier.length());
    const int lengthOfValue = static_cast<int>(value.length());
    std::string subject = fmt::format(
        "{}{}{}{}", lengthOfIdentifier, identifier, lengthOfValue, value
    );

    const std::string lengthOfSubject = formatLengthOfSubject(subject.length());

    return fmt::format("{}{}{}{}", ProtocolVersion, getSIMPType(messageType), lengthOfSubject, subject);
}

std::string formatConnectionMessage(std::string_view software) {
    std::string subject = fmt::format(
        "{}", software
    );

    const std::string lengthOfSubject = formatLengthOfSubject(subject.length());

    return fmt::format("{}{}{}{}", ProtocolVersion, "CONN", lengthOfSubject, subject);
}

std::string formatDisconnectionMessage() {
    const std::string lengthOfSubject = formatLengthOfSubject(0);
    return fmt::format("{}{}{}", ProtocolVersion, "DISC", lengthOfSubject);
}

} // namespace simp

} // namespace openspace