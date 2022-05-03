/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

namespace openspace {

namespace simp {

enum class MessageType : uint32_t {
    Connection = 0,
    ReadPointData,
    RemoveSceneGraphNode,
    Color,
    Opacity,
    Size,
    Visibility,
    Disconnection,
    Unknown
};

const std::map<std::string, MessageType> _messageTypeFromSIMPType {
    {"CONN", MessageType::Connection},
    {"PDAT", MessageType::ReadPointData},
    {"RSGN", MessageType::RemoveSceneGraphNode},
    {"UPCO", MessageType::Color},
    {"UPOP", MessageType::Opacity},
    {"UPSI", MessageType::Size},
    {"TOVI", MessageType::Visibility},
    {"DISC", MessageType::Disconnection},
};

MessageType getMessageType(const std::string& type);

std::string getSIMPType(const MessageType& type);

const float ProtocolVersion = 1.5;

std::string formatLengthOfSubject(size_t lengthOfSubject);

std::string formatUpdateMessage(MessageType messageType,
                                std::string_view identifier,
                                std::string_view value);
    
std::string formatConnectionMessage(std::string_view value);

std::string formatDisconnectionMessage();

} // namespace simp

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SIMP___H__
