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

#ifndef __OPENSPACE_CORE___SESSIONRECORDING___H__
#define __OPENSPACE_CORE___SESSIONRECORDING___H__

#include <openspace/navigation/keyframenavigator.h>
#include <ghoul/glm.h>
#include <filesystem>
#include <string>
#include <variant>
#include <vector>

namespace openspace::interaction {

enum class DataMode {
    Ascii = 0,
    Binary
};

struct SessionRecording {
    struct Entry {
        using Camera = KeyframeNavigator::CameraPose;
        using Script = std::string;

        double timestamp = 0.0;
        double simulationTime = 0.0;
        std::variant<Camera, Script> value;
    };

    std::vector<Entry> entries;
};

//struct SessionRecordingEntry {
//    using Camera = KeyframeNavigator::CameraPose;
//    using Script = std::string;
//
//    double timestamp = 0.0;
//    double simulationTime = 0.0;
//    std::variant<Camera, Script> value;
//};
//using SessionRecording = std::vector<SessionRecordingEntry>;


SessionRecording loadSessionRecording(const std::filesystem::path& filename);
void saveSessionRecording(const std::filesystem::path& filename,
    const SessionRecording& sessionRecording, DataMode dataMode);


} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___SESSIONRECORDINGHANDLER___H__
