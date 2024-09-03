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

#include <openspace/interaction/sessionrecording.h>

#include <ghoul/misc/assert.h>
#include <array>
#include <format>
#include <optional>

namespace {
    using namespace openspace::interaction;

    class LoadingError : public ghoul::RuntimeError {
    public:
        LoadingError(std::string error_, std::filesystem::path file_, int entry_)
            : ghoul::RuntimeError(
                std::format(
                    "Error loading session recording '{}' (entry #{}): {}",
                    file_, entry_, error_
                ),
                "SessionRecording"
            )
            , error(std::move(error_))
            , file(std::move(file_))
            , entry(entry_)
        {}

        LoadingError(std::string error_, std::filesystem::path file_)
            : ghoul::RuntimeError(
                std::format("Error loading session recording '{}': {}", file_, error_),
                "SessionRecording"
            )
            , error(std::move(error_))
            , file(std::move(file_))
        {}

        explicit LoadingError(std::string error_)
            : ghoul::RuntimeError(error_, "SessionRecording")
            , error(std::move(error_))
        {}

        std::string error;
        std::filesystem::path file;
        int entry = -1;
    };

    enum class DataMode {
        Ascii = 0,
        Binary
    };

    constexpr const std::string_view FrameTypeCameraAscii = "camera";
    constexpr const std::string_view FrameTypeScriptAscii = "script";
    constexpr const std::string_view FrameTypeCommentAscii = "#";
    constexpr const char FrameTypeCameraBinary = 'c';
    constexpr const char FrameTypeScriptBinary = 's';

    const std::unordered_map<std::string_view, int> VersionMap = {
        { "00.85", 0 },
        { "01.00", 1 },
        { "02.00", 2 }
    };

    struct Header {
        static constexpr std::string_view MagicBytes = "OpenSpace_record/playback";
        static constexpr int VersionLength = 5 * sizeof(std::byte);
        static constexpr char DataModeAscii = 'A';
        static constexpr char DataModeBinary = 'B';

        int version;
        DataMode dataMode = DataMode::Ascii;
    };

    Header readHeader(std::istream& file, const std::filesystem::path& filename) {
        Header result;

        // Read magic title bytes that must always be the same
        std::string magicBytes;
        magicBytes.resize(Header::MagicBytes.size());
        file.read(magicBytes.data(), Header::MagicBytes.size());
        if (!file || magicBytes != Header::MagicBytes) {
            throw LoadingError("Error loading header magic bytes", filename);
        }

        // Read the version of the session recording
        std::string version;
        version.resize(Header::VersionLength);
        file.read(version.data(), Header::VersionLength);
        if (!file) {
            throw LoadingError("Error loading header version information", filename);
        }
        if (auto it = VersionMap.find(version);  it != VersionMap.end()) {
            result.version = it->second;
        }
        else {
            throw LoadingError(std::format("Unsupported version {}", version), filename);
        }


        // Read whether the rest of the file is in ASCII or binary mode
        char dataMode = 0;
        file.read(&dataMode, sizeof(char));
        const bool goodDataMode =
            dataMode == Header::DataModeAscii || dataMode == Header::DataModeBinary;
        if (!file || !goodDataMode) {
            throw LoadingError("Error loading header data mode", filename);
        }
        result.dataMode =
            (dataMode == Header::DataModeAscii) ? DataMode::Ascii : DataMode::Binary;

        // Skip over the line ending
        char buffer = 0;
        file.read(&buffer, sizeof(char));
        if (buffer == '\r') {
            // Skip over the following \n as well as we have a DOS line ending
            file.seekg(1, std::ios::cur);
        }

        return result;
    }

    enum class FrameType {
        Camera,
        Script
    };

    struct Timestamps {
        double timestamp = 0.0;
        double simulationTime = 0.0;

        operator std::tuple<double&, double&>() { return { timestamp, simulationTime }; }
    };

#define UNIMPLEMENTED static_assert(sizeof(int) == 0, "Unimplemented overload")

    template <DataMode mode>
    std::optional<FrameType> readFrameType(std::istream&, int) { UNIMPLEMENTED; }

    template <>
    std::optional<FrameType> readFrameType<DataMode::Ascii>(std::istream& stream, int) {
        std::string frameType;
        stream >> frameType;

        if (!stream || frameType.empty()) {
            // Reading the frame type is the first action when reading a frame so we have
            // to check here if we have reached the end of the file. This is either
            // signalled by the stream ending (if there no terminating empty line) or by
            // the `frameType` being empty (if there is a terminating empty line)
            return std::nullopt;
        }

        if (frameType == FrameTypeCameraAscii) {
            return FrameType::Camera;
        }
        else if (frameType == FrameTypeScriptAscii) {
            return FrameType::Script;
        }
        else {
            throw LoadingError(std::format("Unrecognized frame '{}'", frameType));
        }
    }

    template <>
    std::optional<FrameType> readFrameType<DataMode::Binary>(std::istream& stream, int) {
        char frameType = 0;
        stream.read(&frameType, sizeof(char));

        if (!stream) {
            // Reading the frame type is the first action when reading a frame so we have
            // to check here if we have reached the end of the file.
            return std::nullopt;
        }

        switch (frameType) {
            case FrameTypeCameraBinary:
                return FrameType::Camera;
            case FrameTypeScriptBinary:
                return FrameType::Script;
            default:
                throw LoadingError(std::format("Unrecognized frame '{}'", frameType));
        }
    }

    // Read timestamps
    template <DataMode mode>
    Timestamps readTimestamps(std::istream&, int) { UNIMPLEMENTED; }

    template <>
    Timestamps readTimestamps<DataMode::Ascii>(std::istream& stream, int version) {
        Timestamps result;
        if (version < 2) {
            double dummy; // previously `times.timeOs`
            stream >> dummy;
        }

        stream >> result.timestamp >> result.simulationTime;
        return result;
    }

    template <>
    Timestamps readTimestamps<DataMode::Binary>(std::istream& stream, int version) {
        Timestamps result;
        if (version < 2) {
            stream.seekg(sizeof(double), std::ios::cur); // previously `times.timeOs`
        }
        stream.read(reinterpret_cast<char*>(&result.timestamp), sizeof(double));
        stream.read(reinterpret_cast<char*>(&result.simulationTime), sizeof(double));
        return result;
    }

    // Read camera frames

    template <DataMode mode>
    SessionRecordingEntry::Camera readCamera(std::istream&, int) { UNIMPLEMENTED; }

    template <>
    SessionRecordingEntry::Camera readCamera<DataMode::Ascii>(std::istream& stream, int) {
        SessionRecordingEntry::Camera camera;
        std::string rotationFollowing;
        stream >> camera.position.x >> camera.position.y >> camera.position.z
               >> camera.rotation.x >> camera.rotation.y
               >> camera.rotation.z >> camera.rotation.w
               >> camera.scale
               >> rotationFollowing
               >> camera.focusNode;
        camera.followFocusNodeRotation = (rotationFollowing == "F");
        return camera;
    }

    template <>
    SessionRecordingEntry::Camera readCamera<DataMode::Binary>(std::istream& stream,
                                                               int version)
    {
        SessionRecordingEntry::Camera camera;
        std::array<double, 4> buffer;
        stream.read(reinterpret_cast<char*>(buffer.data()), 3 * sizeof(double));
        camera.position = glm::dvec3(buffer[0], buffer[1], buffer[2]);

        stream.read(reinterpret_cast<char*>(buffer.data()), 4 * sizeof(double));
        camera.rotation = glm::dquat(buffer[3], buffer[0], buffer[1], buffer[2]);
        char follow = 0;
        stream.read(&follow, sizeof(char));
        camera.followFocusNodeRotation = (follow == 1);

        int32_t nodeNameLength = 0;
        stream.read(reinterpret_cast<char*>(&nodeNameLength), sizeof(int32_t));
        camera.focusNode.resize(nodeNameLength);
        stream.read(camera.focusNode.data(), nodeNameLength);

        stream.read(reinterpret_cast<char*>(&camera.scale), sizeof(float));

        if (version < 2) {
            stream.seekg(sizeof(double), std::ios::cur); // previously `timestamp`
        }
        return camera;
    }

    // Read script frames

    template <DataMode mode>
    SessionRecordingEntry::Script readScript(std::istream&, int) { UNIMPLEMENTED; }

    template <>
    SessionRecordingEntry::Script readScript<DataMode::Ascii>(std::istream& stream, int) {
        SessionRecordingEntry::Script script;

        int numScriptLines = 0;
        stream >> numScriptLines;

        std::string tmpReadbackScript;
        for (int i = 0; i < numScriptLines; i++) {
            ghoul::getline(stream, tmpReadbackScript);
            size_t start = tmpReadbackScript.find_first_not_of(" ");
            tmpReadbackScript = tmpReadbackScript.substr(start);
            script.append(tmpReadbackScript);
            if (i < (numScriptLines - 1)) {
                script.append("\n");
            }
        }

        return script;
    }

    template <>
    SessionRecordingEntry::Script readScript<DataMode::Binary>(std::istream& stream,
                                                               int version)
    {
        SessionRecordingEntry::Script script;

        uint32_t scriptLength = 0;
        stream.read(reinterpret_cast<char*>(&scriptLength), sizeof(uint32_t));
        script.resize(scriptLength);
        stream.read(script.data(), scriptLength);

        return script;
    }

    // Read entire entries

    template <DataMode mode>
    std::optional<SessionRecordingEntry> readEntry(std::istream& stream, int version) {
        std::optional<FrameType> frameType = readFrameType<mode>(stream, version);

        if (!frameType.has_value()) {
            // We have reached the end of the file
            return std::nullopt;
        }

        SessionRecordingEntry entry;
        std::tie(entry.timestamp, entry.simulationTime) =
            readTimestamps<mode>(stream, version);

        switch (*frameType) {
            case FrameType::Camera:
                entry.value = readCamera<mode>(stream, version);
                break;
            case FrameType::Script:
                entry.value = readScript<mode>(stream, version);
                break;
        }

        return entry;
    }

    std::optional<SessionRecordingEntry> readEntry(std::istream& stream,
                                                   const Header& header)
    {
        return
            header.dataMode == DataMode::Ascii ?
            readEntry<DataMode::Ascii>(stream, header.version) :
            readEntry<DataMode::Binary>(stream, header.version);
    }
} // namespace

namespace openspace::interaction {

SessionRecording loadSessionRecording(const std::filesystem::path& filename) {
    ghoul_assert(std::filesystem::exists(filename), "Session recording did not exist");

    std::ifstream file = std::ifstream(filename, std::ios::in | std::ios::binary);
    if (!file) {
        throw LoadingError("Failed to open file", filename);
    }

    SessionRecording sessionRecording;

    Header header = readHeader(file, filename);
    while (true) {
        std::optional<SessionRecordingEntry> entry;
        try {
            entry = readEntry(file, header);
        }
        catch (const LoadingError& e) {
            const int nEntries = static_cast<int>(sessionRecording.size());
            throw LoadingError(e.error, filename, nEntries + 1);
        }

        if (!entry.has_value()) {
            // Reached the end of the file
            break;
        }

        sessionRecording.push_back(std::move(*entry));
    };

    return sessionRecording;


    // Run through conversion in case file is older. Does nothing if the file format
    // is up-to-date
    // absFilename = convertFile(absFilename);
}



} // namespace openspace::interaction
