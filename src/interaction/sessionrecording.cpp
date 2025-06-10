/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <ghoul/glm.h>
#include <ghoul/misc/assert.h>
#include <array>
#include <format>
#include <optional>

namespace {
    template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

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

    constexpr std::string_view FrameTypeCameraAscii = "camera";
    constexpr std::string_view FrameTypeScriptAscii = "script";
    constexpr char FrameTypeCameraBinary = 'c';
    constexpr char FrameTypeScriptBinary = 's';

    // Mapping for version numbers in session recording files
    constexpr std::array<std::pair<std::string_view, int>, 3> Versions = {
        std::pair("00.85", 0),
        std::pair("01.00", 1),
        std::pair("02.00", 2)
    };


    //
    // Header information
    //
    struct Header {
        static constexpr std::string_view MagicBytes = "OpenSpace_record/playback";
        static constexpr char DataModeAscii = 'A';
        static constexpr char DataModeBinary = 'B';

        int version;
        DataMode dataMode = DataMode::Ascii;
    };

    Header readHeader(std::istream& stream, const std::filesystem::path& filename) {
        Header result;

        // Read magic title bytes that must always be the same
        std::string magicBytes;
        magicBytes.resize(Header::MagicBytes.size());
        stream.read(magicBytes.data(), Header::MagicBytes.size());
        if (!stream || magicBytes != Header::MagicBytes) {
            throw LoadingError("Error loading header magic bytes", filename);
        }

        // Read the version of the session recording
        std::string version;
        constexpr int VersionLength = 5 * sizeof(std::byte);
        version.resize(VersionLength);
        stream.read(version.data(), VersionLength);
        if (!stream) {
            throw LoadingError("Error loading header version information", filename);
        }
        result.version = [&version, &filename]() {
            for (const std::pair<std::string_view, int>& p : Versions) {
                if (p.first == version) {
                    return p.second;
                }
            }
            throw LoadingError(std::format("Unsupported version {}", version), filename);
        }();

        // Read whether the rest of the file is in ASCII or binary mode
        char dataMode = 0;
        stream.read(&dataMode, sizeof(char));
        const bool goodDataMode =
            dataMode == Header::DataModeAscii || dataMode == Header::DataModeBinary;
        if (!stream || !goodDataMode) {
            throw LoadingError("Error loading header data mode", filename);
        }
        result.dataMode =
            (dataMode == Header::DataModeAscii) ? DataMode::Ascii : DataMode::Binary;

        // Skip over the line ending
        char buffer = 0;
        stream.read(&buffer, sizeof(char));
        if (buffer == '\r') {
            // Skip over the following \n as well as we have a DOS line ending
            stream.seekg(1, std::ios::cur);
        }

        return result;
    }

    void writeHeader(std::ostream& stream, const Header& header) {
        stream.write(Header::MagicBytes.data(), Header::MagicBytes.size());

        std::string_view version = [&header]() {
            for (const std::pair<std::string_view, int>& p : Versions) {
                if (p.second == header.version) {
                    return p.first;
                }
            }
            throw std::logic_error(std::format("Unsupported version {}", header.version));
        }();
        stream.write(version.data(), version.size());

        const char dataMode =
            header.dataMode == DataMode::Ascii ?
            Header::DataModeAscii :
            Header::DataModeBinary;
        stream.write(&dataMode, sizeof(char));

        stream.write("\n", sizeof(char));
    }


    //
    // Type of the frame
    //
    enum class FrameType {
        Camera,
        Script
    };

    template <DataMode mode>
    std::optional<FrameType> readFrameType(std::istream&, int);

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

    template <DataMode mode>
    void writeFrameType(std::ostream&, const FrameType&);

    template <>
    void writeFrameType<DataMode::Ascii>(std::ostream& stream, const FrameType& type) {
        switch (type) {
            case FrameType::Camera:
                stream.write(FrameTypeCameraAscii.data(), FrameTypeCameraAscii.size());
                break;
            case FrameType::Script:
                stream.write(FrameTypeScriptAscii.data(), FrameTypeScriptAscii.size());
                break;
        }
    }

    template <>
    void writeFrameType<DataMode::Binary>(std::ostream& stream, const FrameType& type) {
        switch (type) {
            case FrameType::Camera:
                stream.write(&FrameTypeCameraBinary, sizeof(char));
                break;
            case FrameType::Script:
                stream.write(&FrameTypeScriptBinary, sizeof(char));
                break;
        }
    }

    //
    // Reading the first part of an entry
    //
    struct Timestamps {
        double timestamp = 0.0;
        double simulationTime = 0.0;
    };

    // Not defined on purpose
    template <DataMode mode>
    Timestamps readTimestamps(std::istream&, int);

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

    // Not defined on purpose
    template <DataMode mode>
    void writeTimestamps(std::ostream&, const Timestamps&);

    template <>
    void writeTimestamps<DataMode::Ascii>(std::ostream& stream,
                                          const Timestamps& timestamps)
    {
        std::string buffer = std::format(
            "{} {}", timestamps.timestamp, timestamps.simulationTime
        );
        stream.write(buffer.data(), buffer.size());
    }

    template <>
    void writeTimestamps<DataMode::Binary>(std::ostream& stream,
                                           const Timestamps& timestamps)
    {
        stream.write(
            reinterpret_cast<const char*>(&timestamps.timestamp),
            sizeof(double)
        );
        stream.write(
            reinterpret_cast<const char*>(&timestamps.simulationTime),
            sizeof(double)
        );
    }


    //
    // Camera frames
    //

    // Not defined on purpose
    template <DataMode mode>
    SessionRecording::Entry::Camera readCamera(std::istream&, int);

    template <>
    SessionRecording::Entry::Camera readCamera<DataMode::Ascii>(std::istream& stream, int)
    {
        SessionRecording::Entry::Camera camera;
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
    SessionRecording::Entry::Camera readCamera<DataMode::Binary>(std::istream& stream,
                                                                 int version)
    {
        SessionRecording::Entry::Camera camera;
        std::array<double, 4> buffer = {};
        stream.read(reinterpret_cast<char*>(buffer.data()), 3 * sizeof(double));
        camera.position = glm::dvec3(buffer[0], buffer[1], buffer[2]);

        if (version < 2) {
            // Rotations are stored as four doubles immediately get downcasted to floats
            stream.read(reinterpret_cast<char*>(buffer.data()), 4 * sizeof(double));
            camera.rotation = glm::dquat(buffer[3], buffer[0], buffer[1], buffer[2]);
        }
        else {
            std::array<float, 4> b = {};
            stream.read(reinterpret_cast<char*>(b.data()), 4 * sizeof(float));
            camera.rotation = glm::quat(b[3], b[0], b[1], b[2]);
        }
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

    // Not defined on purpose
    template <DataMode mode>
    void writeCamera(std::ostream&, const SessionRecording::Entry::Camera&);

    template <>
    void writeCamera<DataMode::Ascii>(std::ostream& stream,
                                      const SessionRecording::Entry::Camera& camera)
    {
        std::string buffer = std::format(
            "{} {} {} {} {} {} {} {} {} {}",
            camera.position.x, camera.position.y, camera.position.z,
            camera.rotation.x, camera.rotation.y, camera.rotation.z, camera.rotation.w,
            camera.scale,
            camera.followFocusNodeRotation ? "F" : "-",
            camera.focusNode
        );
        stream.write(buffer.data(), buffer.size());
    }

    template <>
    void writeCamera<DataMode::Binary>(std::ostream& stream,
                                       const SessionRecording::Entry::Camera& camera)
    {
        stream.write(
            reinterpret_cast<const char*>(glm::value_ptr(camera.position)),
            3 * sizeof(double)
        );
        stream.write(
            reinterpret_cast<const char*>(glm::value_ptr(camera.rotation)),
            4 * sizeof(float)
        );

        const char follow = camera.followFocusNodeRotation ? 1 : 0;
        stream.write(&follow, sizeof(char));

        const int32_t nodeNameLength = static_cast<int32_t>(camera.focusNode.size());
        stream.write(reinterpret_cast<const char*>(&nodeNameLength), sizeof(int32_t));
        stream.write(camera.focusNode.data(), camera.focusNode.size());

        stream.write(reinterpret_cast<const char*>(&camera.scale), sizeof(float));
    }

    //
    // Script frames
    //

    // Not defined on purpose
    template <DataMode mode>
    SessionRecording::Entry::Script readScript(std::istream&, int);

    template <>
    SessionRecording::Entry::Script readScript<DataMode::Ascii>(std::istream& stream, int)
    {
        SessionRecording::Entry::Script script;

        int numScriptLines = 0;
        stream >> numScriptLines;

        std::string tmpReadbackScript;
        for (int i = 0; i < numScriptLines; i++) {
            ghoul::getline(stream, tmpReadbackScript);
            size_t start = tmpReadbackScript.find_first_not_of(" ");
            tmpReadbackScript = tmpReadbackScript.substr(start);
            if (tmpReadbackScript.back() == '\r') {
                tmpReadbackScript.pop_back();
            }
            script.append(tmpReadbackScript);
            if (i < (numScriptLines - 1)) {
                script.append("\n");
            }
        }

        return script;
    }

    template <>
    SessionRecording::Entry::Script readScript<DataMode::Binary>(std::istream& stream,
                                                                 int)
    {
        SessionRecording::Entry::Script script;

        uint32_t scriptLength = 0;
        stream.read(reinterpret_cast<char*>(&scriptLength), sizeof(uint32_t));
        script.resize(scriptLength);
        stream.read(script.data(), scriptLength);

        return script;
    }

    // Not defined on purpose
    template <DataMode>
    void writeScript(std::ostream&, const SessionRecording::Entry::Script&);

    template <>
    void writeScript<DataMode::Ascii>(std::ostream& stream,
                                      const SessionRecording::Entry::Script& script)
    {
        SessionRecording::Entry::Script s = script;

        // Erase all \r (from windows newline), and all \n from line endings and replace
        // with ';' so that lua will treat them as separate lines. This is done in order
        // to treat a multi-line script as a single line in the file.
        size_t startPos = s.find('\r', 0);
        while (startPos != std::string::npos) {
            s.erase(startPos, 1);
            startPos = s.find('\r', startPos);
        }
        startPos = s.find('\n', 0);
        while (startPos != std::string::npos) {
            s.replace(startPos, 1, ";");
            startPos = s.find('\n', startPos);
        }
        stream.write("1 ", 2 * sizeof(char));
        stream.write(s.data(), s.size());
    }

    template <>
    void writeScript<DataMode::Binary>(std::ostream& stream,
                                       const SessionRecording::Entry::Script& script)
    {
        uint32_t scriptLength = static_cast<uint32_t>(script.size());
        stream.write(reinterpret_cast<const char*>(&scriptLength), sizeof(uint32_t));
        stream.write(script.data(), script.size());
    }

    //
    // SessionRecordingEntry
    //
    template <DataMode mode>
    std::optional<SessionRecording::Entry> readEntry(std::istream& stream, int version) {
        std::optional<FrameType> frameType = readFrameType<mode>(stream, version);

        if (!frameType.has_value()) {
            // We have reached the end of the file
            return std::nullopt;
        }

        Timestamps timestamps = readTimestamps<mode>(stream, version);
        SessionRecording::Entry entry = {
            .timestamp = timestamps.timestamp,
            .simulationTime = timestamps.simulationTime
        };

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

    std::optional<SessionRecording::Entry> readEntry(std::istream& stream,
                                                     DataMode dataMode, int version)
    {
        return
            dataMode == DataMode::Ascii ?
            readEntry<DataMode::Ascii>(stream, version) :
            readEntry<DataMode::Binary>(stream, version);
    }

    template <DataMode mode>
    void writeEntry(std::ostream& stream, const SessionRecording::Entry& entry) {
        if (std::holds_alternative<SessionRecording::Entry::Camera>(entry.value)) {
            writeFrameType<mode>(stream, FrameType::Camera);
        }
        else if (std::holds_alternative<SessionRecording::Entry::Script>(entry.value)) {
            writeFrameType<mode>(stream, FrameType::Script);
        }
        else {
            throw std::logic_error("Unhandled variant");
        }
        if constexpr (mode == DataMode::Ascii) {
            stream.write(" ", sizeof(char));
        }


        writeTimestamps<mode>(stream, { entry.timestamp, entry.simulationTime });
        if constexpr (mode == DataMode::Ascii) {
            stream.write(" ", sizeof(char));
        }

        std::visit(overloaded {
            [&stream](const SessionRecording::Entry::Camera& value) {
                writeCamera<mode>(stream, value);
            },
            [&stream](const SessionRecording::Entry::Script& value) {
                writeScript<mode>(stream, value);
            }
        }, entry.value);
    }

    void writeEntry(std::ostream& stream, const SessionRecording::Entry& entry,
                    DataMode dataMode)
    {
        dataMode == DataMode::Ascii ?
            writeEntry<DataMode::Ascii>(stream, entry) :
            writeEntry<DataMode::Binary>(stream, entry);
    }


} // namespace

namespace openspace::interaction {

bool SessionRecording::hasCameraFrame() const noexcept {
    for (const Entry& e : entries) {
        if (std::holds_alternative<Entry::Camera>(e.value)) {
            return true;
        }
    }
    return false;
}

SessionRecording loadSessionRecording(const std::filesystem::path& filename) {
    ghoul_assert(std::filesystem::exists(filename), "Session recording did not exist");

    std::ifstream file = std::ifstream(filename, std::ios::in | std::ios::binary);
    if (!file) {
        throw LoadingError("Failed to open file", filename);
    }

    SessionRecording sessionRecording;

    Header header = readHeader(file, filename);
    while (true) {
        std::optional<SessionRecording::Entry> entry;
        try {
            entry = readEntry(file, header.dataMode, header.version);
        }
        catch (const LoadingError& e) {
            const int nEntries = static_cast<int>(sessionRecording.entries.size());
            throw LoadingError(e.error, filename, nEntries + 1);
        }

        if (!entry.has_value()) {
            // Reached the end of the file
            break;
        }

        sessionRecording.entries.push_back(std::move(*entry));
    };

    ghoul_assert(
        std::is_sorted(
            sessionRecording.entries.begin(),
            sessionRecording.entries.end(),
            [](const SessionRecording::Entry& lhs, const SessionRecording::Entry& rhs) {
                return lhs.timestamp < rhs.timestamp;
            }
        ),
        "Session Recording not sorted by timestamp"
    );

    return sessionRecording;
}

void saveSessionRecording(const std::filesystem::path& filename,
                          const SessionRecording& sessionRecording, DataMode dataMode)
{
    std::ofstream file = std::ofstream(filename, std::ios::binary);

    constexpr int CurrentVersion = Versions.back().second;
    const Header header = {
        .version = CurrentVersion,
        .dataMode = dataMode
    };
    writeHeader(file, header);

    for (const SessionRecording::Entry& entry : sessionRecording.entries) {
        writeEntry(file, entry, dataMode);

        if (dataMode == DataMode::Ascii) {
            file.write("\n", sizeof(char));
        }
    }
}

std::vector<ghoul::Dictionary> sessionRecordingToDictionary(
                                                        const SessionRecording& recording)
{
    std::vector<ghoul::Dictionary> result;
    for (const SessionRecording::Entry& entry : recording.entries) {
        ghoul::Dictionary e;
        e.setValue("Timestamp", entry.timestamp);
        e.setValue("SimulationTime", entry.simulationTime);

        if (std::holds_alternative<SessionRecording::Entry::Camera>(entry.value)) {
            const auto& cam = std::get<SessionRecording::Entry::Camera>(entry.value);

            ghoul::Dictionary c;
            c.setValue("Position", cam.position);
            glm::dvec4 q = glm::dvec4(
                cam.rotation.w,
                cam.rotation.x,
                cam.rotation.y,
                cam.rotation.z
            );
            c.setValue("Rotation", q);
            c.setValue("FocusNode", cam.focusNode);
            c.setValue("Scale", static_cast<double>(cam.scale));
            c.setValue("FollowFocusNode", cam.followFocusNodeRotation);
            e.setValue("Camera", std::move(c));
        }
        else if (std::holds_alternative<SessionRecording::Entry::Script>(entry.value)) {
            const std::string& s = std::get<SessionRecording::Entry::Script>(entry.value);
            e.setValue("Script", s);
        }

        result.push_back(e);
    }

    return result;
}

} // namespace openspace::interaction
