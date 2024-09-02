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

#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/events/eventengine.h>
#include <openspace/interaction/tasks/convertrecformattask.h>
#include <openspace/navigation/keyframenavigator.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/network/messagestructureshelper.h>
#include <openspace/query/query.h>
#include <openspace/rendering/luaconsole.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scripting/scriptscheduler.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/task.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/stringhelper.h>
#include <algorithm>
#include <filesystem>
#include <iomanip>

#ifdef WIN32
#include <Windows.h>
#endif // WIN32

#include "sessionrecording_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "SessionRecording";

    constexpr openspace::properties::Property::PropertyInfo RenderPlaybackInfo = {
        "RenderInfo",
        "Render Playback Information",
        "If enabled, information about a currently played back session recording is "
        "rendering to screen.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo IgnoreRecordedScaleInfo = {
        "IgnoreRecordedScale",
        "Ignore Recorded Scale",
        "If this value is enabled, the scale value from a recording is ignored and the "
        "computed values are used instead.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AddModelMatrixinAsciiInfo = {
        "AddModelMatrixinAscii",
        "Add Model Matrix in ASCII recording",
        "If this is 'true', the model matrix is written into the ASCII recording format "
        "in the line before each camera keyframe. The model matrix is the full matrix "
        "that converts the position into a J2000+Galactic reference frame.",
        openspace::properties::Property::Visibility::Developer
    };

    class LoadingError : public ghoul::RuntimeError {
    public:
        explicit LoadingError(std::string error)
            : ghoul::RuntimeError(std::move(error), "SessionRecording")
        {};
    };

    const std::string FileHeaderTitle = "OpenSpace_record/playback";
    const std::string HeaderCameraAscii = "camera";
    const std::string HeaderScriptAscii = "script";
    const std::string HeaderCommentAscii = "#";
    const char HeaderCameraBinary = 'c';
    const char HeaderScriptBinary = 's';
    const std::string FileExtensionBinary = ".osrec";
    const std::string FileExtensionAscii = ".osrectxt";

    static const size_t FileHeaderVersionLength = 5;
    char FileHeaderVersion[FileHeaderVersionLength + 1] = "01.00";
    char TargetConvertVersion[FileHeaderVersionLength + 1] = "01.00";
    static const char DataFormatAsciiTag = 'A';
    static const char DataFormatBinaryTag = 'B';
    static const size_t keyframeHeaderSize_bytes = 33;
    static const size_t saveBufferCameraSize_min = 82;
    static const size_t saveBufferStringSize_max = 2000;
    // This buffer is totally pointless
    unsigned char _keyframeBuffer[keyframeHeaderSize_bytes +
        +saveBufferCameraSize_min + saveBufferStringSize_max];

    constexpr std::string_view scriptReturnPrefix = "return ";

    void writeToFileBuffer(unsigned char* buf, size_t& idx, double src) {
        const size_t writeSize_bytes = sizeof(double);
        unsigned char const* p = reinterpret_cast<unsigned char const*>(&src);
        memcpy((buf + idx), p, writeSize_bytes);
        idx += writeSize_bytes;
    }

    void writeToFileBuffer(unsigned char* buf, size_t& idx, std::vector<char>& cv) {
        const size_t writeSize_bytes = cv.size() * sizeof(char);
        memcpy((buf + idx), cv.data(), writeSize_bytes);
        idx += writeSize_bytes;
    }

    void writeToFileBuffer(unsigned char* buf, size_t& idx, unsigned char c) {
        const size_t writeSize_bytes = sizeof(char);
        buf[idx] = c;
        idx += writeSize_bytes;
    }

    void writeToFileBuffer(unsigned char* buf, size_t& idx, bool b) {
        buf[idx] = b ? 1 : 0;
        idx += sizeof(char);
    }

    using namespace openspace;
    using namespace openspace::interaction;
    using namespace openspace::datamessagestructures;

    /**
     * Writes a header to a binary recording file buffer.
     *
     * \param times Reference to a timestamps structure which contains recorded times
     * \param type Single character signifying the keyframe type
     * \param kfBuffer The char buffer holding the recording info to be written
     * \param idx Index into write buffer (this is updated with the num of chars written)
     */
    void saveHeaderBinary(const Timestamps& times, char type,
        unsigned char* kfBuffer, size_t& idx)
    {
        kfBuffer[idx++] = type;
        writeToFileBuffer(kfBuffer, idx, times.timeOs);
        writeToFileBuffer(kfBuffer, idx, times.timeRec);
        writeToFileBuffer(kfBuffer, idx, times.timeSim);
    }

    /**
     * Writes a header to an ASCII recording file buffer.
     *
     * \param times Reference to a timestamps structure which contains recorded times
     * \param type String signifying the keyframe type
     * \param line The stringstream buffer being written to
     */
    void saveHeaderAscii(const Timestamps& times, const std::string& type,
        std::stringstream& line)
    {
        line << type << ' ';
        line << times.timeOs << ' ';
        line << times.timeRec << ' ';
        line << std::fixed << std::setprecision(3) << times.timeSim << ' ';
    }

    /**
     * Writes a camera keyframe to a binary format recording file using a CameraKeyframe.
     *
     * \param times Reference to a timestamps structure which contains recorded times
     * \param kf Reference to a camera keyframe which contains the camera details
     * \param file An ofstream reference to the recording file being written-to
     */
    void saveCameraKeyframeBinary(const Timestamps& times,
        const CameraKeyframe& kf, std::ostream& file)
    {
        // Writing to a binary session recording file
        size_t idx = 0;
        saveHeaderBinary(times, HeaderCameraBinary, _keyframeBuffer, idx);
        // Writing to internal buffer, and then to file, for performance reasons
        std::vector<char> writeBuffer;
        kf.serialize(writeBuffer);
        writeToFileBuffer(_keyframeBuffer, idx, writeBuffer);
        file.write(reinterpret_cast<char*>(_keyframeBuffer), idx);
    }

    /**
     * Writes a camera keyframe to an ascii format recording file using a CameraKeyframe.
     *
     * \param times Reference to a timestamps structure which contains recorded times
     * \param kf Reference to a camera keyframe which contains the camera details
     * \param file An ofstream reference to the recording file being written-to
     */
    void saveCameraKeyframeAscii(const Timestamps& times, const CameraKeyframe& kf,
        std::ostream& file, bool addModelMatrixinAscii)
    {
        if (addModelMatrixinAscii) {
            SceneGraphNode* node = sceneGraphNode(kf._focusNode);
            const glm::dmat4 modelTransform = node->modelTransform();

            file << HeaderCommentAscii << ' ' << ghoul::to_string(modelTransform) << '\n';
        }

        std::stringstream keyframeLine;
        saveHeaderAscii(times, HeaderCameraAscii, keyframeLine);
        kf.write(keyframeLine);
        file << keyframeLine.str() << '\n';
    }

    /**
     * Writes a script keyframe to a binary format recording file using a ScriptMessage.
     *
     * \param times Reference to a timestamps structure which contains recorded times
     * \param sm Reference to a ScriptMessage object which contains the script details
     * \param smBuffer A buffer temporarily used for preparing data to be written
     * \param file An ofstream reference to the recording file being written-to
     */
    void saveScriptKeyframeBinary(const Timestamps& times,
        const ScriptMessage& sm, std::ostream& file)
    {
        size_t idx = 0;
        saveHeaderBinary(times, HeaderScriptBinary, _keyframeBuffer, idx);
        // Writing to internal buffer, and then to file, for performance reasons
        std::vector<char> writeBuffer;
        sm.serialize(writeBuffer);
        writeToFileBuffer(_keyframeBuffer, idx, writeBuffer);
        file.write(reinterpret_cast<char*>(_keyframeBuffer), idx);
    }

    /**
     * Writes a script keyframe to an ascii format recording file using a ScriptMessage.
     *
     * \param times Reference to a timestamps structure which contains recorded times
     * \param sm Reference to a ScriptMessage which contains the script details
     * \param file An ofstream reference to the recording file being written-to
     */
    void saveScriptKeyframeAscii(const Timestamps& times,
        const ScriptMessage& sm, std::ostream& file)
    {
        std::stringstream keyframeLine;
        saveHeaderAscii(times, HeaderScriptAscii, keyframeLine);
        // Erase all \r (from windows newline), and all \n from line endings and replace with
        // ';' so that lua will treat them as separate lines. This is done in order to treat
        // a multi-line script as a single line in the file.
        ScriptMessage s = sm;
        size_t startPos = s._script.find('\r', 0);
        while (startPos != std::string::npos) {
            s._script.erase(startPos, 1);
            startPos = s._script.find('\r', startPos);
        }
        startPos = s._script.find('\n', 0);
        while (startPos != std::string::npos) {
            s._script.replace(startPos, 1, ";");
            startPos = s._script.find('\n', startPos);
        }
        s.write(keyframeLine);
        file << keyframeLine.str() << '\n';
    }

    void saveSingleKeyframeCamera(const CameraKeyframe& kf,
        const Timestamps& times, SessionRecording::DataMode mode,
        std::ostream& file, bool addModelMatrixinAscii)
    {
        if (mode == SessionRecording::DataMode::Binary) {
            saveCameraKeyframeBinary(times, kf, file);
        }
        else {
            saveCameraKeyframeAscii(times, kf, file, addModelMatrixinAscii);
        }
    }

    /**
     * Reads a camera keyframe from a binary format playback file, and populates input
     * references with the parameters of the keyframe.
     *
     * \param times Reference to a timestamps structure which contains recorded times
     * \param kf Reference to a camera keyframe which contains camera details
     * \param file An ifstream reference to the playback file being read
     * \param lineN Keyframe number in playback file where this keyframe resides
     * \return `true` if data read has no errors
     */
    std::pair<Timestamps, CameraKeyframe> readCameraKeyframeBinary(std::istream& file,
        int lineN)
    {
        Timestamps times;
        file.read(reinterpret_cast<char*>(&times.timeOs), sizeof(double));
        file.read(reinterpret_cast<char*>(&times.timeRec), sizeof(double));
        file.read(reinterpret_cast<char*>(&times.timeSim), sizeof(double));

        CameraKeyframe kf;
        kf.read(&file);

        if (!file) {
            throw LoadingError(std::format(
                "Error reading camera playback from keyframe entry {}", lineN - 1
            ));
        }

        return { times, kf };
    }

    /**
     * Reads a camera keyframe from an ascii format playback file, and populates input
     * references with the parameters of the keyframe.
     *
     * \param times Reference to a timestamps structure which contains recorded times
     * \param kf Reference to a camera keyframe which contains camera details
     * \param currentParsingLine String containing the most current line that was read
     * \param lineN Line number in playback file where this keyframe resides
     * \return `true` if data read has no errors
     */
    std::pair<Timestamps, CameraKeyframe> readCameraKeyframeAscii(
        const std::string& currentParsingLine, int lineN)
    {
        std::istringstream iss = std::istringstream(currentParsingLine);
        std::string entryType;
        iss >> entryType;
        Timestamps times;
        iss >> times.timeOs >> times.timeRec >> times.timeSim;
        CameraKeyframe kf;
        kf.read(iss);
        // ASCII format does not contain trailing timestamp so add it here
        kf._timestamp = times.timeOs;

        if (iss.fail() || !iss.eof()) {
            throw LoadingError(std::format(
                "Error parsing camera line {} of playback file", lineN
            ));
        }

        return { times, kf };
    }

    std::pair<Timestamps, CameraKeyframe> readSingleKeyframeCamera(
        SessionRecording::DataMode mode, std::istream& file, std::string& inLine,
        const int lineNum)
    {
        if (mode == SessionRecording::DataMode::Binary) {
            return readCameraKeyframeBinary(file, lineNum);
        }
        else {
            return readCameraKeyframeAscii(inLine, lineNum);
        }
    }

    /**
     * Reads a time keyframe from a binary format playback file, and populates input
     * references with the parameters of the keyframe.
     *
     * \param times Reference to a timestamps structure which contains recorded times
     * \param kf Reference to a time keyframe which contains time details
     * \param file An ifstream reference to the playback file being read
     * \param lineN Keyframe number in playback file where this keyframe resides
     * \return `true` if data read has no errors
     */
    std::pair<Timestamps, TimeKeyframe> readTimeKeyframeBinary(
        std::istream& file, int lineN)
    {
        Timestamps times;
        file.read(reinterpret_cast<char*>(&times.timeOs), sizeof(double));
        file.read(reinterpret_cast<char*>(&times.timeRec), sizeof(double));
        file.read(reinterpret_cast<char*>(&times.timeSim), sizeof(double));

        TimeKeyframe kf;
        kf.read(&file);

        if (!file) {
            throw LoadingError(std::format(
                "Error reading time playback from keyframe entry {}", lineN - 1
            ));
        }

        return { times, kf };
    }

    /**
     * Reads a time keyframe from an ascii format playback file, and populates input
     * references with the parameters of the keyframe.
     *
     * \param times Reference to a timestamps structure which contains recorded times
     * \param kf Reference to a time keyframe which contains time details
     * \param currentParsingLine String containing the most current line that was read
     * \param lineN Line number in playback file where this keyframe resides
     * \return `true` if data read has no errors
     */
    std::pair<Timestamps, TimeKeyframe> readTimeKeyframeAscii(
        const std::string& currentParsingLine, int lineN)
    {
        std::string entryType;

        std::istringstream iss(currentParsingLine);
        iss >> entryType;
        Timestamps times;
        iss >> times.timeOs >> times.timeRec >> times.timeSim;
        TimeKeyframe kf;
        kf.read(iss);

        if (iss.fail() || !iss.eof()) {
            throw LoadingError(std::format(
                "Error parsing time line {} of playback file", lineN
            ));
        }

        return { times, kf };
    }

    std::pair<Timestamps, TimeKeyframe> readSingleKeyframeTime(
        SessionRecording::DataMode mode,
        std::istream& file, std::string& inLine,
        const int lineNum)
    {
        if (mode == SessionRecording::DataMode::Binary) {
            return readTimeKeyframeBinary(file, lineNum);
        }
        else {
            return readTimeKeyframeAscii(inLine, lineNum);
        }
    }

    /**
     * Reads header information from a session recording file.
     *
     * \param stream Reference to ifstream that contains the session recording file data
     * \param readLen_chars Number of characters to be read, which may be the expected
     *        length of the header line, or an arbitrary number of characters within it
     */
    std::string readHeaderElement(std::istream& stream, size_t readLenChars) {
        std::vector<char> readTemp = std::vector<char>(readLenChars);
        stream.read(readTemp.data(), readLenChars);
        return std::string(readTemp.begin(), readTemp.end());
    }

    void saveSingleKeyframeScript(const ScriptMessage& kf,
        const Timestamps& times, SessionRecording::DataMode mode,
        std::ostream& file)
    {
        if (mode == SessionRecording::DataMode::Binary) {
            saveScriptKeyframeBinary(times, kf, file);
        }
        else {
            saveScriptKeyframeAscii(times, kf, file);
        }
    }

    /**
     * Reads a script keyframe from a binary format playback file, and populates input
     * references with the parameters of the keyframe.
     *
     * \param times Reference to a timestamps structure which contains recorded times
     * \param kf Reference to a script keyframe which contains the size of the script (in
     *        chars) and the text itself
     * \param file An ifstream reference to the playback file being read
     * \param lineN Keyframe number in playback file where this keyframe resides
     * \return `true` if data read has no errors
     */
    std::pair<Timestamps, ScriptMessage> readScriptKeyframeBinary(std::istream& file,
        int lineN)
    {
        Timestamps times;
        file.read(reinterpret_cast<char*>(&times.timeOs), sizeof(double));
        file.read(reinterpret_cast<char*>(&times.timeRec), sizeof(double));
        file.read(reinterpret_cast<char*>(&times.timeSim), sizeof(double));

        ScriptMessage kf;
        kf.read(&file);

        if (!file) {
            throw LoadingError(std::format(
                "Error reading script playback from keyframe entry {}",
                lineN - 1
            ));
        }

        return { times, kf };
    }

    /**
     * Reads a script keyframe from an ascii format playback file, and populates input
     * references with the parameters of the keyframe.
     *
     * \param times Reference to a timestamps structure which contains recorded times
     * \param kf Reference to a script keyframe which contains the size of the script (in
     *        chars) and the text itself
     * \param currentParsingLine String containing the most current line that was read
     * \param lineN Line number in playback file where this keyframe resides
     * \return `true` if data read has no errors
     */
    std::pair<Timestamps, ScriptMessage> readScriptKeyframeAscii(
        const std::string& currentParsingLine,
        int lineN)
    {
        std::string entryType;
        std::istringstream iss(currentParsingLine);
        iss >> entryType;
        Timestamps times;
        iss >> times.timeOs >> times.timeRec >> times.timeSim;
        ScriptMessage kf;
        kf.read(iss);
        if (iss.fail()) {
            throw LoadingError(std::format(
                "Error parsing script line {} of playback file", lineN
            ));
        }
        else if (!iss.eof()) {
            throw LoadingError(std::format(
                "Did not find an EOL at line {} of playback file", lineN
            ));
        }

        return { times, kf };
    }

    std::pair<Timestamps, ScriptMessage> readSingleKeyframeScript(
        SessionRecording::DataMode mode,
        std::istream& file, std::string& inLine,
        const int lineNum)
    {
        if (mode == SessionRecording::DataMode::Binary) {
            return readScriptKeyframeBinary(file, lineNum);
        }
        else {
            return readScriptKeyframeAscii(inLine, lineNum);
        }
    }

    SessionRecording::DataMode readModeFromHeader(const std::string& filename) {
        std::ifstream inputFile = std::ifstream(filename, std::ifstream::in);

        // Read header
        const std::string readBackHeaderString = readHeaderElement(
            inputFile,
            FileHeaderTitle.length()
        );
        if (readBackHeaderString != FileHeaderTitle) {
            LERROR("Specified playback file does not contain expected header");
        }
        readHeaderElement(inputFile, FileHeaderVersionLength);
        std::string readDataMode = readHeaderElement(inputFile, 1);
        SessionRecording::DataMode mode;
        if (readDataMode[0] == DataFormatAsciiTag) {
            mode = SessionRecording::DataMode::Ascii;
        }
        else if (readDataMode[0] == DataFormatBinaryTag) {
            mode = SessionRecording::DataMode::Binary;
        }
        else {
            throw LoadingError("Unknown data type in header (should be Ascii or Binary)");
        }
        return mode;
    }

    void readFileIntoStringStream(std::string filename,
        std::stringstream& stream)
    {
        std::filesystem::path conversionInFilename = absPath(filename);
        if (!std::filesystem::is_regular_file(conversionInFilename)) {
            throw LoadingError(std::format(
                "Cannot find the specified playback file '{}' to convert",
                conversionInFilename
            ));
        }

        const SessionRecording::DataMode mode =
            readModeFromHeader(conversionInFilename.string());

        stream.str("");
        stream.clear();
        std::ifstream inputFstream;
        if (mode == SessionRecording::DataMode::Binary) {
            inputFstream.open(conversionInFilename, std::ifstream::in | std::ios::binary);
        }
        else {
            inputFstream.open(conversionInFilename, std::ifstream::in);
        }
        stream << inputFstream.rdbuf();
        if (!inputFstream.is_open() || !inputFstream.good()) {
            throw LoadingError(std::format(
                "Unable to open file '{}' for conversion", filename
            ));
        }
    }

    void readPlaybackHeader_stream(std::stringstream& conversionInStream,
        std::string& version, SessionRecording::DataMode& mode)
    {
        // Read header
        const std::string readBackHeaderString = readHeaderElement(
            conversionInStream,
            FileHeaderTitle.length()
        );

        if (readBackHeaderString != FileHeaderTitle) {
            throw LoadingError("File to convert does not contain expected header");
        }
        version = readHeaderElement(conversionInStream, FileHeaderVersionLength);
        std::string readDataMode = readHeaderElement(conversionInStream, 1);
        if (readDataMode[0] == DataFormatAsciiTag) {
            mode = SessionRecording::DataMode::Ascii;
        }
        else if (readDataMode[0] == DataFormatBinaryTag) {
            mode = SessionRecording::DataMode::Binary;
        }
        else {
            throw LoadingError("Unknown data type in header (needs Ascii or Binary)");
        }
        // Read to throw out newline at end of header
        readHeaderElement(conversionInStream, 1);
    }

    Timestamps generateCurrentTimestamp(double keyframeTime, double recordStartTime) {
        return {
            keyframeTime,
            keyframeTime - recordStartTime,
            global::timeManager->time().j2000Seconds()
        };
    }

    std::vector<TimelineEntry>::const_iterator findNextOfType(const std::vector<TimelineEntry>& timeline, RecordedType type, std::vector<TimelineEntry>::const_iterator start) {
        start++;
        for (auto it = start; it != timeline.end(); it++) {
            if (it->keyframeType == type) {
                return it;
            }
        }

        return timeline.end();
    }
} // namespace

namespace openspace::interaction {

SessionRecording::SessionRecording()
    : properties::PropertyOwner({ "SessionRecording", "Session Recording" })
    , _renderPlaybackInformation(RenderPlaybackInfo, false)
    , _ignoreRecordedScale(IgnoreRecordedScaleInfo, false)
    , _addModelMatrixinAscii(AddModelMatrixinAsciiInfo, false)
{
    addProperty(_renderPlaybackInformation);
    addProperty(_ignoreRecordedScale);
    addProperty(_addModelMatrixinAscii);
}

void SessionRecording::setRecordDataFormat(DataMode dataMode) {
    _recordingDataMode = dataMode;
}

void SessionRecording::startRecording(const std::string& fn) {
    _timeline.clear();
    if (_state == SessionState::Recording) {
        throw ghoul::RuntimeError(
            "Unable to start recording while already in recording mode",
            "SessionRecording"
        );
    }
    else if (isPlayingBack()) {
        throw ghoul::RuntimeError(
            "Unable to start recording while in session playback mode",
            "SessionRecording"
        );
    }
    if (!std::filesystem::is_directory(absPath("${RECORDINGS}"))) {
        std::filesystem::create_directories(absPath("${RECORDINGS}"));
    }

    std::filesystem::path filename = fn;
    auto extension = filename.extension();

    if (_recordingDataMode == DataMode::Binary) {
        if (extension == FileExtensionAscii) {
            throw ghoul::RuntimeError(
                "Specified filename for binary recording has ascii file extension",
                "SessionRecording"
            );
        }
        else if (extension != FileExtensionBinary) {
            filename.replace_extension(FileExtensionBinary);
        }
    }
    else if (_recordingDataMode == DataMode::Ascii) {
        if (extension == FileExtensionBinary) {
            throw ghoul::RuntimeError(
                "Specified filename for ascii recording has binary file extension",
                "SessionRecording"
            );
        }
        else if (extension != FileExtensionAscii) {
            filename.replace_extension(FileExtensionAscii);
        }
    }

    std::filesystem::path absFilename = filename;
    if (absFilename.parent_path().empty() || absFilename.parent_path() == absFilename) {
        absFilename = absPath("${RECORDINGS}/" + filename.string());
    }
    else if (absFilename.parent_path().is_relative()) {
        throw ghoul::RuntimeError(
            "If path is provided with the filename, then it must be an absolute path",
            "SessionRecording"
        );
    }
    else if (!std::filesystem::exists(absFilename.parent_path())) {
        throw ghoul::RuntimeError(std::format(
            "The recording filename path '{}' is not a valid location in the filesytem",
            absFilename.parent_path().string()
        ), "SessionRecording");
    }

    if (std::filesystem::is_regular_file(absFilename)) {
        throw ghoul::RuntimeError(std::format(
            "Unable to start recording; file '{}' already exists", absFilename
        ), "SessionRecording");
    }

    _recordingFile = absFilename;

    _state = SessionState::Recording;
    _playbackActive_camera = false;
    _playbackActive_script = false;
    _savePropertiesBaseline.clear();

    // Record the current delta time as the first property to save in the file.
    // This needs to be saved as a baseline whether or not it changes during recording
    _timestampsRecordStarted = {
        .timeOs = global::windowDelegate->applicationTime(),
        .timeSim = global::timeManager->time().j2000Seconds()
    };

    // Dummy `_time` "property" to store the time setup in the baseline
    _savePropertiesBaseline["_time"] = std::format(
        "openspace.time.setPause({});openspace.time.setDeltaTime({});",
        global::timeManager->isPaused() ? "true" : "false",
        global::timeManager->targetDeltaTime()
    );

    LINFO("Session recording started");
}

void SessionRecording::stopRecording() {
    if (_state != SessionState::Recording) {
        return;
    }

    defer {
        cleanUpTimelinesAndKeyframes();
    };

    std::ofstream recordFile;
    if (_recordingDataMode == DataMode::Binary) {
        recordFile.open(_recordingFile, std::ios::binary);
    }
    else {
        recordFile.open(_recordingFile);
    }

    if (!recordFile.is_open() || !recordFile.good()) {
        std::filesystem::path newName = absPath(std::format("${{TEMPORARY}}/{}", rand()));
        LERROR(std::format(
            "Unable to open file '{}' for recording, using '{}' instead",
            _recordingFile, newName
        ));
        _recordingFile = newName;
        stopRecording();
        return;
    }

    recordFile << FileHeaderTitle;
    recordFile.write(FileHeaderVersion, FileHeaderVersionLength);
    if (_recordingDataMode == DataMode::Binary) {
        recordFile << DataFormatBinaryTag;
    }
    else {
        recordFile << DataFormatAsciiTag;
    }
    recordFile << '\n';


    // Add all property baseline scripts to the beginning of the recording file
    datamessagestructures::ScriptMessage smTmp;
    for (const auto& [prop, script] : _savePropertiesBaseline) {
        smTmp._script = script;
        saveSingleKeyframeScript(
            smTmp,
            _timestampsRecordStarted,
            _recordingDataMode,
            recordFile
        );
    }
    for (const TimelineEntry& entry : _timeline) {
        switch (entry.keyframeType) {
            case RecordedType::Camera:
            {
                auto kf = std::get<CameraEntry>(entry.value);
                datamessagestructures::CameraKeyframe kfMsg(
                    std::move(kf.position),
                    std::move(kf.rotation),
                    std::move(kf.focusNode),
                    kf.followFocusNodeRotation,
                    kf.scale
                );
                saveSingleKeyframeCamera(
                    kfMsg,
                    entry.timestamps,
                    _recordingDataMode,
                    recordFile,
                    _addModelMatrixinAscii
                );
                break;
            }
            case RecordedType::Script:
            {
                smTmp._script = std::get<ScriptEntry>(entry.value);
                saveSingleKeyframeScript(
                    smTmp,
                    entry.timestamps,
                    _recordingDataMode,
                    recordFile
                );
                break;
            }
        }
    }
    _state = SessionState::Idle;
    LINFO("Session recording stopped");
}

void SessionRecording::startPlayback(std::string& filename,
                                     bool loop, bool shouldWaitForFinishedTiles)
{
    std::string absFilename;
    if (std::filesystem::is_regular_file(filename)) {
        absFilename = filename;
    }
    else {
        absFilename = absPath("${RECORDINGS}/" + filename).string();
    }

    if (!std::filesystem::is_regular_file(absFilename)) {
        cleanUpPlayback();
        throw ghoul::RuntimeError("Cannot find the specified playback file");
    }

    // Run through conversion in case file is older. Does nothing if the file format
    // is up-to-date
    absFilename = convertFile(absFilename);

    if (_state == SessionState::Recording) {
        throw ghoul::RuntimeError(
            "Unable to start playback while in session recording mode"
        );
    }
    else if (isPlayingBack()) {
        throw ghoul::RuntimeError(
            "Unable to start new playback while in session playback mode"
        );
    }

    _playbackLoopMode = loop;
    _shouldWaitForFinishLoadingWhenPlayback = shouldWaitForFinishedTiles;

    // Open in ASCII first
    _playbackFile.open(absFilename, std::ifstream::in);
    // Read header
    const std::string readBackHeaderString = readHeaderElement(
        _playbackFile,
        FileHeaderTitle.length()
    );
    if (readBackHeaderString != FileHeaderTitle) {
        cleanUpPlayback();
        throw ghoul::RuntimeError(
            "Specified playback file does not contain expected header"
        );
    }
    readHeaderElement(_playbackFile, FileHeaderVersionLength);
    std::string readDataMode = readHeaderElement(_playbackFile, 1);
    if (readDataMode[0] == DataFormatAsciiTag) {
        _recordingDataMode = DataMode::Ascii;
    }
    else if (readDataMode[0] == DataFormatBinaryTag) {
        _recordingDataMode = DataMode::Binary;
    }
    else {
        cleanUpPlayback();
        throw ghoul::RuntimeError(
            "Unknown data type in header (should be Ascii or Binary)"
        );
    }
    // throwaway newline character(s)
    std::string lineEnd = readHeaderElement(_playbackFile, 1);
    bool hasDosLineEnding = (lineEnd == "\r");
    if (hasDosLineEnding) {
        // throwaway the second newline character (\n) also
        readHeaderElement(_playbackFile, 1);
    }

    if (_recordingDataMode == DataMode::Binary) {
        // Close & re-open the file, starting from the beginning, and do dummy read
        // past the header, version, and data type
        _playbackFile.close();
        _playbackFile.open(absFilename, std::ifstream::in | std::ios::binary);
        const size_t headerSize = FileHeaderTitle.length() + FileHeaderVersionLength
            + sizeof(DataFormatBinaryTag) + sizeof('\n');
        std::vector<char> hBuffer;
        hBuffer.resize(headerSize);
        _playbackFile.read(hBuffer.data(), headerSize);
    }

    if (!_playbackFile.is_open() || !_playbackFile.good()) {
        stopPlayback();
        cleanUpPlayback();
        throw ghoul::RuntimeError(std::format(
            "Unable to open file '{}' for keyframe playback", absFilename.c_str()
        ));
    }
    _saveRendering_isFirstFrame = true;
    double now = global::windowDelegate->applicationTime();
    initializePlayback_time(now);

    // Populate liste of loaded scene graph nodes
    _loadedNodes.clear();
    const std::vector<SceneGraphNode*> nodes =
        global::renderEngine->scene()->allSceneGraphNodes();
    for (SceneGraphNode* n : nodes) {
        _loadedNodes.push_back(n->identifier());
    }

    try {
        playbackAddEntriesToTimeline(absFilename);
    }
    catch (const LoadingError& e) {
        LERRORC(e.component, e.message);
        cleanUpPlayback();
        return;
    }

    // Make sure that there is at least one camera keyframe
    bool foundCameraKeyframe = false;
    for (const TimelineEntry& e : _timeline) {
        if (e.keyframeType == RecordedType::Camera) {
            foundCameraKeyframe = true;
            break;
        }
    }
    if (!foundCameraKeyframe) {
        LERROR(std::format(
            "Session recording '{}' did not contain any camera keyframes",
            absFilename
        ));
        cleanUpPlayback();
        return;
    }

    initializePlayback_modeFlags();
    initializePlayback_timeline();

    const bool canTriggerPlayback = global::openSpaceEngine->setMode(
        OpenSpaceEngine::Mode::SessionRecordingPlayback
    );
    if (!canTriggerPlayback) {
        cleanUpPlayback();
        return;
    }

    LINFO(std::format("Playback session started with {} entries", _timeline.size()));

    global::eventEngine->publishEvent<events::EventSessionRecordingPlayback>(
        events::EventSessionRecordingPlayback::State::Started
    );
    _state = SessionState::Playback;

    global::navigationHandler->orbitalNavigator().updateOnCameraInteraction();
}

void SessionRecording::initializePlayback_time(double now) {
    _timestampPlaybackStarted = now;
    _saveRenderingCurrentRecordedTime_interpolation = std::chrono::steady_clock::now();
    _saveRenderingCurrentApplicationTime_interpolation =
        global::windowDelegate->applicationTime();
    _playbackPauseOffset = 0.0;
    global::navigationHandler->keyframeNavigator().setTimeReferenceMode(
        KeyframeTimeRef::Relative_recordedStart, now);
}

void SessionRecording::initializePlayback_modeFlags() {
    _playbackActive_camera = true;
    _playbackActive_script = true;
}

void SessionRecording::initializePlayback_timeline() {
    for (unsigned int i = 0; i < _timeline.size(); i++) {
        if (_timeline[i].keyframeType == RecordedType::Camera) {
            _idxTimeline_cameraFirstInTimeline = i;
            break;
        }
    }
    for (int i = _timeline.size() - 1; i >= 0; i--) {
        if (_timeline[i].keyframeType == RecordedType::Camera) {
            _idxTimeline_cameraLastInTimeline = i;
            break;
        }
    }

    std::string startFocusNode = std::get<CameraEntry>(
        _timeline[_idxTimeline_cameraFirstInTimeline].value
    ).focusNode;
    auto it = std::find(_loadedNodes.begin(), _loadedNodes.end(), startFocusNode);
    if (it == _loadedNodes.end()) {
        throw LoadingError(std::format(
            "Playback file requires scenegraph node '{}', which is "
            "not currently loaded", startFocusNode
        ));
    }

    const Timestamps times = _timeline[_idxTimeline_cameraFirstInTimeline].timestamps;
    global::timeManager->setTimeNextFrame(Time(times.timeSim));
    _saveRenderingCurrentRecordedTime = times.timeRec;

    _idxTimeline_nonCamera = 0;
    _idxTimeline_cameraPtrNext = 0;
    _idxTimeline_cameraPtrPrev = 0;
}

bool SessionRecording::isPlaybackPaused() const {
    return (_state == SessionState::PlaybackPaused);
}

void SessionRecording::setPlaybackPause(bool pause) {
    if (pause && _state == SessionState::Playback) {
        _playbackPausedWithinDeltaTimePause = global::timeManager->isPaused();
        if (!_playbackPausedWithinDeltaTimePause) {
            global::timeManager->setPause(true);
        }
        _state = SessionState::PlaybackPaused;
        global::eventEngine->publishEvent<events::EventSessionRecordingPlayback>(
            events::EventSessionRecordingPlayback::State::Paused
        );
    }
    else if (!pause && _state == SessionState::PlaybackPaused) {
        if (!_playbackPausedWithinDeltaTimePause) {
            global::timeManager->setPause(false);
        }
        _state = SessionState::Playback;
        global::eventEngine->publishEvent<events::EventSessionRecordingPlayback>(
            events::EventSessionRecordingPlayback::State::Resumed
        );
    }
}

void SessionRecording::signalPlaybackFinishedForComponent(RecordedType type) {
    if (type == RecordedType::Camera) {
        _playbackActive_camera = false;
        LINFO("Playback finished signal: camera");
    }
    else if (type == RecordedType::Script) {
        _playbackActive_script = false;
        LINFO("Playback finished signal: script");
    }

    if (!_playbackActive_camera && !_playbackActive_script) {
        if (_playbackLoopMode) {
            // Loop back to the beginning to replay
            _saveRenderingDuringPlayback = false;
            initializePlayback_time(global::windowDelegate->applicationTime());
            initializePlayback_modeFlags();
            initializePlayback_timeline();
            _state = SessionState::Playback;
        }
        else {
            LINFO("Playback session finished");
            handlePlaybackEnd();
        }
    }
}

void SessionRecording::handlePlaybackEnd() {
    _state = SessionState::Idle;
    _cleanupNeededPlayback = true;
    global::eventEngine->publishEvent<events::EventSessionRecordingPlayback>(
        events::EventSessionRecordingPlayback::State::Finished
    );
    global::openSpaceEngine->resetMode();
    global::navigationHandler->resetNavigationUpdateVariables();
}

void SessionRecording::enableTakeScreenShotDuringPlayback(int fps) {
    _saveRenderingDuringPlayback = true;
    _saveRenderingDeltaTime = 1.0 / fps;
}

void SessionRecording::disableTakeScreenShotDuringPlayback() {
    _saveRenderingDuringPlayback = false;
}

void SessionRecording::stopPlayback() {
    if (isPlayingBack()) {
        LINFO("Session playback stopped");
        handlePlaybackEnd();
    }
}

void SessionRecording::cleanUpPlayback() {
    _playbackFile.close();
    cleanUpTimelinesAndKeyframes();
    _cleanupNeededPlayback = false;
}

void SessionRecording::cleanUpTimelinesAndKeyframes() {
    _timeline.clear();
    _savePropertiesBaseline.clear();
    _loadedNodes.clear();
    _idxTimeline_nonCamera = 0;
    _idxTimeline_cameraPtrNext = 0;
    _idxTimeline_cameraPtrPrev = 0;
    _saveRenderingDuringPlayback = false;
    _saveRendering_isFirstFrame = true;
    _playbackPauseOffset = 0.0;
    _playbackLoopMode = false;
}

void SessionRecording::saveScriptKeyframeToTimeline(std::string script) {
    constexpr std::array<std::string_view, 6> ScriptRejects = {
        "openspace.sessionRecording.enableTakeScreenShotDuringPlayback",
        "openspace.sessionRecording.startPlayback",
        "openspace.sessionRecording.stopPlayback",
        "openspace.sessionRecording.startRecording",
        "openspace.sessionRecording.stopRecording",
        "openspace.scriptScheduler.clear"
    };

    constexpr std::array<std::string_view, 1> ScriptsToBeTrimmed = {
        "openspace.sessionRecording.togglePlaybackPause"
    };

    if (script.starts_with(scriptReturnPrefix)) {
        script = script.substr(scriptReturnPrefix.length());
    }
    for (std::string_view reject : ScriptRejects) {
        if (script.starts_with(reject)) {
            return;
        }
    }

    // Trim commands from script if found
    for (std::string_view trimSnippet : ScriptsToBeTrimmed) {
        auto findIdx = script.find(trimSnippet);
        if (findIdx != std::string::npos) {
            auto findClosingParens = script.find_first_of(')', findIdx);
            script.erase(findIdx, findClosingParens + 1);
        }
    }

    // Any script snippet included in this vector will be trimmed from any script
    // from the script manager, before it is recorded in the session recording file.
    // The remainder of the script will be retained.
    using ScriptSubstringReplace = std::pair<std::string_view, std::string_view>;
    constexpr std::array<ScriptSubstringReplace, 2> ScriptsToBeReplaced = {
        std::pair {
            "openspace.time.pauseToggleViaKeyboard",
            "openspace.time.interpolateTogglePause"
        }
    };

    // Replace commands from script if found
    for (const ScriptSubstringReplace& replacementSnippet : ScriptsToBeReplaced) {
        auto findIdx = script.find(replacementSnippet.first);
        if (findIdx != std::string::npos) {
            script.erase(findIdx, replacementSnippet.first.length());
            script.insert(findIdx, replacementSnippet.second);
        }
    }

    const datamessagestructures::ScriptMessage sm =
        datamessagestructures::generateScriptMessage(script);

    Timestamps times = generateCurrentTimestamp(sm._timestamp, _timestampsRecordStarted.timeOs);
    addKeyframe(std::move(times), sm._script);
}

void SessionRecording::savePropertyBaseline(properties::Property& prop) {
    constexpr std::array<std::string_view, 4> PropertyBaselineRejects{
        "NavigationHandler.OrbitalNavigator.Anchor",
        "NavigationHandler.OrbitalNavigator.Aim",
        "NavigationHandler.OrbitalNavigator.RetargetAnchor",
        "NavigationHandler.OrbitalNavigator.RetargetAim"
    };

    const std::string propIdentifier = prop.uri();
    for (std::string_view reject : PropertyBaselineRejects) {
        if (propIdentifier.starts_with(reject)) {
            return;
        }
    }

    const bool isPropAlreadySaved = _savePropertiesBaseline.contains(propIdentifier);
    if (!isPropAlreadySaved) {
        const std::string initialScriptCommand = std::format(
            "openspace.setPropertyValueSingle(\"{}\", {})",
            propIdentifier, prop.stringValue()
        );
        _savePropertiesBaseline[propIdentifier] = initialScriptCommand;
    }
}

void SessionRecording::preSynchronization() {
    ZoneScoped;

    if (_state == SessionState::Recording) {
        CameraKeyframe kf = generateCameraKeyframe();
        Timestamps times = generateCurrentTimestamp(kf._timestamp, _timestampsRecordStarted.timeOs);
        addKeyframe(std::move(times), std::move(kf));
    }
    else if (isPlayingBack()) {
        if (!_timeline.empty()) {
            moveAheadInTime();
        }
    }
    else if (_cleanupNeededPlayback) {
        cleanUpPlayback();
    }

    // Handle callback(s) for change in idle/record/playback state
    if (_state != _lastState) {
        using K = CallbackHandle;
        using V = StateChangeCallback;
        for (const std::pair<const K, V>& it : _stateChangeCallbacks) {
            it.second();
        }
    }
    _lastState = _state;
}

void SessionRecording::render() const {
    ZoneScoped;

    if (!_renderPlaybackInformation || !isPlayingBack()) {
        return;
    }

    constexpr std::string_view FontName = "Mono";
    constexpr float FontSizeFrameinfo = 32.f;
    const std::shared_ptr<ghoul::fontrendering::Font> font =
        global::fontManager->font(FontName, FontSizeFrameinfo);

    const glm::vec2 res = global::renderEngine->fontResolution();
    glm::vec2 penPosition = glm::vec2(
        res.x / 2 - 150.f,
        res.y / 4
    );
    const std::string text1 = std::to_string(currentTime());
    ghoul::fontrendering::RenderFont(
        *font,
        penPosition,
        text1,
        glm::vec4(1.f),
        ghoul::fontrendering::CrDirection::Down
    );
    const std::string text2 = std::format(
        "Scale: {}", global::navigationHandler->camera()->scaling()
    );
    ghoul::fontrendering::RenderFont(*font, penPosition, text2, glm::vec4(1.f));
}

bool SessionRecording::isRecording() const {
    return (_state == SessionState::Recording);
}

bool SessionRecording::isPlayingBack() const {
    return (_state == SessionState::Playback || _state == SessionState::PlaybackPaused);
}

bool SessionRecording::isSavingFramesDuringPlayback() const {
    return (isPlayingBack() && _saveRenderingDuringPlayback);
}

bool SessionRecording::shouldWaitForTileLoading() const {
    return _shouldWaitForFinishLoadingWhenPlayback;
}

SessionRecording::SessionState SessionRecording::state() const {
    return _state;
}

void SessionRecording::playbackAddEntriesToTimeline(std::string playbackFilename) {
    int lineNumber = 1;
    if (_recordingDataMode == DataMode::Binary) {
        while (true) {
            unsigned char frameType;
            _playbackFile.read(reinterpret_cast<char*>(&frameType), sizeof(unsigned char));
            // Check if have reached EOF
            if (!_playbackFile) {
                LINFO(std::format(
                    "Finished parsing {} entries from playback file '{}'",
                    lineNumber - 1, playbackFilename
                ));
                break;
            }
            if (frameType == HeaderCameraBinary) {
                playbackCamera("", lineNumber);
            }
            else if (frameType == HeaderScriptBinary) {
                playbackScript("", lineNumber);
            }
            else {
                throw LoadingError(std::format(
                    "Unknown frame type {} @ index {} of playback file '{}'",
                    frameType, lineNumber - 1, playbackFilename
                ));
            }

            lineNumber++;
        }
    }
    else {
        std::string lineParsing;
        while (ghoul::getline(_playbackFile, lineParsing)) {
            lineNumber++;

            std::istringstream iss = std::istringstream(lineParsing);
            std::string entryType;
            if (!(iss >> entryType)) {
                LERROR(std::format(
                    "Error reading entry type @ line {} of playback file '{}'",
                    lineNumber, playbackFilename
                ));
                break;
            }

            if (entryType == HeaderCameraAscii) {
                playbackCamera(lineParsing, lineNumber);
            }
            else if (entryType == HeaderScriptAscii) {
                playbackScript(lineParsing, lineNumber);
            }
            else if (entryType.substr(0, 1) == HeaderCommentAscii) {
                continue;
            }
            else {
                throw LoadingError(std::format(
                    "Unknown frame type {} @ line {} of playback file '{}'",
                    entryType, lineNumber, playbackFilename
                ));
            }
        }
        LINFO(std::format(
            "Finished parsing {} entries from playback file '{}'",
            lineNumber, playbackFilename
        ));
    }
}

double SessionRecording::currentTime() const {
    if (isSavingFramesDuringPlayback()) {
        return _saveRenderingCurrentRecordedTime;
    }
    else {
        return global::windowDelegate->applicationTime() -
               _timestampPlaybackStarted - _playbackPauseOffset;
    }
}

double SessionRecording::fixedDeltaTimeDuringFrameOutput() const {
    // Check if renderable in focus is still resolving tile loading
    // do not adjust time while we are doing this
    const SceneGraphNode* an = global::navigationHandler->orbitalNavigator().anchorNode();
    const Renderable* focusRenderable = an->renderable();
    if (!focusRenderable || focusRenderable->renderedWithDesiredData()) {
        return _saveRenderingDeltaTime;
    }
    else {
        return 0.0;
    }
}

std::chrono::steady_clock::time_point
SessionRecording::currentPlaybackInterpolationTime() const {
    return _saveRenderingCurrentRecordedTime_interpolation;
}

double SessionRecording::currentApplicationInterpolationTime() const {
    return _saveRenderingCurrentApplicationTime_interpolation;
}

void SessionRecording::playbackCamera(std::string lineParsing, int lineNumber) {
    auto [times, kf] = readSingleKeyframeCamera(
        _recordingDataMode,
        _playbackFile,
        lineParsing,
        lineNumber
    );
    addKeyframe(std::move(times), std::move(kf));
}

void SessionRecording::convertCamera(std::stringstream& inStream, DataMode mode,
                                     int lineNum, std::string& inputLine,
                                     std::ofstream& outFile)
{
    auto [times, kf] = readSingleKeyframeCamera(mode, inStream, inputLine, lineNum);
    saveSingleKeyframeCamera(kf, times, mode, outFile, _addModelMatrixinAscii);
}

void SessionRecording::playbackScript(std::string lineParsing, int lineNumber) {
    auto [times, kf] = readSingleKeyframeScript(
        _recordingDataMode,
        _playbackFile,
        lineParsing,
        lineNumber
    );
    checkIfScriptUsesScenegraphNode(kf._script);
    addKeyframe(std::move(times), std::move(kf._script));
}

void SessionRecording::checkIfScriptUsesScenegraphNode(std::string s) const {
    auto isolateTermFromQuotes = [](std::string s) -> std::string {
        // Remove any leading spaces
        while (s.front() == ' ') {
            s.erase(0, 1);
        }
        const std::string possibleQuotes = "\'\"[]";
        while (possibleQuotes.find(s.front()) != std::string::npos) {
            s.erase(0, 1);
        }
        for (const char q : possibleQuotes) {
            if (s.find(q) != std::string::npos) {
                s = s.substr(0, s.find(q));
                return s;
            }
        }
        // If no quotes found, remove other possible characters from end
        const std::string unwantedChars = " );";
        while (!s.empty() && (unwantedChars.find(s.back()) != std::string::npos)) {
            s.pop_back();
        }
        return s;
    };

    auto checkForScenegraphNodeAccessNav = [](std::string& navTerm) {
        constexpr std::array<std::string_view, 3> NavScriptsUsingNodes = {
            "RetargetAnchor",
            "Anchor",
            "Aim"
        };

        const std::string nextTerm = "NavigationHandler.OrbitalNavigator.";
        const size_t posNav = navTerm.find(nextTerm);
        if (posNav != std::string::npos) {
            for (std::string_view accessName : NavScriptsUsingNodes) {
                if (navTerm.find(accessName) != std::string::npos) {
                    return true;
                }
            }
        }
        return false;
    };

    auto extractScenegraphNodeFromScene = [](const std::string & s) -> std::string {
        const std::string scene = "Scene.";
        std::string extracted;
        const size_t posScene = s.find(scene);
        if (posScene != std::string::npos) {
            const size_t posDot = s.find('.', posScene + scene.length() + 1);
            if (posDot > posScene && posDot != std::string::npos) {
                extracted = s.substr(posScene + scene.length(), posDot -
                    (posScene + scene.length()));
            }
        }
        return extracted;
    };

    if (s.rfind(scriptReturnPrefix, 0) == 0) {
        s.erase(0, scriptReturnPrefix.length());
    }
    // This works for both setPropertyValue and setPropertyValueSingle
    const bool containsSetPropertyVal = (s.rfind("openspace.setPropertyValue", 0) == 0);
    const bool containsParensStart = (s.find('(') != std::string::npos);
    if (containsSetPropertyVal && containsParensStart) {
        std::string subjectOfSetProp = isolateTermFromQuotes(s.substr(s.find('(') + 1));
        if (checkForScenegraphNodeAccessNav(subjectOfSetProp)) {
            const size_t commaPos = s.find(',');
            std::string navNode = isolateTermFromQuotes(s.substr(commaPos + 1));
            if (navNode != "nil") {
                auto it = std::find(_loadedNodes.begin(), _loadedNodes.end(), navNode);
                if (it == _loadedNodes.end()) {
                    LWARNING(std::format(
                        "Playback file contains a property setting of navigation using "
                        "scenegraph node '{}', which is not currently loaded", navNode
                    ));
                }
            }
        }
        else if (subjectOfSetProp.find("Scene.") != std::string::npos) {
            std::string found = extractScenegraphNodeFromScene(subjectOfSetProp);
            if (!found.empty()) {
                const std::vector<properties::Property*> matchHits =
                    global::renderEngine->scene()->propertiesMatchingRegex(
                        subjectOfSetProp
                    );
                if (matchHits.empty()) {
                    LWARNING(std::format(
                        "Playback file contains a property setting of scenegraph "
                        "node '{}', which is not currently loaded", found
                    ));
                }
            }
        }
    }
}

void SessionRecording::convertScript(std::stringstream& inStream, DataMode mode,
                                     int lineNum, std::string& inputLine,
                                     std::ofstream& outFile)
{
    auto [times, kf] = readSingleKeyframeScript(mode, inStream, inputLine, lineNum);
    saveSingleKeyframeScript(std::move(kf), std::move(times), mode, outFile);
}

void SessionRecording::addKeyframe(Timestamps t3stamps,
                                   datamessagestructures::CameraKeyframe keyframe)
{
    _timeline.emplace_back(
        RecordedType::Camera,
        t3stamps,
        interaction::KeyframeNavigator::CameraPose(std::move(keyframe))
    );
}

void SessionRecording::addKeyframe(Timestamps t3stamps, std::string scriptToQueue) {
    _timeline.emplace_back(
        RecordedType::Script,
        t3stamps,
        std::move(scriptToQueue)
    );
}

void SessionRecording::moveAheadInTime() {
    const bool isPlaybackPaused = (_state == SessionState::PlaybackPaused);
    if (isPlaybackPaused) {
        _playbackPauseOffset += global::windowDelegate->applicationTime() - _previousTime;
    }
    _previousTime = global::windowDelegate->applicationTime();

    const double currTime = currentTime();
    lookForNonCameraKeyframesThatHaveComeDue(currTime);
    updateCameraWithOrWithoutNewKeyframes(currTime);

    // Unfortunately the first frame is sometimes rendered because globebrowsing reports
    // that all chunks are rendered when they apparently are not.
    if (_saveRendering_isFirstFrame) {
        _saveRendering_isFirstFrame = false;
        return;
    }
    if (_shouldWaitForFinishLoadingWhenPlayback && isSavingFramesDuringPlayback()) {
        // Check if renderable in focus is still resolving tile loading
        // do not adjust time while we are doing this, or take screenshot
        const SceneGraphNode* focusNode =
            global::navigationHandler->orbitalNavigator().anchorNode();
        const Renderable* focusRenderable = focusNode->renderable();
        if (!focusRenderable || focusRenderable->renderedWithDesiredData()) {
            if (!isPlaybackPaused) {
                _saveRenderingCurrentRecordedTime_interpolation +=
                    std::chrono::microseconds(static_cast<long>(_saveRenderingDeltaTime * 1000000));
                _saveRenderingCurrentRecordedTime += _saveRenderingDeltaTime;
                _saveRenderingCurrentApplicationTime_interpolation +=
                    _saveRenderingDeltaTime;
                global::renderEngine->takeScreenshot();
            }
        }
    }
}

void SessionRecording::lookForNonCameraKeyframesThatHaveComeDue(double currTime) {
    while (_playbackActive_script && (currTime > _timeline[_idxTimeline_nonCamera].timestamps.timeRec)) {
        ghoul_assert(
            _timeline[_idxTimeline_nonCamera].keyframeType == RecordedType::Script,
            "Index out of whack"
        );

        global::scriptEngine->queueScript(
            std::get<ScriptEntry>(_timeline[_idxTimeline_nonCamera].value),
            scripting::ScriptEngine::ShouldBeSynchronized::Yes,
            scripting::ScriptEngine::ShouldSendToRemote::Yes
        );

        // Move the index forward to the next script entry or until the end of the vector
        do {
            _idxTimeline_nonCamera++;
        }
        while (
            _idxTimeline_nonCamera != _timeline.size() &&
            _timeline[_idxTimeline_nonCamera].keyframeType != RecordedType::Script
        );

        if (_idxTimeline_nonCamera == _timeline.size()) {
            if (_playbackActive_script) {
                signalPlaybackFinishedForComponent(RecordedType::Script);
            }
            break;
        }
    }
}

void SessionRecording::updateCameraWithOrWithoutNewKeyframes(double currTime) {
    if (!_playbackActive_camera) {
        return;
    }

    const bool didFindFutureCameraKeyframes = findNextFutureCameraIndex(currTime);
    const bool isPrevAtFirstKeyframe =
        (_idxTimeline_cameraPtrPrev == _idxTimeline_cameraFirstInTimeline);
    const bool isFirstTimelineCameraKeyframeInFuture =
        (currTime < _timeline[_idxTimeline_cameraFirstInTimeline].timestamps.timeRec);

    if (!isPrevAtFirstKeyframe || !isFirstTimelineCameraKeyframeInFuture) {
        const TimelineEntry& prev = _timeline[_idxTimeline_cameraPtrPrev];
        interaction::KeyframeNavigator::CameraPose prevPose =
            std::get<CameraEntry>(prev.value);
        const double prevTime = prev.timestamps.timeRec;

        const TimelineEntry& next = _timeline[_idxTimeline_cameraPtrNext];
        interaction::KeyframeNavigator::CameraPose nextPose =
            std::get<CameraEntry>(next.value);
        const double nextTime = next.timestamps.timeRec;

        double t = 0.0;
        if ((nextTime - prevTime) >= 1e-7) {
            t = (currTime - prevTime) / (nextTime - prevTime);
        }

        // Need to actively update the focusNode position of the camera in relation to
        // the rendered objects will be unstable and actually incorrect
        const SceneGraphNode* n = sceneGraphNode(prevPose.focusNode);
        if (n) {
            global::navigationHandler->orbitalNavigator().setFocusNode(n->identifier());
        }

        interaction::KeyframeNavigator::updateCamera(
            global::navigationHandler->camera(),
            prevPose,
            nextPose,
            t,
            _ignoreRecordedScale
        );
    }
    if (!didFindFutureCameraKeyframes) {
        signalPlaybackFinishedForComponent(RecordedType::Camera);
    }
}

bool SessionRecording::findNextFutureCameraIndex(double currTime) {
    bool hasHitEndOfCameraKeyframes = false;
    for (unsigned int seekAheadIndex = _idxTimeline_cameraPtrPrev + 1;
         seekAheadIndex < _timeline.size();
         seekAheadIndex++)
    {
        if (_timeline[seekAheadIndex].keyframeType != RecordedType::Camera) {
            continue;
        }

        if (seekAheadIndex == _idxTimeline_cameraLastInTimeline) {
            hasHitEndOfCameraKeyframes = true;
        }

        if (seekAheadIndex > _idxTimeline_cameraPtrNext) {
            _idxTimeline_cameraPtrPrev = _idxTimeline_cameraPtrNext;
            _idxTimeline_cameraPtrNext = seekAheadIndex;
        }
        if (currTime < _timeline[seekAheadIndex].timestamps.timeRec) {
            break;
        }
        else {
            // Force interpolation between consecutive keyframes
            _idxTimeline_cameraPtrPrev = seekAheadIndex;
        }
    }

    if ((currTime > _timeline[_idxTimeline_cameraPtrNext].timestamps.timeRec) &&
        hasHitEndOfCameraKeyframes)
    {
        _idxTimeline_cameraPtrPrev = _idxTimeline_cameraPtrNext;
        return false;
    }

    return true;
}

SessionRecording::CallbackHandle SessionRecording::addStateChangeCallback(
                                                                   StateChangeCallback cb)
{
    const CallbackHandle handle = _nextCallbackHandle++;
    _stateChangeCallbacks.emplace_back(handle, std::move(cb));
    return handle;
}

void SessionRecording::removeStateChangeCallback(CallbackHandle handle) {
    const auto it = std::find_if(
        _stateChangeCallbacks.begin(),
        _stateChangeCallbacks.end(),
        [handle](const std::pair<CallbackHandle, std::function<void()>>& cb) {
            return cb.first == handle;
        }
    );

    ghoul_assert(
        it != _stateChangeCallbacks.end(),
        "handle must be a valid callback handle"
    );

    _stateChangeCallbacks.erase(it);
}

std::vector<std::string> SessionRecording::playbackList() const {
    const std::filesystem::path path = absPath("${RECORDINGS}");
    if (!std::filesystem::is_directory(path)) {
        return std::vector<std::string>();
    }

    std::vector<std::string> fileList;
    namespace fs = std::filesystem;
    for (const fs::directory_entry& e : fs::directory_iterator(path)) {
        if (!e.is_regular_file()) {
            continue;
        }

        // Remove path and keep only the filename
        const std::string filename = e.path().filename().string();
#ifdef WIN32
        DWORD attributes = GetFileAttributes(e.path().string().c_str());
        bool isHidden = attributes & FILE_ATTRIBUTE_HIDDEN;
#else
        const bool isHidden = filename.find('.') == 0;
#endif // WIN32
        if (!isHidden) {
            // Don't add hidden files
            fileList.push_back(filename);
        }
    }
    std::sort(fileList.begin(), fileList.end());
    return fileList;
}

std::string SessionRecording::convertFile(std::string filename, int depth) {
    std::string conversionOutFilename = filename;
    std::stringstream conversionInStream;
    static constexpr int MaximumRecursionDepth = 50;
    if (depth >= MaximumRecursionDepth) {
        throw ghoul::RuntimeError(
            "Runaway recursion in session recording conversion of file version",
            "SessionRecording"
        );
    }
    std::string newFilename = filename;
    readFileIntoStringStream(filename, conversionInStream);
    DataMode mode;
    std::string fileVersion;
    readPlaybackHeader_stream(conversionInStream, fileVersion, mode);
    const int conversionLineNum = 1;

    // If this instance of the SessionRecording class isn't the instance with the
    // correct version of the file to be converted, then call getLegacy() to recurse
    // to the next level down in the legacy subclasses until we get the right
    // version, then proceed with conversion from there.
    if (fileVersion != fileFormatVersion()) {
        //conversionInStream.seekg(conversionInStream.beg);
        newFilename = getLegacyConversionResult(filename, depth + 1);

        // Remove trailing path slashes
        while (newFilename.substr(newFilename.length() - 1, 1) == "/") {
            newFilename.pop_back();
        }
        while (newFilename.substr(newFilename.length() - 1, 1) == "\\") {
            newFilename.pop_back();
        }

        readFileIntoStringStream(newFilename, conversionInStream);
        readPlaybackHeader_stream(conversionInStream, fileVersion, mode);
    }
    if (depth == 0) {
        return newFilename;
    }

    conversionOutFilename = determineConversionOutFilename(filename, mode);
    LINFO(std::format(
        "Starting conversion on rec file '{}', version {} in {} mode. "
        "Writing result to '{}'",
        newFilename, fileVersion, (mode == DataMode::Ascii) ? "ascii" : "binary",
        conversionOutFilename
    ));
    std::ofstream conversionOutFile;
    if (mode == DataMode::Binary) {
        conversionOutFile.open(conversionOutFilename, std::ios::binary);
    }
    else {
        conversionOutFile.open(conversionOutFilename);
    }
    if (!conversionOutFile.is_open() || !conversionOutFile.good()) {
        LERROR(std::format(
            "Unable to open file '{}' for conversion result", conversionOutFilename
        ));
        return "";
    }
    conversionOutFile << FileHeaderTitle;
    conversionOutFile.write(
        targetFileFormatVersion().c_str(),
        FileHeaderVersionLength
    );
    if (mode == DataMode::Binary) {
        conversionOutFile << DataFormatBinaryTag;
    }
    else {
        conversionOutFile << DataFormatAsciiTag;
    }
    conversionOutFile << '\n';
    convertEntries(
        newFilename,
        conversionInStream,
        mode,
        conversionLineNum,
        conversionOutFile
    );

    return conversionOutFilename;
}

void SessionRecording::convertEntries(const std::string& inFilename,
                                      std::stringstream& inStream, DataMode mode,
                                      int lineNum, std::ofstream& outFile)
{
    // There is some more elegant way of doing this
    std::string lineParsing;

    if (mode == DataMode::Binary) {
        while (true) {
            unsigned char frameType;
            inStream.read(reinterpret_cast<char*>(&frameType), sizeof(unsigned char));
            // Check if have reached EOF
            if (!inStream) {
                LINFO(std::format(
                    "Finished converting {} entries from playback file '{}'",
                    lineNum - 1, inFilename
                ));
                break;
            }
            if (frameType == HeaderCameraBinary) {
                convertCamera(inStream, mode, lineNum, lineParsing, outFile);
            }
            else if (frameType == HeaderScriptBinary) {
                convertScript(inStream, mode, lineNum, lineParsing, outFile);
            }
            else {
                throw LoadingError(std::format(
                    "Unknown frame type {} @ index {} of conversion file '{}'",
                    frameType, lineNum - 1, inFilename
                ));
            }
            lineNum++;
        }
    }
    else {
        while (ghoul::getline(inStream, lineParsing)) {
            lineNum++;

            std::istringstream iss(lineParsing);
            std::string entryType;
            if (!(iss >> entryType)) {
                LERROR(std::format(
                    "Error reading entry type @ line {} of conversion file '{}'",
                    lineNum, inFilename
                ));
                break;
            }

            if (entryType == HeaderCameraAscii) {
                convertCamera(inStream, mode, lineNum, lineParsing, outFile);
            }
            else if (entryType == HeaderScriptAscii) {
                convertScript(inStream, mode, lineNum, lineParsing, outFile);
            }
            else if (entryType.substr(0, 1) == HeaderCommentAscii) {
                continue;
            }
            else {
                throw LoadingError(std::format(
                    "Unknown frame type {} @ line {} of conversion file '{}'",
                    entryType, lineNum, inFilename
                ));
            }
        }
        LINFO(std::format(
            "Finished parsing {} entries from conversion file '{}'",
            lineNum, inFilename
        ));
    }
}

std::string SessionRecording::getLegacyConversionResult(std::string filename, int depth) {
    SessionRecording_legacy_0085 legacy;
    return legacy.convertFile(std::move(filename), depth);
}

std::string SessionRecording_legacy_0085::getLegacyConversionResult(std::string filename,
                                                                    int)
{
    // This method is overriden in each legacy subclass, but does nothing in this instance
    // as the oldest supported legacy version.
    LERROR(std::format(
        "Version 00.85 is the oldest supported legacy file format; no conversion "
        "can be made. It is possible that file '{}' has a corrupted header or an invalid "
        "file format version number",
        filename
    ));
    return filename;
}

std::string SessionRecording::fileFormatVersion() {
    return std::string(FileHeaderVersion);
}

std::string SessionRecording::targetFileFormatVersion() {
    return std::string(FileHeaderVersion);
}

std::string SessionRecording::determineConversionOutFilename(const std::string& filename,
                                                             DataMode mode)
{
    std::string filenameSansExtension = filename;
    const std::string fileExtension = (mode == DataMode::Binary) ?
        FileExtensionBinary : FileExtensionAscii;

    if (filename.find_last_of('.') != std::string::npos) {
        filenameSansExtension = filename.substr(0, filename.find_last_of('.'));
    }
    filenameSansExtension += "_" + fileFormatVersion() + "-" + targetFileFormatVersion();
    return filenameSansExtension + fileExtension;
}

void SessionRecording_legacy_0085::convertScript(std::stringstream& inStream,
                                                 DataMode mode, int lineNum,
                                                 std::string& inputLine,
                                                 std::ofstream& outFile)
{
    auto [times, kf] = readSingleKeyframeScript(mode, inStream, inputLine, lineNum);
    saveSingleKeyframeScript(kf, times, mode, outFile);
}

void SessionRecording_legacy_0085::ScriptMessage_legacy_0085::read(std::istream* in) {
    size_t strLen;
    // Read string length from file
    in->read(reinterpret_cast<char*>(&strLen), sizeof(strLen));
    // 2000 = Previous max length for scripts
    if (strLen > 2000) {
        throw LoadingError("Invalid script size for conversion read");
    }
    // Read back full string
    std::vector<char> temp(strLen + 1);
    in->read(temp.data(), strLen);
    temp[strLen] = '\0';

    _script.erase();
    _script = temp.data();
}

scripting::LuaLibrary SessionRecording::luaLibrary() {
    return {
        "sessionRecording",
        {
            codegen::lua::StartRecording,
            codegen::lua::StartRecordingAscii,
            codegen::lua::StopRecording,
            codegen::lua::StartPlayback,
            codegen::lua::StopPlayback,
            codegen::lua::EnableTakeScreenShotDuringPlayback,
            codegen::lua::DisableTakeScreenShotDuringPlayback,
            codegen::lua::FileFormatConversion,
            codegen::lua::SetPlaybackPause,
            codegen::lua::TogglePlaybackPause,
            codegen::lua::IsPlayingBack,
            codegen::lua::IsRecording
        }
    };
}








void convertToAscii(std::filesystem::path inPath, std::filesystem::path outPath) {
    int lineNum = 1;
    std::ofstream oFile = std::ofstream(outPath, std::ifstream::app);
    const char tmpType = DataFormatAsciiTag;
    oFile.write(&tmpType, 1);
    oFile.write("\n", 1);

    std::ifstream file = std::ifstream(inPath, std::ifstream::in | std::ifstream::binary);

    while (true) {
        unsigned char frameType;
        file.read(reinterpret_cast<char*>(&frameType), sizeof(unsigned char));
        // Check if have reached EOF
        if (!file) {
            LINFO(std::format(
                "Finished converting {} entries from file '{}'", lineNum - 1, inPath
            ));
            break;
        }

        std::stringstream keyframeLine = std::stringstream();
        keyframeLine.str(std::string());

        if (frameType == HeaderCameraBinary) {
            auto [times, kf] = readCameraKeyframeBinary(file, lineNum);
            saveHeaderAscii(times, HeaderCameraAscii, keyframeLine);
            kf.write(keyframeLine);
        }
        else if (frameType == HeaderScriptBinary) {
            auto [times, kf] = readScriptKeyframeBinary(file, lineNum);
            saveHeaderAscii(times, HeaderScriptAscii, keyframeLine);
            kf.write(keyframeLine);
        }
        else {
            LERROR(std::format(
                "Unknown frame type @ index {} of playback file '{}'", lineNum - 1, inPath
            ));
            break;
        }

        oFile << keyframeLine.str() << '\n';
        lineNum++;
    }
}

void convertToBinary(std::filesystem::path inPath, std::filesystem::path outPath) {
    int lineNum = 1;
    std::ofstream oFile = std::ofstream(outPath, std::ifstream::app | std::ios::binary);
    const char tmpType = DataFormatBinaryTag;
    oFile.write(&tmpType, 1);
    oFile.write("\n", 1);

    std::ifstream file = std::ifstream(inPath, std::ifstream::in | std::ifstream::binary);

    std::string lineContents;
    while (ghoul::getline(file, lineContents)) {
        lineNum++;

        std::istringstream iss(lineContents);
        std::string entryType;
        if (!(iss >> entryType)) {
            LERROR(std::format(
                "Error reading entry type @ line {} of file '{}'", lineNum, inPath
            ));
            break;
        }

        if (entryType == HeaderCameraAscii) {
            auto [times, kf] = readCameraKeyframeAscii(lineContents, lineNum);
            saveCameraKeyframeBinary(times, kf, oFile);
        }
        else if (entryType == HeaderScriptAscii) {
            auto [times, kf] = readScriptKeyframeAscii(lineContents, lineNum);
            saveScriptKeyframeBinary(times, kf, oFile);
        }
        else if (entryType.substr(0, 1) == HeaderCommentAscii) {
            continue;
        }
        else {
            LERROR(std::format(
                "Unknown frame type {} @ line {} of file '{}'",
                entryType, lineContents, inPath
            ));
            break;
        }
    }
    LINFO(std::format("Finished converting {} entries from file '{}'", lineNum, inPath));
}

void convertTypes(SessionRecording::DataMode fileFormatType,
                  std::filesystem::path inFilePath, std::filesystem::path outFilePath,
                  std::string version)
{
    std::string currentFormat;
    std::string expectedFileExtension_in;
    std::string expectedFileExtension_out;
    if (fileFormatType == SessionRecording::DataMode::Binary) {
        currentFormat = "binary";
        expectedFileExtension_in = FileExtensionBinary;
        expectedFileExtension_out = FileExtensionAscii;
    }
    else if (fileFormatType == SessionRecording::DataMode::Ascii) {
        currentFormat = "ascii";
        expectedFileExtension_in = FileExtensionAscii;
        expectedFileExtension_out = FileExtensionBinary;
    }

    if (inFilePath.extension() != expectedFileExtension_in) {
        LWARNING(std::format(
            "Input filename doesn't have expected '{}' format file extension",
            currentFormat
        ));
    }
    if (outFilePath.extension() == expectedFileExtension_in) {
        LERROR(std::format(
            "Output filename has '{}' file extension, but is conversion from '{}'",
            currentFormat, currentFormat
        ));
        return;
    }
    else if (outFilePath.extension() != expectedFileExtension_out) {
        outFilePath += expectedFileExtension_out;
    }

    std::ifstream iFile;
    std::ofstream oFile;
    if (fileFormatType == SessionRecording::DataMode::Ascii) {
        iFile.open(inFilePath, std::ifstream::in);
        //Throw out first line
        std::string throw_out;
        ghoul::getline(iFile, throw_out);
        oFile.open(outFilePath);
    }
    else if (fileFormatType == SessionRecording::DataMode::Binary) {
        oFile.open(outFilePath, std::ios::binary);
    }
    oFile.write(FileHeaderTitle.c_str(), FileHeaderTitle.length());
    oFile.write(version.c_str(), FileHeaderVersionLength);
    oFile.close();

    if (fileFormatType == SessionRecording::DataMode::Ascii) {
        convertToBinary(inFilePath, outFilePath);
    }
    else if (fileFormatType == SessionRecording::DataMode::Binary) {
        convertToAscii(inFilePath, outFilePath);
    }
    else {
        throw LoadingError("Session recording file unrecognized format type");
    }
}

std::tuple<SessionRecording::DataMode, std::string> determineFormatTypeAndVersion(
                                                             std::filesystem::path inPath)
{
    std::ifstream file = std::ifstream(inPath, std::ifstream::in | std::ifstream::binary);

    std::string line = readHeaderElement(file, FileHeaderTitle.length());
    if (line.substr(0, FileHeaderTitle.length()) == FileHeaderTitle) {
        throw LoadingError(std::format(
            "Session recording file '{}' does not have expected header", inPath
        ));
    }

    std::string version = readHeaderElement(file, FileHeaderVersionLength);
    line = readHeaderElement(file, 1);
    readHeaderElement(file, 1);

    SessionRecording::DataMode fileFormatType;
    if (line.at(0) == DataFormatAsciiTag) {
        fileFormatType = SessionRecording::DataMode::Ascii;
    }
    else if (line.at(0) == DataFormatBinaryTag) {
        fileFormatType = SessionRecording::DataMode::Binary;
    }
    else {
        throw LoadingError(std::format("Unrecognized file format tag {}", line.at(0)));
    }

    return { fileFormatType, version };
}

} // namespace openspace
