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

#include <openspace/interaction/sessionrecordinghandler.h>

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

#include "sessionrecordinghandler_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "SessionRecording";

    template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;


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

    const std::string FileExtensionBinary = ".osrec";
    const std::string FileExtensionAscii = ".osrectxt";

    constexpr std::string_view scriptReturnPrefix = "return ";
} // namespace

namespace openspace::interaction {

SessionRecordingHandler::SessionRecordingHandler()
    : properties::PropertyOwner({ "SessionRecording", "Session Recording" })
    , _renderPlaybackInformation(RenderPlaybackInfo, false)
    , _ignoreRecordedScale(IgnoreRecordedScaleInfo, false)
    , _addModelMatrixinAscii(AddModelMatrixinAsciiInfo, false)
{
    addProperty(_renderPlaybackInformation);
    addProperty(_ignoreRecordedScale);
    addProperty(_addModelMatrixinAscii);
}

void SessionRecordingHandler::startRecording() {
    _timeline.clear();
    if (_state == SessionState::Recording) {
        throw ghoul::RuntimeError(
            "Unable to start recording while already in recording mode",
            "SessionRecordingHandler"
        );
    }
    else if (isPlayingBack()) {
        throw ghoul::RuntimeError(
            "Unable to start recording while in session playback mode",
            "SessionRecordingHandler"
        );
    }

    _state = SessionState::Recording;
    _savePropertiesBaseline.clear();

    _recording.elapsedTime = 0.0;

    // Record the current delta time as the first property to save in the file.
    // This needs to be saved as a baseline whether or not it changes during recording
    // Dummy `_time` "property" to store the time setup in the baseline
    _savePropertiesBaseline["_time"] = std::format(
        "openspace.time.setPause({});openspace.time.setDeltaTime({});",
        global::timeManager->isPaused() ? "true" : "false",
        global::timeManager->targetDeltaTime()
    );

    LINFO("Session recording started");
}

void SessionRecordingHandler::stopRecording(const std::string& fn, DataMode dataMode) {
    if (_state != SessionState::Recording) {
        return;
    }

    if (!std::filesystem::is_directory(absPath("${RECORDINGS}"))) {
        std::filesystem::create_directories(absPath("${RECORDINGS}"));
    }

    std::filesystem::path absFilename = fn;
    if (absFilename.parent_path().empty() || absFilename.parent_path() == absFilename) {
        absFilename = absPath("${RECORDINGS}/" + fn);
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


    defer {
        cleanUpTimelinesAndKeyframes();
    };

    std::vector<SessionRecordingEntry> propEntries;
    for (const auto& [prop, script] : _savePropertiesBaseline) {
        propEntries.emplace_back(0.0, 0.0, script);
    }
    _timeline.insert(_timeline.begin(), propEntries.begin(), propEntries.end());

    saveSessionRecording(absFilename, _timeline, dataMode);
    _state = SessionState::Idle;
    LINFO("Session recording stopped");
}

void SessionRecordingHandler::startPlayback(std::string& filename, bool loop,
                                            bool shouldWaitForFinishedTiles,
                                            std::optional<int> saveScreenshotFps)
{
    const bool canTriggerPlayback = global::openSpaceEngine->setMode(
        OpenSpaceEngine::Mode::SessionRecordingPlayback
    );
    if (!canTriggerPlayback) {
        return;
    }

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

    std::filesystem::path absFilename;
    if (std::filesystem::is_regular_file(filename)) {
        absFilename = filename;
    }
    else {
        absFilename = absPath("${RECORDINGS}/" + filename);
    }

    if (!std::filesystem::is_regular_file(absFilename)) {
        cleanUpTimelinesAndKeyframes();
        throw ghoul::RuntimeError("Cannot find the specified playback file");
    }

    _playback.isLooping = loop;
    _playback.waitForLoading = shouldWaitForFinishedTiles;

    try {
        _timeline = loadSessionRecording(absFilename);
    }
    catch (...) {
        cleanUpTimelinesAndKeyframes();
        throw;
    }

    double now = global::windowDelegate->applicationTime();

    // Populate liste of loaded scene graph nodes
    _loadedNodes.clear();
    const std::vector<SceneGraphNode*> nodes =
        global::renderEngine->scene()->allSceneGraphNodes();
    for (SceneGraphNode* n : nodes) {
        _loadedNodes.push_back(n->identifier());
    }

    if (_timeline.empty()) {
        LERROR(std::format("Session recording '{}' is empty", absFilename));
        cleanUpTimelinesAndKeyframes();
        return;
    }

    // Make sure that there is at least one camera keyframe
    bool foundCameraKeyframe = false;
    for (const SessionRecordingEntry& e : _timeline) {
        if (std::holds_alternative<SessionRecordingEntry::Camera>(e.value)) {
            foundCameraKeyframe = true;
            break;
        }
    }
    if (!foundCameraKeyframe) {
        LERROR(std::format(
            "Session recording '{}' did not contain any camera keyframes",
            absFilename
        ));
        cleanUpTimelinesAndKeyframes();
        return;
    }

    setupPlayback(now);
    _playback.saveScreenshots.enabled = saveScreenshotFps.has_value();
    if (saveScreenshotFps.has_value()) {
        _playback.saveScreenshots.deltaTime = 1.0 / *saveScreenshotFps;
    }


    LINFO(std::format("Playback session started with {} entries", _timeline.size()));

    global::eventEngine->publishEvent<events::EventSessionRecordingPlayback>(
        events::EventSessionRecordingPlayback::State::Started
    );
    global::navigationHandler->orbitalNavigator().updateOnCameraInteraction();
}

void SessionRecordingHandler::setupPlayback(double startTime) {
    _playback.elapsedTime = 0.0;
    _playback.saveScreenshots.currentRecordedTime = std::chrono::steady_clock::now();
    _playback.saveScreenshots.currentApplicationTime =
        global::windowDelegate->applicationTime();
    global::navigationHandler->keyframeNavigator().setTimeReferenceMode(
        KeyframeTimeRef::Relative_recordedStart, startTime);


    std::vector<SessionRecordingEntry>::const_iterator firstCamera = _timeline.begin();
    while (firstCamera != _timeline.end() && !std::holds_alternative<SessionRecordingEntry::Camera>(firstCamera->value)) {
        firstCamera++;
    }

    std::string startFocusNode = std::get<SessionRecordingEntry::Camera>(firstCamera->value).focusNode;
    auto it = std::find(_loadedNodes.begin(), _loadedNodes.end(), startFocusNode);
    if (it == _loadedNodes.end()) {
        throw ghoul::RuntimeError(std::format(
            "Playback file requires scenegraph node '{}', which is "
            "not currently loaded", startFocusNode
        ));
    }

    global::timeManager->setTimeNextFrame(Time(firstCamera->simulationTime));

    _currentEntry = _timeline.begin();

    _state = SessionState::Playback;
}

bool SessionRecordingHandler::isPlaybackPaused() const {
    return (_state == SessionState::PlaybackPaused);
}

void SessionRecordingHandler::setPlaybackPause(bool pause) {
    if (pause && _state == SessionState::Playback) {
        _playback.playbackPausedWithDeltaTimePause = global::timeManager->isPaused();
        if (!_playback.playbackPausedWithDeltaTimePause) {
            global::timeManager->setPause(true);
        }
        _state = SessionState::PlaybackPaused;
        global::eventEngine->publishEvent<events::EventSessionRecordingPlayback>(
            events::EventSessionRecordingPlayback::State::Paused
        );
    }
    else if (!pause && _state == SessionState::PlaybackPaused) {
        if (!_playback.playbackPausedWithDeltaTimePause) {
            global::timeManager->setPause(false);
        }
        _state = SessionState::Playback;
        global::eventEngine->publishEvent<events::EventSessionRecordingPlayback>(
            events::EventSessionRecordingPlayback::State::Resumed
        );
    }
}

void SessionRecordingHandler::stopPlayback() {
    if (isPlayingBack()) {
        LINFO("Session playback finished");
        _state = SessionState::Idle;
        cleanUpTimelinesAndKeyframes();
        global::eventEngine->publishEvent<events::EventSessionRecordingPlayback>(
            events::EventSessionRecordingPlayback::State::Finished
        );
        global::openSpaceEngine->resetMode();
        global::navigationHandler->resetNavigationUpdateVariables();
    }
}

void SessionRecordingHandler::cleanUpTimelinesAndKeyframes() {
    _timeline.clear();
    _savePropertiesBaseline.clear();
    _loadedNodes.clear();
    _currentEntry = _timeline.end();
    _playback.saveScreenshots.enabled = false;
    _playback.isLooping = false;
}

void SessionRecordingHandler::saveScriptKeyframeToTimeline(std::string script) {
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

    _timeline.emplace_back(
        _recording.elapsedTime,
        global::timeManager->time().j2000Seconds(),
        std::move(script)
    );
}

void SessionRecordingHandler::savePropertyBaseline(properties::Property& prop) {
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

void SessionRecordingHandler::preSynchronization(double dt) {
    ZoneScoped;

    if (_state == SessionState::Recording) {
        _recording.elapsedTime += dt;

        using namespace datamessagestructures;
        CameraKeyframe kf = generateCameraKeyframe();
        _timeline.emplace_back(
            _recording.elapsedTime,
            global::timeManager->time().j2000Seconds(),
            interaction::KeyframeNavigator::CameraPose(std::move(kf))
        );
    }
    else if (isPlayingBack()) {
        moveAheadInTime(dt);
    }

    // Handle callback(s) for change in idle/record/playback state
    if (_state != _lastState) {
        using K = CallbackHandle;
        using V = StateChangeCallback;
        for (const std::pair<const K, V>& it : _stateChangeCallbacks) {
            it.second();
        }
        _lastState = _state;
    }
}

void SessionRecordingHandler::render() const {
    ZoneScoped;

    if (!_renderPlaybackInformation || !isPlayingBack()) {
        return;
    }

    constexpr std::string_view FontName = "Mono";
    constexpr float FontSizeFrameinfo = 32.f;
    const std::shared_ptr<ghoul::fontrendering::Font> font =
        global::fontManager->font(FontName, FontSizeFrameinfo);

    const glm::vec2 res = global::renderEngine->fontResolution();
    glm::vec2 penPosition = glm::vec2(res.x / 2 - 150.f, res.y / 2);

    const std::string text = std::format(
        "Elapsed:  {:.3f} / {}\n"
        "Keyframe: {} / {}\n"
        "Is Looping: {}\n"
        "Saving frames: {}\n"
        "Wait for Loading: {}\n"
        "Scale: {}",
        _playback.elapsedTime, _timeline.back().timestamp,
        std::distance(_timeline.begin(), _currentEntry), _timeline.size(),
        _playback.isLooping ? "true" : "false",
        _playback.saveScreenshots.enabled ? "true" : "false",
        _playback.waitForLoading ? "true" : "false",
        global::navigationHandler->camera()->scaling()
    );
    ghoul::fontrendering::RenderFont(
        *font,
        penPosition,
        text,
        glm::vec4(1.f),
        ghoul::fontrendering::CrDirection::Down
    );
}

bool SessionRecordingHandler::isRecording() const {
    return _state == SessionState::Recording;
}

bool SessionRecordingHandler::isPlayingBack() const {
    return _state == SessionState::Playback || _state == SessionState::PlaybackPaused;
}

bool SessionRecordingHandler::isSavingFramesDuringPlayback() const {
    return isPlayingBack() && _playback.saveScreenshots.enabled;
}

bool SessionRecordingHandler::shouldWaitForTileLoading() const {
    return _playback.waitForLoading;
}

SessionRecordingHandler::SessionState SessionRecordingHandler::state() const {
    return _state;
}

double SessionRecordingHandler::fixedDeltaTimeDuringFrameOutput() const {
    // Check if renderable in focus is still resolving tile loading
    // do not adjust time while we are doing this
    const SceneGraphNode* an = global::navigationHandler->orbitalNavigator().anchorNode();
    const Renderable* focusRenderable = an->renderable();
    if (!focusRenderable || focusRenderable->renderedWithDesiredData()) {
        return _playback.saveScreenshots.deltaTime;
    }
    else {
        return 0.0;
    }
}

std::chrono::steady_clock::time_point
SessionRecordingHandler::currentPlaybackInterpolationTime() const {
    return _playback.saveScreenshots.currentRecordedTime;
}

double SessionRecordingHandler::currentApplicationInterpolationTime() const {
    return _playback.saveScreenshots.currentApplicationTime;
}

void SessionRecordingHandler::checkIfScriptUsesScenegraphNode(std::string s) const {
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

    auto extractScenegraphNodeFromScene = [](const std::string& s) -> std::string {
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

void SessionRecordingHandler::moveAheadInTime(double dt) {
    if (_state == SessionState::PlaybackPaused) {
        return;
    }

    const double previousTime = _playback.elapsedTime;
    _playback.elapsedTime += dt;
    
    // Find the first value whose recording time is past now
    std::vector<SessionRecordingEntry>::const_iterator probe = _currentEntry;
    while (probe != _timeline.end() && _playback.elapsedTime > probe->timestamp) {
        probe++;
    }

    // All script entries between _previous and now have to be applied
    for (auto& it = _currentEntry; it != probe; it++) {
        if (std::holds_alternative<SessionRecordingEntry::Script>(it->value)) {
            global::scriptEngine->queueScript(
                std::get<SessionRecordingEntry::Script>(it->value),
                scripting::ScriptEngine::ShouldBeSynchronized::Yes,
                scripting::ScriptEngine::ShouldSendToRemote::Yes
            );
        }
    }

    //  ... < _previous < ... < prevCamera <= now <= nextCamera < ...
    std::vector<SessionRecordingEntry>::const_iterator prevCamera = probe - 1;
    while (prevCamera != _timeline.begin() && !std::holds_alternative<SessionRecordingEntry::Camera>(prevCamera->value)) {
        prevCamera--;
    }
    std::vector<SessionRecordingEntry>::const_iterator nextCamera = probe;
    while (nextCamera != _timeline.end() && !std::holds_alternative<SessionRecordingEntry::Camera>(nextCamera->value)) {
        nextCamera++;
    }

    // update camera with or without new keyframes
    if (prevCamera != nextCamera && prevCamera != _timeline.begin() && nextCamera != _timeline.end()) {
        const SessionRecordingEntry::Camera prevPose = std::get<SessionRecordingEntry::Camera>(prevCamera->value);
        const double prevTime = prevCamera->timestamp;

        const SessionRecordingEntry::Camera nextPose = std::get<SessionRecordingEntry::Camera>(nextCamera->value);
        const double nextTime = nextCamera->timestamp;

        const double t = std::clamp(
            (_playback.elapsedTime - prevTime) / (nextTime - prevTime),
            0.0,
            1.0
        );

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

    if (isSavingFramesDuringPlayback()) {
        ghoul_assert(dt == _playback.saveScreenshots.deltaTime, "Misaligned delta times");

        // Check if renderable in focus is still resolving tile loading
        // do not adjust time while we are doing this, or take screenshot
        const SceneGraphNode* focusNode =
            global::navigationHandler->orbitalNavigator().anchorNode();
        const Renderable* focusRenderable = focusNode->renderable();
        if (!_playback.waitForLoading ||
            !focusRenderable || focusRenderable->renderedWithDesiredData())
        {
            _playback.saveScreenshots.currentRecordedTime +=
                std::chrono::microseconds(static_cast<long>(dt * 1000000));
            _playback.saveScreenshots.currentApplicationTime += dt;

            // Unfortunately the first frame is sometimes rendered because globebrowsing
            // reports that all chunks are rendered when they apparently are not.
            // previousTime == 0.0 -> rendering the first frame
            if (previousTime != 0.0) {
                global::renderEngine->takeScreenshot();
            }
        }
    }


    _currentEntry = probe;
    if (probe == _timeline.end()) {
        if (_playback.isLooping) {
            _playback.saveScreenshots.enabled = false;
            setupPlayback(global::windowDelegate->applicationTime());
        }
        else {
            stopPlayback();
        }
    }
}

SessionRecordingHandler::CallbackHandle SessionRecordingHandler::addStateChangeCallback(
                                                                   StateChangeCallback cb)
{
    const CallbackHandle handle = _nextCallbackHandle++;
    _stateChangeCallbacks.emplace_back(handle, std::move(cb));
    return handle;
}

void SessionRecordingHandler::removeStateChangeCallback(CallbackHandle handle) {
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

std::vector<std::string> SessionRecordingHandler::playbackList() const {
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

scripting::LuaLibrary SessionRecordingHandler::luaLibrary() {
    return {
        "sessionRecording",
        {
            codegen::lua::StartRecording,
            codegen::lua::StopRecording,
            codegen::lua::StartPlayback,
            codegen::lua::StopPlayback,
            codegen::lua::SetPlaybackPause,
            codegen::lua::TogglePlaybackPause,
            codegen::lua::IsPlayingBack,
            codegen::lua::IsRecording
        }
    };
}

} // namespace openspace
