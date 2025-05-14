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

#include <openspace/interaction/sessionrecordinghandler.h>

#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/events/eventengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/network/messagestructureshelper.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

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

    constexpr std::string_view ScriptReturnPrefix = "return ";
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

void SessionRecordingHandler::preSynchronization(double dt) {
    ZoneScoped;

    if (_state == SessionState::Recording) {
        tickRecording(dt);
    }
    else if (isPlayingBack()) {
        tickPlayback(dt);
    }

    // Handle callback(s) for change in idle/record/playback state
    if (_state != _lastState) {
        using K = CallbackHandle;
        using V = StateChangeCallback;
        for (const std::pair<K, V>& it : _stateChangeCallbacks) {
            it.second();
        }
        _lastState = _state;
    }
}

void SessionRecordingHandler::tickPlayback(double dt) {
    if (_state == SessionState::PlaybackPaused) {
        return;
    }

    const double previousTime = _playback.elapsedTime;
    _playback.elapsedTime += dt;

    // Find the first value whose recording time is past now
    std::vector<SessionRecording::Entry>::const_iterator probe = _currentEntry;
    while (probe != _timeline.entries.end() && _playback.elapsedTime > probe->timestamp) {
        probe++;
    }

    // All script entries between _previous and now have to be applied
    for (auto& it = _currentEntry; it != probe; it++) {
        if (std::holds_alternative<SessionRecording::Entry::Script>(it->value)) {
            global::scriptEngine->queueScript(
                std::get<SessionRecording::Entry::Script>(it->value)
            );
        }
    }

    //  ... < _previous < ... < prevCamera <= now <= nextCamera < ...
    std::vector<SessionRecording::Entry>::const_iterator prevCamera = probe - 1;
    while (prevCamera != _timeline.entries.begin() &&
           !std::holds_alternative<SessionRecording::Entry::Camera>(prevCamera->value))
    {
        prevCamera--;
    }
    // We need to check this here as there might be a chance that the first camera entry
    // is after the current elapsed time, which would result in `prevCamera` being equal
    // to `begin()` but not being a camera keyframe
    const bool hasValidPrevCamera =
        std::holds_alternative<SessionRecording::Entry::Camera>(prevCamera->value);

    std::vector<SessionRecording::Entry>::const_iterator nextCamera = probe;
    while (nextCamera != _timeline.entries.end() &&
           !std::holds_alternative<SessionRecording::Entry::Camera>(nextCamera->value))
    {
        nextCamera++;
    }
    // Same argument but in reverse from the `hasValidPrevCamera` comment
    const bool hasValidNextCamera =
        (nextCamera != _timeline.entries.end()) &&
        std::holds_alternative<SessionRecording::Entry::Camera>(nextCamera->value);


    if (!hasValidPrevCamera && !hasValidNextCamera) {
        throw ghoul::RuntimeError("No valid camera keyframes found in recording");
    }

    // update camera with or without new keyframes
    const auto& prevPose =
        hasValidPrevCamera ?
        std::get<SessionRecording::Entry::Camera>(prevCamera->value) :
        std::get<SessionRecording::Entry::Camera>(nextCamera->value);
    const double prevTime =
        hasValidPrevCamera ?
        prevCamera->timestamp :
        0.0;

    const auto& nextPose =
        hasValidNextCamera ?
        std::get<SessionRecording::Entry::Camera>(nextCamera->value) :
        std::get<SessionRecording::Entry::Camera>(prevCamera->value);
    const double nextTime =
        hasValidNextCamera ?
        nextCamera->timestamp :
        _timeline.entries.back().timestamp;

    // Need to actively update the focusNode position of the camera in relation to
    // the rendered objects will be unstable and actually incorrect
    const SceneGraphNode* n = sceneGraphNode(prevPose.focusNode);
    if (n) {
        global::navigationHandler->orbitalNavigator().setFocusNode(n->identifier());
    }

    const double t = std::clamp(
        (_playback.elapsedTime - prevTime) / (nextTime - prevTime),
        0.0,
        1.0
    );

    interaction::KeyframeNavigator::updateCamera(
        global::navigationHandler->camera(),
        prevPose,
        nextPose,
        t,
        _ignoreRecordedScale
    );

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
    if (probe == _timeline.entries.end()) {
        if (_playback.isLooping) {
            _playback.saveScreenshots.enabled = false;
            setupPlayback(global::windowDelegate->applicationTime());
        }
        else {
            stopPlayback();
        }
    }
}

void SessionRecordingHandler::tickRecording(double dt) {
    _recording.elapsedTime += dt;

    using namespace datamessagestructures;
    CameraKeyframe kf = generateCameraKeyframe();
    _timeline.entries.emplace_back(
        _recording.elapsedTime,
        global::timeManager->time().j2000Seconds(),
        KeyframeNavigator::CameraPose(std::move(kf))
    );
}

void SessionRecordingHandler::render() const {
    if (!_renderPlaybackInformation || !isPlayingBack()) {
        return;
    }

    constexpr std::string_view FontName = "Mono";
    constexpr float FontSizeFrameinfo = 32.f;
    const std::shared_ptr<ghoul::fontrendering::Font> font =
        global::fontManager->font(FontName, FontSizeFrameinfo);

    glm::vec2 penPosition = global::renderEngine->fontResolution() - glm::ivec2(150, 0);
    const std::string text = std::format(
        "Elapsed:  {:.3f} / {}\n"
        "Keyframe: {} / {}\n"
        "Is Looping: {}\n"
        "Saving frames: {}\n"
        "Wait for Loading: {}\n"
        "Scale: {}",
        _playback.elapsedTime, _timeline.entries.back().timestamp,
        std::distance(_timeline.entries.begin(), _currentEntry), _timeline.entries.size(),
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

void SessionRecordingHandler::startRecording() {
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

    LINFO("Session recording started");

    _state = SessionState::Recording;
    _timeline = SessionRecording();
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
}

void SessionRecordingHandler::stopRecording(const std::filesystem::path& filename,
                                            DataMode dataMode, bool overwrite)
{
    if (_state != SessionState::Recording) {
        return;
    }

    if (!overwrite && std::filesystem::is_regular_file(filename)) {
        throw ghoul::RuntimeError(std::format(
            "Unable to start recording. File '{}' already exists", filename
        ), "SessionRecording");
    }

    for (const auto& [prop, script] : _savePropertiesBaseline) {
        _timeline.entries.insert(_timeline.entries.begin(), { 0.0, 0.0, script });
    }

    saveSessionRecording(filename, _timeline, dataMode);
    _state = SessionState::Idle;
    cleanUpTimelinesAndKeyframes();
    LINFO("Session recording stopped");
}

void SessionRecordingHandler::startPlayback(SessionRecording timeline, bool loop,
                                            bool shouldWaitForFinishedTiles,
                                            std::optional<int> saveScreenshotFps)
{
    OpenSpaceEngine::Mode prevMode = global::openSpaceEngine->currentMode();
    const bool canTriggerPlayback = global::openSpaceEngine->setMode(
        OpenSpaceEngine::Mode::SessionRecordingPlayback
    );
    if (!canTriggerPlayback) {
        return;
    }

    if (_state == SessionState::Recording) {
        global::openSpaceEngine->setMode(prevMode);
        throw ghoul::RuntimeError(
            "Unable to start playback while in session recording mode"
        );
    }
    else if (isPlayingBack()) {
        global::openSpaceEngine->setMode(prevMode);
        throw ghoul::RuntimeError(
            "Unable to start new playback while in session playback mode"
        );
    }

    _playback.isLooping = loop;
    _playback.waitForLoading = shouldWaitForFinishedTiles;

    timeline.forAll<SessionRecording::Entry::Script>(
        [this](const SessionRecording::Entry::Script& script) {
            checkIfScriptUsesScenegraphNode(script);
            return false;
        }
    );

    if (timeline.entries.empty()) {
        global::openSpaceEngine->setMode(prevMode);
        throw ghoul::RuntimeError("Session recording is empty");
    }
    if (!timeline.hasCameraFrame()) {
        global::openSpaceEngine->setMode(prevMode);
        throw ghoul::RuntimeError("Session recording did not contain camera keyframes");
    }

    _timeline = std::move(timeline);

    // Populate list of loaded scene graph nodes
    _loadedNodes.clear();
    const std::vector<SceneGraphNode*> nodes = sceneGraph()->allSceneGraphNodes();
    for (SceneGraphNode* n : nodes) {
        _loadedNodes.push_back(n->identifier());
    }

    double now = global::windowDelegate->applicationTime();
    setupPlayback(now);
    _playback.saveScreenshots.enabled = saveScreenshotFps.has_value();
    if (saveScreenshotFps.has_value()) {
        _playback.saveScreenshots.deltaTime = 1.0 / *saveScreenshotFps;
    }

    global::navigationHandler->orbitalNavigator().updateOnCameraInteraction();

    LINFO("Playback session started");
    global::eventEngine->publishEvent<events::EventSessionRecordingPlayback>(
        events::EventSessionRecordingPlayback::State::Started
    );
}

void SessionRecordingHandler::setupPlayback(double startTime) {
    _playback.elapsedTime = 0.0;
    _playback.saveScreenshots.currentRecordedTime = std::chrono::steady_clock::now();
    _playback.saveScreenshots.currentApplicationTime =
        global::windowDelegate->applicationTime();
    global::navigationHandler->keyframeNavigator().setTimeReferenceMode(
        KeyframeTimeRef::Relative_recordedStart, startTime);


    auto firstCamera = _timeline.entries.begin();
    while (firstCamera != _timeline.entries.end() &&
           !std::holds_alternative<SessionRecording::Entry::Camera>(firstCamera->value))
    {
        firstCamera++;
    }

    std::string startFocusNode =
        std::get<SessionRecording::Entry::Camera>(firstCamera->value).focusNode;
    auto it = std::find(_loadedNodes.begin(), _loadedNodes.end(), startFocusNode);
    if (it == _loadedNodes.end()) {
        throw ghoul::RuntimeError(std::format(
            "Playback file requires scenegraph node '{}', which is "
            "not currently loaded", startFocusNode
        ));
    }

    global::timeManager->setTimeNextFrame(Time(firstCamera->simulationTime));
    _currentEntry = _timeline.entries.begin();
    _state = SessionState::Playback;
}

void SessionRecordingHandler::seek(double recordingTime) {
    _currentEntry = std::upper_bound(
        _timeline.entries.begin(),
        _timeline.entries.end(),
        recordingTime,
        [](double rt, const SessionRecording::Entry& e) {
            return rt < e.timestamp;
        }
    );
    _playback.elapsedTime = recordingTime;
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
    if (!isPlayingBack()) {
        return;
    }

    LINFO("Session playback finished");
    _state = SessionState::Idle;
    cleanUpTimelinesAndKeyframes();
    global::eventEngine->publishEvent<events::EventSessionRecordingPlayback>(
        events::EventSessionRecordingPlayback::State::Finished
    );
    global::openSpaceEngine->resetMode();
    global::navigationHandler->resetNavigationUpdateVariables();
}

void SessionRecordingHandler::cleanUpTimelinesAndKeyframes() {
    _timeline = SessionRecording();
    _savePropertiesBaseline.clear();
    _loadedNodes.clear();
    _currentEntry = _timeline.entries.end();
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

    if (script.starts_with(ScriptReturnPrefix)) {
        script = script.substr(ScriptReturnPrefix.length());
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

    _timeline.entries.emplace_back(
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

    const std::string propIdentifier = std::string(prop.uri());
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

void SessionRecordingHandler::checkIfScriptUsesScenegraphNode(
                                                            std::string_view script) const
{
    auto isolateTermFromQuotes = [](std::string_view s) -> std::string_view {
        // Remove any leading spaces
        s.remove_prefix(s.find_first_not_of(" "));

        // Find the first substring that is surrounded by possible quotes
        constexpr std::string_view PossibleQuotes = "\'\"[]";
        s.remove_prefix(s.find_first_not_of(PossibleQuotes));
        size_t end = s.find_first_of(PossibleQuotes);
        if (end != std::string::npos) {
            return s.substr(0, end);
        }
        else {
            // There were no closing quotes so we remove as much as possible
            constexpr std::string_view UnwantedChars = " );";
            s.remove_suffix(s.find_last_not_of(UnwantedChars));
            return s;
        }
    };

    auto checkForScenegraphNodeAccessNav = [](std::string_view navTerm) -> bool {
        constexpr std::array<std::string_view, 3> NavScriptsUsingNodes = {
            "NavigationHandler.OrbitalNavigator.RetargetAnchor",
            "NavigationHandler.OrbitalNavigator.Anchor",
            "NavigationHandler.OrbitalNavigator.Aim"
        };

        for (std::string_view s : NavScriptsUsingNodes) {
            if (navTerm.find(s) != std::string::npos) {
                return true;
            }
        }
        return false;
    };

    if (script.starts_with(ScriptReturnPrefix)) {
        script.remove_prefix(ScriptReturnPrefix.length());
    }
    // This works for both setPropertyValue and setPropertyValueSingle
    if (!script.starts_with("openspace.setPropertyValue") ||
        script.find('(') == std::string::npos)
    {
        return;
    }

    std::string_view subjectOfSetProp = isolateTermFromQuotes(
        script.substr(script.find('(') + 1)
    );
    if (checkForScenegraphNodeAccessNav(subjectOfSetProp)) {
        std::string_view navNode = isolateTermFromQuotes(
            script.substr(script.find(',') + 1)
        );
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
        auto extractScenegraphNodeFromScene = [](std::string_view s) -> std::string_view {
            constexpr std::string_view Scene = "Scene.";
            size_t scene = s.find(Scene);
            if (scene == std::string_view::npos) {
                return "";
            }
            s.remove_prefix(scene + Scene.length());
            size_t end = s.find('.');
            return end != std::string_view::npos ? s.substr(0, end) : "";
        };

        std::string_view found = extractScenegraphNodeFromScene(subjectOfSetProp);
        if (!found.empty()) {
            const std::vector<properties::Property*> matchHits =
                sceneGraph()->propertiesMatchingRegex(subjectOfSetProp);
            if (matchHits.empty()) {
                LWARNING(std::format(
                    "Playback file contains a property setting of scenegraph "
                    "node '{}', which is not currently loaded", found
                ));
            }
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

    ghoul_assert(it != _stateChangeCallbacks.end(), "handle must be a valid callback");
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
