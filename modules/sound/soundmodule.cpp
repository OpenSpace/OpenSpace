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

#include <modules/sound/soundmodule.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <ghoul/logging/logmanager.h>

#include "soundmodule_lua.inl"

#include <soloud.h>
#include <soloud_wavstream.h>

namespace {
    constexpr std::string_view _loggerCat = "SoundModule";

    struct [[codegen::Dictionary(SoundModule)]] Parameters {
        // Sets the maximum number of simultaneous channels that can be played back by the
        // audio subsystem. If this value is not specified, it defaults to 128.
        std::optional<int> maxNumberOfChannels [[codegen::greater(0)]];
    };

#include "soundmodule_codegen.cpp"
} // namespace

namespace openspace {

SoundModule::SoundModule()
    : OpenSpaceModule(Name)
    , _engine(std::make_unique<SoLoud::Soloud>())
{}

SoundModule::~SoundModule() {}

void SoundModule::internalInitialize(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    LDEBUG(fmt::format("Initializing SoLoud version: {}", SOLOUD_VERSION));

    _engine->init();
    _engine->setGlobalVolume(1.f);
    const int nChannels = p.maxNumberOfChannels.value_or(16);
    _engine->setMaxActiveVoiceCount(static_cast<unsigned int>(nChannels));

    global::callback::postDraw->emplace_back([this]() {
        if (!_sounds.empty()) {
            _engine->update3dAudio();
        }
    });

    LDEBUG(fmt::format("Audio backend: {}", _engine->getBackendString()));
    LDEBUG(fmt::format("Number of channels: {}", _engine->getBackendChannels()));
}

void SoundModule::internalDeinitializeGL() {
    _sounds.clear();
    _engine->deinit();
    _engine = nullptr;
}

std::unique_ptr<SoLoud::WavStream> SoundModule::loadSound(
                                                        const std::filesystem::path& path)
{
    std::unique_ptr<SoLoud::WavStream> sound = std::make_unique<SoLoud::WavStream>();
    const std::string p = path.string();
    SoLoud::result res = sound->load(p.c_str());
    if (res != 0) {
        throw ghoul::RuntimeError(fmt::format(
            "Error loading sound from {}. {}: {}",
            path, static_cast<int>(res), _engine->getErrorString(res)
        ));
    }

    return sound;
}

std::map<int, std::unique_ptr<SoLoud::WavStream>>::const_iterator SoundModule::findSound(
                                                                              int h) const
{
    auto it = _sounds.find(h);
    if (it == _sounds.end()) {
        throw ghoul::RuntimeError(fmt::format(
            "Handle {} is not a valid sound handle", h
        ));
    }
    return it;
}

int SoundModule::playAudio(const std::filesystem::path& path, ShouldLoop loop) {
    std::unique_ptr<SoLoud::WavStream> sound = loadSound(path);
    sound->setLooping(loop);
    SoLoud::handle handle = _engine->playBackground(*sound);

    ghoul_assert(_sounds.find(handle) == _sounds.end(), "Handle already used");
    _sounds[handle] = std::move(sound);
    return handle;
}

void SoundModule::stopAudio(int handle) {
    auto it = findSound(handle);
    _sounds.erase(it);
    _engine->stop(handle);
}

bool SoundModule::isPlaying(int handle) const {
    findSound(handle);
    return _engine->isValidVoiceHandle(handle);
}

void SoundModule::pauseAudio(int handle) const {
    findSound(handle);
    _engine->setPause(handle, true);
}

void SoundModule::resumeAudio(int handle) const {
    findSound(handle);
    _engine->setPause(handle, false);
}

bool SoundModule::isPaused(int handle) const {
    findSound(handle);
    const bool isPaused = _engine->getPause(handle);
    return isPaused;
}

void SoundModule::setLooping(int handle, ShouldLoop loop) const {
    findSound(handle);
    _engine->setLooping(handle, loop);
}

SoundModule::ShouldLoop SoundModule::isLooping(int handle) const {
    findSound(handle);
    return _engine->getLooping(handle) ? ShouldLoop::Yes : ShouldLoop::No;
}

void SoundModule::stopAll() {
    _engine->stopAll();
    _sounds.clear();
}

std::vector<int> SoundModule::currentlyPlaying() const {
    std::vector<int> res;
    res.reserve(_sounds.size());
    for (const auto& [key, value] : _sounds) {
        res.push_back(key);
    }
    return res;
}

void SoundModule::setVolume(int handle, float volume, float interpolation) const {
    findSound(handle);
    if (interpolation == 0.f) {
        _engine->setVolume(handle, volume);
    }
    else {
        _engine->fadeVolume(handle, volume, 0.5f);
    }
}

float SoundModule::volume(int handle) const {
    findSound(handle);
    const float volume = _engine->getVolume(handle);
    return volume;
}

int SoundModule::playAudio3d(const std::filesystem::path& path, const glm::vec3& position,
                             ShouldLoop loop)
{
    std::unique_ptr<SoLoud::WavStream> sound = loadSound(path);
    sound->setLooping(loop);
    SoLoud::handle handle = _engine->play3d(*sound, position.x, position.y, position.z);

    ghoul_assert(_sounds.find(handle) == _sounds.end(), "Handle already used");
    _sounds[handle] = std::move(sound);
    return handle;
}

void SoundModule::set3dListenerParameters(const std::optional<glm::vec3>& position,
                                          const std::optional<glm::vec3>& lookAt,
                                          const std::optional<glm::vec3>& up) const
{
    if (position.has_value()) {
        _engine->set3dListenerPosition(position->x, position->y, position->z);
    }
    if (lookAt.has_value()) {
        _engine->set3dListenerAt(lookAt->x, lookAt->y, lookAt->z);
    }
    if (up.has_value()) {
        _engine->set3dListenerUp(up->x, up->y, up->z);
    }
}

void SoundModule::set3dSourcePosition(int handle, const glm::vec3& position) const {
    findSound(handle);
    _engine->set3dSourcePosition(
        handle,
        position.x, position.y, position.z
    );
}

void SoundModule::setSpeakerPosition(int channel, const glm::vec3& position) const {
    _engine->setSpeakerPosition(channel, position.x, position.y, position.z);
}

glm::vec3 SoundModule::speakerPosition(int channel) const {
    float x = 0.f;
    float y = 0.f;
    float z = 0.f;
    _engine->getSpeakerPosition(channel, x, y, z);
    return glm::vec3(x, y, z);
}

std::vector<documentation::Documentation> SoundModule::documentations() const {
    return {
    };
}

scripting::LuaLibrary SoundModule::luaLibrary() const {
    return {
        "sound",
        {
            codegen::lua::PlayAudio,
            codegen::lua::StopAudio,
            codegen::lua::IsPlaying,
            codegen::lua::CurrentlyPlaying,
            codegen::lua::SetVolume,
            codegen::lua::Volume,
            codegen::lua::PauseAudio,
            codegen::lua::ResumeAudio,
            codegen::lua::IsPaused,
            codegen::lua::SetLooping,
            codegen::lua::IsLooping,
            codegen::lua::PlayAudio3d,
            codegen::lua::Set3dListenerPosition,
            codegen::lua::Set3dSourcePosition,
            codegen::lua::SetSpeakerPosition,
            codegen::lua::SpeakerPosition
        }
    };
}

} // namespace openspace
