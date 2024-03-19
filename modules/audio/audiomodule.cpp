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

#include <modules/audio/audiomodule.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <ghoul/logging/logmanager.h>

#include "audiomodule_lua.inl"

#include <soloud.h>
#include <soloud_wav.h>

namespace {
    constexpr std::string_view _loggerCat = "AudioModule";

    struct [[codegen::Dictionary(AudioModule)]] Parameters {
        // Sets the maximum number of simultaneous channels that can be played back by the
        // audio subsystem. If this value is not specified, it defaults to 128.
        std::optional<int> maxNumberOfChannels [[codegen::greater(0)]];
    };

#include "audiomodule_codegen.cpp"
} // namespace

namespace openspace {

AudioModule::AudioModule()
    : OpenSpaceModule(Name)
    , _engine(std::make_unique<SoLoud::Soloud>())
{}

AudioModule::~AudioModule() {}

void AudioModule::internalInitialize(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    LDEBUG(fmt::format("Initializing SoLoud version: {}", SOLOUD_VERSION));

    _engine->init();
    _engine->setGlobalVolume(0.5f);
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

void AudioModule::internalDeinitializeGL() {
    ghoul_assert(_engine, "No audio engine loaded");

    _sounds.clear();
    _engine->deinit();
    _engine = nullptr;
}

std::unique_ptr<SoLoud::Wav> AudioModule::loadSound(const std::filesystem::path& path) {
    ghoul_assert(_engine, "No audio engine loaded");

    std::unique_ptr<SoLoud::Wav> audio = std::make_unique<SoLoud::Wav>();
    const std::string p = path.string();
    SoLoud::result res = sound->load(p.c_str());
    if (res != 0) {
        throw ghoul::RuntimeError(fmt::format(
            "Error loading sound from {}. {}: {}",
            path, static_cast<int>(res), _engine->getErrorString(res)
        ));
    }

    // While we are loading a sound, we also want to do a little garbage collection on our
    // internal data structure to remove the songs that someone has loaded at some point
    // and that have since organically stopped. In general, this should only happen if the
    // song was started without looping and has ended
    for (auto it = _sounds.begin(); it != _sounds.end();) {
        if (!isPlaying(it->first)) {
            // We have found one of the candidates
            LDEBUG(fmt::format("Removing song {} as it has ended", it->first));
            _sounds.erase(it);
            // It is easier to just reset the iterator to the beginning than deal with the
            // off-by-one error when deleting the last element in the list and the
            // subsequent crash
            it = _sounds.begin();
        }
        else {
            it++;
        }
    }

    return sound;
}

int AudioModule::playAudio(const std::filesystem::path& path, ShouldLoop loop,
                           std::string name)
{
    ghoul_assert(_engine, "No audio engine loaded");

    std::unique_ptr<SoLoud::Wav> sound = loadSound(path);
    //sound->setAutoStop(false);
    sound->setLooping(loop);
    SoLoud::handle handle = _engine->playBackground(*sound);

    ghoul_assert(_sounds.find(handle) == _sounds.end(), "Handle already used");
    _sounds[handle] = {
        .sound = std::move(sound),
        .name = std::move(name)
    };
    return handle;
}

void AudioModule::stopAudio(int handle) {
    ghoul_assert(_engine, "No audio engine loaded");

    auto it = _sounds.find(handle);
    if (it != _sounds.end()) {
        _sounds.erase(it);
        _engine->stop(handle);
    }
}

bool AudioModule::isPlaying(int handle) const {
    ghoul_assert(_engine, "No audio engine loaded");
    return _engine->isValidVoiceHandle(handle);
}

void AudioModule::pauseAudio(int handle) const {
    ghoul_assert(_engine, "No audio engine loaded");
    _engine->setPause(handle, true);
}

void AudioModule::resumeAudio(int handle) const {
    ghoul_assert(_engine, "No audio engine loaded");
    _engine->setPause(handle, false);
}

bool AudioModule::isPaused(int handle) const {
    ghoul_assert(_engine, "No audio engine loaded");

    const bool isPaused = _engine->getPause(handle);
    return isPaused;
}

void AudioModule::setLooping(int handle, ShouldLoop loop) const {
    ghoul_assert(_engine, "No audio engine loaded");
    _engine->setLooping(handle, loop);
}

AudioModule::ShouldLoop AudioModule::isLooping(int handle) const {
    ghoul_assert(_engine, "No audio engine loaded");
    return _engine->getLooping(handle) ? ShouldLoop::Yes : ShouldLoop::No;
}

void AudioModule::stopAll() {
    ghoul_assert(_engine, "No audio engine loaded");

    _engine->stopAll();
    _sounds.clear();
}

std::vector<int> AudioModule::currentlyPlaying() const {
    // This function is *technically* not the ones that are playing, but that ones that we
    // are keeping track of. So we still have songs in our internal data structure that
    // were started as not-looping and that have ended playing. We need to filter them out
    // here.
    // The alternative would be to have a periodic garbage collection running, but that
    // feels worse. We are doing the garbage collection in the two playAudio functions
    // instead, since we have to do some work their either way

    std::vector<int> res;
    res.reserve(_sounds.size());
    for (const auto& [key, value] : _sounds) {
        if (isPlaying(key)) {
            res.push_back(key);
        }
    }
    return res;
}

void AudioModule::setGlobalVolume(float volume, float fade) const {
    ghoul_assert(_engine, "No audio engine loaded");

    // We clamp the volume level between [0, 1] to not accidentally blow any speakers
    volume = glm::clamp(volume, 0.f, 1.f);
    if (fade == 0.f) {
        _engine->setGlobalVolume(volume);
    }
    else {
        _engine->fadeGlobalVolume(volume, fade);
    }
}

float AudioModule::globalVolume() const {
    ghoul_assert(_engine, "No audio engine loaded");
    return _engine->getGlobalVolume();
}

void AudioModule::setVolume(int handle, float volume, float fade) const {
    ghoul_assert(_engine, "No audio engine loaded");

    // We clamp the volume level between [0, 1] to not accidentally blow any speakers
    volume = glm::clamp(volume, 0.f, 1.f);
    if (fade == 0.f) {
        _engine->setVolume(handle, volume);
    }
    else {
        _engine->fadeVolume(handle, volume, fade);
    }
}

float AudioModule::volume(int handle) const {
    ghoul_assert(_engine, "No audio engine loaded");

    const float volume = _engine->getVolume(handle);
    return volume;
}

int AudioModule::findAudio(const std::string& name) const {
    if (name.empty()) {
        // The empty name can never be a valid name
        return -1;
    }
    for (const auto& [key, value] : _sounds) {
        if (value.name == name) {
            return key;
        }
    }

    // If we get this far, we didn't find an entry for the name
    return -1;
}

int AudioModule::playAudio3d(const std::filesystem::path& path, const glm::vec3& position,
                             ShouldLoop loop, std::string name)
{
    ghoul_assert(_engine, "No audio engine loaded");

    std::unique_ptr<SoLoud::Wav> sound = loadSound(path);
    //sound->setAutoStop(false);
    sound->setLooping(loop);
    SoLoud::handle handle = _engine->play3d(*sound, position.x, position.y, position.z);

    ghoul_assert(_sounds.find(handle) == _sounds.end(), "Handle already used");
    _sounds[handle] = {
        .sound = std::move(sound),
        .name = std::move(name)
    };
    return handle;
}

void AudioModule::set3dListenerParameters(const std::optional<glm::vec3>& position,
                                          const std::optional<glm::vec3>& lookAt,
                                          const std::optional<glm::vec3>& up) const
{
    ghoul_assert(_engine, "No audio engine loaded");

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

void AudioModule::set3dSourcePosition(int handle, const glm::vec3& position) const {
    ghoul_assert(_engine, "No audio engine loaded");

    _engine->set3dSourcePosition(
        handle,
        position.x, position.y, position.z
    );
}

void AudioModule::setSpeakerPosition(int channel, const glm::vec3& position) const {
    ghoul_assert(_engine, "No audio engine loaded");
    _engine->setSpeakerPosition(channel, position.x, position.y, position.z);
}

glm::vec3 AudioModule::speakerPosition(int channel) const {
    ghoul_assert(_engine, "No audio engine loaded");

    float x = 0.f;
    float y = 0.f;
    float z = 0.f;
    _engine->getSpeakerPosition(channel, x, y, z);
    return glm::vec3(x, y, z);
}

std::vector<documentation::Documentation> AudioModule::documentations() const {
    return {
    };
}

scripting::LuaLibrary AudioModule::luaLibrary() const {
    return {
        "audio",
        {
            codegen::lua::PlayAudio,
            codegen::lua::StopAudio,
            codegen::lua::IsPlaying,
            codegen::lua::CurrentlyPlaying,
            codegen::lua::SetGlobalVolume,
            codegen::lua::GlobalVolume,
            codegen::lua::SetVolume,
            codegen::lua::Volume,
            codegen::lua::FindAudio,
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
