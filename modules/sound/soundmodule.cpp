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
#include <soloud_wav.h>

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

    _engine->init();
    _engine->setGlobalVolume(1.f);
    const int nChannels = p.maxNumberOfChannels.value_or(128);
    _engine->setMaxActiveVoiceCount(static_cast<unsigned int>(nChannels));
}

void SoundModule::internalDeinitializeGL() {
    _engine->deinit();
    _engine = nullptr;
}

int SoundModule::playAudio(std::string path, ShouldLoop loop) {
    std::unique_ptr<SoLoud::Wav> sound = std::make_unique<SoLoud::Wav>();
    SoLoud::result res = sound->load(path.c_str());
    if (res != 0) {
        throw ghoul::RuntimeError(fmt::format(
            "Error loading sound from {}. {}: {}",
            path, static_cast<int>(res), _engine->getErrorString(res)
        ));
    }

    sound->setLooping(loop);
    SoLoud::handle handle = _engine->playBackground(*sound);

    ghoul_assert(
        std::find(_sounds.begin(), _sounds.end(), handle) == _sounds.end(),
        "Handle already used"
    );
    _sounds[handle] = std::move(sound);
    return handle;
}

void SoundModule::stopAudio(int handle) {
    auto it = std::find(_sounds.begin(), _sounds.end(), handle);
    if (it == _sounds.end()) {
        throw ghoul::RuntimeError(fmt::format(
            "Handle {} is not a valid sound handle", handle
        ));
    }
    _sounds.erase(it);
    _engine->stop(handle);
}

bool SoundModule::isPlaying(int handle) const {
    auto it = std::find(_sounds.begin(), _sounds.end(), handle);
    if (it == _sounds.end()) {
        throw ghoul::RuntimeError(fmt::format(
            "Handle {} is not a valid sound handle", handle
        ));
    }

    return _engine->isValidVoiceHandle(handle);
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

void SoundModule::setVolume(int handle, float volume) const {
    auto it = std::find(_sounds.begin(), _sounds.end(), handle);
    if (it == _sounds.end()) {
        throw ghoul::RuntimeError(fmt::format(
            "Handle {} is not a valid sound handle", handle
        ));
    }

    _engine->setVolume(handle, volume);
}

float SoundModule::volume(int handle) const {
    auto it = std::find(_sounds.begin(), _sounds.end(), handle);
    if (it == _sounds.end()) {
        throw ghoul::RuntimeError(fmt::format(
            "Handle {} is not a valid sound handle", handle
        ));
    }

    const float volume = _engine->getVolume(handle);
    return volume;
}

std::vector<std::string> SoundModule::drivers() const {
    //int nDrivers = 0;
    //_system->getNumDrivers(&nDrivers);

    //std::vector<std::string> result;
    //for (int i = 0; i < nDrivers; i++) {
    //    std::array<char, 512> buffer = {};
    //    _system->getDriverInfo(i, buffer.data(), 512, nullptr, nullptr, nullptr, nullptr);
    //    result.push_back(buffer.data());
    //}
    return {};
}

void SoundModule::setDriver(int index) const {
    //_system->setDriver(index);
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
            codegen::lua::Drivers,
            codegen::lua::SetDriver
        }
    };
}

} // namespace openspace
