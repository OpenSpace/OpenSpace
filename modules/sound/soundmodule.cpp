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
#include <fmod.hpp>
#include <fmod_errors.h>

#include "soundmodule_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "SoundModule";
} // namespace

namespace openspace {

SoundModule::SoundModule()
    : OpenSpaceModule(Name)
{}

void SoundModule::internalInitialize(const ghoul::Dictionary& dictionary) {
    FMOD_RESULT result = FMOD::System_Create(&_system);
    if (result != FMOD_OK) {
        LERROR(fmt::format(
            "Error creating FMOD with code {}: {}",
            static_cast<int>(result), FMOD_ErrorString(result)
        ));
        return;
    }

    result = _system->init(128, FMOD_INIT_NORMAL, nullptr);
    if (result != FMOD_OK) {
        LERROR(fmt::format(
            "Error initializing FMOD with code {}: {}",
            static_cast<int>(result), FMOD_ErrorString(result)
        ));
        return;
    }

    global::callback::postDraw->emplace_back([this]() {
        _system->update();
    });
}

void SoundModule::internalDeinitializeGL() {
    FMOD_RESULT result = _system->release();
    if (result != FMOD_OK) {
        LERROR(fmt::format(
            "Error creating FMOD with code {}: {}",
            static_cast<int>(result), FMOD_ErrorString(result)
        ));
    }

    for (const std::pair<const int, Info>& p : _channels) {
        stopAudio(p.first);
    }

    _system = nullptr;
}

int SoundModule::playAudio(std::string path, ShouldLoop loop) {
    FMOD::Sound* sound = nullptr;
    // See:
    // https://www.fmod.com/docs/2.02/api/core-api-common.html#fmod_createsample
    FMOD_MODE mode = FMOD_DEFAULT | FMOD_CREATESTREAM;
    if (loop) {
        mode |= FMOD_LOOP_NORMAL;
    }
    
    FMOD_RESULT result = _system->createSound(path.c_str(), mode, nullptr, &sound);
    if (result != FMOD_OK) {
        throw ghoul::RuntimeError(fmt::format(
            "Error loading sound from {}. {}: {}",
            path, static_cast<int>(result), FMOD_ErrorString(result)
        ));
    }

    FMOD::Channel* channel = nullptr;
    result = _system->playSound(sound, nullptr, false, &channel);
    if (result != FMOD_OK) {
        throw ghoul::RuntimeError(fmt::format(
            "Error playing sound from {}. {}: {}",
            path, static_cast<int>(result), FMOD_ErrorString(result)
        ));
    }

    const int handle = _nextHandle;
    _channels[handle] = { .channel = channel, .sound = sound };
    _nextHandle++;
    return handle;
}

void SoundModule::stopAudio(int handle) {
    auto it = _channels.find(handle);
    if (it == _channels.end()) {
        throw ghoul::RuntimeError(fmt::format(
            "Handle {} is not a valid sound handle", handle
        ));
    }

    it->second.channel->stop();
    it->second.sound->release();
}

bool SoundModule::isPlaying(int handle) const {
    auto it = _channels.find(handle);
    if (it == _channels.end()) {
        throw ghoul::RuntimeError(fmt::format(
            "Handle {} is not a valid sound handle", handle
        ));
    }

    bool result = false;
    it->second.channel->isPlaying(&result);
    return result;
}

void SoundModule::stopAll() const {
    std::vector<int> list;
    for (const std::pair<const int, Info>& p : _channels) {
        p.second.channel->stop();
    }
}

std::vector<int> SoundModule::currentlyPlaying() const {
    std::vector<int> list;
    for (const std::pair<const int, Info>& p : _channels) {
        bool result = false;
        p.second.channel->isPlaying(&result);
        if (result) {
            list.push_back(p.first);
        }
    }

    return list;
}

void SoundModule::setVolumeChangeRamped(int handle, IsRamped isRamped) const {
    auto it = _channels.find(handle);
    if (it == _channels.end()) {
        throw ghoul::RuntimeError(fmt::format(
            "Handle {} is not a valid sound handle", handle
        ));
    }

    it->second.channel->setVolumeRamp(isRamped);
}

void SoundModule::setVolume(int handle, float volume) const {
    auto it = _channels.find(handle);
    if (it == _channels.end()) {
        throw ghoul::RuntimeError(fmt::format(
            "Handle {} is not a valid sound handle", handle
        ));
    }

    it->second.channel->setVolume(volume);
}

float SoundModule::volume(int handle) const {
    auto it = _channels.find(handle);
    if (it == _channels.end()) {
        throw ghoul::RuntimeError(fmt::format(
            "Handle {} is not a valid sound handle", handle
        ));
    }

    float volume = 0.f;
    it->second.channel->getVolume(&volume);
    return volume;
}

void SoundModule::setMute(int handle, IsMute mute) const {
    auto it = _channels.find(handle);
    if (it == _channels.end()) {
        throw ghoul::RuntimeError(fmt::format(
            "Handle {} is not a valid sound handle", handle
        ));
    }

    it->second.channel->setMute(mute);
}

bool SoundModule::isMute(int handle) const {
    auto it = _channels.find(handle);
    if (it == _channels.end()) {
        throw ghoul::RuntimeError(fmt::format(
            "Handle {} is not a valid sound handle", handle
        ));
    }

    bool isMute = true;
    it->second.channel->getMute(&isMute);
    return isMute;
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
            codegen::lua::SetVolumeChangeRamped,
            codegen::lua::SetVolume,
            codegen::lua::Volume,
            codegen::lua::SetMute,
            codegen::lua::IsMute
        }
    };
}

} // namespace openspace
