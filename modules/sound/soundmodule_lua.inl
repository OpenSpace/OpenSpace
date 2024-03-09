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

#include <openspace/engine/moduleengine.h>

namespace {

/**
 * Starts playing an audio file. The file must be something that is supported by the FMOD
 * library, which is basically any audio file imaginable. The function returns a number
 * which can be used in other functions to refer to this audio channel. Please note that
 * the provided address can either be a file on disk (in which case the file has to exist)
 * or a URL for an internet resource.
 *
 * For a full list of supported audio files, see this list:
 * https://www.fmod.com/docs/2.02/api/core-api-sound.html#fmod_sound_type
 */
[[codegen::luawrap]] int playAudio(std::string address, bool shouldLoop) {
    using namespace openspace;

    int handle = global::moduleEngine->module<SoundModule>()->playAudio(
        std::move(address),
        SoundModule::ShouldLoop(shouldLoop)
    );
    return handle;
}

[[codegen::luawrap]] void stopAudio(int handle) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->stopAudio(handle);
}

[[codegen::luawrap]] bool isPlaying(int handle) {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->isPlaying(handle);
}

[[codegen::luawrap]] std::vector<int> currentlyPlaying() {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->currentlyPlaying();
}

[[codegen::luawrap]] void setVolumeChangeRamped(int handle, bool isRamped) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->setVolumeChangeRamped(
        handle,
        SoundModule::IsRamped(isRamped)
    );
}

[[codegen::luawrap]] void setVolume(int handle, float volume) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->setVolume(handle, volume);
}

[[codegen::luawrap]] float volume(int handle) {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->volume(handle);
}

[[codegen::luawrap]] void setMute(int handle, bool isMute) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->setMute(
        handle,
        SoundModule::IsMute(isMute)
    );
}

[[codegen::luawrap]] bool isMute(int handle) {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->isMute(handle);
}


#include "soundmodule_lua_codegen.cpp"

} // namespace
