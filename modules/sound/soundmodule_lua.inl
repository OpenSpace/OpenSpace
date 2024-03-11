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
 *
 * \param address The address of the file that should be played, which could either be a
 *        local file or a web address
 * \param shouldLoop If this value is `true`, the audio file will be played in a loop
 *        until explicitly stopped. If it is `false`, the audio file will be played once
 * \return A numerical handle that can be used to refer to the playing audio in later
 *         function calls. Each number is only returned once
 */
[[codegen::luawrap]] int playAudio(std::string address, bool shouldLoop) {
    using namespace openspace;

    int handle = global::moduleEngine->module<SoundModule>()->playAudio(
        std::move(address),
        SoundModule::ShouldLoop(shouldLoop)
    );
    return handle;
}

/**
 * Stops the audio playing of the provided \p handle. If the sound has already stopped,
 * this function does not do anything.
 *
 * \param handle The handle previously returned by a `playAudio` call that will be stopped
 */
[[codegen::luawrap]] void stopAudio(int handle) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->stopAudio(handle);
}

/**
 * Returns whether the audio file referred to by the \p handle is currently playing.
 *
 * \param handle The handle for which we want to know whether it is playing
 * \return `true` if the audio file is playing. `false` otherwise
 */
[[codegen::luawrap]] bool isPlaying(int handle) {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->isPlaying(handle);
}

/**
 * Returns a list of all handles that are currently playing.
 */
[[codegen::luawrap]] std::vector<int> currentlyPlaying() {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->currentlyPlaying();
}

/**
 * Sets a state for the specific audio file on whether volume changes should be abruptly
 * `=false` or if they should be ramped over a short time `=true`. The actual volume
 * changes are applied through the `setVolume` function instead.
 *
 * \param handle The handle for which we want to change the volume change behavior
 * \param isRamped If this is `true` later volume changes are ramped over time
 */
[[codegen::luawrap]] void setVolumeChangeRamped(int handle, bool isRamped) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->setVolumeChangeRamped(
        handle,
        SoundModule::IsRamped(isRamped)
    );
}

/**
 * Sets the volume for the audio file referred to by the provided to the new value. The
 * volume is provided as a number between 0 and 1.
 *
 * \param handle The handle for which we want to change the volume
 * \param volume The new volume value that we want to audio file to have. This value has
 *        to be between 0 and 1
 */
[[codegen::luawrap]] void setVolume(int handle, float volume) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->setVolume(handle, volume);
}

/**
 * Returns the volume for the audio file referred to by the handle.
 *
 * \return The volume at which the audio file is playing, provided as a value between 0
 *         and 1
 */
[[codegen::luawrap]] float volume(int handle) {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->volume(handle);
}

/**
 * Sets the audio file referred to by the handle to be mute or unmute.
 *
 * \param handle The handle for which we want to change the state of muteness
 * \param isMute If `true` the audio file is muted after this call, if it is `false`, the
 *        audio is playing at the volume set by the last `setVolume` call
 */
[[codegen::luawrap]] void setMute(int handle, bool isMute) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->setMute(
        handle,
        SoundModule::IsMute(isMute)
    );
}

/**
 * Returns whether the audio referred to by the `handle` is currently muted.
 */
[[codegen::luawrap]] bool isMute(int handle) {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->isMute(handle);
}

/**
 * Returns a list of all sound drivers that are detected on this computer
 */
[[codegen::luawrap]] std::vector<std::string> drivers() {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->drivers();
}

/**
 * Sets the provided driver as the currently playback sound card. The provided index must
 * be an index into the list returned by the `drivers` function.
 */
[[codegen::luawrap]] void setDriver(int index) {
    using namespace openspace;
    std::vector<std::string> ds = global::moduleEngine->module<SoundModule>()->drivers();
    if (index >= ds.size()) {
        throw ghoul::RuntimeError(fmt::format(
            "Requested sound driver index {} higher than number of available drivers {}",
            index, ds.size()
        ));
    }
    LINFOC("setDriver", fmt::format("Setting sound driver to {}", ds[index]));
    return global::moduleEngine->module<SoundModule>()->setDriver(index);
}

#include "soundmodule_lua_codegen.cpp"

} // namespace
