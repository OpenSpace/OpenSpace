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
 * Starts playing the audio file located and the provided \p path. The \p loop
 * parameter determines whether the file is only played once, or on a loop. The return
 * value is an opaque handle that has to be passed in to most of the other audio related
 * functions. The audio file will be played in streaming mode, which means that the
 * loading time should be negligable and independent of the length of the track. The audio
 * file will be played in "background" mode, which means that each channel will be played
 * at full volume. To play a video using spatial audio, use the #playAudio3d function
 * instead.
 *
 * \param path The audio file that should be played
 * \param loop If `Yes` then the song will be played in a loop until the program is
 *        closed or the playing is stopped through the #stopAudio function
 * \param name An optional name for the sound that can be used to later look up the handle
 * \return A handle that can be used to refer to this audio playback in later stages
 */
[[codegen::luawrap]] int playAudio(std::filesystem::path path, bool shouldLoop = true,
                                   std::string name = "")
{
    using namespace openspace;

    int handle = global::moduleEngine->module<SoundModule>()->playAudio(
        std::move(path),
        SoundModule::ShouldLoop(shouldLoop),
        std::move(name)
    );
    return handle;
}

/**
 * Stops the audio referenced by the \p handle. The \p handle must be a handle to a
 * track that is valid and was returned by the #playAudio or #playAudio3d functions.
 *
 * \param handle The handle to the track that should be stopped
 */
[[codegen::luawrap]] void stopAudio(int handle) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->stopAudio(handle);
}

/**
 * Returns whether the track referred to by the \p handle is currently playing or not.
 * A volume of 0 is still considered to be playing. The \p handle must be a handle to
 * a track that is valid and was returned by the #playAudio or #playAudio3d functions.
 *
 * \param handle The handle to the track that should be checked
 * \return `true` if the track is currently playing, `false` otherwise
 */
[[codegen::luawrap]] bool isPlaying(int handle) {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->isPlaying(handle);
}

/**
 * Returns the list of all tracks that are currently playing.
 *
 * \return The list of all tracks that are currently playing
 */
[[codegen::luawrap]] std::vector<int> currentlyPlaying() {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->currentlyPlaying();
}

/**
 * Sets the global volume for all track referred to the new \p volume. The total
 * for each track is the global volume set by this function multiplied with the volume
 * for the specific track set through the #setVolume function. The default value for
 * the global volume is 0.5. The volume should be a number bigger than 0, where 1 is
 * the maximum volume level. The \p fade controls whether the volume change should be
 * immediately (if it is 0) or over how many seconds it should change. The default is
 * for it to change over 500 ms.
 *
 * \param volume The new volume level. Must be greater or equal to 0
 * \param fade How much time the fade from the current volume to the new volume should
 *        take
 */
[[codegen::luawrap]] void setGlobalVolume(float volume, float fade = 0.5f) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->setGlobalVolume(volume, fade);
}

/**
 * Returns the global volume for all track. The number returned will be greater or
 * equal to 0.
 *
 * \return The global volume
 */
[[codegen::luawrap]] float globalVolume() {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->globalVolume();
}

/**
 * Sets the volume of the track referred to by \p handle to the new \p volume. The
 * volume should be a number bigger than 0, where 1 is the maximum volume level.
 * The \p fade controls whether the volume change should be immediately (if
 * it is 0) or over how many seconds it should change. The default is for it to change
 * over 500 ms.
 *
 * \param handle The handle to the track whose volume should be changed
 * \param volume The new volume level. Must be greater or equal to 0
 * \param fade How much time the fade from the current volume to the new volume should
 *        take
 */
[[codegen::luawrap]] void setVolume(int handle, float volume, float fade = 0.5f)
{
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->setVolume(handle, volume, fade);
}

/**
 * Returns the volume for the track referred to by the \p handle. The number returned
 * will be greater or equal to 0.
 *
 * \return The volume for the track referred to by the \p handle, which will be
 *         greater or equal to 0
 */
[[codegen::luawrap]] float volume(int handle) {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->volume(handle);
}

/**
 * Returns the handle for the audio track with the provided name. If no audio track
 * could be found, `-1` is returned instead.
 *
 * \param name The name of the audio file that should be looked up
 * \return The handle for the track or `-1` if the track could not be found
 */
[[codegen::luawrap]] int findAudio(std::string name) {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->findAudio(name);
}

/**
 * Pauses the playback of the track referred to by the \p handle. The playback can
 * later be resumed through the #resumeAudio function. Trying to pause an already
 * paused track will not do anything, but is valid. The \p handle must be a handle to
 * a track that is valid and was returned by the #playAudio or #playAudio3d functions.
 *
 * \param handle The handle to the track that should be paused
 */
[[codegen::luawrap]] void pauseAudio(int handle) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->pauseAudio(handle);
}

/**
 * Resumes the playback of a track that was previously paused through the #pauseAudio
 * function. Trying to resume an already playing track will not do anything, but is
 * valid. The \p handle must be a handle to a track that is valid and was returned by
 * the #playAudio or #playAudio3d functions.
 *
 * \param handle The handle to the track that should be resumed
 */
[[codegen::luawrap]] void resumeAudio(int handle) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->resumeAudio(handle);
}

/**
 * Returns whether the track refered to by the \p handle is currently playing or
 * paused. If it was be paused through a previous call to #pauseAudio, this function
 * will return `true`. If it has just been created or resumed through a call to
 * #resumeAudio, it will return `false`.
 *
 * \param handle The handle to the track that should be checked
 * \return `true` if the tracky is currently paused, `false` if it is playing
 */
[[codegen::luawrap]] bool isPaused(int handle) {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->isPaused(handle);
}

/**
 * Controls whether the track referred to by the \p handle should be looping or just
 * playing once. If a track is converted to not looping, it will finish playing until
 * the end of the file.
 *
 * \param handle The handle of the track that should be looping or not
 * \param loop If `Yes` then the song will be played in a loop until the program is
 *        closed or the playing is stopped through the #stopAudio function
 */
[[codegen::luawrap]] void setLooping(int handle, bool shouldLoop) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->setLooping(
        handle,
        SoundModule::ShouldLoop(shouldLoop)
    );
}

/**
 * Returns whether the track referred to by the \p handle is set to be looping or
 * whether it should played only once.
 *
 * \param handle The handle of the track that should be checked for looping
 * \return `Yes` if the track is looping, `No` otherwise
 */
[[codegen::luawrap]] bool isLooping(int handle) {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->isLooping(handle);
}

/**
 * Starts playing the audio file located and the provided \p path. The \p loop
 * parameter determines whether the file is only played once, or on a loop. The return
 * value is an opaque handle that has to be passed in to most of the other audio
 * related functions. The audio file will be played in streaming mode, which means
 * that the loading time should be negligable and independent of the length of the
 * track. The \p position parameter determines the spatial location of the sound in
 * a meter-based coordinate system. The position of the listener is (0,0,0) with the
 * forward direction along the +y axis. This means that the "left" channel in a stereo
 * setting is towards -x and the "right" channel towards x. This default value can be
 * customized through the #set3dListenerParameters function. If you want to play a
 * video without spatial audio, use the #playAudio funciton instead.
 *
 * \param path The audio file that should be played
 * \param position The position of the audio file in the 3D environment
 * \param loop If `Yes` then the song will be played in a loop until the program is
 *        closed or the playing is stopped through the #stopAudio function
 * \param name An optional name for the sound that can be used to later look up the handle
 * \return A handle that can be used to refer to this audio playback in later stages
 */
[[codegen::luawrap]] int playAudio3d(std::string path, glm::vec3 position,
                                     bool shouldLoop = true, std::string name = "")
{
    using namespace openspace;

    int handle = global::moduleEngine->module<SoundModule>()->playAudio3d(
        std::move(path),
        position,
        SoundModule::ShouldLoop(shouldLoop)
    );
    return handle;
}

/**
 * Sets the position and orientation of the listener. This new position is
 * automatically used to adjust the relative position of all 3D tracks. Each parameter
 * to this function call is optional and if a value is omitted, the currently set
 * value continues to be used instead. The coordinate system for the tracks and the
 * listener is a meter-based coordinate system.
 *
 * \param position The position of the listener.
 * \param lookAt The direction vector of the forward direction
 * \param up The up-vector of the coordinate system
 */
[[codegen::luawrap]] void set3dListenerPosition(glm::vec3 position,
                                                  std::optional<glm::vec3> lookAt,
                                                  std::optional<glm::vec3> up)
{
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->set3dListenerParameters(
        position,
        lookAt,
        up
    );
}

/**
 * Updates the 3D position of a track started through the #playAudio3d function. See
 * that function and the #set3dListenerParameters function for a complete description.
 *
 * \param handle A valid handle for a track started through the #playAudio3d function
 * \param position The new position from which the track originates
 */
[[codegen::luawrap]] void set3dSourcePosition(int handle, glm::vec3 position) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->set3dSourcePosition(handle, position);
}

/**
 * Sets the position of the speaker for the provided \p channel to the provided
 * \position. In general, this is considered an advanced feature to accommodate
 * non-standard audio environments.
 *
 * \param channel The channel whose speaker's position should be changed
 * \param position The new position for the speaker
 */
[[codegen::luawrap]] void setSpeakerPosition(int handle, glm::vec3 position) {
    using namespace openspace;
    global::moduleEngine->module<SoundModule>()->setSpeakerPosition(handle, position);
}

/**
 * Returns the position for the speaker of the provided \p channel.
 * \return The position for the speaker of the provided \p channel
 */
[[codegen::luawrap]] glm::vec3 speakerPosition(int handle) {
    using namespace openspace;
    return global::moduleEngine->module<SoundModule>()->speakerPosition(handle);
}

#include "soundmodule_lua_codegen.cpp"

} // namespace
