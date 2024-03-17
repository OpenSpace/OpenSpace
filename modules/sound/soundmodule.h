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

#ifndef __OPENSPACE_MODULE_SPACE___SOUNDMODULE___H__
#define __OPENSPACE_MODULE_SPACE___SOUNDMODULE___H__

#include <openspace/util/openspacemodule.h>

#include <openspace/documentation/documentation.h>
#include <openspace/scripting/lualibrary.h>
#include <ghoul/misc/boolean.h>

namespace SoLoud {
    class Soloud;
    class WavStream;
} // namespace SoLoud

namespace openspace {

class SoundModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "Sound";

    SoundModule();
    ~SoundModule() override;
    std::vector<documentation::Documentation> documentations() const override;

    scripting::LuaLibrary luaLibrary() const override;

    BooleanType(ShouldLoop);

    /**
     * Starts playing the audio file located and the provided \p path. The \p loop
     * parameter determines whether the file is only played once, or on a loop. The return
     * value is an opaque handle that has to be passed in to most of the other audio
     * related functions. The audio file will be played in streaming mode, which means
     * that the loading time should be negligable and independent of the length of the
     * track.
     *
     * \param path The audio file that should be played
     * \param loop If `Yes` then the song will be played in a loop until the program is
     *        closed or the playing is stopped through the #stopAudio function
     * \return A handle that can be used to refer to this audio playback in later stages
     */
    int playAudio(const std::filesystem::path& path, ShouldLoop loop);

    /**
     * Stops the audio referenced by the \p handle. The \p handle must be a handle to a
     * track that is valid and was returned by the #playAudio or #playAudio3d functions.
     *
     * \param handle The handle to the track that should be stopped
     */
    void stopAudio(int handle);

    /**
     * Returns whether the track referred to by the \p handle is currently playing or not.
     * A volume of 0 is still considered to be playing. The \p handle must be a handle to
     * a track that is valid and was returned by the #playAudio or #playAudio3d functions.
     *
     * \param handle The handle to the track that should be checked
     * \return `true` if the track is currently playing, `false` otherwise
     */
    bool isPlaying(int handle) const;

    /**
     * Pauses the playback of the track referred to by the \p handle. The playback can
     * later be resumed through the #resumeAudio function. Trying to pause an already
     * paused track will not do anything, but is valid. The \p handle must be a handle to
     * a track that is valid and was returned by the #playAudio or #playAudio3d functions.
     *
     * \param handle The handle to the track that should be paused
     */
    void pauseAudio(int handle) const;

    /**
     * Resumes the playback of a track that was previously paused through the #pauseAudio
     * function. Trying to resume an already playing track will not do anything, but is
     * valid. The \p handle must be a handle to a track that is valid and was returned by
     * the #playAudio or #playAudio3d functions.
     *
     * \param handle The handle to the track that should be resumed
     */
    void resumeAudio(int handle) const;

    /**
     * Returns whether the track refered to by the \p handle is currently playing or
     * paused. If it was be paused through a previous call to #pauseAudio, this function
     * will return `true`. If it has just been created or resumed through a call to
     * #resumeAudio, it will return `false`.
     *
     * \param handle The handle to the track that should be checked
     * \return `true` if the tracky is currently paused, `false` if it is playing
     */
    bool isPaused(int handle) const;

    /**
     * Controls whether the track referred to by the \p handle should be looping or just
     * playing once. If a track is converted to not looping, it will finish playing until
     * the end of the file.
     *
     * \param handle The handle of the track that should be looping or not
     * \param loop If `Yes` then the song will be played in a loop until the program is
     *        closed or the playing is stopped through the #stopAudio function
     */
    void setLooping(int handle, ShouldLoop loop) const;

    /**
     * Returns whether the track referred to by the \p handle is set to be looping or
     * whether it should played only once.
     *
     * \param handle The handle of the track that should be checked for looping
     * \return `Yes` if the track is looping, `No` otherwise
     */
    ShouldLoop isLooping(int handle) const;

    /**
     * Stops all currently playing tracks.
     */
    void stopAll();

    /**
     * Returns the list of all tracks that are currently playing.
     *
     * \return The list of all tracks that are currently playing
     */
    std::vector<int> currentlyPlaying() const;

    /**
     * Sets the volume of the track referred to by \p handle to the new \p volume. The
     * volume should be a number bigger than 0, where 1 would be a "normal" volume level.
     * The \p interpolation controls whether the volume change should be immediately (if
     * it is 0) or over how many seconds it should change. The default is for it to change
     * over 500 ms.
     *
     * \param handle The handle to the track whose volume should be changed
     * \param volume The new volume level. Must be greater or equal to 0
     * \param interpolation How much time the interpolation from the current volume to the
     *        new volume should take
     */
    void setVolume(int handle, float volume, float interpolation = 0.5f) const;

    /**
     * Returns the volume for the track referred to by the \p handle. The number returned
     * will be greater or equal to 0.
     *
     * \return The volume for the track referred to by the \p handle, which will be
     *         greater or equal to 0.
     */
    float volume(int handle) const;

    int playAudio3d(const std::filesystem::path& path, const glm::vec3& position,
        ShouldLoop loop);
    void set3dListenerParameters(const std::optional<glm::vec3>& position,
        const std::optional<glm::vec3>& lookAt, const std::optional<glm::vec3>& up) const;
    void set3dSourcePosition(int handle, const glm::vec3& position) const;

    void setSpeakerPosition(int channel, const glm::vec3& position) const;
    glm::vec3 speakerPosition(int channel) const;

private:
    void internalInitialize(const ghoul::Dictionary&) override;
    void internalDeinitializeGL() override;
    std::unique_ptr<SoLoud::WavStream> loadSound(const std::filesystem::path& path);
    std::map<int, std::unique_ptr<SoLoud::WavStream>>::const_iterator findSound(
        int h) const;

    std::unique_ptr<SoLoud::Soloud> _engine = nullptr;
    std::map<int, std::unique_ptr<SoLoud::WavStream>> _sounds;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___SOUNDMODULE___H__
