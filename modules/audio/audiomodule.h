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

#ifndef __OPENSPACE_MODULE_SPACE___AUDIOMODULE___H__
#define __OPENSPACE_MODULE_SPACE___AUDIOMODULE___H__

#include <openspace/util/openspacemodule.h>

#include <openspace/documentation/documentation.h>
#include <openspace/scripting/lualibrary.h>
#include <ghoul/misc/boolean.h>

namespace SoLoud {
    class Soloud;
    class Wav;
} // namespace SoLoud

namespace openspace {

class AudioModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "Audio";

    AudioModule();
    ~AudioModule() override;
    std::vector<documentation::Documentation> documentations() const override;

    scripting::LuaLibrary luaLibrary() const override;

    BooleanType(ShouldLoop);

    /**
     * Starts playing the audio file located and the provided \p path. The \p loop
     * parameter determines whether the file is only played once, or on a loop. The sound
     * is later referred to by the \p identifier name. The audio file will be played in
     * "background" mode, which means that each channel will be played at full volume. To
     * play a video using spatial audio, use the #playAudio3d function instead.
     *
     * \param path The audio file that should be played
     * \param identifier The name for the sound that is used to refer to the sound
     * \param loop If `Yes` then the song will be played in a loop until the program is
     *        closed or the playing is stopped through the #stopAudio function
     */
    void playAudio(const std::filesystem::path& path, std::string identifier,
        ShouldLoop loop);

    /**
     * Starts playing the audio file located and the provided \p path. The \p loop
     * parameter determines whether the file is only played once, or on a loop. The sound
     * is later referred to by the \p identifier name. The \p position parameter
     * determines the spatial location of the sound in a meter-based coordinate system.
     * The position of the listener is (0,0,0) with the forward direction along the +y
     * axis. This means that the "left" channel in a stereo setting is towards -x and the
     * "right" channel towards x. This default value can be customized through the
     * #set3dListenerParameters function. If you want to play a video without spatial
     * audio, use the #playAudio function instead.
     *
     * \param path The audio file that should be played
     * \param identifier The name for the sound that is used to refer to the sound
     * \param position The position of the audio file in the 3D environment
     * \param loop If `Yes` then the song will be played in a loop until the program is
     *        closed or the playing is stopped through the #stopAudio function
     */
    void playAudio3d(const std::filesystem::path& path, std::string identifier,
        const glm::vec3& position, ShouldLoop loop);

    /**
     * Stops the audio referenced by the \p identifier. The \p identifier must be a name
     * for a sound that was started through the #playAudio or #playAudio3d functions.
     * After this function, the \p identifier can not be used for any other function
     * anymore except for #playAudio or #playAudio3d to start a new sound.
     *
     * \param identifier The identifier to the track that should be stopped
     */
    void stopAudio(const std::string& identifier);

    /**
     * Stops all currently playing tracks. After this function, none of the identifiers
     * used to previously play a sound a valid any longer, but can still be used by the
     * #playAudio or #playAudio3d functions to start a new sound.
     * This function behaves the same way as if manually calling #stopAudio on all of the
     * sounds that have been started.
     */
    void stopAll();

    /**
     * Pauses the playback for all sounds, while keeping them valid. This function behaves
     * the same as if calling #pauseAudio on all of the sounds that are currently playing.
     */
    void pauseAll() const;

    /**
     * Resumes the playback for all sounds that have been paused. Please note that this
     * will also resume the playback for the sounds that have been manually paused, not
     * just those that were paused through the #pauseAll function.
     */
    void resumeAll() const;

    /**
     * Takes all of the sounds that are currently registers, unpauses them and plays them
     * from their starting points
     */
    void playAllFromStart() const;

    /**
     * Returns whether the track referred to by the \p identifier is currently playing. A
     * volume of 0 is still considered to be playing. The \p identifier must be a name for
     * a sound that was started through the #playAudio or #playAudio3d functions.
     *
     * \param identifier The identifier to the track that should be stopped
     * \return `true` if the track is currently playing, `false` otherwise
     */
    bool isPlaying(const std::string& identifier) const;

    /**
     * Pauses the playback of the track referred to by the \p identifier. The playback can
     * later be resumed through the #resumeAudio function. Trying to pause an already
     * paused track will not do anything, but is valid. The \p identifier must be a name
     * for a sound that was started through the #playAudio or #playAudio3d functions.
     *
     * \param identifier The identifier to the track that should be stopped
     */
    void pauseAudio(const std::string& identifier) const;

    /**
     * Resumes the playback of a track that was previously paused through the #pauseAudio
     * function. Trying to resume an already playing track will not do anything, but is
     * valid. The \p identifier must be a name for a sound that was started through the
     * #playAudio or #playAudio3d functions.
     *
     * \param identifier The identifier to the track that should be stopped
     */
    void resumeAudio(const std::string& identifier) const;

    /**
     * Returns whether the track refered to by the \p identifier is currently playing or
     * paused. If it was be paused through a previous call to #pauseAudio, this function
     * will return `true`. If it has just been created or resumed through a call to
     * #resumeAudio, it will return `false`. The \p identifier must be a name for a sound
     * that was started through the #playAudio or #playAudio3d functions.
     *
     * \param identifier The identifier to the track that should be stopped
     * \return `true` if the track is currently paused, `false` if it is playing
     */
    bool isPaused(const std::string& identifier) const;

    /**
     * Controls whether the track referred to by the \p identifier should be looping or
     * just be played once. If a track is converted to not looping, it will finish playing
     * until the end of the file. The \p identifier must be a name for a sound that was
     * started through the #playAudio or #playAudio3d functions.
     *
     * \param identifier The identifier to the track that should be stopped
     * \param loop If `Yes` then the song will be played in a loop until the program is
     *        closed or the playing is stopped through the #stopAudio function
     */
    void setLooping(const std::string& identifier, ShouldLoop loop) const;

    /**
     * Returns whether the track referred to by the \p identifier is set to be looping or
     * whether it should played only once. The \p identifier must be a name for a sound
     * that was started through the #playAudio or #playAudio3d functions.
     *
     * \param identifier The identifier to the track that should be stopped
     * \return `Yes` if the track is looping, `No` otherwise
     */
    ShouldLoop isLooping(const std::string& identifier) const;

    /**
     * Sets the volume of the track referred to by \p identifier to the new \p volume. The
     * volume should be a number bigger than 0, where 1 is the maximum volume level.
     * The \p fade controls whether the volume change should be immediately (if
     * it is 0) or over how many seconds it should change. The default is for it to change
     * over 500 ms. The \p identifier must be a name for a sound that was started through
     * the #playAudio or #playAudio3d functions.
     *
     * \param identifier The identifier to the track that should be stopped
     * \param volume The new volume level. Must be greater or equal to 0
     * \param fade How much time the fade from the current volume to the new volume should
     *        take
     */
    void setVolume(const std::string& identifier, float volume, float fade = 0.5f) const;

    /**
     * Returns the volume for the track referred to by the \p handle. The number returned
     * will be greater or equal to 0. The \p identifier must be a name for a sound that
     * was started through the #playAudio or #playAudio3d functions.
     *
     * \param identifier The identifier to the track that should be stopped
     * \return The volume for the track referred to by the \p handle, which will be
     *         greater or equal to 0
     */
    float volume(const std::string& identifier) const;

    /**
     * Updates the 3D position of a track started through the #playAudio3d function. See
     * that function and the #set3dListenerParameters function for a complete description.
     * The \p identifier must be a name for a sound that was started through the
     * #playAudio3d function.
     *
     * \param handle A valid handle for a track started through the #playAudio3d function
     * \param position The new position from which the track originates
     */
    void set3dSourcePosition(const std::string& identifier,
        const glm::vec3& position) const;

    /**
     * Returns the list of all tracks that are currently playing.
     *
     * \return The list of all tracks that are currently playing
     */
    std::vector<std::string> currentlyPlaying() const;

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
    void setGlobalVolume(float volume, float fade = 0.5f) const;

    /**
     * Returns the global volume for all track. The number returned will be greater or
     * equal to 0.
     *
     * \return The global volume
     */
    float globalVolume() const;

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
    void set3dListenerParameters(const std::optional<glm::vec3>& position,
        const std::optional<glm::vec3>& lookAt = std::nullopt,
        const std::optional<glm::vec3>& up = std::nullopt) const;

    /**
     * Sets the position of the speaker for the provided \p channel to the provided
     * \position. In general, this is considered an advanced feature to accommodate
     * non-standard audio environments.
     *
     * \param channel The channel whose speaker's position should be changed
     * \param position The new position for the speaker
     */
    void setSpeakerPosition(int channel, const glm::vec3& position) const;

    /**
     * Returns the position for the speaker of the provided \p channel.
     * \return The position for the speaker of the provided \p channel
     */
    glm::vec3 speakerPosition(int channel) const;

private:
    struct Info {
        std::unique_ptr<SoLoud::Wav> sound;
        unsigned int handle = 0;
    };

    void internalInitialize(const ghoul::Dictionary&) override;
    void internalDeinitializeGL() override;

    /**
     * Loads the sound at the provided \p path as an audio source and returns the pointer
     * to it. The sound has only been loaded and no other attributes have changed.
     *
     * \param path The path to the audio file on disk that should be loaded
     * \return The SoLoud::Wav object of the loaded file
     * \throw ghoul::RuntimeError If the \p path is not a loadable audio file
     */
    std::unique_ptr<SoLoud::Wav> loadSound(const std::filesystem::path& path);

    std::unique_ptr<SoLoud::Soloud> _engine;

    std::map<std::string, Info> _sounds;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___AUDIOMODULE___H__
