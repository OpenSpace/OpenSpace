/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__VIDEOPLAYER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__VIDEOPLAYER___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/documentation/documentation.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/vector/ivec2property.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/texture.h>

// libmpv
#include <client.h>
#include <render_gl.h>

namespace openspace {

class VideoPlayer : public properties::PropertyOwner {
public:
    VideoPlayer(const ghoul::Dictionary& dictionary);
    ~VideoPlayer();

    // Video interaction
    void pause();
    void play();
    void goToStart();
    void stepFrameForward();
    void stepFrameBackward();
    void seekToTime(double time);

    const std::unique_ptr<ghoul::opengl::Texture>& frameTexture() const;
    bool isPaused() const;
    bool isInitialized() const;
    double videoDuration() const;
    double currentPlaybackTime() const;

    void reset();
    void destroy();

    documentation::Documentation Documentation();
private:
    // libmpv property keys
    enum class LibmpvPropertyKey : uint64_t {
        Duration = 1,
        Height,
        Width,
        Meta,
        Params,
        Time,
        Command,
        Seek,
        Fps,
        Pause
    };

    void createFBO(int width, int height);
    void resizeFBO(int width, int height);

    // Libmpv
    static void on_mpv_render_update(void*); // Has to be static because of C api
    void initializeMpv(); // Called first time in postSyncPreDraw
    void renderMpv(); // Called in postSyncPreDraw
    void handleMpvEvents();
    void handleMpvProperties(mpv_event* event);
    void swapBuffersMpv(); // Called in postDraw
    void observePropertyMpv(std::string name, mpv_format format, LibmpvPropertyKey key);
    void setPropertyStringMpv(std::string name, std::string value);
    void getPropertyAsyncMpv(std::string name, mpv_format format, LibmpvPropertyKey key);
    void commandAsyncMpv(const char* cmd[], 
        LibmpvPropertyKey key = LibmpvPropertyKey::Command);

    std::filesystem::path _videoFile;

    // Video properties. Try to read all these values from the video
    double _currentVideoTime = 0.0;
    double _fps = 24.0; // If when we read it it is 0, use 24 fps 
    double _videoDuration = 0.0;
    glm::ivec2 _videoResolution = glm::ivec2(2048, 1024); // Used for the fbos
    bool _isPaused = false;
    // Libmpv
    mpv_handle* _mpvHandle = nullptr;
    mpv_render_context* _mpvRenderContext = nullptr;
    std::unique_ptr<ghoul::opengl::Texture> _frameTexture = nullptr;
    GLuint _fbo = 0; // Our opengl framebuffer where mpv renders to
    int _wakeup = 0; // Signals when libmpv has a new frame ready
    bool _didRender = false; // To know when to swap buffers
    bool _isInitialized = false; // If libmpv has been inititalized
    bool _isDestroying = false; // If libmpv has been inititalized
    bool _isSeeking = false; // Prevent seeking while already seeking
    double _seekThreshold = 1.0; // Threshold to ensure we seek to a different time
};
} // namespace video::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__VIDEOPLAYER___H__
