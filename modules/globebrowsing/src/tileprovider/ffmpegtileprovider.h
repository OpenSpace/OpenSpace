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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__FFMPEGTILEPROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__FFMPEGTILEPROVIDER___H__

#include <modules/globebrowsing/src/tileprovider/tileprovider.h>

#include <ghoul/glm.h>

// libmpv
#include <client.h>
#include <render_gl.h>

namespace openspace { struct Documentation; }

namespace openspace::globebrowsing {

class FfmpegTileProvider : public TileProvider {
public:
    static constexpr glm::ivec2 TileSize = { 512, 512 };
    //static constexpr glm::ivec2 FinalResolution = { 2048, 1024 };
    static constexpr int NumTilePixels = 262144;

    FfmpegTileProvider(const ghoul::Dictionary& dictionary);
    ~FfmpegTileProvider();

    Tile tile(const TileIndex& tileIndex) override final;
    Tile::Status tileStatus(const TileIndex& tileIndex) override final;
    TileDepthTransform depthTransform() override final;
    void update() override final;
    void reset() override final;
    int minLevel() override final;
    int maxLevel() override final;
    float noDataValueAsFloat() override final;

    ChunkTile chunkTile(TileIndex tileIndex, int parents, int maxParents = 1337) override;

    static documentation::Documentation Documentation();

private:
    void createFBO(int width, int height);
    void resizeFBO(int width, int height);
    void handleMpvProperties(mpv_event* event);
    void handleMpvEvents();

    void internalInitialize() override final;
    void internalDeinitialize() override final;

    enum class AnimationMode {
        MapToSimulationTime = 0,
        RealTimeLoopFromStart,
        RealTimeLoopInfinitely
    };

    AnimationMode _animationMode = AnimationMode::MapToSimulationTime;
    std::filesystem::path _videoFile;
    std::string _startTime;
    std::string _endTime;
    double _startJ200Time = 0.0;
    double _endJ200Time = 0.0;
    double _videoDuration = -1.0;
    double _currentVideoTime = 0.0;
    double _prevVideoTime = 0.0;
    double _frameTime = 1.0 / 24.0;
    bool _hasReachedEnd = false;
    bool _tileIsReady = false;
    bool _isInitialized = false;
    bool _isWaiting = false;
    glm::ivec2 _resolution = { 2048, 1024 };

    // libmpv
    mpv_handle* _mpvHandle = nullptr;
    mpv_render_context* _mpvRenderContext = nullptr;
    ghoul::opengl::Texture* _frameTexture = nullptr;
    GLuint _fbo = 0;
    TileTextureInitData::HashKey _frameTextureHashKey;

    // libmpv property keys
    enum class LibmpvPropertyKey : uint64_t {
        Duration = 1,
        Eof,
        Height,
        Meta,
        Params,
        Time,
        Width
    };
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__FFMPEGTILEPROVIDER___H__
