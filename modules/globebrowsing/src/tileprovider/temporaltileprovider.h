/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__TEMPORALTILEPROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__TEMPORALTILEPROVIDER___H__

#include <modules/globebrowsing/src/tileprovider/tileprovider.h>

#include <modules/globebrowsing/src/tileprovider/defaulttileprovider.h>
#include <modules/globebrowsing/src/tileprovider/singleimagetileprovider.h>

namespace openspace::globebrowsing {

/**
 * Provide `Tile`s from web map services that have temporal resolution.
 *
 * TemporalTileProviders are instantiated using a ghoul::Dictionary, and must define a
 * filepath to a Openspace Temporal dataset description file. This is an xml-file that
 * defines the same meta data as the GDAL wms description
 * (http://www.gdal.org/frmt_wms.html), but augmented with some extra tags describing the
 * temporal properties of the dataset.
 *
 * \sa TemporalTileProvider::TemporalXMLTags
 */
class TemporalTileProvider : public TileProvider {
public:
    explicit TemporalTileProvider(const ghoul::Dictionary& dictionary);

    Tile tile(const TileIndex& tileIndex) override final;
    Tile::Status tileStatus(const TileIndex& index) override final;
    TileDepthTransform depthTransform() override final;
    void update() override final;
    void reset() override final;
    int minLevel() override final;
    int maxLevel() override final;
    float noDataValueAsFloat() override final;

    static documentation::Documentation Documentation();

private:
    enum class Mode {
        Prototype,
        Folder
    };

    struct InterpolateTileProvider : public TileProvider {
        explicit InterpolateTileProvider(const ghoul::Dictionary&);
        ~InterpolateTileProvider() override;

        Tile tile(const TileIndex& tileIndex) override final;
        Tile::Status tileStatus(const TileIndex& index) override final;
        TileDepthTransform depthTransform() override final;
        void update() override final;
        void reset() override final;
        int minLevel() override final;
        int maxLevel() override final;
        float noDataValueAsFloat() override final;

        TileProvider* before = nullptr;
        TileProvider* t1 = nullptr;
        TileProvider* t2 = nullptr;
        TileProvider* future = nullptr;
        float factor = 1.f;
        GLuint vaoQuad = 0;
        GLuint vboQuad = 0;
        GLuint fbo = 0;
        std::unique_ptr<ghoul::opengl::ProgramObject> shaderProgram;
        std::unique_ptr<ghoul::opengl::Texture> colormap;
    };

    DefaultTileProvider createTileProvider(std::string_view timekey) const;
    DefaultTileProvider* retrieveTileProvider(const Time& t);

    template <Mode mode, bool interpolation>
    TileProvider* tileProvider(const Time& time);

    TileProvider* tileProvider(const Time& time);

    Mode _mode;

    struct {
        double startTimeJ2000 = 0.0;
        double endTimeJ2000 = 0.0;

        std::string temporalResolution;
        std::string timeFormat;
        TimeQuantizer timeQuantizer;
        std::string prototype;
    } _prototyped;

    struct {
        std::filesystem::path folder;
        std::string format;

        std::vector<std::pair<double, std::string>> files;
    } _folder;

    ghoul::Dictionary _initDict;
    properties::BoolProperty _useFixedTime;
    properties::StringProperty _fixedTime;
    bool _fixedTimeDirty = true;

    TileProvider* _currentTileProvider = nullptr;
    std::unordered_map<double, DefaultTileProvider> _tileProviderMap;

    bool _isInterpolating = false;

    std::string _colormap;
    std::unique_ptr<InterpolateTileProvider> _interpolateTileProvider;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__TEMPORALTILEPROVIDER___H__
