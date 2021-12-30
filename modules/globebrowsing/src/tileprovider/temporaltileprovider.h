/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/globebrowsing/src/tileprovider/interpolatetileprovider.h>
#include <modules/globebrowsing/src/tileprovider/singleimagetileprovider.h>

namespace openspace::globebrowsing {

/**
 * Provide <code>Tile</code>s from web map services that have temporal resolution.
 *
 * TemporalTileProviders are instantiated using a ghoul::Dictionary, and must define a
 * filepath to a Openspace Temporal dataset description file. This is an xml-file that
 * defines the same meta data as the GDAL wms description
 * (http://www.gdal.org/frmt_wms.html), but augmented with some extra tags describing the
 * temporal properties of the dataset. See
 * <code>TemporalTileProvider::TemporalXMLTags</code>
 */
struct TemporalTileProvider : public TileProvider {
    enum class TimeFormatType {
        YYYY_MM_DD = 0,
        YYYYMMDD_hhmmss,
        YYYYMMDD_hhmm,
        YYYY_MM_DDThhColonmmColonssZ,
        YYYY_MM_DDThh_mm_ssZ
    };

    using TimeKey = std::string;

    TemporalTileProvider(const ghoul::Dictionary& dictionary);

    Tile tile(const TileIndex& tileIndex) override final;
    Tile::Status tileStatus(const TileIndex& index) override final;
    TileDepthTransform depthTransform() override final;
    void update() override final;
    void reset() override final;
    int maxLevel() override final;
    float noDataValueAsFloat() override final;

    ghoul::Dictionary initDict;
    properties::StringProperty filePath;
    properties::BoolProperty useFixedTime;
    properties::StringProperty fixedTime;
    std::string gdalXmlTemplate;

    std::unordered_map<TimeKey, std::unique_ptr<TileProvider>> tileProviderMap;

    bool interpolation = false;

    TileProvider* currentTileProvider = nullptr;
    double startTimeJ2000;
    double endTimeJ2000;
    TimeFormatType timeFormat;
    TimeQuantizer timeQuantizer;
    std::string colormap;

    std::string myResolution;
    std::unique_ptr<InterpolateTileProvider> interpolateTileProvider;

private:
    void readFilePath();
    std::string consumeTemporalMetaData(const std::string& xml);
    void ensureUpdated();
    std::string_view timeStringify(TimeFormatType type, const Time& t);
    std::unique_ptr<TileProvider> initTileProvider(std::string_view timekey);
    TileProvider* getTileProvider(std::string_view timekey);
    TileProvider* getTileProvider(const Time& time);
};


} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__TEMPORALTILEPROVIDER___H__
