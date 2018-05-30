/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TEMPORAL_TILE_PROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TEMPORAL_TILE_PROVIDER___H__

#ifdef GLOBEBROWSING_USE_GDAL

#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

#include <modules/globebrowsing/other/timequantizer.h>
#include <openspace/properties/stringproperty.h>
#include <unordered_map>

struct CPLXMLNode;

namespace openspace::globebrowsing::tileprovider {

/**
 * Provide <code>Tile</code>s from web map services that have temporal resolution.
 *
 * TemporalTileProviders are instantiated using a ghoul::Dictionary,
 * and must define a filepath to a Openspace Temporal dataset description file.
 * This is an xml-file that defines the same meta data as the GDAL wms description
 * (http://www.gdal.org/frmt_wms.html), but augmented with some
 * extra tags describing the temporal properties of the dataset. See
 * <code>TemporalTileProvider::TemporalXMLTags</code>
 *
 */
class TemporalTileProvider : public TileProvider {
public:
    enum class TimeFormatType {
        YYYY_MM_DD = 0,
        YYYYMMDD_hhmmss,
        YYYYMMDD_hhmm,
        YYYY_MM_DDThhColonmmColonssZ,
        YYYY_MM_DDThh_mm_ssZ
    };

    /**
     * Dictionary constructor. Must provide KeyFilePath as defined in .cpp file.
     */
    TemporalTileProvider(const ghoul::Dictionary& dictionary);

    bool initialize() override;

    // These methods implements the TileProvider interface

    virtual Tile tile(const TileIndex& tileIndex) override;
    virtual Tile::Status tileStatus(const TileIndex& tileIndex) override;
    virtual TileDepthTransform depthTransform() override;
    virtual void update() override;
    virtual void reset() override;
    virtual int maxLevel() override;

    using TimeKey = std::string;

    std::shared_ptr<TileProvider> getTileProvider(const Time& t);
    std::shared_ptr<TileProvider> getTileProvider(const TimeKey& timekey);

private:
    /**
     * A placeholder string that must be provided in the WMS template url. This
     * placeholder will be replaced by quantized date-time strings during run time
     * in order to access the datasets for different instances of time.
     */
    constexpr static const char* UrlTimePlaceholder = "${OpenSpaceTimeId}";

    /**
     * These are tags that TemporalTileProviders must be able to read from the XML
     * file provided in the ghoul::Dictionary used to create this provider. These
     * tags describe the temporal properties of the dataset.
     */
    struct TemporalXMLTags {
        /**
         * Tag should contain a ISO8601 time specifying the datasets start time
         */
        constexpr static const char* TimeStart = "OpenSpaceTimeStart";

        /**
         * Tag should contain a ISO8601 time specifying the datasets end time
         * Example 1: "2016 SEP 08".
         * Example 2: "now" - sets the dataset's end time to the current time.
         */
        constexpr static const char* TimeEnd = "OpenSpaceTimeEnd";

        /**
         * Tag should contain the time resolution of the dataset.
         * The resolution is defined by a number along with a unit specifying how
         * often the dataset is updated temporally. Supported units are:
         * (s)econds, (m)inutes, (h)ours, (d)ays, (y)ears.
         *
         * Example 1: "2d" - dataset updated every other day.
         * Example 2: "1h" - dataset is updated every hour.
         */
        constexpr static const char* TimeResolution = "OpenSpaceTimeResolution";

        /**
         * Tag should contain a string specifying the date-time format expected by the
         * WMS.
         */
        constexpr static const char* TimeFormat = "OpenSpaceTimeIdFormat";
    };

    /**
     * Create a GDAL dataset description based on the time t,
     *
     * \param t Time to generate a GDAL dataset description for
     * \return a GDAL dataset description
     */
    std::string getGdalDatasetXML(const Time& t);

    /**
     * Create a GDAL dataset description associated with the provided TimeKey
     *
     * \param timeKey The TimeKey specifying time
     * \return a GDAL dataset description
     */
    std::string getGdalDatasetXML(const TimeKey& timeKey);

    /**
     * Instantiates a new TileProvder for the temporal dataset at the time
     * specified.
     *
     * This method replaced the <code>UrlTimePlaceholder</code> in the template URL
     * with the provided timekey, the opens a new GDAL dataset with that URL.
     *
     * \param timekey time specifying dataset's temporality
     * \return newly instantiated TileProvider
     */
    std::shared_ptr<TileProvider> initTileProvider(TimeKey timekey);

    /**
     * Takes as input a Openspace Temporal dataset description, extracts the temporal
     * metadata provided by reading the <code>TemporalXMLTags</code>, removes the
     * read tags from the description, and returns a GDAL template GDAL dataset
     * description. The template GDAL dataset description has the a
     * <code>UrlTimePlaceholder</code> still in it, which needs to be replaced before
     * GDAL can open it as a GDALDataset.
     *
     * \param xml Openspace Temporal dataset description
     * \returns a GDAL template data description.
     */
    std::string consumeTemporalMetaData(const std::string &xml);

    /**
     * Helper method to read a XML value from a XML tree.
     *
     * \param node XML tree to search in
     * \param key XML tag to find the value for
     * \param defaultVal value to return if key was not found
     * \return the value of the Key, or defaultVal if key was undefined.
     */
    std::string getXMLValue(CPLXMLNode* node, const std::string& key,
        const std::string& defaultVal);

    /**
     * Ensures that the TemporalTileProvider is up to date.
     */
    void ensureUpdated();

    bool readFilePath();

    // Used for creation of time specific instances of CachingTileProvider
    ghoul::Dictionary _initDict;
    properties::StringProperty _filePath;
    std::string _gdalXmlTemplate;

    std::unordered_map<TimeKey, std::shared_ptr<TileProvider>> _tileProviderMap;


    std::shared_ptr<TileProvider> _currentTileProvider;

    TimeFormatType _timeFormat;
    TimeQuantizer _timeQuantizer;

    std::vector<Time> _preCacheTimes;

    bool _successfulInitialization;
};

} // namespace openspace::globebrowsing::tileprovider

#endif // GLOBEBROWSING_USE_GDAL

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TEMPORAL_TILE_PROVIDER___H__
