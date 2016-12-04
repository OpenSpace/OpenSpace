/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

#include <openspace/util/time.h>
#include <openspace/util/timerange.h>

#include <memory>
#include <string>
#include <unordered_map>

struct CPLXMLNode;

namespace openspace {
namespace globebrowsing {

/**
 * Interface for stringifying OpenSpace Time instances.
 *
 * Once OpenSpace has a proper Time format class, this should be handled by that instead
 * of here.    
 */
struct TimeFormat {
    /**
     * Stringifies a OpenSpace time instance
     * \param t The time to be stringifyed
     * \returns A string description of the provided time
     */
    virtual std::string stringify(const Time& t) const = 0;
};

/**
 * Stringifies OpenSpace to the format "YYYY-MM-DD".
 * Example: 2016-09-08
 */
struct YYYY_MM_DD : public TimeFormat {
    virtual std::string stringify(const Time& t) const;
};

/**
 * Stringifies OpenSpace to the format "YYYY-MM-DDThh:mm:ssZ"
 * Example: 2016-09-08T23:05:05Z
 */
struct YYYY_MM_DDThhColonmmColonssZ : public TimeFormat {
    virtual std::string stringify(const Time& t) const;
};
    
/**
 * Stringifies OpenSpace to the format "YYYY-MM-DDThh:mm:ssZ"
 * Example: 2016-09-08T23:05:05Z
 */
struct YYYY_MM_DDThh_mm_ssZ : public TimeFormat {
    virtual std::string stringify(const Time& t) const;
};

/**
 * Static factory class for providing different TimeFormats.
 * A time format stringifier is retrieved by a name of the format.
 * See implementation of <code>init()</code> to see what time 
 * id formats are supported.
 */
struct TimeIdProviderFactory {
    /**
     * Maps a name of a format to an implementation of a TimeFormat.
     * Calling this method will also initialize the TimeIdProviderFactory
     * if it hasn't been.
     *
     * See implementation of <code>init()</code> for supported time formats.
     *
     * \param format - name of TimeFormat, eg "YYYY-MM-DDThh:mm:ssZ".
     * \returns a concrete TimeFormat used to stringify instances of Time 
     */
    static TimeFormat* getProvider(const std::string& format);

    /**
     * Registers all supported TimeFormats.
     */
    static void init();

    static std::unordered_map<std::string, std::unique_ptr<TimeFormat>> _timeIdProviderMap;
    static bool initialized;
};

/**
 * Used to quantize time to descrete values. 
 */
struct TimeQuantizer {
    TimeQuantizer() {}
    TimeQuantizer(const Time& start, const Time& end, double resolution);
    TimeQuantizer(const Time& start, const Time& end, const std::string& resolutionStr);

    /**
     * Takes a time resulition string and parses it into a double 
     * value representing the time resolution as seconds.
     *
     * Example: parseTimeResolutionStr("1d");
     * 
     * \param resoltutionStr with the format {number}{unit}
     *        where supported units are: 
     *        (s)econds, (m)inutes, (h)ours, (d)ays, (y)ears
     *
     * \returns the time resolution in seconds
     */
    static double parseTimeResolutionStr(const std::string& resoltutionStr);

    /**
     * Quantizes a OpenSpace Time into descrete values.
     * If the provided Time t is outside the time range, it will
     * be clamped to the the time range.
     *
     * \param t Time instance, which will be quantized
     * \param clamp Whether or not time should be clamped if not t is in the time range
     * \returns wether or not time was quantized
     */
    bool quantize(Time& t, bool clamp) const;

private:
    TimeRange _timerange;
    double _resolution;
};

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
    /**
     * Dictionary constructor. Must provide KeyFilePath as defined in .cpp file.
     */
    TemporalTileProvider(const ghoul::Dictionary& dictionary);

    // These methods implements the TileProvider interface

    virtual Tile getTile(const TileIndex& tileIndex);
    virtual Tile getDefaultTile();
    virtual Tile::Status getTileStatus(const TileIndex& tileIndex);
    virtual TileDepthTransform depthTransform();
    virtual void update();
    virtual void reset();
    virtual int maxLevel();


    typedef std::string TimeKey;

    std::shared_ptr<TileProvider> getTileProvider(Time t = Time::ref());
    std::shared_ptr<TileProvider> getTileProvider(TimeKey timekey);

private:
    /**
     * A placeholder string that must be provided in the WMS template url. This 
     * placeholder will be replaced by quantized date-time strings during run time
     * in order to access the datasets for different instances of time.
     */
    static const char* URL_TIME_PLACEHOLDER;

    /**
     * These are tags that TemporalTileProviders must be able to read from the XML
     * file provided in the ghoul::Dictionary used to create this provider. These
     * tags describe the temporal properties of the dataset.
     */
    static const struct TemporalXMLTags {
        /**
         * Tag should contain a ISO8601 time specifying the datasets start time
         */
        static const char* TIME_START;

        /**
         * Tag should contain a ISO8601 time specifying the datasets end time
         * Example 1: "2016 SEP 08".
         * Example 2: "now" - sets the dataset's end time to the current time.
         */
        static const char* TIME_END;

        /**
         * Tag should contain the time resolution of the dataset. 
         * The resolution is defined by a number along with a unit specifying how 
         * often the dataset is updated temporally. Supported units are:
         * (s)econds, (m)inutes, (h)ours, (d)ays, (y)ears.
         *
         * Example 1: "2d" - dataset updated every other day.
         * Example 2: "1h" - dataset is updated every hour.
         */
        static const char* TIME_RESOLUTION;

        /**
         * Tag should contain a string specifying the date-time format expected by the
         * WMS. 
         */
        static const char* TIME_FORMAT;
    };

        
    /**
     * Create a GDAL dataset description based on the time t
     * \param t Time to generate a GDAL dataset description for
     * \returns a GDAL dataset description
     */
    std::string getGdalDatasetXML(Time t);

    /**
     * Create a GDAL dataset description associated with the provided TimeKey
     * \param key The TimeKey specifying time
     * \returns a GDAL dataset description
     */
    std::string getGdalDatasetXML(TimeKey key);

    /**
     * Instantiates a new TileProvder for the temporal dataset at the time 
     * specified. 
     * 
     * This method replaced the <code>URL_TIME_PLACEHOLDER</code> in the template URL
     * with the provided timekey, the opens a new GDAL dataset with that URL.
     * 
     * \param timekey time specifying dataset's temporality
     * \returns newly instantiated TileProvider
     */
    std::shared_ptr<TileProvider> initTileProvider(TimeKey timekey);

    /**
     * Takes as input a Openspace Temporal dataset description, extracts the temporal
     * metadata provided by reading the <code>TemporalXMLTags</code>, removes the 
     * read tags from the description, and returns a GDAL template GDAL dataset 
     * description. The template GDAL dataset description has the a 
     * <code>URL_TIME_PLACEHOLDER</code> still in it, which needs to be replaced before
     * GDAL can open it as a GDALDataset.
     *
     * \param xml Openspace Temporal dataset description
     * \returns a GDAL template data description. 
     */
    std::string consumeTemporalMetaData(const std::string &xml);

    /**
     * Helper method to read a XML value from a XML tree.
     * \param node XML tree to search in
     * \param key XML tag to find the value for
     * \param defaultVal value to return if key was not found
     * \returns the value of the Key, or defaultVal if key was undefined.
     */
    std::string getXMLValue(CPLXMLNode* node, const std::string& key, const std::string& defaultVal);

    /**
     * Ensures that the TemporalTileProvider is up to date.
     */
    void ensureUpdated();

    std::string _datasetFile;
    std::string _gdalXmlTemplate;

    std::unordered_map<TimeKey, std::shared_ptr<TileProvider>> _tileProviderMap;

    // Used for creation of time specific instances of CachingTileProvider
    ghoul::Dictionary _initDict;

    Tile _defaultTile;

    std::shared_ptr<TileProvider> _currentTileProvider;
        
    TimeFormat* _timeFormat;
    TimeQuantizer _timeQuantizer;
};

} // namespace globebrowsing
} // namespace openspace

#endif  // __OPENSPACE_MODULE_GLOBEBROWSING___TEMPORAL_TILE_PROVIDER___H__
