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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GDAL_WRAPPER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GDAL_WRAPPER___H__

#ifdef GLOBEBROWSING_USE_GDAL

#include <openspace/properties/propertyowner.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>

#include <cpl_error.h>

namespace openspace::globebrowsing {

/**
 * Function for passing GDAL error messages to the GHOUL logging system.
 */
void gdalErrorHandler(CPLErr eErrClass, int errNo, const char* msg);

/**
 * Singleton class interfacing with global GDAL functions.
 */
class GdalWrapper : public properties::PropertyOwner {
public:
    /**
     * Create the singleton. Must be called before the class can be used.
     * \param maximumCacheSize is the current maximum cache size GDAL can use
     * for caching blocks in memory given in bytes.
     * \param maximumMaximumCacheSize is the maximum cache size GDAL can use
     * for caching blocks in memory given in bytes.
     */
    static void create(size_t maximumCacheSize, size_t maximumMaximumCacheSize);
    static void destroy();

    static GdalWrapper& ref();

    /**
     * Get the current size of the GDAL in memory cache.
     * \returns the number of bytes currently in the GDAL memory cache.
     */
    static size_t GDALCacheUsed();

    /**
     * Get the maximum GDAL in memory cache size.
     * \returns the maximum number of bytes allowed for the GDAL cache.
     */
    static size_t GDALMaximumCacheSize();

    bool logGdalErrors() const;

private:
    GdalWrapper(size_t maximumCacheSize, size_t maximumMaximumCacheSize);
    ~GdalWrapper() = default;

    void setGdalProxyConfiguration();

    properties::BoolProperty _logGdalErrors;
    properties::IntProperty _gdalMaximumCacheSize;

    static GdalWrapper* _singleton;
};

} // namespace openspace::globebrowsing

#endif // GLOBEBROWSING_USE_GDAL

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GDAL_WRAPPER___H__
