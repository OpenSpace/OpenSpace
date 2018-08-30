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

#ifdef GLOBEBROWSING_USE_GDAL

#include <modules/globebrowsing/tile/rawtiledatareader/gdalwrapper.h>

#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/ghoul.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/logging/logmanager.h>
#include <gdal.h>

#include <cpl_conv.h>

namespace {
    constexpr const char* _loggerCat = "GdalWrapper";

    constexpr openspace::properties::Property::PropertyInfo LogGdalErrorInfo = {
        "LogGdalErrors",
        "Log GDAL errors",
        "If this value is enabled, any error that is raised by GDAL will be logged using "
        "the logmanager. If this value is disabled, any error will be ignored."
    };

    constexpr openspace::properties::Property::PropertyInfo GdalMaximumCacheInfo = {
        "GdalMaximumCacheSize",
        "GDAL maximum cache size",
        "This function sets the maximum amount of RAM memory in MB that GDAL is "
        "permitted to use for caching."
    };
} // namespace

namespace openspace::globebrowsing {

void gdalErrorHandler(CPLErr eErrClass, int, const char* msg) {
    if (GdalWrapper::ref().logGdalErrors()) {
        switch (eErrClass) {
            case CE_None: break;
            case CE_Debug:    LDEBUGC  ("GDAL", msg); break;
            case CE_Warning:  LWARNINGC("GDAL", msg); break;
            case CE_Failure:  LERRORC  ("GDAL", msg); break;
            case CE_Fatal:    LFATALC  ("GDAL", msg); break;
        }
    }
}

GdalWrapper* GdalWrapper::_singleton = nullptr;

void GdalWrapper::create(size_t maximumCacheSize, size_t maximumMaximumCacheSize) {
    _singleton = new GdalWrapper(maximumCacheSize, maximumMaximumCacheSize);
}

void GdalWrapper::destroy() {
    ghoul_assert(_singleton, "Cannot delete null");
    delete _singleton;
}

GdalWrapper& GdalWrapper::ref() {
    ghoul_assert(_singleton, "GdalWrapper not created");
    return *_singleton;
}

size_t GDALCacheUsed() {
    return GDALGetCacheUsed64();
}

size_t GDALMaximumCacheSize() {
    return GDALGetCacheMax64();
}

bool GdalWrapper::logGdalErrors() const {
    return _logGdalErrors;
}

GdalWrapper::GdalWrapper(size_t maximumCacheSize, size_t maximumMaximumCacheSize)
    : PropertyOwner({ "GdalWrapper" })
    , _logGdalErrors(LogGdalErrorInfo, false)
    , _gdalMaximumCacheSize (
        GdalMaximumCacheInfo,
        static_cast<int>(maximumCacheSize / (1024ULL * 1024ULL)), // Default
        0,                                          // Minimum: No caching
        static_cast<int>(maximumMaximumCacheSize / (1024ULL * 1024ULL)), // Maximum
        1                                           // Step: One MB
    )
{
    addProperty(_logGdalErrors);
    addProperty(_gdalMaximumCacheSize);

    GDALAllRegister();
    CPLSetConfigOption("GDAL_DATA", absPath("${MODULE_GLOBEBROWSING}/gdal_data").c_str());
    CPLSetConfigOption("CPL_TMPDIR", absPath("${BASE}").c_str());
    CPLSetConfigOption("GDAL_HTTP_UNSAFESSL", "YES");

    CPLSetConfigOption("GDAL_HTTP_TIMEOUT", "3"); // 3 seconds

    setGdalProxyConfiguration();
    CPLSetErrorHandler(gdalErrorHandler);

    _gdalMaximumCacheSize.onChange([&] {
        // MB to Bytes
        GDALSetCacheMax64(
            static_cast<size_t>(_gdalMaximumCacheSize) *
            1024ULL * 1024ULL
        );
    });
}

void GdalWrapper::setGdalProxyConfiguration() {
    if (global::configuration.httpProxy.usingHttpProxy) {
        const std::string address = global::configuration.httpProxy.address;
        const unsigned int port = global::configuration.httpProxy.port;
        const std::string user = global::configuration.httpProxy.user;
        const std::string password = global::configuration.httpProxy.password;
        std::string auth = global::configuration.httpProxy.authentication;
        std::transform(
            auth.begin(),
            auth.end(),
            auth.begin(),
            [](char c) { return static_cast<char>(::toupper(c)); }
        );

        const std::string proxy = address + ":" + std::to_string(port);
        CPLSetConfigOption("GDAL_HTTP_PROXY", proxy.c_str());
        LDEBUG(fmt::format("Using proxy server {}", proxy));

        if (!user.empty() && !password.empty()) {
            std::string userPwd = user + ":" + password;
            CPLSetConfigOption("GDAL_HTTP_PROXYUSERPWD", userPwd.c_str());
            CPLSetConfigOption("GDAL_HTTP_PROXYAUTH", auth.c_str());
            LDEBUG(fmt::format("Using authentication method: {}", auth));
        }
    }
}

} // namespace openspace::globebrowsing

#endif // GLOBEBROWSING_USE_GDAL
