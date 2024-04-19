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

#include <modules/globebrowsing/src/gdalwrapper.h>

#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <ghoul/ghoul.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/stringhelper.h>
#include <cpl_conv.h>
#include <gdal.h>

namespace {
    constexpr std::string_view _loggerCat = "GdalWrapper";

    constexpr openspace::properties::Property::PropertyInfo LogGdalErrorInfo = {
        "LogGdalErrors",
        "Log GDAL errors",
        "If this value is enabled, any error that is raised by GDAL will be logged using "
        "the logmanager. If this value is disabled, any error will be ignored",
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo GdalMaximumCacheInfo = {
        "GdalMaximumCacheSize",
        "GDAL maximum cache size",
        "This function sets the maximum amount of RAM memory in MB that GDAL is "
        "permitted to use for caching",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    void gdalErrorHandler(CPLErr eErrClass, int, const char* msg) {
        // No need to try to do this check earlier and only install this method as an
        // error handler if the logging is desired as the default behavior of GDAL is to
        // log errors to stderr.
        if (openspace::globebrowsing::GdalWrapper::ref().logGdalErrors()) {
            switch (eErrClass) {
                case CE_None: break;
                case CE_Debug:    LDEBUGC("GDAL", msg); break;
                case CE_Warning:  LWARNINGC("GDAL", msg); break;
                case CE_Failure:  LERRORC("GDAL", msg); break;
                case CE_Fatal:    LFATALC("GDAL", msg); break;
            }
        }
    }
} // namespace

namespace openspace::globebrowsing {

GdalWrapper* GdalWrapper::_singleton = nullptr;

void GdalWrapper::create(size_t maximumCacheSize, size_t maximumMaximumCacheSize) {
    _singleton = new GdalWrapper(maximumCacheSize, maximumMaximumCacheSize);
}

void GdalWrapper::destroy() {
    delete _singleton;
}

GdalWrapper& GdalWrapper::ref() {
    ghoul_assert(_singleton, "GdalWrapper not created");
    return *_singleton;
}

bool GdalWrapper::logGdalErrors() const {
    return _logGdalErrors;
}

GdalWrapper::GdalWrapper(size_t maximumCacheSize, size_t maximumMaximumCacheSize)
    : PropertyOwner({ "GdalWrapper", "Gdal Wrapper" })
    , _logGdalErrors(LogGdalErrorInfo, false)
    , _gdalMaximumCacheSize(
        GdalMaximumCacheInfo,
        static_cast<int>(maximumCacheSize / (1024ULL * 1024ULL)), // Default
        0, // Minimum: No caching
        static_cast<int>(maximumMaximumCacheSize / (1024ULL * 1024ULL)), // Maximum
        1 // Step: One MB
    )
{
    ZoneScoped;

    addProperty(_logGdalErrors);
    addProperty(_gdalMaximumCacheSize);

    GDALAllRegister();
    const std::string data = absPath("${MODULE_GLOBEBROWSING}/gdal_data").string();
    CPLSetConfigOption("GDAL_DATA", data.c_str());
    const std::string base = absPath("${BASE}").string();
    CPLSetConfigOption("CPL_TMPDIR", base.c_str());
    CPLSetConfigOption("GDAL_HTTP_UNSAFESSL", "YES");

    CPLSetConfigOption("GDAL_HTTP_TIMEOUT", "3"); // 3 seconds
    CPLSetConfigOption("CURLOPT_TIMEOUT", "3"); // 3 seconds

    setGdalProxyConfiguration();
    CPLSetErrorHandler(gdalErrorHandler);

    _gdalMaximumCacheSize.onChange([this] {
        // MB to Bytes
        GDALSetCacheMax64(
            static_cast<int64_t>(_gdalMaximumCacheSize) * 1024ULL * 1024ULL
        );
    });
}

void GdalWrapper::setGdalProxyConfiguration() {
    if (global::configuration->httpProxy.usingHttpProxy) {
        const std::string address = global::configuration->httpProxy.address;
        const unsigned int port = global::configuration->httpProxy.port;
        const std::string user = global::configuration->httpProxy.user;
        const std::string password = global::configuration->httpProxy.password;
        std::string auth = global::configuration->httpProxy.authentication;
        auth = ghoul::toUpperCase(auth);

        const std::string proxy = address + ":" + std::to_string(port);
        CPLSetConfigOption("GDAL_HTTP_PROXY", proxy.c_str());
        LDEBUG(std::format("Using proxy server '{}'", proxy));

        if (!user.empty() && !password.empty()) {
            const std::string userPwd = user + ":" + password;
            CPLSetConfigOption("GDAL_HTTP_PROXYUSERPWD", userPwd.c_str());
            CPLSetConfigOption("GDAL_HTTP_PROXYAUTH", auth.c_str());
            LDEBUG(std::format("Using authentication method: {}", auth));
        }
    }
}

} // namespace openspace::globebrowsing
