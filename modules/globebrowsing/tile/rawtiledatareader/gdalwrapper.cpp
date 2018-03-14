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

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/configurationmanager.h>

#include <ghoul/filesystem/filesystem.h> // abspath
#include <ghoul/ghoul.h>
#include <ghoul/logging/consolelog.h>


#ifdef WIN32
#pragma warning (push)
#pragma warning (disable : 4251) // needs to have dll-interface to be used by clients
#endif // WIN32

#include <gdal_priv.h>

#ifdef WIN32
#pragma warning (pop)
#endif // WIN32

namespace {
    constexpr const char* _loggerCat = "GdalWrapper";

    static const openspace::properties::Property::PropertyInfo LogGdalErrorInfo = {
        "LogGdalErrors",
        "Log GDAL errors",
        "If this value is enabled, any error that is raised by GDAL will be logged using "
        "the logmanager. If this value is disabled, any error will be ignored."
    };

    static const openspace::properties::Property::PropertyInfo GdalMaximumCacheInfo = {
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
std::mutex GdalWrapper::_mutexLock;

void GdalWrapper::create(size_t maximumCacheSize, size_t maximumMaximumCacheSize) {
    std::lock_guard<std::mutex> guard(_mutexLock);
    _singleton = new GdalWrapper(maximumCacheSize, maximumMaximumCacheSize);
}

void GdalWrapper::destroy() {
    std::lock_guard<std::mutex> guard(_mutexLock);
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
    ghoul::Dictionary proxySettings;
    bool proxyEnabled = OsEng.configurationManager().getValue(
        ConfigurationManager::KeyHttpProxy, proxySettings
    );
    if (proxyEnabled) {
        std::string proxyAddress, proxyPort, proxyUser, proxyPassword,
            proxyAuth;

        bool success = proxySettings.getValue(
            ConfigurationManager::PartHttpProxyAddress,
            proxyAddress
        );
        success &= proxySettings.getValue(
            ConfigurationManager::PartHttpProxyPort,
            proxyPort
        );
        proxySettings.getValue(
            ConfigurationManager::PartHttpProxyAuthentication,
            proxyAuth
        );

        std::string proxyAuthString = "BASIC";
        if (proxyAuth == "basic" || proxyAuth == "") {
            proxyAuthString = "BASIC";
        } else if (proxyAuth == "ntlm") {
            proxyAuthString = "NTLM";
        } else if (proxyAuth == "digest") {
            proxyAuthString = "DIGEST";
        } else if (proxyAuth == "any") {
            proxyAuthString = "ANY";
        } else {
            success = false;
        }

        bool userAndPassword = proxySettings.getValue(
            ConfigurationManager::PartHttpProxyUser,
            proxyUser
        );
        userAndPassword &= proxySettings.getValue(
            ConfigurationManager::PartHttpProxyPassword,
            proxyPassword
        );

        if (success) {
            std::string proxy = proxyAddress + ":" + proxyPort;
            CPLSetConfigOption("GDAL_HTTP_PROXY", proxy.c_str());
            LDEBUG(fmt::format("Using proxy server {}", proxy));
            if (userAndPassword) {
                std::string proxyUserPwd = proxyUser + ":" + proxyPassword;
                CPLSetConfigOption("GDAL_HTTP_PROXYUSERPWD", proxyUserPwd.c_str());
                CPLSetConfigOption("GDAL_HTTP_PROXYAUTH", proxyAuthString.c_str());
                LDEBUG(fmt::format("Using authentication method: {}", proxyAuthString));
            }
        } else {
            LERROR("Invalid proxy settings for GDAL");
        }
    } else {
        LDEBUG("Setting up GDAL without proxy server");
    }
}

} // namespace openspace::globebrowsing

#endif // GLOBEBROWSING_USE_GDAL
