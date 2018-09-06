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

#include <openspace/scene/scenelicense.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

namespace {
    constexpr const char* LicenseKeyName = "Name";
    constexpr const char* LicenseKeyAttribution = "Attribution";
    constexpr const char* LicenseKeyUrl = "URL";
    constexpr const char* LicenseKeyLicenseText = "License";
} // namespace

namespace openspace {

documentation::Documentation SceneLicense::Documentation() {
    using namespace documentation;

    return {
        "License Information",
        "core_license",
        {
            {
                LicenseKeyName,
                new StringVerifier,
                Optional::No,
                "A short, descriptive name for the license employed for this node."
            },
            {
                LicenseKeyAttribution,
                new StringVerifier,
                Optional::No,
                "The organization that shall be attributed to the licensed content."
            },
            {
                LicenseKeyUrl,
                new StringVerifier,
                Optional::Yes,
                "The URL pointing to the original license."
            },
            {
                LicenseKeyLicenseText,
                new StringVerifier,
                Optional::No,
                "The full text of the license agreements."
            }
        }
    };
}

SceneLicense::SceneLicense(const ghoul::Dictionary& dictionary, std::string m)
    : module(std::move(m))
{
    ghoul_assert(!module.empty(), "Module name must not be empty");

    documentation::testSpecificationAndThrow(Documentation(), dictionary, "SceneLicense");

    name = dictionary.value<std::string>(LicenseKeyName);
    attribution = dictionary.value<std::string>(LicenseKeyAttribution);
    dictionary.getValue(LicenseKeyUrl, url);
    licenseText = dictionary.value<std::string>(LicenseKeyLicenseText);
}

} // namespace openspace
