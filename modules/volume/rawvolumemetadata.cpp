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

#include <modules/volume/rawvolumemetadata.h>

#include <openspace/documentation/verifier.h>

#include <openspace/util/time.h>

namespace {
    constexpr const char* KeyDimensions = "Dimensions";
    constexpr const char* KeyLowerDomainBound = "LowerDomainBound";
    constexpr const char* KeyUpperDomainBound = "UpperDomainBound";

    constexpr const char* KeyMinValue = "MinValue";
    constexpr const char* KeyMaxValue = "MaxValue";

    constexpr const char* KeyTime = "Time";
    constexpr const char* KeyDomainUnit = "DomainUnit";
    constexpr const char* KeyValueUnit = "ValueUnit";

    constexpr const char* KeyGridType = "GridType";
} // namespace

namespace openspace::volume {

RawVolumeMetadata RawVolumeMetadata::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RawVolumeMetadata"
    );

    RawVolumeMetadata metadata;
    metadata.dimensions = dictionary.value<glm::vec3>(KeyDimensions);

    metadata.hasDomainBounds = dictionary.hasValue<glm::vec3>(KeyLowerDomainBound) &&
            dictionary.hasValue<glm::vec3>(KeyUpperDomainBound);

    if (metadata.hasDomainBounds) {
        metadata.lowerDomainBound = dictionary.value<glm::vec3>(KeyLowerDomainBound);
        metadata.upperDomainBound = dictionary.value<glm::vec3>(KeyUpperDomainBound);
    }
    metadata.hasDomainUnit = dictionary.hasValue<float>(KeyDomainUnit);
    if (metadata.hasDomainUnit) {
        metadata.domainUnit = dictionary.value<std::string>(KeyDomainUnit);
    }

    metadata.hasValueRange = dictionary.hasValue<float>(KeyMinValue) &&
        dictionary.hasValue<float>(KeyMaxValue);

    if (metadata.hasValueRange) {
        metadata.minValue = dictionary.value<float>(KeyMinValue);
        metadata.maxValue = dictionary.value<float>(KeyMaxValue);
    }
    metadata.hasValueUnit = dictionary.hasValue<float>(KeyValueUnit);
    if (metadata.hasValueUnit) {
        metadata.valueUnit = dictionary.value<std::string>(KeyValueUnit);
    }

    metadata.hasTime = dictionary.hasValue<std::string>(KeyTime);
    if (metadata.hasTime) {
        std::string timeString = dictionary.value<std::string>(KeyTime);
        metadata.time = Time::convertTime(timeString);
    }

    return metadata;
}

ghoul::Dictionary RawVolumeMetadata::dictionary() {
    ghoul::Dictionary dict;
    dict.setValue<glm::vec3>(KeyDimensions, dimensions);
    dict.setValue<std::string>(KeyGridType, gridTypeToString(gridType));

    if (hasDomainUnit) {
        dict.setValue<std::string>(KeyDomainUnit, domainUnit);
    }
    if (hasDomainBounds) {
        dict.setValue<glm::vec3>(KeyLowerDomainBound, lowerDomainBound);
        dict.setValue<glm::vec3>(KeyUpperDomainBound, upperDomainBound);
    }

    if (hasValueRange) {
        dict.setValue<double>(KeyMinValue, minValue);
        dict.setValue<double>(KeyMaxValue, maxValue);
    }
    if (hasDomainUnit) {
        dict.setValue<std::string>(KeyValueUnit, valueUnit);
    }

    if (hasTime) {
        std::string timeString = Time(time).ISO8601();
        // Do not include time offset in time string
        if (timeString.back() == 'Z') {
            timeString.pop_back();
        }
        dict.setValue<std::string>(KeyTime, timeString);
    }
    return dict;
}

documentation::Documentation RawVolumeMetadata::Documentation() {
    using namespace documentation;
    return {
        "RawVolumeMetadata",
        "volume_rawvolumemetadata",
        {
            {
                KeyDimensions,
                new Vector3Verifier<float>,
                Optional::No,
                "Specifies the number of grid cells in each dimension",
            },
            {
                KeyDomainUnit,
                new StringVerifier,
                Optional::Yes,
                "Specifies the unit used to specity the domain",
            },
            {
                KeyLowerDomainBound,
                new Vector3Verifier<float>,
                Optional::Yes,
                "Specifies the lower domain bounds in the model coordinate system",
            },
            {
                KeyUpperDomainBound,
                new Vector3Verifier<float>,
                Optional::Yes,
                "Specifies the upper domain bounds in the model coordinate system",
            },
            {
                KeyTime,
                new StringVerifier,
                Optional::Yes,
                "Specifies the time on the format YYYY-MM-DDTHH:MM:SS.000Z",
            },
            {
                KeyValueUnit,
                new StringVerifier,
                Optional::Yes,
                "Specifies the unit used to specity the value",
            },
            {
                KeyMinValue,
                new DoubleVerifier,
                Optional::Yes,
                "Specifies the minimum value stored in the volume"
            },
            {
                KeyMaxValue,
                new DoubleVerifier,
                Optional::Yes,
                "Specifies the maximum value stored in the volume"
            }
        }
    };
}

} // namespace openspace::volume
