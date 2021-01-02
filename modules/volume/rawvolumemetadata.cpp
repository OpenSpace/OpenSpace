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

#include <modules/volume/rawvolumemetadata.h>

#include <openspace/documentation/verifier.h>
#include <openspace/documentation/documentation.h>
#include <openspace/util/time.h>
#include <ghoul/misc/dictionary.h>

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
    metadata.dimensions = dictionary.value<glm::dvec3>(KeyDimensions);

    metadata.hasDomainBounds = dictionary.hasValue<glm::dvec3>(KeyLowerDomainBound) &&
            dictionary.hasValue<glm::dvec3>(KeyUpperDomainBound);

    if (metadata.hasDomainBounds) {
        metadata.lowerDomainBound = dictionary.value<glm::dvec3>(KeyLowerDomainBound);
        metadata.upperDomainBound = dictionary.value<glm::dvec3>(KeyUpperDomainBound);
    }
    metadata.hasDomainUnit = static_cast<float>(
        dictionary.hasValue<double>(KeyDomainUnit)
    );
    if (metadata.hasDomainUnit) {
        metadata.domainUnit = dictionary.value<std::string>(KeyDomainUnit);
    }

    metadata.hasValueRange = dictionary.hasValue<double>(KeyMinValue) &&
        dictionary.hasValue<double>(KeyMaxValue);

    if (metadata.hasValueRange) {
        metadata.minValue = static_cast<float>(dictionary.value<double>(KeyMinValue));
        metadata.maxValue = static_cast<float>(dictionary.value<double>(KeyMaxValue));
    }
    metadata.hasValueUnit = static_cast<float>(dictionary.hasValue<double>(KeyValueUnit));
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
    dict.setValue(KeyDimensions, glm::dvec3(dimensions));
    dict.setValue(KeyGridType, gridTypeToString(gridType));

    if (hasDomainUnit) {
        dict.setValue(KeyDomainUnit, domainUnit);
    }
    if (hasDomainBounds) {
        dict.setValue(KeyLowerDomainBound, glm::dvec3(lowerDomainBound));
        dict.setValue(KeyUpperDomainBound, glm::dvec3(upperDomainBound));
    }

    if (hasValueRange) {
        dict.setValue(KeyMinValue, static_cast<double>(minValue));
        dict.setValue(KeyMaxValue, static_cast<double>(maxValue));
    }
    if (hasDomainUnit) {
        dict.setValue(KeyValueUnit, valueUnit);
    }

    if (hasTime) {
        std::string_view timeString = Time(time).ISO8601();
        // Do not include time offset in time string
        if (timeString.back() == 'Z') {
            timeString = timeString.substr(0, timeString.size() - 1);
        }
        dict.setValue(KeyTime, std::string(timeString));
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
                new DoubleVector3Verifier,
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
                new DoubleVector3Verifier,
                Optional::Yes,
                "Specifies the lower domain bounds in the model coordinate system",
            },
            {
                KeyUpperDomainBound,
                new DoubleVector3Verifier,
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
