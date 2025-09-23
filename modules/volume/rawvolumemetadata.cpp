/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
#include <optional>

namespace {
    struct [[codegen::Dictionary(RawVolumeMetaData)]] Parameters {
        // Specifies the number of grid cells in each dimension
        glm::ivec3 dimensions;

        // Specifies the unit used to specity the domain
        std::optional<std::string> domainUnit;

        // Specifies the lower domain bounds in the model coordinate system
        std::optional<glm::vec3> lowerDomainBound;

        // Specifies the upper domain bounds in the model coordinate system
        std::optional<glm::vec3> upperDomainBound;

        // Specifies the time on the format YYYY-MM-DDTHH:MM:SS.000Z
        std::optional<std::string> time;

        // Specifies the unit used to specity the value
        std::optional<std::string> valueUnit;

        // Specifies the minimum value stored in the volume
        std::optional<float> minValue;

        // Specifies the maximum value stored in the volume
        std::optional<float> maxValue;
   };
#include "rawvolumemetadata_codegen.cpp"
} // namespace

namespace openspace::volume {

RawVolumeMetadata RawVolumeMetadata::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    RawVolumeMetadata metadata;
    metadata.dimensions = p.dimensions;

    metadata.hasDomainBounds =
        p.lowerDomainBound.has_value() &&
        p.upperDomainBound.has_value();

    if (metadata.hasDomainBounds) {
        metadata.lowerDomainBound = *p.lowerDomainBound;
        metadata.upperDomainBound = *p.upperDomainBound;
    }
    metadata.hasDomainUnit = p.domainUnit.has_value();
    if (metadata.hasDomainUnit) {
        metadata.domainUnit = *p.domainUnit;
    }

    metadata.hasValueRange = p.minValue.has_value() && p.maxValue.has_value();
    if (metadata.hasValueRange) {
        metadata.minValue = *p.minValue;
        metadata.maxValue = *p.maxValue;
    }
    metadata.hasValueUnit = p.valueUnit.has_value();
    if (metadata.hasValueUnit) {
        metadata.valueUnit = *p.valueUnit;
    }

    metadata.hasTime = p.time.has_value();
    if (metadata.hasTime) {
        metadata.time = Time::convertTime(*p.time);
    }

    return metadata;
}

ghoul::Dictionary RawVolumeMetadata::dictionary() const {
    ghoul::Dictionary dict;
    dict.setValue("Dimensions", glm::dvec3(dimensions));
    dict.setValue("GridType", gridTypeToString(gridType));

    if (hasDomainUnit) {
        dict.setValue("DomainUnit", domainUnit);
    }
    if (hasDomainBounds) {
        dict.setValue("LowerDomainBound", glm::dvec3(lowerDomainBound));
        dict.setValue("UpperDomainBound", glm::dvec3(upperDomainBound));
    }

    if (hasValueRange) {
        dict.setValue("MinValue", static_cast<double>(minValue));
        dict.setValue("MaxValue", static_cast<double>(maxValue));
    }
    if (hasDomainUnit) {
        dict.setValue("ValueUnit", valueUnit);
    }

    if (hasTime) {
        std::string_view timeString = Time(time).ISO8601();
        // Do not include time offset in time string
        if (timeString.back() == 'Z') {
            timeString = timeString.substr(0, timeString.size() - 1);
        }
        dict.setValue("Time", std::string(timeString));
    }
    return dict;
}

documentation::Documentation RawVolumeMetadata::Documentation() {
    return codegen::doc<Parameters>("volume_rawvolumemetadata");
}

} // namespace openspace::volume
