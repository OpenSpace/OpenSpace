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

#include <modules/volume/xmlreader.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/stringhelper.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <tinyxml2.h>


namespace openspace {

std::string getAttribute(const std::string& line, const std::string& attribute) {
    size_t pos = line.find(attribute + "=\"");
    if (pos == std::string::npos) {
        return "";
    }
    pos += attribute.length() + 2;
    const size_t end = line.find("\"", pos);
    if (pos == std::string::npos) {
        return "";
    }

    return line.substr(pos, end - pos);

}

std::pair<volume::RawVolumeMetadata, std::vector<float>> readVTIFile(
    const std::filesystem::path& path, double timestep)
{
    tinyxml2::XMLDocument doc;

    if (doc.LoadFile(path.string().c_str()) != tinyxml2::XML_SUCCESS) {
        throw ghoul::RuntimeError(std::format("Failed to load .vti file '{}'", path.string()));
    }

    tinyxml2::XMLElement* root = doc.RootElement();
    tinyxml2::XMLElement* imageData = root->FirstChildElement("ImageData");
    tinyxml2::XMLElement* dataArray =
        imageData->FirstChildElement("Piece")
        ->FirstChildElement("PointData")
        ->FirstChildElement("DataArray");

    const char* wholeExtent = imageData->Attribute("WholeExtent");
    const char* origin = imageData->Attribute("Origin");
    const char* spacing = imageData->Attribute("Spacing");

    const char* dataText = dataArray->GetText();


    std::stringstream ss(wholeExtent);

    glm::ivec3 minExtent, maxExtent;
    ss >> minExtent.x >> maxExtent.x >> minExtent.y >> maxExtent.y >> minExtent.z >> maxExtent.z;
    glm::ivec3 extents = maxExtent - minExtent + 1;


    const int totalSize = extents.x * extents.y * extents.z;

    std::vector<float> scalars;
    scalars.reserve(totalSize);

    ss.str(dataText);
    ss.clear();

    float minValue = std::numeric_limits<float>::max();
    float maxValue = std::numeric_limits<float>::lowest();
    std::string v;
    while (ss >> v) {
        try {
            float value = std::stof(v);
            scalars.push_back(value);
            if (!std::isinf(value) && !std::isnan(value)) {
                minValue = std::min(minValue, value);
                maxValue = std::max(maxValue, value);
            }
        }
        catch (std::invalid_argument e) {
            LWARNINGC("XmlReader", std::format("Cannot read value of '{}'", v));
        }
    }

    volume::RawVolumeMetadata metadata;

    metadata.dimensions = extents;

    metadata.hasDomainBounds = true;
    metadata.lowerDomainBound = minExtent;
    metadata.upperDomainBound = maxExtent;

    metadata.hasValueRange = true;
    metadata.minValue = minValue;
    metadata.maxValue = maxValue;

    metadata.hasTime = true;
    metadata.time = timestep;

    return std::make_pair(metadata, scalars);
}

} // namespace openspace
