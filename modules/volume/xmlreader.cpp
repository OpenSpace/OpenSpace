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
#include <fstream>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/stringhelper.h>
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

void readVTIFile(const std::filesystem::path& path) {
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

    float value;
    while (ss >> value) {
        scalars.push_back(value);
    }

    std::cout << scalars.size();


    //std::ifstream file;
    //file.open(path);
    //ghoul_assert(file.good(), "File handle should be good");

    //std::stringstream ss;
    //std::string line;
    //while (ghoul::getline(file, line)) {
    //    std::cout << line << std::endl;

    //    if (line.find("<ImageData") != std::string::npos) {
    //        const std::string wholeExtent = getAttribute(line, "WholeExtent");
    //        const std::string origin = getAttribute(line, "Origin");
    //        const std::string spacing = getAttribute(line, "Spacing");

    //        std::cout << "Whole extent '" << wholeExtent << "'\n"
    //            << "origin '" << origin << "'\n"
    //            << "spacing '" << spacing << "'\n";
    //    }
    //    else if (line.find("<Piece") != std::string::npos) {
    //        const std::string extent = getAttribute(line, "Extent");
    //        std::cout << "Extent: " << extent << std::endl;
    //    }

    //    else if (line.find("<DataArray") != std::string::npos) {
    //        ghoul::getline(file, line);
    //        ss << line << " ";
    //    }
    //}

    //float value = 0.f;
    //std::vector<float> dataValues;
    //while (ss >> value) {
    //    dataValues.push_back(value);
    //}

    //file.close();
}

} // namespace openspace
