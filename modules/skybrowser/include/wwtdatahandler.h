/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___WWTDATAHANDLER___H__
#define __OPENSPACE_MODULE_SKYBROWSER___WWTDATAHANDLER___H__

#include <modules/space/speckloader.h>
#include <openspace/documentation/documentation.h>
#include <unordered_map>

#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsuggest-override"
#pragma GCC diagnostic ignored "-Wzero-as-null-pointer-constant"
#endif

#include <modules/skybrowser/ext/tinyxml2/tinyxml2.h>

#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic pop
#endif

namespace openspace::documentation { struct Documentation; }

namespace openspace::wwt {
    const std::string Thumbnail = "Thumbnail";
    const std::string Name = "Name";
    const std::string ImageSet = "ImageSet";
    const std::string Dec = "Dec";
    const std::string RA = "RA";
    const std::string Undefined = "";
    const std::string Folder = "Folder";
    const std::string Place = "Place";
    const std::string ThumbnailUrl = "ThumbnailUrl";
    const std::string Url = "Url";
    const std::string Credits = "Credits";
    const std::string CreditsUrl = "CreditsUrl";
    const std::string ZoomLevel = "ZoomLevel";
    const std::string DataSetType = "DataSetType";
    const std::string Sky = "Sky";
} // namespace openspace::wwt

namespace openspace {

struct ImageData {
    std::string name = wwt::Undefined;
    std::string thumbnailUrl = wwt::Undefined;
    std::string imageUrl = wwt::Undefined;
    std::string credits = wwt::Undefined;
    std::string creditsUrl = wwt::Undefined;
    std::string collection = wwt::Undefined;
    bool hasCelestialCoords = false;
    float fov = 0.f;
    glm::dvec2 equatorialSpherical = glm::dvec2(0.0);
    glm::dvec3 equatorialCartesian = glm::dvec3(0.0);
};

class WwtDataHandler {
public:
    WwtDataHandler() = default;
    ~WwtDataHandler();

    void loadImages(const std::string& root, const std::filesystem::path& directory);
    int nLoadedImages() const;
    const ImageData& getImage(int i) const;

private:
    void saveImageFromNode(tinyxml2::XMLElement* node, std::string collection);
    void saveImagesFromXml(tinyxml2::XMLElement* root, std::string collection);

    // Images
    std::vector<ImageData> _images;
    std::vector<tinyxml2::XMLDocument*> _xmls;
};
} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___WWTDATAHANDLER___H__

