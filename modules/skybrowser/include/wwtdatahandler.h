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

#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsuggest-override"
#pragma GCC diagnostic ignored "-Wzero-as-null-pointer-constant"
#endif

#include <modules/skybrowser/ext/tinyxml2/tinyxml2.h>

#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic pop
#endif

#include <filesystem>
#include <memory>
#include <string_view>

namespace openspace {

namespace documentation { struct Documentation; }

namespace wwt {
    constexpr std::string_view Thumbnail = "Thumbnail";
    constexpr std::string_view Name = "Name";
    constexpr std::string_view ImageSet = "ImageSet";
    constexpr std::string_view Dec = "Dec";
    constexpr std::string_view RA = "RA";
    constexpr std::string_view Undefined = "";
    constexpr std::string_view Folder = "Folder";
    constexpr std::string_view Place = "Place";
    constexpr std::string_view ThumbnailUrl = "ThumbnailUrl";
    constexpr std::string_view Url = "Url";
    constexpr std::string_view Credits = "Credits";
    constexpr std::string_view CreditsUrl = "CreditsUrl";
    constexpr std::string_view ZoomLevel = "ZoomLevel";
    constexpr std::string_view DataSetType = "DataSetType";
    constexpr std::string_view Sky = "Sky";
} // namespace wwt

struct ImageData {
    std::string name;
    std::string thumbnailUrl;
    std::string imageUrl;
    std::string credits;
    std::string creditsUrl;
    std::string collection;
    bool hasCelestialCoords = false;
    float fov = 0.f;
    glm::dvec2 equatorialSpherical = glm::dvec2(0.0);
    glm::dvec3 equatorialCartesian = glm::dvec3(0.0);
};

class WwtDataHandler {
public:
    WwtDataHandler() = default;

    void loadImages(const std::string& root, const std::filesystem::path& directory);
    int nLoadedImages() const;
    const ImageData& image(int i) const;

private:
    void saveImageFromNode(tinyxml2::XMLElement* node, std::string collection);
    void saveImagesFromXml(tinyxml2::XMLElement* root, std::string collection);

    // Images
    std::vector<ImageData> _images;
    std::vector<std::unique_ptr<tinyxml2::XMLDocument>> _xmls;
};
} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___WWTDATAHANDLER___H__

