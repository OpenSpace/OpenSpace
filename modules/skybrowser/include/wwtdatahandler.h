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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___WWTDATAHANDLER___H__
#define __OPENSPACE_MODULE_SKYBROWSER___WWTDATAHANDLER___H__

#include <filesystem>
#include <memory>
#include <optional>

namespace tinyxml2 { class XMLElement; }

namespace openspace {

namespace documentation { struct Documentation; }

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
    std::string identifier;
};

class WwtDataHandler {
public:
    void loadImages(const std::string& root, const std::filesystem::path& directory);
    int nLoadedImages() const;
    std::optional<const ImageData> image(const std::string& imageUrl) const;
    const std::map<std::string, ImageData>& images() const;

private:
    void saveImagesFromXml(const tinyxml2::XMLElement* root, std::string collection);

    // Images
    std::map<std::string, ImageData> _images;
};
} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___WWTDATAHANDLER___H__

