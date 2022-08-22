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

#include <modules/skybrowser/include/wwtdatahandler.h>

#include <modules/skybrowser/include/utility.h>
#include <openspace/util/httprequest.h>
#include <ghoul/logging/logmanager.h>
#include <string_view>

#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsuggest-override"
#pragma GCC diagnostic ignored "-Wzero-as-null-pointer-constant"
#endif

#include <modules/skybrowser/ext/tinyxml2/tinyxml2.h>

#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC diagnostic pop
#endif

namespace {
    constexpr std::string_view _loggerCat = "WwtDataHandler";

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

    bool hasAttribute(const tinyxml2::XMLElement* element, std::string_view name) {
        std::string n = std::string(name);
        return element->FindAttribute(n.c_str());
    }

    std::string attribute(const tinyxml2::XMLElement* element, std::string_view name) {
        if (hasAttribute(element, name)) {
            std::string n = std::string(name);
            return element->FindAttribute(n.c_str())->Value();
        }
        return std::string(Undefined);
    }

    // Parsing and downloading of wtml files
    bool downloadFile(const std::string& url, const std::filesystem::path& destination) {
        using namespace openspace;

        HttpFileDownload wtmlRoot(url, destination, HttpFileDownload::Overwrite::Yes);
        wtmlRoot.start(std::chrono::milliseconds(10000));
        return wtmlRoot.wait();
    }

    bool directoryExists(const std::filesystem::path& path) {
        return std::filesystem::exists(path) && std::filesystem::is_directory(path);
    }

    const tinyxml2::XMLElement* directChildNode(const tinyxml2::XMLElement* node,
                                                std::string_view name)
    {
        while (node && node->Name() != name) {
            node = node->FirstChildElement();
        }
        return node;
    }

    const tinyxml2::XMLElement* childNode(const tinyxml2::XMLElement* node,
                                          std::string_view name)
    {
        const tinyxml2::XMLElement* child = node->FirstChildElement();

        // Traverse the children and look at all their first child to find ImageSet
        while (child) {
            const tinyxml2::XMLElement* imageSet = directChildNode(child, name);
            if (imageSet) {
                return imageSet;
            }
            child = child->NextSiblingElement();
        }
        return nullptr;
    }

    std::string childNodeContentFromImageSet(const tinyxml2::XMLElement* imageSet,
                                             std::string_view elementName)
    {
        // Find the thumbnail image url
        // The thumbnail is the last node so traverse backwards for speed
        std::string n = std::string(elementName);
        const tinyxml2::XMLElement* child = imageSet->FirstChildElement(n.c_str());
        return child && child->GetText() ? child->GetText() : std::string(Undefined);
    }

    std::string urlFromPlace(const tinyxml2::XMLElement* place) {
        // If the place has a thumbnail url, return it
        if (hasAttribute(place, Thumbnail)) {
            return attribute(place, Thumbnail);
        }

        // If the place doesn't have a thumbnail url data attribute,
        // Load the image set it stores instead
        const tinyxml2::XMLElement* imageSet = childNode(place, ImageSet);

        // If there is an imageSet, collect thumbnail url, if it doesn't contain an
        // ImageSet, it doesn't have an url
        return imageSet ?
            childNodeContentFromImageSet(imageSet, ThumbnailUrl) :
            std::string(Undefined);
    }

    bool downloadWtmlFiles(const std::filesystem::path& directory, const std::string& url,
                           const std::string& fileName)
    {
        using namespace openspace;
        // Download file from url
        std::filesystem::path file = directory.string() + fileName + ".aspx";
        const bool success = downloadFile(url, file);
        if (!success) {
            LINFO(fmt::format(
                "Could not download file '{}' to directory {}", url, directory
            ));
            return false;
        }

        // Parse file to XML
        auto document = std::make_unique<tinyxml2::XMLDocument>();
        document->LoadFile(file.string().c_str());

        // Search XML file for folders with urls
        const tinyxml2::XMLElement* root = document->RootElement();
        const tinyxml2::XMLElement* element = root->FirstChildElement(Folder.data());
        const bool folderExists = element != nullptr;
        const bool folderContainNoUrls = folderExists && !hasAttribute(element, Url);

        // If the file contains no folders, or there are folders but without urls,
        // stop recursion
        if (!folderExists || folderContainNoUrls) {
            LINFO(fmt::format("Saving {}", url));
            return true;
        }

        // Iterate through all the folders in the XML file
        while (element && std::string(element->Value()) == Folder) {
            // If folder contains urls, download and parse those urls
            if (hasAttribute(element, Url) && hasAttribute(element, Name)) {
                std::string urlAttr = attribute(element, Url);
                std::string fileNameAttr = attribute(element, Name);
                downloadWtmlFiles(directory, urlAttr, fileNameAttr);
            }
            element = element->NextSiblingElement();
        }
        return true;
    }

    std::optional<openspace::ImageData> loadImageFromNode(
                                                         const tinyxml2::XMLElement* node,
                                                                   std::string collection)
    {
        using namespace openspace;

        // Collect the image set of the node. The structure is different depending on if
        // it is a Place or an ImageSet
        std::string thumbnailUrl = std::string(Undefined);
        const tinyxml2::XMLElement* imageSet = nullptr;
        std::string type = node->Name();

        if (type == ImageSet) {
            thumbnailUrl = childNodeContentFromImageSet(node, ThumbnailUrl);
            imageSet = node;
        }
        else if (type == Place) {
            thumbnailUrl = urlFromPlace(node);
            imageSet = childNode(node, ImageSet);
        }

        // Only collect the images that have a thumbnail image, that are sky images and
        // that have an image
        const bool hasThumbnailUrl = thumbnailUrl != Undefined;
        const bool isSkyImage = attribute(node, DataSetType) == Sky;
        const bool hasImageUrl = imageSet ? hasAttribute(imageSet, Url) : false;

        if (!(hasThumbnailUrl && isSkyImage && hasImageUrl)) {
            return std::nullopt;
        }

        // Collect name, image url and credits
        std::string name = attribute(node, Name);
        if (std::islower(name[0])) {
            // convert string to upper case
            name[0] = static_cast<char>(std::toupper(name[0]));
        }

        std::string imageUrl = attribute(imageSet, Url);
        std::string credits = childNodeContentFromImageSet(imageSet, Credits);
        std::string creditsUrl = childNodeContentFromImageSet(imageSet, CreditsUrl);

        // Collect equatorial coordinates. All-sky surveys do not have these coordinates
        bool hasCelestialCoords = hasAttribute(node, RA) && hasAttribute(node, Dec);
        glm::dvec2 equatorialSpherical = glm::dvec2(0.0);
        glm::dvec3 equatorialCartesian = glm::dvec3(0.0);

        if (hasCelestialCoords) {
            // The RA from WWT is in the unit hours:
            // to convert to degrees, multiply with 360 (deg) /24 (h) = 15
            double ra = 15.0 * std::stod(attribute(node, RA));
            double dec = std::stod(attribute(node, Dec));
            equatorialSpherical = glm::dvec2(ra, dec);
            equatorialCartesian = skybrowser::sphericalToCartesian(equatorialSpherical);
        }

        // Collect field of view. The WWT definition of ZoomLevel is: VFOV = ZoomLevel / 6
        float fov = 0.f;
        if (hasAttribute(node, ZoomLevel)) {
            fov = std::stof(attribute(node, ZoomLevel)) / 6.f;
        }

        return ImageData{
            name,
            thumbnailUrl,
            imageUrl,
            credits,
            creditsUrl,
            collection,
            hasCelestialCoords,
            fov,
            equatorialSpherical,
            equatorialCartesian
        };
    }
} //namespace

namespace openspace {

void WwtDataHandler::loadImages(const std::string& root,
                                const std::filesystem::path& directory)
{
    // Steps to download new images
    // 1. Create the target directory if it doesn't already exist
    // 2. If the 'root' has an associated hash file, download and compare it with the
    //    local file. If the hash has changed, nuke the folder
    // 3. If the folder is empty, download files

    // 1.
    if (!directoryExists(directory)) {
        LINFO(fmt::format("Creating directory {}", directory));
        std::filesystem::create_directory(directory);
    }

    // Get the hash from the remote. If no such hash exists, the remoteHash will be empty
    std::string remoteHash;
    {
        std::string remoteHashFile = root.substr(0, root.find_last_of('/')) + "/hash.md5";
        bool success = downloadFile(remoteHashFile, directory / "hash.tmp");
        // The hash download might fail if the provided 'root' does not have a hash
        // in which case we assume that the underlying data has not changed
        if (success) {
            std::ifstream(directory / "hash.tmp") >> remoteHash;
            std::filesystem::remove(directory / "hash.tmp");
        }
    }

    // Load the local hash. If no such hash exists, the localHash will be empty
    std::string localHash;
    std::filesystem::path localHashFile = directory / "hash.md5";
    if (std::filesystem::exists(localHashFile)) {
        std::ifstream(localHashFile) >> localHash;
    }
    
    // Check if the hash has changed. This will be ignored if either the local of remote
    // hash does not exist
    if (!localHash.empty() && !remoteHash.empty() && localHash != remoteHash) {
        LINFO(fmt::format(
            "Local hash '{}' differs from remote hash '{}'. Cleaning directory",
            localHash, remoteHash
        ));

        std::filesystem::remove_all(directory);
        std::filesystem::create_directory(directory);
    }
    
    // If there is no directory (either because it is the first start, or the previous
    // contents were deleted because of a change in hash) we have to download the files
    if (std::filesystem::is_empty(directory)) {
        LINFO("Loading images from url");
        downloadWtmlFiles(directory, root, "root");
        std::ofstream(localHashFile) << remoteHash;
    }

    // Finally, we can load the files that are now on disk
    LINFO("Loading images from directory");
    for (const auto& entry : std::filesystem::directory_iterator(directory)) {
        tinyxml2::XMLDocument document;
        std::string path = entry.path().string();
        tinyxml2::XMLError successCode = document.LoadFile(path.c_str());

        if (successCode == tinyxml2::XMLError::XML_SUCCESS) {
            tinyxml2::XMLElement* rootNode = document.FirstChildElement();
            std::string collectionName = attribute(rootNode, Name);
            saveImagesFromXml(rootNode, collectionName);
        }
    }

    // Sort images in alphabetical order
    std::sort(
        _images.begin(),
        _images.end(),
        [](ImageData& a, ImageData& b) { return a.name < b.name; }
    );

    LINFO(fmt::format("Loaded {} WorldWide Telescope images", _images.size()));
}

int WwtDataHandler::nLoadedImages() const {
    return static_cast<int>(_images.size());
}

const ImageData& WwtDataHandler::image(int i) const {
    ghoul_assert(i < static_cast<int>(_images.size()), "Index outside of vector size");
    return _images[i];
}

void WwtDataHandler::saveImagesFromXml(const tinyxml2::XMLElement* root,
                                       std::string collection)
{
    // Get direct child of node called Place
    const tinyxml2::XMLElement* node = root->FirstChildElement();

    // Iterate through all siblings of node. If sibling is folder, open recursively.
    // If sibling is image, save it.
    while (node) {
        const std::string name = node->Name();
        // If node is an image or place, load it
        if (name == ImageSet || name == Place) {
            std::optional<ImageData> image = loadImageFromNode(node, collection);
            if (image.has_value()) {
                _images.push_back(std::move(*image));
            }

        }
        // If node is another folder, open recursively
        else if (name == Folder) {
            std::string nodeName = attribute(node, Name);
            std::string newCollectionName = fmt::format("{}/{}", collection, nodeName);
            saveImagesFromXml(node, newCollectionName);
        }
        node = node->NextSiblingElement();
    }
}

} // namespace openspace
