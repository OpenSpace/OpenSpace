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
#include <modules/space/speckloader.h>
#include <openspace/util/httprequest.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <algorithm>
#include <filesystem>
#include <sys/types.h>
#include <sys/stat.h>

namespace {
    constexpr const char* _loggerCat = "WwtDataHandler";
} //namespace

namespace openspace {

bool hasAttribute(const tinyxml2::XMLElement* element, const std::string_view& name) {
    return element->FindAttribute(std::string(name).c_str());
}

std::string attribute(const tinyxml2::XMLElement* element, const std::string& name) {
    if (hasAttribute(element, name)) {
        return element->FindAttribute(name.c_str())->Value();
    }
    return wwt::Undefined;
}

// Parsing and downloading of wtml files
bool downloadFile(const std::string& url, const std::filesystem::path& fileDestination) {
    // Get the web page and save to file
    HttpFileDownload wtmlRoot(
        url,
        fileDestination,
        HttpFileDownload::Overwrite::Yes
    );
    wtmlRoot.start(std::chrono::milliseconds(10000));
    return wtmlRoot.wait();
}

bool directoryExists(const std::filesystem::path& path) {
    return  std::filesystem::exists(path) && std::filesystem::is_directory(path);
}

std::string createSearchableString(std::string str) {
    // Remove white spaces and all special characters
    str.erase(
        std::remove_if(
            str.begin(), str.end(),
            [](char c) {
                const bool isNumberOrLetter = std::isdigit(c) || std::isalpha(c);
                return !isNumberOrLetter;
            }
        ),
        str.end()
    );
    // Make the word lower case
    std::transform(
        str.begin(), str.end(),
        str.begin(),
        [](char c) { return static_cast<char>(std::tolower(c)); }
    );
    return str;
}

tinyxml2::XMLElement* getDirectChildNode(tinyxml2::XMLElement* node,
                                         const std::string& name)
{
    while (node && node->Name() != name) {
        node = node->FirstChildElement();
    }
    return node;
}

tinyxml2::XMLElement* getChildNode(tinyxml2::XMLElement* node,
                                   const std::string& name)
{
    tinyxml2::XMLElement* child = node->FirstChildElement();

    // Traverse the children and look at all their first child to find ImageSet
    while (child) {
        tinyxml2::XMLElement* imageSet = getDirectChildNode(child, name);
        // Found
        if (imageSet) {
            return imageSet;
        }
        child = child->NextSiblingElement();
    }
    return nullptr;
}

std::string getChildNodeContentFromImageSet(tinyxml2::XMLElement* imageSet,
                                            const std::string& elementName)
{
    // Find the thumbnail image url
    // The thumbnail is the last node so traverse backwards for speed
    tinyxml2::XMLElement* imageSetChild =
        imageSet->FirstChildElement(elementName.c_str());

    if (imageSetChild && imageSetChild->GetText()) {
        return imageSetChild->GetText();
    }
    else {
        return wwt::Undefined;
    }
}

std::string getUrlFromPlace(tinyxml2::XMLElement* place) {
    // If the place has a thumbnail url, return it
    if (hasAttribute(place, wwt::Thumbnail)) {
        return attribute(place, wwt::Thumbnail);
    }

    // If the place doesn't have a thumbnail url data attribute,
    // Load the image set it stores instead
    tinyxml2::XMLElement* imageSet = getChildNode(place, wwt::ImageSet);

    // If there is an imageSet, collect thumbnail url
    if (imageSet) {
        return getChildNodeContentFromImageSet(imageSet, wwt::ThumbnailUrl);
    }
    else {
        // If it doesn't contain an ImageSet, it doesn't have an url
        return wwt::Undefined;
    }
}

void parseWtmlsFromDisc(std::vector<tinyxml2::XMLDocument*>& xmls,
                        const std::filesystem::path& directory)
{
    for (const auto& entry : std::filesystem::directory_iterator(directory)) {
        tinyxml2::XMLDocument* document = new tinyxml2::XMLDocument();
        std::string path = entry.path().string();
        tinyxml2::XMLError successCode = document->LoadFile(path.c_str());

        if (successCode == tinyxml2::XMLError::XML_SUCCESS) {
            xmls.push_back(document);
        }
    }
}

bool downloadAndParseWtmlFilesFromUrl(std::vector<tinyxml2::XMLDocument*>& xmls,
                                      const std::filesystem::path& directory,
                                      const std::string& url, const std::string& fileName)
{
    // Look for WWT image data folder, create folder  if it doesn't exist
    if (!directoryExists(directory)) {
        std::string newDir = directory.string();
        // Remove the '/' at the end
        newDir.pop_back();
        LINFO("Creating directory" + newDir);
        std::filesystem::create_directory(newDir);
    }

    // Download file from url
    std::filesystem::path file = directory.string() + fileName + ".aspx";
    if (!downloadFile(url, file)) {
        LINFO(
            fmt::format("Couldn't download file '{}' to directory '{}'", url, directory)
        );
        return false;
    }

    // Parse file to XML
    using namespace tinyxml2;
    tinyxml2::XMLDocument* doc = new tinyxml2::XMLDocument();
    doc->LoadFile(file.string().c_str());

    // Search XML file for folders with urls
    XMLElement* root = doc->RootElement();
    XMLElement* element = root->FirstChildElement(wwt::Folder.c_str());
    const bool folderExists = element;
    const bool folderContainNoUrls = folderExists && !hasAttribute(element, wwt::Url);

    // If the file contains no folders, or there are folders but without urls,
    // stop recursion
    if (!folderExists || folderContainNoUrls) {
        xmls.push_back(doc);
        LINFO("Saving " + url);
        return true;
    }

    // Iterate through all the folders in the XML file
    while (element && std::string(element->Value()) == wwt::Folder) {
        // If folder contains urls, download and parse those urls
        if (hasAttribute(element, wwt::Url) && hasAttribute(element, wwt::Name)) {
            std::string urlAttr = attribute(element, wwt::Url);
            std::string fileNameAttr = attribute(element, wwt::Name);
            downloadAndParseWtmlFilesFromUrl(xmls, directory, urlAttr, fileNameAttr);
        }
        element = element->NextSiblingElement();
    }
    return true;
}

WwtDataHandler::~WwtDataHandler() {
    // Call destructor of all allocated xmls
    _xmls.clear();
}

void WwtDataHandler::loadImages(const std::string& root,
                                const std::filesystem::path& directory)
{
    // Collect the wtml files, either by reading from disc or from a url
    if (directoryExists(directory) && !std::filesystem::is_empty(directory)) {
        parseWtmlsFromDisc(_xmls, directory);
        LINFO("Loading images from directory");
    }
    else {
        downloadAndParseWtmlFilesFromUrl(_xmls, directory, root, "root");
        LINFO("Loading images from url");
    }

    // Traverse through the collected wtml documents and collect the images
    for (tinyxml2::XMLDocument* doc : _xmls) {
        tinyxml2::XMLElement* rootNode = doc->FirstChildElement();
        std::string collectionName = attribute(rootNode, wwt::Name);
        saveImagesFromXml(rootNode, collectionName);
    }

    // Sort images in alphabetical order
    std::sort(
        _images.begin(),
        _images.end(),
        [](ImageData& a, ImageData& b) {
            // If the first character in the names are lowercase, make it upper case
            if (std::islower(a.name[0])) {
                // convert string to upper case
                a.name[0] = static_cast<char>(::toupper(a.name[0]));
            }
            if (std::islower(b.name[0])) {
                b.name[0] = static_cast<char>(::toupper(b.name[0]));
            }
            return a.name < b.name;
        }
    );

    LINFO(fmt::format("Loaded {} WorldWide Telescope images", _images.size()));
}

int WwtDataHandler::nLoadedImages() const {
    return static_cast<int>(_images.size());
}

const ImageData& WwtDataHandler::getImage(int i) const {
    ghoul_assert(i < static_cast<int>(_images.size()), "Index outside of vector size");
    return _images[i];
}

void WwtDataHandler::saveImageFromNode(tinyxml2::XMLElement* node, std::string collection)
{
    // Collect the image set of the node. The structure is different depending on if
    // it is a Place or an ImageSet
    std::string thumbnailUrl = wwt::Undefined;
    tinyxml2::XMLElement* imageSet = nullptr;
    std::string type = std::string(node->Name());

    if (type == wwt::ImageSet) {
        thumbnailUrl = getChildNodeContentFromImageSet(node, wwt::ThumbnailUrl);
        imageSet = node;
    }
    else if (type == wwt::Place) {
        thumbnailUrl = getUrlFromPlace(node);
        imageSet = getChildNode(node, wwt::ImageSet);
    }

    // Only collect the images that have a thumbnail image, that are sky images and
    // that have an image
    const bool hasThumbnailUrl = thumbnailUrl != wwt::Undefined;
    const bool isSkyImage = attribute(node, wwt::DataSetType) == wwt::Sky;
    const bool hasImageUrl = imageSet ? hasAttribute(imageSet, wwt::Url) : false;

    if (!(hasThumbnailUrl && isSkyImage && hasImageUrl)) {
        return;
    }

    // Collect name, image url and credits
    std::string  name = attribute(node, wwt::Name);
    std::string imageUrl = attribute(imageSet, wwt::Url);
    std::string credits = getChildNodeContentFromImageSet(imageSet, wwt::Credits);
    std::string creditsUrl = getChildNodeContentFromImageSet(imageSet, wwt::CreditsUrl);

    // Collect equatorial coordinates. All-sky surveys do not have this kind of
    // coordinate
    bool hasCelestialCoords = hasAttribute(node, wwt::RA) && hasAttribute(node, wwt::Dec);
    glm::dvec2 equatorialSpherical = glm::dvec2(0.0);
    glm::dvec3 equatorialCartesian = glm::vec3(0.0);

    if (hasCelestialCoords) {
        // The RA from WWT is in the unit hours:
        // to convert to degrees, multiply with 360 (deg) /24 (h) = 15
        double ra = 15.0 * std::stod(attribute(node, wwt::RA));
        double dec = std::stod(attribute(node, wwt::Dec));
        equatorialSpherical = glm::dvec2(ra, dec);
        equatorialCartesian = skybrowser::sphericalToCartesian(equatorialSpherical);
    }

    // Collect field of view. The WWT definition of ZoomLevel is: VFOV = ZoomLevel / 6
    float fov = 0.f;
    if (hasAttribute(node, wwt::ZoomLevel)) {
        fov = std::stof(attribute(node, wwt::ZoomLevel)) / 6.0f;
    }

    ImageData image = {
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

    _images.push_back(image);
}

void WwtDataHandler::saveImagesFromXml(tinyxml2::XMLElement* root, std::string collection)
{
    // Get direct child of node called Place
    using namespace tinyxml2;
    XMLElement* node = root->FirstChildElement();

    // Iterate through all siblings of node. If sibling is folder, open recursively.
    // If sibling is image, save it.
    while (node) {
        const std::string name = node->Name();
        // If node is an image or place, load it
        if (name == wwt::ImageSet || name == wwt::Place) {
            saveImageFromNode(node, collection);
        }
        // If node is another folder, open recursively
        else if (name == wwt::Folder) {
            std::string newCollectionName = collection + "/";
            newCollectionName += attribute(node, wwt::Name);

            saveImagesFromXml(node, newCollectionName);
        }
        node = node->NextSiblingElement();
    }
}

} // namespace openspace
