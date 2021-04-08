#include <modules/skybrowser/include/wwtdatahandler.h>
#include <openspace/util/httprequest.h> // For downloading files from url
#include <filesystem> // To iterate through files in directory
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "WWTDataHandler";




} //namespace

namespace openspace {

    WWTDataHandler::~WWTDataHandler() {
        // Call destructor of all allocated xmls
        xmls.clear();
    }

    bool WWTDataHandler::downloadFile(std::string& url, std::string& fileDestination) {
        // Get the webpage and save to file
        HttpRequest::RequestOptions opt{ 5 };
        SyncHttpFileDownload wtml_root(url, fileDestination, HttpFileDownload::Overwrite::Yes);
        wtml_root.download(opt);
        return wtml_root.hasSucceeded();
    }

    void WWTDataHandler::loadWTMLCollectionsFromURL(std::string url, std::string fileName) {
        // Get file
        std::string fileDestination = absPath("${MODULE_SKYBROWSER}/WWTimagedata/") + fileName + ".aspx";
        if (!downloadFile(url, fileDestination)) {
            LINFO("Couldn't download file " + url);
            return;
        }
        // Parse to XML
        using namespace tinyxml2;
        tinyxml2::XMLDocument* doc = new tinyxml2::XMLDocument();
        doc->LoadFile(fileDestination.c_str());

        XMLElement* root = doc->RootElement();
        XMLElement* element = root->FirstChildElement(std::string("Folder").c_str());
        // If there are no folders, or there are folder but without urls, stop recursion
        if (!element || (element && !element->FindAttribute("Url"))) {

            imageUrls.push_back(url);
            xmls.push_back(doc);
            LINFO("Saving " + url);

            return;
        }
        // Iterate through all the folders
        while (element && std::string(element->Value()) == "Folder") {
            // Get all attributes for the <Folder>
            std::string subUrl = element->FindAttribute("Url") ? element->FindAttribute("Url")->Value() : "";
            std::string subName = element->FindAttribute("Name") ? element->FindAttribute("Name")->Value() : "";
            if (subUrl != "" && subName != "") {
                loadWTMLCollectionsFromURL(subUrl, subName);
            }
            element = element->NextSiblingElement();
        }
    }

    void WWTDataHandler::loadWTMLCollectionsFromDirectory(std::string directory) {

        for (const auto& entry : std::filesystem::directory_iterator(directory)) {
            tinyxml2::XMLDocument* doc = new tinyxml2::XMLDocument();
            std::cout << entry.path().u8string().c_str() << std::endl;
            if (doc->LoadFile(entry.path().u8string().c_str()) == tinyxml2::XMLError::XML_SUCCESS) {
                xmls.push_back(doc);
            }
        }
    }

    std::ostream& operator<<(std::ostream& os, const ImageData& img) {
        os << "Name: " << img.name << " Coords: ra: " << img.celestCoords.x << " dec: " << img.celestCoords.y << std::endl;
        os << "Thumbnail: " << img.thumbnailUrl << std::endl;
        os << "Collection: " << img.collection << std::endl << std::endl;
        return os;
    }


    int WWTDataHandler::loadAllImagesFromXMLs() {
        for (tinyxml2::XMLDocument* doc : xmls) {
            tinyxml2::XMLElement* root = doc->FirstChildElement();
            std::string collectionName = root->FindAttribute("Name") ? root->FindAttribute("Name")->Value() : "";
            loadImagesFromXML(root, collectionName);
        }

        for (ImageData img : images) {
            std::cout << img;
        }
        return images.size();
    }

    const std::vector<std::string>& WWTDataHandler::getAllImageCollectionUrls() const {
        return imageUrls;
    }

    void WWTDataHandler::loadImagesFromXML(tinyxml2::XMLElement* node, std::string collectionName) {
        // Get direct child of node called "Place"
        using namespace tinyxml2;
        XMLElement* folder = getChildNode(node->FirstChildElement(), "Folder");

        // Terminate recursion if no folders
        if (!folder) {
            // When we are at leaf folder
            // Iterate through all the <Place>:s and <ImageSet>:s and load as images
            // Go down to the level where places and image sets are
            XMLElement* ptr = getChildNode(node->FirstChildElement(), "Place");
            if (!ptr) {
                ptr = getChildNode(node->FirstChildElement(), "ImageSet");
            }
            // Iterate through all siblings at same level and load
            while (ptr) {
                if (std::string(ptr->Name()) == "ImageSet") {
                    loadImageSet(ptr, collectionName);
                }
                else if (std::string(ptr->Name()) == "Place") {
                    loadPlace(ptr, collectionName);
                }

                ptr = ptr->NextSiblingElement();
            }            
        }
        else {
            // Open all folders at same level
            while (folder) {
                std::string newCollectionName = collectionName + "/";
                if (folder->FindAttribute("Name")) {
                    newCollectionName += std::string(folder->FindAttribute("Name")->Value());
                }
                loadImagesFromXML(folder, newCollectionName);
                folder = folder->NextSiblingElement();
            }
        }
    }

    int WWTDataHandler::loadPlace(tinyxml2::XMLElement* place, std::string collectionName) {
        // Only load "Sky" type images
        if (std::string(place->FindAttribute("DataSetType")->Value()) != "Sky")
            return -1;

        std::string url = "";
        // If the place doesn't have a thumbnail url data attribute,
        // Load the containing image set instead
        if (!place->FindAttribute("Thumbnail")) {
            // Traverse the children and look at all their first child to find ImageSet
            tinyxml2::XMLElement* child = place->FirstChildElement();
            tinyxml2::XMLElement* imageSet = nullptr;
            while (child) {
                imageSet = getChildNode(child, "ImageSet");
                if (imageSet) break;
                child = child->NextSiblingElement();
            }
            // If the place doesn't contain an image, nothing to add
            if (!imageSet) return -1;

            // Collect thumbnail url from ImageSet
            url = getURLFromImageSet(imageSet);
            if (url == "") return -1;
        }
        ImageData image;
        // Get attributes for the image
        image.name = place->FindAttribute("Name") ? place->FindAttribute("Name")->Value() : "";
        image.celestCoords.x = place->FindAttribute("RA") ? std::stof(place->FindAttribute("RA")->Value()) : 0.f;
        image.celestCoords.y = place->FindAttribute("Dec") ? std::stof(place->FindAttribute("Dec")->Value()) : 0.f;
        image.thumbnailUrl = url == "" ? place->FindAttribute("Thumbnail")->Value() : url;
        image.collection = collectionName;

        images.push_back(image);
        // Return index of image in vector
        return images.size();
    }

    int WWTDataHandler::loadImageSet(tinyxml2::XMLElement* imageSet, std::string collectionName) {
        std::string type = imageSet->FindAttribute("DataSetType") ? imageSet->FindAttribute("DataSetType")->Value() : "";
        // Only load "Sky" type images
        if (type != "Sky")
            return -1;

        ImageData image;

        // Get attributes for the image
        image.name = imageSet->FindAttribute("Name") ? imageSet->FindAttribute("Name")->Value() : "";
        image.celestCoords.x = imageSet->FindAttribute("RA") ? std::stof(imageSet->FindAttribute("RA")->Value()) : 0.f;
        image.celestCoords.y = imageSet->FindAttribute("Dec") ? std::stof(imageSet->FindAttribute("Dec")->Value()) : 0.f;
        image.thumbnailUrl = getURLFromImageSet(imageSet);
        image.collection = collectionName;

        images.push_back(image);
        // Return index of image in vector
        return images.size();
    }

    std::string WWTDataHandler::getURLFromImageSet(tinyxml2::XMLElement* imageSet) {
        // FInd the thumbnail image url
       // The thumbnail is the last node so traverse backwards for speed
        tinyxml2::XMLElement* imageSetChild = imageSet->FirstChildElement("ThumbnailUrl");
        return imageSetChild ? imageSetChild->GetText() ? imageSetChild->GetText() : "" : "";
    }

    tinyxml2::XMLElement* WWTDataHandler::getChildNode(tinyxml2::XMLElement* node, std::string name) {
        while (node && std::string(node->Name()) != name) {
            node = node->FirstChildElement();
        }
        return node;
    }

    std::vector < std::pair < std::string, std::string> > WWTDataHandler::getAllThumbnailUrls() {
        std::vector < std::pair < std::string, std::string> >  imgResult;
        std::for_each(images.begin(), images.end(), [&](ImageData obj) {
            imgResult.push_back(std::pair(obj.name, obj.thumbnailUrl));
            });
        return imgResult;
    }
    
    const std::vector<ImageData>& WWTDataHandler::getImages() const {
        return images;
    }
}
