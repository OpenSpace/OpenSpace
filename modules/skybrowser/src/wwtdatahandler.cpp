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
            // Save the url
            std::string collectionName = root->FindAttribute("Name") ? root->FindAttribute("Name")->Value() : "";
            if (collectionName != "") {
                ImageCollection newCollection{ collectionName, url };
                imageUrls.push_back(newCollection);
            }
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
          
            if (doc->LoadFile(entry.path().u8string().c_str()) == tinyxml2::XMLError::XML_SUCCESS) {
                tinyxml2::XMLElement* root = doc->RootElement();
                std::string collectionName = root->FindAttribute("Name") ? root->FindAttribute("Name")->Value() : "";
                if (collectionName != "") {
                    ImageCollection newCollection{collectionName, entry.path().u8string()};               
                    imageUrls.push_back(newCollection);
                }
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
        return images.size();
    }

    const std::vector<ImageCollection>& WWTDataHandler::getAllImageCollectionUrls() const {
        return imageUrls;
    }

    void WWTDataHandler::loadImagesFromXML(tinyxml2::XMLElement* node, std::string collectionName) {
        // Get direct child of node called "Place"
        using namespace tinyxml2;
        XMLElement* ptr = node->FirstChildElement();

        // Go through all siblings of ptr and open folders recursively
        while (ptr) {
            // Iterate through all siblings at same level and load
            while (ptr) {
                // If node is an image or place, load it 
                if (std::string(ptr->Name()) == "ImageSet" || std::string(ptr->Name()) == "Place") {
                    loadImage(ptr, collectionName);
                }
                // If node is another folder, open recursively
                else if (std::string(ptr->Name()) == "Folder") {
                    std::string newCollectionName = collectionName + "/";
                    if (ptr->FindAttribute("Name")) {
                        newCollectionName += std::string(ptr->FindAttribute("Name")->Value());
                    }
                    loadImagesFromXML(ptr, newCollectionName);
                }

                ptr = ptr->NextSiblingElement();
            }            
        }
    }

    int WWTDataHandler::loadImage(tinyxml2::XMLElement* node, std::string collectionName) {
        // Only load "Sky" type images
        if (std::string(node->FindAttribute("DataSetType")->Value()) != "Sky")
            return -1;

        std::string url;
        // Get url
        if (std::string(node->Name()) == "ImageSet") {
            url = getURLFromImageSet(node);
        }
        else if (std::string(node->Name()) == "Place") {
            url = getURLFromPlace(node);
        }
        
        // Only load images that have a thumbnail
        if (url == "") {
            return -1;
        }

        ImageData image{};
        setImageDataValues(node, url, collectionName, image);

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

    std::string WWTDataHandler::getURLFromPlace(tinyxml2::XMLElement* place)  {
        // Get thumbnail attribute, if there is one
        std::string url = place->FindAttribute("Thumbnail") ? place->FindAttribute("Thumbnail")->Value() : "";
        // Url found! Return it
        if (url != "") return url;

        // If the place doesn't have a thumbnail url data attribute,
        // Load the image set it stores instead
        tinyxml2::XMLElement* imageSet = getChildNode(place, "ImageSet");
        // If it doesn't contain an ImageSet, it doesn't have an url -> return empty string
        // If there is an imageSet, collect thumbnail url
        return imageSet ? getURLFromImageSet(imageSet) : "";
    }

    tinyxml2::XMLElement* WWTDataHandler::getDirectChildNode(tinyxml2::XMLElement* node, std::string name) {
        while (node && std::string(node->Name()) != name) {
            node = node->FirstChildElement();
        }
        return node;
    }

    tinyxml2::XMLElement* WWTDataHandler::getChildNode(tinyxml2::XMLElement* node, std::string name) {
        // Traverse the children and look at all their first child to find ImageSet
        tinyxml2::XMLElement* child = node->FirstChildElement();
        tinyxml2::XMLElement* imageSet = nullptr;
        while (child) {
            imageSet = getDirectChildNode(child, name);
            if (imageSet) break;
            child = child->NextSiblingElement();
        }
        return imageSet;
    }

    void WWTDataHandler::setImageDataValues(tinyxml2::XMLElement* node, std::string thumbnail, std::string collectionName, ImageData& img) {
        // Get attributes for the image
        img.name = node->FindAttribute("Name") ? node->FindAttribute("Name")->Value() : "";
        img.hasCoords = node->FindAttribute("RA") && node->FindAttribute("Dec");
        if (img.hasCoords) {
            // The RA from WWT is in the unit hours: to convert to degrees, multiply with 360 (deg) /24 (h) = 15
            img.celestCoords.x = 15.0f * std::stof(node->FindAttribute("RA")->Value());
            img.celestCoords.y = std::stof(node->FindAttribute("Dec")->Value());
        }
        img.collection = collectionName;
        img.thumbnailUrl = thumbnail;
        img.zoomLevel = node->FindAttribute("ZoomLevel") ? std::stof(node->FindAttribute("ZoomLevel")->Value()) : 0.f;
    }
    
    const std::vector<ImageData>& WWTDataHandler::getLoadedImages() const {
        return images;
    }
}
