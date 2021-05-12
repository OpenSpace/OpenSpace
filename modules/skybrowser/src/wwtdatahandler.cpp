#include <modules/skybrowser/include/wwtdatahandler.h>
#include <openspace/util/httprequest.h> // For downloading files from url
#include <filesystem> // To iterate through files in directory
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <algorithm>

// For loading the speck files
#include <ghoul/fmt.h>
#include <ghoul/misc/assert.h>
#include <cctype>
#include <fstream>
#include <string_view>

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

    void WWTDataHandler::loadWTMLCollectionsFromURL(std::string directory, std::string url, std::string fileName) {
        // Look for WWT image data folder
        if (!directoryExists(directory)) {
            std::string newDir = directory;
            newDir.pop_back();
            LINFO("Creating directory WWTimagedata");
            std::filesystem::create_directory(newDir);
        }

        // Get file
        std::string file = directory + fileName + ".aspx";
        if (!downloadFile(url, file)) {
            LINFO("Couldn't download file " + url);
            return;
        }
        // Parse to XML
        using namespace tinyxml2;
        tinyxml2::XMLDocument* doc = new tinyxml2::XMLDocument();
        doc->LoadFile(file.c_str());

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
                loadWTMLCollectionsFromURL(directory, subUrl, subName);
            }
            element = element->NextSiblingElement();
        }
    }


    bool WWTDataHandler::directoryExists(std::string& path)
    {
        struct stat info;

        int statRC = stat(path.c_str(), &info);
        if (statRC != 0)
        {
            if (errno == ENOENT) { return false; } // something along the path does not exist
            if (errno == ENOTDIR) { return false; } // something in path prefix is not a dir
            return false;
        }

        bool directoryExists = (info.st_mode & S_IFDIR);

        return  directoryExists;
    }

    bool WWTDataHandler::loadWTMLCollectionsFromDirectory(std::string directory) {
       
        if (!directoryExists(directory)) return false;

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
        return true;
    }

    std::ostream& operator<<(std::ostream& os, const ImageData& img) {
        os << "Name: " << img.name << " Coords: ra: " << img.celestCoords.x << " dec: " << img.celestCoords.y << std::endl;
        os << "Thumbnail: " << img.thumbnailUrl << std::endl;
        os << "Collection: " << img.collection << std::endl << std::endl;
        return os;
    }


    int WWTDataHandler::loadImagesFromLoadedXMLs() {
        for (tinyxml2::XMLDocument* doc : xmls) {
            tinyxml2::XMLElement* root = doc->FirstChildElement();
            std::string collectionName = root->FindAttribute("Name") ? root->FindAttribute("Name")->Value() : "";
            loadImagesFromXML(root, collectionName);
        }
        // Sort images in alphabetial order
        std::sort(images.begin(), images.end(), [](ImageData a, ImageData b) {
            // If the first charachter in the names are lowercase, make it upper case
            if (std::islower(a.name[0])) {
                // convert string to upper case
                a.name[0] = ::toupper(a.name[0]);
            }
            if (std::islower(b.name[0])) {
                b.name[0] = ::toupper(b.name[0]);
            }
            return a.name < b.name;
            });
        LINFO(std::to_string(nImagesWith3dPositions) + " 3D positions were matched in the speck files!");

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
        // Iterate through all siblings at same level and load
        while (ptr) {
            // If node is an image or place, load it 
            if (std::string(ptr->Name()) == "ImageSet" || std::string(ptr->Name()) == "Place") {
                loadImageFromXmlNode(ptr, collectionName);
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

    int WWTDataHandler::loadImageFromXmlNode(tinyxml2::XMLElement* node, std::string collectionName) {
        // Only load "Sky" type images
        if (std::string(node->FindAttribute("DataSetType")->Value()) != "Sky")
            return -1;

        std::string thumbnailUrl;
        std::string credits;
        std::string creditsUrl;
        std::string imageUrl;
        tinyxml2::XMLElement* imageSet = nullptr;
        // Get url. The thumbnail can be located either in the Place or the ImageSet
        if (std::string(node->Name()) == "ImageSet") {       
            thumbnailUrl = getChildNodeContentFromImageSet(node, "ThumbnailUrl");
            imageSet = node;
        }
        else if (std::string(node->Name()) == "Place") {
            thumbnailUrl = getURLFromPlace(node);
            imageSet = getChildNode(node, "ImageSet");
        }
        else {
            return -1;
        }
        
        // Only load images that have a thumbnail image url
        if (thumbnailUrl == "" || !imageSet) {
            return -1;
        }
        // Only load images that contain a image url
        if (!imageSet->FindAttribute("Url")) {
            return -1;
        }
        // The credits and image url are always children nodes of ImageSet
        credits = getChildNodeContentFromImageSet(imageSet, "Credits");
        creditsUrl = getChildNodeContentFromImageSet(imageSet, "CreditsUrl");
        imageUrl = imageSet->FindAttribute("Url")->Value();

        ImageData image{};
        setImageDataValues(node, credits, creditsUrl, thumbnailUrl, collectionName, imageUrl, image);

        images.push_back(image);
        // Return index of image in vector
        return images.size();
    }

    std::string WWTDataHandler::getChildNodeContentFromImageSet(tinyxml2::XMLElement* imageSet, std::string elementName) {
        // FInd the thumbnail image url
       // The thumbnail is the last node so traverse backwards for speed
        tinyxml2::XMLElement* imageSetChild = imageSet->FirstChildElement(elementName.c_str());
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
        return imageSet ? getChildNodeContentFromImageSet(imageSet, "ThumbnailUrl") : "";
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

    void WWTDataHandler::setImageDataValues(tinyxml2::XMLElement* node, 
                                            std::string credits, 
                                            std::string creditsUrl, 
                                            std::string thumbnail, 
                                            std::string collectionName,
                                            std::string imageUrl,
                                            ImageData& img) {
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
        img.credits = credits;
        img.creditsUrl = creditsUrl;
        img.imageUrl = imageUrl;
        // Look for 3D position in the data loaded from speck files
        std::string str = createSearchableString(img.name);
        // Look for 3D coordinate
        auto it = _3dPositions.find(str);
        if (it != _3dPositions.end()) {
            img.position3d = it->second;
            nImagesWith3dPositions++;
        }
    }
    
    std::vector<ImageData>& WWTDataHandler::getLoadedImages() {
        return images;
    }

    void WWTDataHandler::loadSpeckData(speck::Dataset& dataset) {
        for (speck::Dataset::Entry entry : dataset.entries) {
            if (entry.comment.has_value()) {
                std::string name = createSearchableString(entry.comment.value());
                _3dPositions[name] = std::move(entry.position);
            }
        }
        LINFO("Loaded speck file with " + std::to_string(_3dPositions.size()) + " entries!");
    }

    std::string WWTDataHandler::createSearchableString(std::string str) {
        // Remove white spaces and all special characters
        str.erase(std::remove_if(str.begin(), str.end(), [](char c) {
            bool isNumberOrLetter = std::isdigit(c) || std::isalpha(c);
            return !isNumberOrLetter;
            }),
            str.end());
        // Make the word lower case
        std::transform(str.begin(), str.end(), str.begin(),
            [](unsigned char c) { return std::tolower(c); });
        return str;
    }

}

// Loading of speck files
namespace {
    constexpr bool startsWith(std::string_view lhs, std::string_view rhs) noexcept {
        return (rhs.size() <= lhs.size()) && (lhs.substr(0, rhs.size()) == rhs);
    }

    void strip(std::string& line) noexcept {
        // 1. Remove all spaces from the beginning
        // 2. Remove #
        // 3. Remove all spaces from the new beginning
        // 4. Remove all spaces from the end

        while (!line.empty() && line[0] == ' ') {
            line = line.substr(1);
        }

        if (!line.empty() && line[0] == '#') {
            line = line.substr(1);
        }

        while (!line.empty() && line[0] == ' ') {
            line = line.substr(1);
        }

        while (!line.empty() && line.back() == ' ') {
            line = line.substr(0, line.size() - 2);
        }
    }

    template <typename T, typename U>
    void checkSize(U value, std::string_view message) {
        if (value > std::numeric_limits<U>::max()) {
            throw ghoul::RuntimeError(fmt::format("Error saving file: {}", message));
        }
    }
} // namespace

namespace openspace::speck {

    Dataset loadSpeckFile(std::filesystem::path path, 
        SkipAllZeroLines skipAllZeroLines) 
    {
        ghoul_assert(std::filesystem::exists(path), "File must exist");

        std::ifstream file(path);
        if (!file.good()) {
            throw ghoul::RuntimeError(fmt::format("Failed to open speck file '{}'", path));
        }

        Dataset res;

        int nDataValues = 0;

        std::string line;
        // First phase: Loading the header information
        while (std::getline(file, line)) {
            // Ignore empty line or commented-out lines
            if (line.empty() || line[0] == '#') {
                continue;
            }

            // Guard against wrong line endings (copying files from Windows to Mac) causes
            // lines to have a final \r
            if (line.back() == '\r') {
                line = line.substr(0, line.length() - 1);
            }

            strip(line);

            // If the first character is a digit, we have left the preamble and are in the
            // data section of the file
            if (std::isdigit(line[0]) || line[0] == '-') {
                break;
            }


            if (startsWith(line, "datavar")) {
                // each datavar line is following the form:
                // datavar <idx> <description>
                // with <idx> being the index of the data variable 

                std::stringstream str(line);
                std::string dummy;
                Dataset::Variable v;
                str >> dummy >> v.index >> v.name;

                //if (v.name == "orientation" || v.name == "ori" || v.name == "texture") {
                //    // The values for orientation and the texture indices are handled by
                //    // their own keywords
                //}
                //else {
                nDataValues += 1;
                res.variables.push_back(v);
                //}

                continue;
            }

            if (startsWith(line, "texturevar")) {
                // each texturevar line is following the form:
                // texturevar <idx>
                // where <idx> is the data value index where the texture index is stored
                if (res.textureDataIndex != -1) {
                    throw ghoul::RuntimeError(fmt::format(
                        "Error loading speck file '{}': Texturevar defined twice",
                        path
                    ));
                }

                std::stringstream str(line);
                std::string dummy;
                str >> dummy >> res.textureDataIndex;

                //nDataValues += 1;
                continue;
            }

            if (startsWith(line, "polyorivar")) {
                // each texturevar line is following the form:
                // texturevar <idx>
                // where <idx> is the data value index where the orientation index storage
                // starts. There are 6 values stored in total, xyz + uvw

                if (res.orientationDataIndex != -1) {
                    throw ghoul::RuntimeError(fmt::format(
                        "Error loading speck file '{}': Orientation index defined twice",
                        path
                    ));
                }

                std::stringstream str(line);
                std::string dummy;
                str >> dummy >> res.orientationDataIndex;

                //nDataValues += 6;
                continue;
            }

            if (startsWith(line, "texture")) {
                // each texture line is following one of two forms:
                // 1:   texture -M 1 halo.sgi
                // 2:   texture 1 M1.sgi
                // The parameter in #1 is currently being ignored

                std::stringstream str(line);

                std::string dummy;
                str >> dummy;

                if (line.find('-') != std::string::npos) {
                    str >> dummy;
                }

                Dataset::Texture texture;
                str >> texture.index >> texture.file;
                res.textures.push_back(texture);
                continue;
            }
        }

        std::sort(
            res.variables.begin(), res.variables.end(),
            [](const Dataset::Variable& lhs, const Dataset::Variable& rhs) {
                return lhs.index < rhs.index;
            }
        );

        std::sort(
            res.textures.begin(), res.textures.end(),
            [](const Dataset::Texture& lhs, const Dataset::Texture& rhs) {
                return lhs.index < rhs.index;
            }
        );

        // For the first line, we already loaded it and rejected it above, so if we do another
        // std::getline, we'd miss the first data value line
        bool isFirst = true;
        while (isFirst || std::getline(file, line)) {
            isFirst = false;

            // Ignore empty line or commented-out lines
            if (line.empty() || line[0] == '#') {
                continue;
            }

            // Guard against wrong line endings (copying files from Windows to Mac) causes
            // lines to have a final \r
            if (line.back() == '\r') {
                line = line.substr(0, line.length() - 1);
            }

            strip(line);

            if (line.empty()) {
                continue;
            }

            // If the first character is a digit, we have left the preamble and are in the
            // data section of the file
            if (!std::isdigit(line[0]) && line[0] != '-') {
                throw ghoul::RuntimeError(fmt::format(
                    "Error loading speck file '{}': Header information and datasegment "
                    "intermixed", path
                ));
            }

            bool allZero = true;

            std::stringstream str(line);
            Dataset::Entry entry;
            str >> entry.position.x >> entry.position.y >> entry.position.z;
            allZero &= (entry.position == glm::dvec3(0.0));

            entry.data.resize(nDataValues);
            for (int i = 0; i < nDataValues; i += 1) {
                str >> entry.data[i];
                allZero &= (entry.data[i] == 0.0);
            }

            if (skipAllZeroLines && allZero) {
                continue;
            }

            std::string rest;
            std::getline(str, rest);
            if (!rest.empty()) {

                strip(rest);
                entry.comment = rest;
            }

            res.entries.push_back(std::move(entry));
           
        }

#ifdef _DEBUG
        if (!res.entries.empty()) {
            size_t nDataValues = res.entries[0].data.size();
            for (const Dataset::Entry& e : res.entries) {
                ghoul_assert(
                    e.data.size() == nDataValues,
                    "Row had different number of data values"
                );
            }
        }
#endif

        return res;
    }

} // namespace openspace::speck
