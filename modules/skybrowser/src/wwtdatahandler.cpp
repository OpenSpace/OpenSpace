#include <modules/skybrowser/include/wwtdatahandler.h>
#include <modules/skybrowser/include/utility.h>
#include <openspace/util/httprequest.h> // For downloading files from url
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <filesystem> // To iterate through files in directory
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

    bool hasAttribute(const tinyxml2::XMLElement* element, const std::string& name) {
         return element->FindAttribute(name.c_str());
    }

    std::string getAttribute(const tinyxml2::XMLElement* element, const std::string& name) {
        if (hasAttribute(element, name)) {
            return element->FindAttribute(name.c_str())->Value();
        }
        else {
            return wwt::Undefined;
        }
    }

    // Parsing and downloading of wtml files
    bool downloadFile(const std::string& url, const std::string& fileDestination) {
        // Get the web page and save to file
        HttpRequest::RequestOptions opt{ 5 };
        SyncHttpFileDownload wtml_root(
            url, fileDestination, HttpFileDownload::Overwrite::Yes
        );
        wtml_root.download(opt);
        return wtml_root.hasSucceeded();
    }

    bool directoryExists(const std::string& path)
    {
        struct stat info;

        int statRC = stat(path.c_str(), &info);
        if (statRC != 0)
        {
            // something along the path does not exist
            if (errno == ENOENT) {
                return false;
            }
            // something in path prefix is not a dir
            if (errno == ENOTDIR) {
                return false;
            }
            return false;
        }

        bool directoryExists = (info.st_mode & S_IFDIR);

        return  directoryExists;
    }

    std::string createSearchableString(std::string str) {
        // Remove white spaces and all special characters
        str.erase(std::remove_if(std::begin(str), std::end(str), [](char c) {
            bool isNumberOrLetter = std::isdigit(c) || std::isalpha(c);
            return !isNumberOrLetter;
            }),
            std::end(str));
        // Make the word lower case
        std::transform(std::begin(str), std::end(str), std::begin(str), [](char c) {
            return std::tolower(c);
            });
        return str;
    }

    std::unordered_map<std::string, glm::dvec3> 
    loadSpeckData(const speck::Dataset& dataset) {
        // Create map
        std::unordered_map<std::string, glm::dvec3> positions3d;

        for (speck::Dataset::Entry entry : dataset.entries) {
            if (entry.comment.has_value()) {
                std::string name = createSearchableString(entry.comment.value());
                positions3d[name] = std::move(entry.position);
            }
        }
        return positions3d;
    }

    tinyxml2::XMLElement* getDirectChildNode(tinyxml2::XMLElement* node, 
                                             const std::string& name) {
        while (node && node->Name() != name) {
            node = node->FirstChildElement();
        }
        return node;
    }

    tinyxml2::XMLElement* getChildNode(tinyxml2::XMLElement* node, 
                                       const std::string& name) {
        
        tinyxml2::XMLElement* child = node->FirstChildElement();
        tinyxml2::XMLElement* imageSet = nullptr;

        // Traverse the children and look at all their first child to find ImageSet
        while (child) {
            imageSet = getDirectChildNode(child, name);
            // Found
            if (imageSet) {
                break;
            }
            child = child->NextSiblingElement();
        }
        return imageSet;
    }

    std::string getChildNodeContentFromImageSet(tinyxml2::XMLElement* imageSet,
                                                const std::string& elementName) {
        // Find the thumbnail image url
        // The thumbnail is the last node so traverse backwards for speed
        tinyxml2::XMLElement* imageSetChild = imageSet->FirstChildElement(
            elementName.c_str()
        );

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
            return getAttribute(place, wwt::Thumbnail);
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
    

    void parseWtmlsFromDisc(std::vector<tinyxml2::XMLDocument*>& _xmls, 
                           const std::string& directory) {
        for (const auto& entry : std::filesystem::directory_iterator(directory)) {

            tinyxml2::XMLDocument* document = new tinyxml2::XMLDocument();
            std::string path = entry.path().u8string();
            tinyxml2::XMLError successCode = document->LoadFile(path.c_str());

            if (successCode == tinyxml2::XMLError::XML_SUCCESS) {
                _xmls.push_back(document);
            }
        }
    }

    bool downloadAndParseWtmlFilesFromUrl(std::vector<tinyxml2::XMLDocument*>& _xmls,
        const std::string& directory, const std::string& url,
        const std::string& fileName)
    {
        // Look for WWT image data folder, create folder  if it doesn't exist
        if (!directoryExists(directory)) {
            std::string newDir = directory;
            // Remove the '/' at the end
            newDir.pop_back();
            LINFO("Creating directory WWTimagedata");
            std::filesystem::create_directory(newDir);
        }

        // Download file from url
        std::string file = directory.c_str() + fileName + ".aspx";
        if (!downloadFile(url, file)) {
            LINFO("Couldn't download file " + url);
            return false;
        }

        // Parse file to XML
        using namespace tinyxml2;
        tinyxml2::XMLDocument* doc = new tinyxml2::XMLDocument();
        doc->LoadFile(file.c_str());

        // Search XML file for folders with urls
        XMLElement* root = doc->RootElement();
        XMLElement* element = root->FirstChildElement(wwt::Folder.c_str());
        bool folderExists = element;
        bool folderContainNoUrls = folderExists && !hasAttribute(element, wwt::Url);
        
        // If the file contains no folders, or there are folders but without urls, 
        // stop recursion
        if (!folderExists || folderContainNoUrls) {
            _xmls.push_back(doc);
            LINFO("Saving " + url);

            return true;
        }

        // Iterate through all the folders in the XML file 
        while (element && std::string(element->Value()) == wwt::Folder) {

            // If folder contains urls, download and parse those urls
            if (hasAttribute(element, wwt::Url) && hasAttribute(element, wwt::Name)) {
                std::string url = getAttribute(element, wwt::Url);
                std::string fileName = getAttribute(element, wwt::Name);
                downloadAndParseWtmlFilesFromUrl(_xmls, directory, url, fileName);
            }
            element = element->NextSiblingElement();
        }
       
    }

    WwtDataHandler::~WwtDataHandler() {
        // Call destructor of all allocated xmls
        _xmls.clear();
    }

    void WwtDataHandler::loadImages(const std::string& root, const std::string& directory,
                                    std::vector<std::filesystem::path>& speckFiles) {

        // Load 3D data from speck files
        for (std::filesystem::path& path : speckFiles) {
            speck::Dataset speck = speck::loadSpeckFile(path);
            _3dPositions = loadSpeckData(speck);
            LINFO("Loaded speck file with " + std::to_string(_3dPositions.size()) +
                  " entries!");
        }

        // Collect the wtml files, either by reading from disc or from a url
        if (directoryExists(directory)) {
            parseWtmlsFromDisc(_xmls, directory);
            LINFO("Loading images from directory");
        }      
        else {    
            downloadAndParseWtmlFilesFromUrl(_xmls, directory, root, "root");
            LINFO("Loading images from url");
        }

        // Traverse through the collected wtml documents and collect the images
        for (tinyxml2::XMLDocument* doc : _xmls) {
            tinyxml2::XMLElement* root = doc->FirstChildElement();
            std::string collectionName = getAttribute(root, wwt::Name);
            saveImagesFromXml(root, collectionName);
        }

        // Sort images in alphabetical order
        std::sort(_images.begin(), _images.end(), [](ImageData& a, ImageData& b) {
            // If the first character in the names are lowercase, make it upper case
            if (std::islower(a.name[0])) {
                // convert string to upper case
                a.name[0] = ::toupper(a.name[0]);
            }
            if (std::islower(b.name[0])) {
                b.name[0] = ::toupper(b.name[0]);
            }
            return a.name < b.name;
            });

        LINFO("Loaded " + std::to_string(_images.size()) + " WorldWide Telescope "
              "images.");

        LINFO(std::to_string(_nMatched3dPositions) + " 3D positions were matched in "
            "the speck files!");
    }

    int WwtDataHandler::nLoadedImages() const
    {
        return _images.size();
    }

    const ImageData& WwtDataHandler::getImage(const int i) const
    {
        assert(i < _images.size(), "Index outside of image vector boundaries!");
        return _images[i];
    }

    void WwtDataHandler::saveImageFromNode(tinyxml2::XMLElement* node,
        std::string collection) {

        // Collect the image set of the node. The structure is different depending on if
        // it is a Place or an ImageSet
        std::string thumbnailUrl = { wwt::Undefined };
        tinyxml2::XMLElement* imageSet{ nullptr };
        std::string type = std::string(node->Name());

        if (type == wwt::ImageSet) {
            thumbnailUrl = getChildNodeContentFromImageSet(node, wwt::ThumbnailUrl);
            imageSet = node;
        } // Place
        else if (type == wwt::Place) {
            thumbnailUrl = getUrlFromPlace(node);
            imageSet = getChildNode(node, wwt::ImageSet);
        }

        // Only collect the images that have a thumbnail image, that are sky images and 
        // that have an image
        bool hasThumbnailUrl = thumbnailUrl != wwt::Undefined;
        bool isSkyImage = getAttribute(node, wwt::DataSetType) == wwt::Sky;
        bool hasImageUrl = imageSet ? hasAttribute(imageSet, wwt::Url) : false;
        
        if (!(hasThumbnailUrl && isSkyImage && hasImageUrl)) {
            return;
        }

        // Collect name, image url and credits
        std::string  name = getAttribute(node, wwt::Name);
        std::string imageUrl = getAttribute(imageSet, wwt::Url);
        std::string credits = getChildNodeContentFromImageSet(imageSet, wwt::Credits);
        std::string creditsUrl = getChildNodeContentFromImageSet(
            imageSet, wwt::CreditsUrl
        );

        // Collect equatorial coordinates. All-sky surveys do not have this kind of 
        // coordinate
        bool hasCelestialCoords = hasAttribute(node, wwt::RA) &&
            hasAttribute(node, wwt::Dec);
        glm::dvec2 equatorialSpherical{ 0.0 };
        glm::dvec3 equatorialCartesian{ 0.0 };

        if (hasCelestialCoords) {
            // The RA from WWT is in the unit hours: 
            // to convert to degrees, multiply with 360 (deg) /24 (h) = 15
            double ra = 15.0 * std::stod(getAttribute(node, wwt::RA));
            double dec = std::stod(getAttribute(node, wwt::Dec));
            equatorialSpherical = { ra, dec };
            equatorialCartesian = skybrowser::sphericalToCartesian(
                equatorialSpherical
            );
        }

        // Collect field of view. The WWT definition of ZoomLevel is: VFOV = ZoomLevel / 6
        float fov{ 0.f };
        if (hasAttribute(node, wwt::ZoomLevel)) {
            fov = std::stof(getAttribute(node, wwt::ZoomLevel)) / 6.0;
        }

        // Find 3D position by matching with speck file
        bool has3dCoords{ false };
        glm::dvec3 position3d{ 0.0 };
        auto it = _3dPositions.find(createSearchableString(name));
        if (it != _3dPositions.end()) {
            position3d = it->second;
            has3dCoords = true;
            _nMatched3dPositions++;
        }

        ImageData image = {
            name,
            thumbnailUrl,
            imageUrl,
            credits,
            creditsUrl,
            collection,
            hasCelestialCoords,
            has3dCoords,
            fov,
            equatorialSpherical,
            equatorialCartesian,
            position3d
        };

        _images.push_back(image);
    }

    void WwtDataHandler::saveImagesFromXml(tinyxml2::XMLElement* root,
        std::string collection) {

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
                newCollectionName += getAttribute(node, wwt::Name);

                saveImagesFromXml(node, newCollectionName);
            }
            node = node->NextSiblingElement();
        }
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
