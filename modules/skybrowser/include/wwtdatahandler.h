#ifndef __OPENSPACE_MODULE_SKYBROWSER___WWTDATAHANDLER___H__
#define __OPENSPACE_MODULE_SKYBROWSER___WWTDATAHANDLER___H__

#include <openspace/documentation/documentation.h>
#include <modules/skybrowser/tinyxml2/tinyxml2.h>

namespace openspace::documentation { struct Documentation; }

namespace openspace {

    struct ImageData {
        std::string name;
        std::string thumbnailUrl;
        glm::vec2 celestCoords;
        std::string collection;
    };

    class WWTDataHandler {
    
    public:
        WWTDataHandler() = default;
        ~WWTDataHandler();
        // Image downloading and xml parsing
        bool downloadFile(std::string& url, std::string& fileDestination);
        void loadImagesFromXML(tinyxml2::XMLElement* node, std::string collectionName);
        void loadWTMLCollectionsFromURL(std::string url, std::string fileName);
        void loadWTMLCollectionsFromDirectory(std::string directory);
        
        int loadAllImagesFromXMLs();
        const std::vector<std::string>& getAllImageCollectionUrls() const;
        std::vector < std::pair < std::string, std::string> > getAllThumbnailUrls();
        const std::vector<ImageData>& getLoadedImages() const;

    private:
        
        int loadImage(tinyxml2::XMLElement* imageSet, std::string collectionName);
        void setImageDataValues(tinyxml2::XMLElement* node, std::string thumbnail, std::string collectionName, ImageData& img);

        std::string getURLFromImageSet(tinyxml2::XMLElement* imageSet);
        std::string getURLFromPlace(tinyxml2::XMLElement* place);
        tinyxml2::XMLElement* getDirectChildNode(tinyxml2::XMLElement* node, std::string name);
        tinyxml2::XMLElement* getChildNode(tinyxml2::XMLElement* node, std::string name);

        std::vector<ImageData> images;
        std::vector<std::string> imageUrls;
        std::vector<tinyxml2::XMLDocument*> xmls;

    };
}

#endif // __OPENSPACE_MODULE_SKYBROWSER___WWTDATAHANDLER___H__

