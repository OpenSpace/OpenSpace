#ifndef __OPENSPACE_MODULE_SKYBROWSER___WWTDATAHANDLER___H__
#define __OPENSPACE_MODULE_SKYBROWSER___WWTDATAHANDLER___H__

#include <modules/skybrowser/ext/tinyxml2/tinyxml2.h>
#include <modules/space/speckloader.h>
#include <openspace/documentation/documentation.h>
#include <unordered_map>

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
} // namespace openspace::wwt\

namespace openspace {

    struct ImageData {
        std::string name{ wwt::Undefined };
        std::string thumbnailUrl{ wwt::Undefined };
        std::string imageUrl{ wwt::Undefined };
        std::string credits{ wwt::Undefined };
        std::string creditsUrl{ wwt::Undefined };
        std::string collection{ wwt::Undefined };
        bool hasCelestialCoords{ false };
        bool has3dCoords{ false };
        float fov{ 0.f };
        glm::dvec2 equatorialSpherical{ 0.0 };
        glm::dvec3 equatorialCartesian{ 0.0 };
        glm::dvec3 position3d{ 0.0 };
	};

	class WwtDataHandler {
	
	public:
		WwtDataHandler() = default;
		~WwtDataHandler();

        void loadImages(const std::string& root, const std::string& directory, 
                       std::vector<std::filesystem::path>& speckFiles);
        int nLoadedImages() const;
        const ImageData& getImage(int i) const;

	private:
        void saveImageFromNode(tinyxml2::XMLElement* node, std::string collection);
        void saveImagesFromXml(tinyxml2::XMLElement* root, std::string collection);

        // Images
		std::vector<ImageData> _images;
		std::vector<tinyxml2::XMLDocument*> _xmls;
        int _nMatched3dPositions = 0;

		// 3D position data loaded from speck files
		std::unordered_map<std::string, glm::dvec3> _3dPositions;
	};
}



#endif // __OPENSPACE_MODULE_SKYBROWSER___WWTDATAHANDLER___H__

