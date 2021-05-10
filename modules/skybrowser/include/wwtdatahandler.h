#ifndef __OPENSPACE_MODULE_SKYBROWSER___WWTDATAHANDLER___H__
#define __OPENSPACE_MODULE_SKYBROWSER___WWTDATAHANDLER___H__

#include <openspace/documentation/documentation.h>
#include <modules/skybrowser/tinyxml2/tinyxml2.h>
#include <unordered_map>
// For speck loading
#include <ghoul/glm.h>
#include <ghoul/misc/boolean.h>
#include <filesystem>
#include <optional>
#include <string>
#include <vector>

namespace openspace::documentation { struct Documentation; }

// Copied from the branch feature/speck-loader
// Should be changed to a more general way of loading
// Speck files one that has been created
namespace openspace::speck {

	BooleanType(SkipAllZeroLines);

	struct Dataset {
		struct Variable {
			int index;
			std::string name;
		};
		std::vector<Variable> variables;

		struct Texture {
			int index;
			std::string file;
		};
		std::vector<Texture> textures;

		int textureDataIndex = -1;
		int orientationDataIndex = -1;

		struct Entry {
			glm::dvec3 position;
			std::vector<float> data;
			std::optional<std::string> comment;
		};
		std::vector<Entry> entries;
	};

	// In-out methods
	Dataset loadSpeckFile(std::filesystem::path path,
		SkipAllZeroLines skipAllZeroLines = SkipAllZeroLines::Yes);

} // namespace openspace::speck

namespace openspace {

	struct ImageData {
		std::string name;
		std::string thumbnailUrl;
		std::string credits;
		std::string creditsUrl;
		glm::dvec2 celestCoords;
		std::string collection;
		float zoomLevel;
		bool hasCoords;
	    glm::dvec3 position3d;
	};

	struct ImageCollection {
		std::string name;
		std::string url;
		bool loaded = false;
	};

	class WWTDataHandler {
	
	public:
		WWTDataHandler() = default;
		~WWTDataHandler();
		// Image downloading and xml parsing
		bool downloadFile(std::string& url, std::string& fileDestination);
		void loadWTMLCollectionsFromURL(std::string url, std::string fileName);
		void loadWTMLCollectionsFromDirectory(std::string directory);
		int loadAllImagesFromXMLs();

		const std::vector<ImageCollection>& getAllImageCollectionUrls() const;
		const std::vector<ImageData>& getLoadedImages() const;
		void loadSpeckData(speck::Dataset& dataset);

	private:
		void loadImagesFromXML(tinyxml2::XMLElement* node, std::string collectionName);
		int loadImage(tinyxml2::XMLElement* imageSet, std::string collectionName);
		void setImageDataValues(tinyxml2::XMLElement* node, std::string credits, std::string creditsUrl, std::string thumbnail, std::string collectionName, ImageData& img);

		std::string getChildNodeContentFromImageSet(tinyxml2::XMLElement* imageSet, std::string elementName);
		std::string getURLFromPlace(tinyxml2::XMLElement* place);
		tinyxml2::XMLElement* getDirectChildNode(tinyxml2::XMLElement* node, std::string name);
		tinyxml2::XMLElement* getChildNode(tinyxml2::XMLElement* node, std::string name);

		std::vector<ImageData> images;
		std::vector<ImageCollection> imageUrls;
		std::vector<tinyxml2::XMLDocument*> xmls;
		// 3D position data loaded from speck files
		std::unordered_map<std::string, glm::dvec3> _3dPositions;
	};
}



#endif // __OPENSPACE_MODULE_SKYBROWSER___WWTDATAHANDLER___H__

