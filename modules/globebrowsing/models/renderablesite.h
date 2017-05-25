/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLESITE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLESITE___H__

#include <openspace/rendering/renderable.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/models/renderableexplorationpath.h>
#include <ghoul/opengl/texture.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <modules/globebrowsing/tasks/imgreader.h>

namespace openspace {

namespace modelgeometry {
	class ModelGeometry;
}

namespace globebrowsing {

	struct Models {
		std::vector<std::string> _texturePaths;
		std::unique_ptr<modelgeometry::ModelGeometry> _model;
		std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;
		std::vector<std::unique_ptr<ghoul::opengl::Texture>> _textures;
	};

	class RenderableExplorationPath;

class RenderableSite : public Renderable {

public:

	struct SiteInformation {
		glm::dvec4 stationPosition;
		double previousStationHeight;
	};

	struct GeneralProperties {
		properties::BoolProperty isEnabled;
	};

	struct DebugProperties {
		properties::BoolProperty test;
	};

	RenderableSite(const ghoul::Dictionary& dictionary);
	~RenderableSite() = default;

	bool initialize() override;
	bool deinitialize() override;
	bool isReady() const override;

	void render(const RenderData& data) override;
	void update(const UpdateData& data) override;

private:
	void calculateSiteWorldCoordinates();

	std::vector < std::string> loadTexturePaths(std::string txtPath);

	std::string _filePath;
	std::string _colorTexturePath;

	bool extractCoordinates();
	void loadTexture();

	std::vector<glm::dvec2> _pathLatlonCoordinates;
	std::map<int, glm::dvec2> _siteLatlonCoordinates;
	std::vector<glm::dvec2> _siteCartesianWorldCoordinates;
	std::vector<SiteInformation> _sitesModelCoordinates;

	properties::StringProperty _textureTxtPath;
	std::vector<std::string> _fileNames;
	std::vector<ImgReader::PointCloudInfo> _cameraInfoVector;

	std::shared_ptr<RenderableExplorationPath> _renderableExplorationPath;

	bool _isReady;
	bool _hasLoopedOnce;
	bool _isCloseEnough;

	double _cameraToPointDistance;

	GeneralProperties _generalProperties;

	properties::Vec3Property _debugModelRotation;
	properties::FloatProperty _debugModelScale;
	properties::BoolProperty _debugModelCullface;
	properties::BoolProperty _debugModelMastCamColor;
	properties::BoolProperty _recalculateHeight;
	properties::BoolProperty _debugUseUVCoord;
	properties::BoolProperty _debugUseMultipleTextures;

	properties::PropertyOwner _renderableSitePropertyOwner;

	bool _prevDebugModelMastCamColor;

	std::vector<Models> _models;
	globebrowsing::RenderableGlobe* _globe;

	glm::dvec3 _sunPos;

	glm::dvec2 _tempLonLat;
	glm::dvec2 _tempLonLatSite;

	int kalle22 = 0;

	int _lookup[21 * 3];
};

}
}
#endif // __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLESITE___H__
