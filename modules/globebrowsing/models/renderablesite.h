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

namespace openspace {

namespace modelgeometry {
	class ModelGeometry;
}

namespace globebrowsing {

	struct SiteInformation {
		int sol;
		std::vector<glm::dvec2> lonlatCoordinates;
	};

	struct Models {
		std::string _texturePath;
		modelgeometry::ModelGeometry* _model;
		std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;
		std::unique_ptr<ghoul::opengl::Texture> _texture;
	};

	class RenderableExplorationPath;

class RenderableSite : public Renderable {

public:

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
	std::vector < std::string> loadTexturePaths(std::string txtPath);

	std::string _filePath;
	std::string _colorTexturePath;

	bool extractCoordinates();
	void loadTexture();

	std::vector<glm::dvec2> _pathCoordinates;
	std::map<int, glm::dvec2> _siteCoordinates;

	properties::StringProperty _textureTxtPath;
	std::vector<std::string> _fileNames;

	std::shared_ptr<RenderableExplorationPath> _renderableExplorationPath;

	bool _isReady;

	GeneralProperties _generalProperties;

	properties::Vec3Property _debugModelRotation;

	std::vector<Models> _models;
	globebrowsing::RenderableGlobe* _globe;

	glm::dvec3 _sunPos;
};

}
}
#endif // __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLESITE___H__
