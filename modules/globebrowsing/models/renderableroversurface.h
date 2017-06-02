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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLEROVERSURFACE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLEROVERSURFACE___H__

#include <openspace/rendering/renderable.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/models/cachingsurfacemodelprovider.h>
#include <modules/globebrowsing/models/roverpathfilereader.h>
#include <modules/globebrowsing/models/lodmodelswitch.h>
#include <modules/globebrowsing/models/modelprovider.h>
#include <modules/globebrowsing/models/subsite.h>
#include <modules/globebrowsing/models/renderableexplorationpath.h>

namespace openspace {
namespace globebrowsing {

class RenderableRoverSurface : public Renderable {
public:

	struct GeneralProperties {
		properties::BoolProperty isEnabled;
		properties::BoolProperty enablePath;
		properties::BoolProperty lockSubsite;
	};

	RenderableRoverSurface(const ghoul::Dictionary& dictionary);
	~RenderableRoverSurface() = default;

	bool initialize() override;
	bool deinitialize() override;
	bool isReady() const override;

	void render(const RenderData& data) override;
	void update(const UpdateData& data) override;
	
private:
	std::vector<std::shared_ptr<SubsiteModels>> calculateSurfacePosition(std::vector<std::shared_ptr<SubsiteModels>> vector);
	void lockSubsite(const int level, std::vector<std::shared_ptr<Subsite>> subsites);

	std::vector<std::shared_ptr<Subsite>> _subsites;
	std::vector<std::shared_ptr<Subsite>> _prevSubsites;
	std::vector<std::shared_ptr<Subsite>> _subsitesWithModels;

	bool _pressedOnce = false;

	GeneralProperties _generalProperties;

	std::string _roverLocationPath;
	std::string _modelPath;
	std::string _texturePath;
	std::string _absModelPath;
	std::string _absTexturePath;

	properties::Vec3Property _debugModelRotation;

	globebrowsing::RenderableGlobe* _globe;
	std::shared_ptr<globebrowsing::ChunkedLodGlobe> _chunkedLodGlobe;

	std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;

	std::shared_ptr<CachingSurfaceModelProvider> _cachingModelProvider;

	int _prevLevel;

	bool _isFirst;
	std::shared_ptr<SubsiteModels> _prevSubsiteModels;

	LodModelSwitch _modelSwitch;

	std::shared_ptr<RenderableExplorationPath> _renderableExplorationPath;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLEROVERSURFACE___H__