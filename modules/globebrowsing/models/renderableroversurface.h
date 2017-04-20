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
#include <modules/globebrowsing/models/surfacemodelrenderingselector.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/models/cachingsurfacemodelprovider.h>

namespace openspace {
namespace globebrowsing {

class RenderableRoverSurface : public Renderable {
public:
	struct SubSite {
		std::string site, drive;
		double lat, lon;
	};

	struct GeneralProperties {
		properties::BoolProperty isEnabled;
	};

	RenderableRoverSurface(const ghoul::Dictionary& dictionary);
	~RenderableRoverSurface() = default;

	bool initialize() override;
	bool deinitialize() override;
	bool isReady() const override;

	void render(const RenderData& data) override;
	void update(const UpdateData& data) override;
	
	std::vector<glm::dvec3> pathChunkVector(const TileIndex& tileIndex) const;

private:
	void extractCoordinates();
	std::vector<std::string> extractFileNames(const std::string filePath);

	std::vector<std::string> _fileNames;
	std::vector<glm::fvec2> _coordinates;
	std::vector<SubSite> _subSites;

	GeneralProperties _generalProperties;


	std::string _roverLocationPath;
	std::string _textFilePath;
	std::string _modelPath;
	std::string _texturePath;
	std::string _absModelPath;

	globebrowsing::RenderableGlobe* _globe;
	std::shared_ptr<globebrowsing::ChunkedLodGlobe> _chunkedLodGlobe;
	std::unordered_map <uint64_t, std::vector<glm::dvec3 >> _pathChunks;

	std::shared_ptr<CachingSurfaceModelProvider> _cachingProvider;
	bool loadedOnce = true;

	//SurfaceModelRenderingSelector _surfaceModelRenderingSelector;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLEROVERSURFACE___H__