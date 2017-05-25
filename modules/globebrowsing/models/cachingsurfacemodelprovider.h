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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___CACHING_SURFACE_MODEL_PROVIDER__H_
#define __OPENSPACE_MODULE_GLOBEBROWSING___CACHING_SURFACE_MODEL_PROVIDER__H_

#include <modules/globebrowsing/models/asyncsurfacemodelprovider.h>
#include <modules/globebrowsing/cache/lrucache.h>
#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/globebrowsing/models/subsitemodels.h>
#include <modules/globebrowsing/models/subsite.h>

#include <memory>

namespace openspace {
namespace globebrowsing {

using ModelCache = cache::LRUCache<uint64_t, std::shared_ptr<SubsiteModels>>;

class CachingSurfaceModelProvider {
public:
	CachingSurfaceModelProvider(Renderable* parent);

	std::vector<std::shared_ptr<SubsiteModels>> getModels(const std::vector<std::shared_ptr<Subsite>> Subsites, const int level);

	void update(Renderable* parent);
	
	void setLevel(const int level);

private:
	std::shared_ptr<AsyncSurfaceModelProvider> _asyncSurfaceModelProvider;
	std::shared_ptr<ModelCache> _modelCache;
	std::vector<int> getLevelsAbove(const std::vector<int> availableLevels, const int requestedLevel);

	void initModelsFromLoadedData(Renderable* parent);
	void clearQueuesAndJobs();

	uint64_t hashKey(const std::string site, const std::string drive, const int level);
	
	int _prevLevel = -1;

	Renderable* _parent;

	Vertex* _vertexBufferData = nullptr;

	int _previousLevel = 0;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___CACHING_SURFACEMODEL_PROVIDER__H_
