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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___ASYNC_SURFACE_MODEL_PROVIDER__H_
#define __OPENSPACE_MODULE_GLOBEBROWSING___ASYNC_SURFACE_MODEL_PROVIDER__H_

#include <modules/globebrowsing/cache/lrucache.h>
#include <modules/globebrowsing/other/threadpool.h>
#include <modules/globebrowsing/tile/loadjob/surfacemodelloadjob.h>
#include <modules/globebrowsing/models/subsitemodels.h>
#include <modules/globebrowsing/models/subsite.h>
#include <modules/globebrowsing/other/prioritizingconcurrentjobmanager.h>

namespace openspace {
namespace globebrowsing {

class AsyncSurfaceModelProvider {
public:
	AsyncSurfaceModelProvider(Renderable* parent);

	bool enqueueModelIO(const std::shared_ptr<Subsite> subsite, const int level);
	
	std::vector<std::shared_ptr<SubsiteModels>> getLoadedModels();

	void clearQueuesAndJobs();

protected:
	virtual bool satisfiesEnqueueCriteria(const uint64_t hashKey) const;

private:
	PrioritizingConcurrentJobManager<SubsiteModels, uint64_t>
		_diskToRamJobManager;
	PrioritizingConcurrentJobManager<SubsiteModels, uint64_t>
		_ramToGpuJobManager;

	Renderable* _parent;

	void enqueueSubsiteInitialization(const std::shared_ptr<SubsiteModels> subsiteModels);
	void unmapBuffers(const std::shared_ptr<SubsiteModels> subsiteModels);

	std::unordered_map <uint64_t, std::shared_ptr<Subsite>> _enqueuedModelRequests;

	uint64_t hashKey(const std::string site, const std::string drive, const int level);
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___ASYNC_SURFACEMODEL_PROVIDER__H_
