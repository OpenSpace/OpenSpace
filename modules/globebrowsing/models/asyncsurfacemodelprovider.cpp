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

#include <modules/globebrowsing/models/asyncsurfacemodelprovider.h>
#include <ghoul/logging/logmanager.h>
#include <modules/globebrowsing/tile/loadjob/surfacemodelloadjob.h>
#include <ghoul/filesystem/filesystem.h>

namespace {
	const std::string _loggerCat = "AsyncSurfaceModelProvider";
}

namespace openspace {
namespace globebrowsing {

AsyncSurfaceModelProvider::AsyncSurfaceModelProvider(std::shared_ptr<ThreadPool> pool)
	: _concurrentJobManager(pool)
{}

bool AsyncSurfaceModelProvider::enqueueModelIO(const Subsite subsite, const int level) {
	if (satisfiesEnqueueCriteria(subsite.hashKey(level))) {
		auto job = std::make_shared<SurfaceModelLoadJob>(subsite, level);
		_concurrentJobManager.enqueueJob(job);

		_enqueuedTileRequests[subsite.hashKey(level)] = subsite;

		return true;
	}
	return false;
}

std::vector<std::shared_ptr<SubsiteModels>> AsyncSurfaceModelProvider::getLoadedModels() {
	std::vector<std::shared_ptr<SubsiteModels>> loadedModels;

	if (_concurrentJobManager.numFinishedJobs() > 0) {
		loadedModels.push_back(_concurrentJobManager.popFinishedJob()->product());
	}
	return loadedModels;
}

bool AsyncSurfaceModelProvider::satisfiesEnqueueCriteria(const uint64_t hashKey) const {
	return _enqueuedTileRequests.find(hashKey) == _enqueuedTileRequests.end();
}


} // namespace globebrowsing
} // namespace openspace