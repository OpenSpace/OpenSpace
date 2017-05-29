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
#include <modules/globebrowsing/models/job/subsiteinitializationjob.h>
#include <ghoul/filesystem/filesystem.h>

namespace {
	const std::string _loggerCat = "AsyncSurfaceModelProvider";
}

namespace openspace {
namespace globebrowsing {

AsyncSurfaceModelProvider::AsyncSurfaceModelProvider(std::shared_ptr<ThreadPool> diskToRamPool, std::shared_ptr<ThreadPool> ramToGpuPool, Renderable* parent)
	: _diskToRamJobManager(diskToRamPool)
	, _ramToGpuJobManager(ramToGpuPool)
	, _parent(parent)
{}

bool AsyncSurfaceModelProvider::enqueueModelIO(const std::shared_ptr<Subsite> subsite, const int level) {
	if (satisfiesEnqueueCriteria(subsite->hashKey(level))) {
		auto job = std::make_shared<SurfaceModelLoadJob>(subsite, level);
		_diskToRamJobManager.enqueueJob(job);
		_enqueuedModelRequests[subsite->hashKey(level)] = subsite;
		return true;
	}
	return false;
}

std::vector<std::shared_ptr<SubsiteModels>> AsyncSurfaceModelProvider::getLoadedModels() {

	std::vector<std::shared_ptr<SubsiteModels>> loadedModels;
	if (_diskToRamJobManager.numFinishedJobs() > 0) {
		std::shared_ptr<SubsiteModels> subsiteModels = _diskToRamJobManager.popFinishedJob()->product();
		enqueueSubsiteInitialization(subsiteModels);
	}

	std::vector<std::shared_ptr<SubsiteModels>> initializedModels;
	if (_ramToGpuJobManager.numFinishedJobs() > 0) {
		std::shared_ptr<SubsiteModels> subsiteModels = _ramToGpuJobManager.popFinishedJob()->product();
		unmapBuffers(subsiteModels);
		initializedModels.push_back(subsiteModels);
		_enqueuedModelRequests.erase(hashKey(subsiteModels->site, subsiteModels->drive, subsiteModels->level));
	}
	return initializedModels;
}

void AsyncSurfaceModelProvider::clearQueuesAndJobs() {
	_ramToGpuJobManager.clearEnqueuedJobs();
	_diskToRamJobManager.clearEnqueuedJobs();
	_enqueuedModelRequests.clear();
}

bool AsyncSurfaceModelProvider::satisfiesEnqueueCriteria(const uint64_t hashKey) const {
	return _enqueuedModelRequests.find(hashKey) == _enqueuedModelRequests.end();
}

void AsyncSurfaceModelProvider::enqueueSubsiteInitialization(const std::shared_ptr<SubsiteModels> subsiteModels) {
	subsiteModels->model->initialize(_parent);

	GLuint textureID;
	GLsizei mipLevelCount = 1;

	const clock_t begin_time = clock();

	glGenTextures(1, &textureID);
	subsiteModels->textureID = textureID;
	glBindTexture(GL_TEXTURE_2D_ARRAY, textureID);

	//Allocate the storage.
	glTexStorage3D(GL_TEXTURE_2D_ARRAY, mipLevelCount, GL_RGBA8, 1024, 1024, subsiteModels->textures.size());

	//Upload pixel data.
	//The first 0 refers to the mipmap level (level 0, since there's only 1)
	//The following 2 zeroes refers to the x and y offsets in case you only want to specify a subrectangle.
	//The final 0 refers to the layer index offset (we start from index 0 and have 2 levels).
	//Altogether you can specify a 3D box subset of the overall texture, but only one mip level at a time.
	int counter = 0;
	for (auto texture : subsiteModels->textures) {
		glTexSubImage3D(GL_TEXTURE_2D_ARRAY, 0, 0, 0, counter, 1024, 1024, 1, GL_RGBA, GL_UNSIGNED_BYTE, texture->pixelData());
		counter++;
	}
	// Temporary solution to release memory and throw texture id
	for (auto texture : subsiteModels->textures) {
		texture = nullptr;
	}

	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

	auto job = std::make_shared<SubsiteInitializationJob>(subsiteModels);
	_ramToGpuJobManager.enqueueJob(job);
}

void AsyncSurfaceModelProvider::unmapBuffers(const std::shared_ptr<SubsiteModels> subsiteModels) {
	subsiteModels->model->unmapBuffers();
}

uint64_t AsyncSurfaceModelProvider::hashKey(const std::string site, const std::string drive, const int level) {
	uint64_t key = 0LL;
	int siteNumber = std::stoi(site);
	int driveNumber = std::stoi(drive);

	key |= level;
	key |= siteNumber << 5;
	key |= ((uint64_t)driveNumber) << 35;

	return key;
}


} // namespace globebrowsing
} // namespace openspace