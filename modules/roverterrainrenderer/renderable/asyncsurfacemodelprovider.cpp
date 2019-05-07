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

#include <modules/roverterrainrenderer/renderable/asyncsurfacemodelprovider.h>
#include <ghoul/logging/logmanager.h>
#include <modules/roverterrainrenderer/loadjob/surfacemodelloadjob.h>
#include <modules/roverterrainrenderer/loadjob/subsiteinitializationjob.h>
#include <ghoul/filesystem/filesystem.h>
#include <modules/roverterrainrenderer/opengl/texturearray.h>

namespace {
    const std::string _loggerCat = "AsyncSurfaceModelProvider";
}

namespace openspace {

AsyncSurfaceModelProvider::AsyncSurfaceModelProvider(Renderable* parent)
    : _diskToRamJobManager(globebrowsing:: LRUThreadPool<uint64_t>(1, 20))
    , _ramToGpuJobManager(globebrowsing::LRUThreadPool<uint64_t>(1, 20))
    , _parent(parent)
{}

bool AsyncSurfaceModelProvider::enqueueModelIO(const std::shared_ptr<Subsite> subsite, const int level) {
    if (satisfiesEnqueueCriteria(subsite->hashKey(level))) {
        auto job = std::make_shared<SurfaceModelLoadJob>(subsite, level);
        _diskToRamJobManager.enqueueJob(job, subsite->hashKey(level));
        _enqueuedModelRequests[subsite->hashKey(level)] = subsite;
        return true;
    }
    return false;
}

std::vector<std::shared_ptr<SubsiteModels>> AsyncSurfaceModelProvider::getLoadedModels() {
    if (_diskToRamJobManager.numFinishedJobs() > 0) {
        SubsiteModels subsiteModels = _diskToRamJobManager.popFinishedJob()->product();
        enqueueSubsiteInitialization(std::make_shared<SubsiteModels>(subsiteModels));
    }

    std::vector<std::shared_ptr<SubsiteModels>> initializedModels;
    if (_ramToGpuJobManager.numFinishedJobs() > 0) {
        SubsiteModels subsiteModels = _ramToGpuJobManager.popFinishedJob()->product();
        std::shared_ptr<SubsiteModels> m = std::make_shared<SubsiteModels>(subsiteModels);
        unmapBuffers(m);
        initializedModels.push_back(m);
        _enqueuedModelRequests.erase(m->hashKey());
    }
    return initializedModels;
}

bool AsyncSurfaceModelProvider::satisfiesEnqueueCriteria(const uint64_t hashKey) const {
    return _enqueuedModelRequests.find(hashKey) == _enqueuedModelRequests.end();
}

void AsyncSurfaceModelProvider::enqueueSubsiteInitialization(const std::shared_ptr<SubsiteModels> subsiteModels) {
    subsiteModels->model->initialize(_parent);

    const clock_t begin_time = clock();
    if (subsiteModels->textures.size() > 0) {
        TextureArray ta = TextureArray(subsiteModels->textures.at(0)->dimensions(), subsiteModels->textures.size());

        //subsiteModels->textureArray2 = std::make_shared<ghoul::opengl::Texture>(textureArray);

        subsiteModels->textureArray = std::make_shared<TextureArray>(ta);

        //Upload pixel data.
        //The first 0 refers to the mipmap level (level 0, since there's only 1)
        //The following 2 zeroes refers to the x and y offsets in case you only want to specify a subrectangle.
        //The final 0 refers to the layer index offset (we start from index 0 and have 2 levels).
        //Altogether you can specify a 3D box subset of the overall texture, but only one mip level at a time
        for (auto texture : subsiteModels->textures) {
            subsiteModels->textureArray->uploadTexture(texture->pixelData());
        }
        // Temporary solution to release memory and throw texture id
        for (auto texture : subsiteModels->textures) {
            texture = nullptr;
        }
    }
    if (subsiteModels->coloredTextures.size() > 0) {
        TextureArray coloredTextureArray = TextureArray(subsiteModels->coloredTextures.at(0)->dimensions(), subsiteModels->coloredTextures.size(), GL_RGB);

        //glGenTextures(1, &coloredTextureID);
        //subsiteModels->coloredTextureID = coloredTextureID;

        subsiteModels->coloredTextureArray = std::make_shared<TextureArray>(coloredTextureArray);

        subsiteModels->coloredTextureID = subsiteModels->coloredTextureArray->id();

        //glBindTexture(GL_TEXTURE_2D_ARRAY, coloredTextureID);

        //Allocate the storage.
        //glTexStorage3D(GL_TEXTURE_2D_ARRAY, mipLevelCount, GL_RGB8, 1344, 1200, subsiteModels->coloredTextures.size());

        int counter = 0;
        for (auto texture2 : subsiteModels->coloredTextures) {
            subsiteModels->coloredTextureArray->uploadTexture(texture2->pixelData());
            //glTexSubImage3D(GL_TEXTURE_2D_ARRAY, 0, 0, 0, counter, 1344, 1200, 1, GL_RGB, GL_UNSIGNED_BYTE, texture2->pixelData());
            counter++;
        }

        // Temporary solution to release memory and throw texture id
        for (auto texture2 : subsiteModels->coloredTextures) {
            texture2 = nullptr;
        }
    }

    glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    auto job = std::make_shared<SubsiteInitializationJob>(subsiteModels);
    _ramToGpuJobManager.enqueueJob(job, hashKey(subsiteModels->site, subsiteModels->drive, subsiteModels->level));
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


} // namespace openspace
