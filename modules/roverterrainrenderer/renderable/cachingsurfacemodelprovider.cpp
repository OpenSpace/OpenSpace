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

#include <modules/roverterrainrenderer/renderable/cachingsurfacemodelprovider.h>
#include <modules/globebrowsing/src/lruthreadpool.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/io/texture/texturereader.h>

namespace {
    const std::string _loggerCat = "CachingSurfaceModelProvider";
}

namespace openspace {

CachingSurfaceModelProvider::CachingSurfaceModelProvider(Renderable* parent)
    : _parent(parent)
{
    double cacheSize = 20;
    _asyncSurfaceModelProvider = std::make_shared<AsyncSurfaceModelProvider>(parent);
    _modelCache = std::make_shared<ModelCache>(static_cast<size_t>(cacheSize));
}

std::vector<std::shared_ptr<SubsiteModels>> CachingSurfaceModelProvider::getModels(const std::vector<std::shared_ptr<Subsite>> subsites,
    const int level) {

    // Reset the fading direction automatically makes all unwanted models to fade out
    auto itemList = _modelCache->list();
    for (auto entry : itemList) {
        entry.second->setFadeDirection(-1);
    }

    // Temporary solution to not load LOD 0 models
    if(level > 0) {
        for (auto subsite : subsites) {
            bool requestedExistsInCache = true;
            ProviderSubsiteKey key = { level, subsite->site, subsite->drive };
            if (_modelCache->exist(key)) {
                _modelCache->get(key)->setFadeDirection(1);
            }
            else {
                for(const int& tempLevel : subsite->availableLevels){
                    // Only enqueue the model if the LOD exists
                    if(tempLevel == level) {
                        _asyncSurfaceModelProvider->enqueueModelIO(subsite, level);
                        break;
                    }
                }
                // Check for available LODs above the requested.
                std::vector<int> levelsAbove = getLevelsAbove(subsite->availableLevels, level);
                std::vector<int>::reverse_iterator rit = levelsAbove.rbegin();
                for (; rit != levelsAbove.rend(); ++rit){
                    ProviderSubsiteKey keyLowerLevel = { *rit, subsite->site, subsite->drive };
                    // If the cache holds the correct mesh but with lower resolution than requested,
                    // fade in that model
                    if (_modelCache->exist(keyLowerLevel)) {
                        _modelCache->get(keyLowerLevel)->setFadeDirection(1);
                        break;
                    }
                    // If the cache doesn't hold the LOD above, enqueue it. The LOD is much smaller
                    // in filesize and will then be loaded faster.
                    else {
                        _asyncSurfaceModelProvider->enqueueModelIO(subsite, *rit);
                    }
                }
            }
        }
    }

    std::vector<std::shared_ptr<SubsiteModels>> vectorOfSubsiteModels;
    // Save all models that are still visible and fade at the same time
    auto itemList2 = _modelCache->list();
    for (auto entry : itemList2) {
        if (entry.second->alpha() > 0.0) {
            vectorOfSubsiteModels.push_back(entry.second);
        }
        entry.second->fade();
    }
    return vectorOfSubsiteModels;
}

void CachingSurfaceModelProvider::update() {
    initModelsFromLoadedData();
}

std::vector<int> CachingSurfaceModelProvider::getLevelsAbove(const std::vector<int> availableLevels, const int requestedLevel) {
    std::vector<int> levelsAbove;
    for (const int& level : availableLevels) {
        if(level < requestedLevel) {
            levelsAbove.push_back(level);
        }
    }
    return levelsAbove;
}

void CachingSurfaceModelProvider::initModelsFromLoadedData() {
    std::vector<std::shared_ptr<SubsiteModels>> vectorOfSubsiteModels =
        _asyncSurfaceModelProvider->getLoadedModels();

    for (auto subsiteModels : vectorOfSubsiteModels) {
        ProviderSubsiteKey key = { subsiteModels->level, subsiteModels->site, subsiteModels->drive };
        _modelCache->put(key, subsiteModels);
    }
}

} // namespace openspace
