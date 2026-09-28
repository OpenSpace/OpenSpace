/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/solarbrowsing/solarbrowsingmodule.h>

#include <modules/solarbrowsing/rendering/renderablesolarimagery.h>
#include <modules/solarbrowsing/rendering/renderablesolarimageryprojection.h>
#include <modules/solarbrowsing/tasks/helioviewerdownloadtask.h>
#include <openspace/documentation/documentation.h>
#include <openspace/filesystem/cachemanager.h>
#include <openspace/filesystem/filesystem.h>
#include <openspace/misc/assert.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/task.h>

namespace openspace {

SolarBrowsingModule::SolarBrowsingModule()
    : OpenSpaceModule(Name)
{}

void SolarBrowsingModule::internalInitialize(const Dictionary&) {
    TemplateFactory<Renderable>* fRenderable =
        FactoryManager::ref().factory<Renderable>();
    assert_msg(fRenderable, "No renderable factory existed");

    fRenderable->registerClass<RenderableSolarImagery>("RenderableSolarImagery");
    fRenderable->registerClass<RenderableSolarImageryProjection>(
        "RenderableSolarImageryProjection"
    );

    TemplateFactory<Task>* fTask = FactoryManager::ref().factory<Task>();
    assert_msg(fTask, "No task factory existed");

    fTask->registerClass<HelioviewerDownloadTask>("HelioviewerDownloadTask");

    const std::filesystem::path cacheDirectory = absPath(
        "${SYNC_DYNAMIC}/solarbrowsing/cache"
    );

    if (!std::filesystem::is_directory(cacheDirectory)) {
        std::filesystem::create_directories(cacheDirectory);
    }

    assert_msg(
        std::filesystem::is_directory(cacheDirectory),
        "Cache directory did not exist"
    );
    assert_msg(!_cacheManager, "CacheManager was already created");

    _cacheManager = std::make_unique<filesystem::CacheManager>(cacheDirectory);
    assert_msg(_cacheManager, "CacheManager creation failed");
}

filesystem::CacheManager* SolarBrowsingModule::cacheManager() const {
    return _cacheManager.get();
}

std::vector<openspace::Documentation> SolarBrowsingModule::documentations() const {
    return {
        RenderableSolarImagery::Documentation(),
        RenderableSolarImageryProjection::Documentation(),
        HelioviewerDownloadTask::Documentation()
    };
}

} // namespace openspace
