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

#include <modules/video/videomodule.h>

#include <modules/video/include/videotileprovider.h>
#include <modules/video/include/screenspacevideo.h>
#include <modules/video/include/renderablevideosphere.h>
#include <modules/video/include/renderablevideoplane.h>
#include <modules/globebrowsing/src/tileprovider/tileprovider.h>
#include <openspace/util/factorymanager.h>
#include <openspace/documentation/documentation.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/templatefactory.h>

namespace openspace {

VideoModule::VideoModule()
    : OpenSpaceModule(VideoModule::Name)
{}

void VideoModule::internalInitialize(const ghoul::Dictionary&) {
    ghoul::TemplateFactory<TileProvider>* fTileProvider =
        FactoryManager::ref().factory<TileProvider>();
    ghoul_assert(fTileProvider, "TileProvider factory was not created");
    fTileProvider->registerClass<VideoTileProvider>("VideoTileProvider");

    ghoul::TemplateFactory<ScreenSpaceRenderable>* fSsRenderable =
        FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fSsRenderable, "ScreenSpaceRenderable factory was not created");

    fSsRenderable->registerClass<ScreenSpaceVideo>("ScreenSpaceVideo");

    ghoul::TemplateFactory<Renderable>* fRenderable =
        FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "Renderable factory was not created");
    fRenderable->registerClass<RenderableVideoSphere>("RenderableVideoSphere");
    fRenderable->registerClass<RenderableVideoPlane>("RenderableVideoPlane");
}

std::vector<Documentation> VideoModule::documentations() const {
    return {
        RenderableVideoPlane::Documentation(),
        RenderableVideoSphere::Documentation(),
        ScreenSpaceVideo::Documentation(),
        VideoTileProvider::Documentation()
    };
}

} // namespace openspace
