/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/scripting/lualibrary.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "Decides if this module should be enabled."
    };

    struct [[codegen::Dictionary(VideoModule)]] Parameters {
        // [[codegen::verbatim(EnabledInfo.description)]]
        std::optional<bool> enabled;
    };

#include "videomodule_codegen.cpp"
} // namespace

namespace openspace {

VideoModule::VideoModule()
    : OpenSpaceModule(VideoModule::Name)
    , _enabled(EnabledInfo)
{
    addProperty(_enabled);
}

void VideoModule::internalInitialize(const ghoul::Dictionary& dict) {
    const Parameters p = codegen::bake<Parameters>(dict);

    _enabled = p.enabled.value_or(_enabled);

    ghoul::TemplateFactory<globebrowsing::TileProvider>* fTileProvider =
        FactoryManager::ref().factory<globebrowsing::TileProvider>();
    ghoul_assert(fTileProvider, "TileProvider factory was not created");
    fTileProvider->registerClass<globebrowsing::VideoTileProvider>("VideoTileProvider");

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

std::vector<documentation::Documentation> VideoModule::documentations() const {
    return {
        RenderableVideoPlane::Documentation(),
        RenderableVideoSphere::Documentation(),
        ScreenSpaceVideo::Documentation(),
        globebrowsing::VideoTileProvider::Documentation()
    };
}

} // namespace openspace
