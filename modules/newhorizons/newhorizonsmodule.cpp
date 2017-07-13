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

#include <modules/newhorizons/newhorizonsmodule.h>

#include <modules/newhorizons/rendering/renderablecrawlingline.h>
#include <modules/newhorizons/rendering/renderablefov.h>
#include <modules/newhorizons/rendering/renderablemodelprojection.h>
#include <modules/newhorizons/rendering/renderableplaneprojection.h>
#include <modules/newhorizons/rendering/renderableplanetprojection.h>
#include <modules/newhorizons/rendering/renderableshadowcylinder.h>

#include <modules/newhorizons/util/decoder.h>
#include <modules/newhorizons/util/instrumentdecoder.h>
#include <modules/newhorizons/util/targetdecoder.h>

#include <openspace/documentation/documentation.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/factorymanager.h>

#include <ghoul/misc/assert.h>

namespace openspace {

NewHorizonsModule::NewHorizonsModule() : OpenSpaceModule(Name) {}

void NewHorizonsModule::internalInitialize() {
    ImageSequencer::initialize();

    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Decoder>>(),
        "Decoder"
    );

    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "No renderable factory existed");

    fRenderable->registerClass<RenderableShadowCylinder>("RenderableShadowCylinder");
    fRenderable->registerClass<RenderableCrawlingLine>("RenderableCrawlingLine");
    fRenderable->registerClass<RenderableFov>("RenderableFov");
    fRenderable->registerClass<RenderablePlaneProjection>("RenderablePlaneProjection");
    fRenderable->registerClass<RenderablePlanetProjection>("RenderablePlanetProjection");
    fRenderable->registerClass<RenderableModelProjection>("RenderableModelProjection");

    auto fDecoder = FactoryManager::ref().factory<Decoder>();
    fDecoder->registerClass<InstrumentDecoder>("Instrument");
    fDecoder->registerClass<TargetDecoder>("Target");
}

std::vector<documentation::Documentation> NewHorizonsModule::documentations() const {
    return {
        RenderableFov::Documentation(),
        RenderableModelProjection::Documentation(),
        RenderablePlanetProjection::Documentation(),
        ProjectionComponent::Documentation()
    };
}

} // namespace openspace
