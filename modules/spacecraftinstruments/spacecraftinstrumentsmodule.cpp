/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/spacecraftinstruments/spacecraftinstrumentsmodule.h>

#include <modules/spacecraftinstruments/dashboard/dashboarditeminstruments.h>
#include <modules/spacecraftinstruments/rendering/renderablecrawlingline.h>
#include <modules/spacecraftinstruments/rendering/renderablefov.h>
#include <modules/spacecraftinstruments/rendering/renderablemodelprojection.h>
#include <modules/spacecraftinstruments/rendering/renderableplaneprojection.h>
#include <modules/spacecraftinstruments/rendering/renderableplanetprojection.h>
#include <modules/spacecraftinstruments/rendering/renderableshadowcylinder.h>
#include <modules/spacecraftinstruments/util/decoder.h>
#include <modules/spacecraftinstruments/util/imagesequencer.h>
#include <modules/spacecraftinstruments/util/instrumentdecoder.h>
#include <modules/spacecraftinstruments/util/targetdecoder.h>
#include <openspace/documentation/documentation.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/templatefactory.h>

namespace openspace {

ghoul::opengl::ProgramObjectManager SpacecraftInstrumentsModule::ProgramObjectManager;

SpacecraftInstrumentsModule::SpacecraftInstrumentsModule() : OpenSpaceModule(Name) {}

void SpacecraftInstrumentsModule::internalInitialize(const ghoul::Dictionary&) {
    ImageSequencer::initialize();

    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Decoder>>(),
        "Decoder"
    );

    auto fDashboard = FactoryManager::ref().factory<DashboardItem>();
    ghoul_assert(fDashboard, "Dashboard factory was not created");

    fDashboard->registerClass<DashboardItemInstruments>("DashboardItemInstruments");

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

void SpacecraftInstrumentsModule::internalDeinitialize() {
    ImageSequencer::deinitialize();
}

void SpacecraftInstrumentsModule::internalDeinitializeGL() {
    ProgramObjectManager.releaseAll(ghoul::opengl::ProgramObjectManager::Warnings::Yes);
}

std::vector<documentation::Documentation>
SpacecraftInstrumentsModule::documentations() const
{
    return {
        RenderableFov::Documentation(),
        RenderableModelProjection::Documentation(),
        RenderablePlanetProjection::Documentation(),
        ProjectionComponent::Documentation()
    };
}

} // namespace openspace
