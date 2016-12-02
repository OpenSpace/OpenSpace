/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <modules/base/basemodule.h>

#include <openspace/rendering/renderable.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/util/factorymanager.h>

#include <ghoul/misc/assert.h>

#include <modules/base/rendering/renderableconstellationbounds.h>
#include <modules/base/rendering/renderablemodel.h>
#include <modules/base/rendering/renderablepath.h>
#include <modules/base/rendering/renderableplanet.h>
#include <modules/base/rendering/renderablerings.h>
#include <modules/base/rendering/renderablesphere.h>
#include <modules/base/rendering/renderablesphericalgrid.h>
#include <modules/base/rendering/renderablestars.h>
#include <modules/base/rendering/renderabletrailorbit.h>
#include <modules/base/rendering/renderabletrailtrajectory.h>
#include <modules/base/rendering/simplespheregeometry.h>
#include <modules/base/rendering/renderableplane.h>
#include <modules/base/rendering/simplespheregeometry.h>
#include <modules/base/rendering/modelgeometry.h>
#include <modules/base/rendering/multimodelgeometry.h>
#include <modules/base/rendering/screenspaceimage.h>
#include <modules/base/rendering/screenspaceframebuffer.h>

#include <modules/base/translation/keplertranslation.h>
#include <modules/base/translation/statictranslation.h>
#include <modules/base/translation/spicetranslation.h>
#include <modules/base/translation/tletranslation.h>

#include <modules/base/rotation/staticrotation.h>
#include <modules/base/rotation/spicerotation.h>

#include <modules/base/scale/staticscale.h>

#include <ghoul/filesystem/filesystem>

namespace openspace {

BaseModule::BaseModule() 
    : OpenSpaceModule("Base")
{}

void BaseModule::internalInitialize() {
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<planetgeometry::PlanetGeometry>>(),
        "PlanetGeometry"
    );
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<modelgeometry::ModelGeometry>>(),
        "ModelGeometry"
    );
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<ScreenSpaceRenderable>>(),
        "ScreenSpaceRenderable"
    );

    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Rotation>>(),
        "Rotation"
    );
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<Scale>>(),
        "Scale"
    );

    auto fScreenSpaceRenderable = FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");

    fScreenSpaceRenderable->registerClass<ScreenSpaceImage>("ScreenSpaceImage");
    fScreenSpaceRenderable->registerClass<ScreenSpaceFramebuffer>("ScreenSpaceFramebuffer");

    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "Renderable factory was not created");

    fRenderable->registerClass<RenderableConstellationBounds>("RenderableConstellationBounds");
    fRenderable->registerClass<RenderableModel>("RenderableModel");
    fRenderable->registerClass<RenderablePath>("RenderablePath");
    fRenderable->registerClass<RenderablePlane>("RenderablePlane");
    fRenderable->registerClass<RenderablePlanet>("RenderablePlanet");
    fRenderable->registerClass<RenderableRings>("RenderableRings");
    fRenderable->registerClass<RenderableSphere>("RenderableSphere");
    fRenderable->registerClass<RenderableSphericalGrid>("RenderableSphericalGrid");
    fRenderable->registerClass<RenderableStars>("RenderableStars");
    fRenderable->registerClass<RenderableTrailOrbit>("RenderableTrailOrbit");
    fRenderable->registerClass<RenderableTrailTrajectory>("RenderableTrailTrajectory");

    auto fTranslation = FactoryManager::ref().factory<Translation>();
    ghoul_assert(fTranslation, "Ephemeris factory was not created");

    fTranslation->registerClass<KeplerTranslation>("KeplerTranslation");
    fTranslation->registerClass<StaticTranslation>("StaticTranslation");
    fTranslation->registerClass<SpiceTranslation>("SpiceTranslation");
    fTranslation->registerClass<TLETranslation>("TLETranslation");

    auto fRotation = FactoryManager::ref().factory<Rotation>();
    ghoul_assert(fRotation, "Rotation factory was not created");

    fRotation->registerClass<StaticRotation>("StaticRotation");
    fRotation->registerClass<SpiceRotation>("SpiceRotation");

    auto fScale = FactoryManager::ref().factory<Scale>();
    ghoul_assert(fScale, "Scale factory was not created");

    fScale->registerClass <StaticScale> ("StaticScale");

    auto fPlanetGeometry = FactoryManager::ref().factory<planetgeometry::PlanetGeometry>();
    ghoul_assert(fPlanetGeometry, "Planet geometry factory was not created");
    fPlanetGeometry->registerClass<planetgeometry::SimpleSphereGeometry>("SimpleSphere");

    auto fModelGeometry = FactoryManager::ref().factory<modelgeometry::ModelGeometry>();
    ghoul_assert(fModelGeometry, "Model geometry factory was not created");
    fModelGeometry->registerClass<modelgeometry::MultiModelGeometry>("MultiModelGeometry");
}

std::vector<Documentation> BaseModule::documentations() const {
    return {
        SpiceRotation::Documentation(),
        StaticScale::Documentation(),
        StaticTranslation::Documentation(),
        SpiceTranslation::Documentation(),
        RenderableRings::Documentation(),
        RenderableTrailOrbit::Documentation(),
        RenderableTrailTrajectory::Documentation(),
        modelgeometry::ModelGeometry::Documentation(),
        planetgeometry::PlanetGeometry::Documentation()
    };
}

} // namespace openspace
