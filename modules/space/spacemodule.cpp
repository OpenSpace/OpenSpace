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

#include <modules/space/spacemodule.h>

#include <openspace/rendering/renderable.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/util/factorymanager.h>

#include <ghoul/misc/assert.h>

#include <modules/space/rendering/renderableconstellationbounds.h>
#include <modules/space/rendering/renderableplanet.h>
#include <modules/space/rendering/renderablerings.h>
#include <modules/space/rendering/renderablestars.h>
#include <modules/space/rendering/simplespheregeometry.h>

#include <modules/space/translation/keplertranslation.h>
#include <modules/space/translation/spicetranslation.h>
#include <modules/space/translation/tletranslation.h>

#include <modules/space/rotation/spicerotation.h>

namespace openspace {

SpaceModule::SpaceModule()
    : OpenSpaceModule("Space")
{}

void SpaceModule::internalInitialize() {
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<planetgeometry::PlanetGeometry>>(),
        "PlanetGeometry"
    );

    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "Renderable factory was not created");

    fRenderable->registerClass<RenderableConstellationBounds>("RenderableConstellationBounds");
    fRenderable->registerClass<RenderablePlanet>("RenderablePlanet");
    fRenderable->registerClass<RenderableRings>("RenderableRings");
    fRenderable->registerClass<RenderableStars>("RenderableStars");

    auto fTranslation = FactoryManager::ref().factory<Translation>();
    ghoul_assert(fTranslation, "Ephemeris factory was not created");

    fTranslation->registerClass<KeplerTranslation>("KeplerTranslation");
    fTranslation->registerClass<SpiceTranslation>("SpiceTranslation");
    fTranslation->registerClass<TLETranslation>("TLETranslation");

    auto fRotation = FactoryManager::ref().factory<Rotation>();
    ghoul_assert(fRotation, "Rotation factory was not created");

    fRotation->registerClass<SpiceRotation>("SpiceRotation");

    auto fPlanetGeometry = FactoryManager::ref().factory<planetgeometry::PlanetGeometry>();
    ghoul_assert(fPlanetGeometry, "Planet geometry factory was not created");
    fPlanetGeometry->registerClass<planetgeometry::SimpleSphereGeometry>("SimpleSphere");
}

std::vector<Documentation> SpaceModule::documentations() const {
    return {
        SpiceRotation::Documentation(),
        SpiceTranslation::Documentation(),
        RenderableRings::Documentation(),
        planetgeometry::PlanetGeometry::Documentation()
    };
}

} // namespace openspace
