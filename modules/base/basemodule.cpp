/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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
#include <openspace/util/factorymanager.h>

#include <ghoul/misc/assert.h>

#include <modules/base/rendering/renderablemodel.h>
#include <modules/base/rendering/renderableconstellationbounds.h>
#include <modules/base/rendering/renderablestars.h>
#include <modules/base/rendering/renderabletrail.h>
#include <modules/base/rendering/renderablepath.h>
#include <modules/base/rendering/renderablesphere.h>
#include <modules/base/rendering/renderablesphericalgrid.h>
#include <modules/base/rendering/renderablefieldlines.h>
#include <modules/base/rendering/renderablecrawlingline.h>
#include <modules/base/rendering/renderableplanet.h>
#include <modules/base/rendering/simplespheregeometry.h>
#include <modules/base/rendering/renderableplane.h>
#include <modules/base/rendering/simplespheregeometry.h>
#include <modules/base/rendering/modelgeometry.h>
#include <modules/base/rendering/wavefrontgeometry.h>

namespace {
    const std::string _loggerCat = "BaseModule";
}

namespace openspace {

bool BaseModule::initialize() {
    FactoryManager::ref().addFactory(new ghoul::TemplateFactory<planetgeometry::PlanetGeometry>);
    FactoryManager::ref().addFactory(new ghoul::TemplateFactory<modelgeometry::ModelGeometry>);

    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "No renderable factory existed");

    fRenderable->registerClass<RenderablePlanet>("RenderablePlanet");
    fRenderable->registerClass<RenderableStars>("RenderableStars");
    fRenderable->registerClass<RenderableConstellationBounds>("RenderableConstellationBounds");
    fRenderable->registerClass<RenderablePath>("RenderablePath");
    fRenderable->registerClass<RenderableTrail>("RenderableTrail");
    fRenderable->registerClass<RenderableSphere>("RenderableSphere");
    fRenderable->registerClass<RenderableSphericalGrid>("RenderableSphericalGrid");
    fRenderable->registerClass<RenderableModel>("RenderableModel");
    fRenderable->registerClass<RenderablePlane>("RenderablePlane");
    fRenderable->registerClass<RenderableFieldlines>("RenderableFieldlines");
    fRenderable->registerClass<RenderableCrawlingLine>("RenderableCrawlingLine");

    auto fPlanetGeometry = FactoryManager::ref().factory<planetgeometry::PlanetGeometry>();
    ghoul_assert(fPlanetGeometry, "No planet geometry factory existed");
    fPlanetGeometry->registerClass<planetgeometry::SimpleSphereGeometry>("SimpleSphere");

    auto fModelGeometry = FactoryManager::ref().factory<modelgeometry::ModelGeometry>();
    ghoul_assert(fModelGeometry, "No model geometry factory existed");
    fModelGeometry->registerClass<modelgeometry::WavefrontGeometry>("WavefrontGeometry");

    return true;
}

bool BaseModule::deinitialize() {
    return false;
}

} // namespace openspace
