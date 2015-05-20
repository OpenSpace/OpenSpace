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

#include <modules/newhorizons/newhorizonsmodule.h>

#include <openspace/rendering/renderable.h>
#include <openspace/util/factorymanager.h>

#include <ghoul/misc/assert.h>

#include <modules/newhorizons/rendering/planetgeometryprojection.h>
#include <modules/newhorizons/rendering/renderablefov.h>
#include <modules/newhorizons/rendering/renderableplaneprojection.h>
#include <modules/newhorizons/rendering/renderableplanetprojection.h>
#include <modules/newhorizons/rendering/simplespheregeometryprojection.h>

namespace openspace {

bool NewHorizonsModule::initialize() {
    FactoryManager::ref().addFactory(new ghoul::TemplateFactory<planetgeometryprojection::PlanetGeometryProjection>);

    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "No renderable factory existed");

    fRenderable->registerClass<RenderableFov>("RenderableFov");
    fRenderable->registerClass<RenderablePlaneProjection>("RenderablePlaneProjection");
    fRenderable->registerClass<RenderablePlanetProjection>("RenderablePlanetProjection");

    auto fPlanetGeometryProjection = FactoryManager::ref().factory<planetgeometryprojection::PlanetGeometryProjection>();
    fPlanetGeometryProjection->registerClass<planetgeometryprojection::SimpleSphereGeometryProjection>("SimpleSphereProjection");

    return true;
}

} // namespace openspace
