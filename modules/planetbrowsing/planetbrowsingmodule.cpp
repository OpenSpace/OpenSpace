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

#include <modules/planetbrowsing/planetbrowsingmodule.h>

#include <modules/planetbrowsing/rendering/planet.h>
#include <modules/planetbrowsing/rendering/distanceswitch.h>
#include <modules/planetbrowsing/rendering/planetmesh.h>

#include <openspace/rendering/renderable.h>
#include <openspace/util/factorymanager.h>

#include <ghoul/misc/assert.h>


namespace openspace {

PlanetBrowsingModule::PlanetBrowsingModule()
    : OpenSpaceModule("PlanetBrowsing")
{}

void PlanetBrowsingModule::internalInitialize() {
	/*
	auto fRenderable = FactoryManager::ref().factory<Renderable>();
	ghoul_assert(fRenderable, "Renderable factory was not created");

	fRenderable->registerClass<Planet>("Planet");
	fRenderable->registerClass<RenderableTestPlanet>("RenderableTestPlanet");
	//fRenderable->registerClass<planettestgeometry::PlanetTestGeometry>("PlanetTestGeometry");

	auto fPlanetGeometry = FactoryManager::ref().factory<planettestgeometry::PlanetTestGeometry>();
	ghoul_assert(fPlanetGeometry, "Planet test geometry factory was not created");
	fPlanetGeometry->registerClass<planettestgeometry::SimpleSphereTestGeometry>("SimpleSphereTest");

	*/





	auto fRenderable = FactoryManager::ref().factory<Renderable>();
	ghoul_assert(fRenderable, "Renderable factory was not created");

	fRenderable->registerClass<Planet>("Planet");
	fRenderable->registerClass<PlanetMesh>("PlanetMesh");
	fRenderable->registerClass<DistanceSwitch>("DistanceSwitch");
}

} // namespace openspace
