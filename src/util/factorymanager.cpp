/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#include <openspace/util/factorymanager.h>

#include <cassert>

// renderables
#include <openspace/rendering/renderablewavefrontobject.h>
#include <openspace/rendering/stars/renderablestars.h>
#include <openspace/rendering/renderableephemeris.h>
#include <openspace/rendering/renderabletrail.h>
#include <openspace/rendering/renderablepath.h>

#include <openspace/rendering/renderablefov.h>
#include <openspace/rendering/renderablesphericalgrid.h>
#include <openspace/rendering/renderablefieldlines.h>
#include <openspace/rendering/planets/renderableplanet.h>
#include <openspace/rendering/renderablevolumeexpert.h>
#include <openspace/rendering/renderableplane.h>
#include <openspace/rendering/renderablevolumegl.h>
#include <openspace/flare/flare.h>

// positioninformation
#include <openspace/scenegraph/staticephemeris.h>
#include <openspace/scenegraph/spiceephemeris.h>

#include <openspace/rendering/planets/simplespheregeometry.h>

namespace openspace {

FactoryManager* FactoryManager::_manager = nullptr;

void FactoryManager::initialize()
{
    assert(_manager == nullptr);
    if (_manager == nullptr)
        _manager = new FactoryManager;
    assert(_manager != nullptr);

    // TODO: This has to be moved into a sort of module structure (ab)
    // Add Renderables
    _manager->addFactory(new ghoul::TemplateFactory<Renderable>);
    _manager->factory<Renderable>()->registerClass<RenderablePlanet>(
          "RenderablePlanet");
	_manager->factory<Renderable>()->registerClass<RenderableStars>(
		"RenderableStars");
	_manager->factory<Renderable>()->registerClass<RenderableEphemeris>(
		"RenderableEphemeris");
	//will replace ephemeris class soon...
	_manager->factory<Renderable>()->registerClass<RenderablePath>(
		"RenderablePath");
	_manager->factory<Renderable>()->registerClass<RenderableTrail>(
		"RenderableTrail");
	_manager->factory<Renderable>()->registerClass<RenderableFov>(
		"RenderableFov");
	_manager->factory<Renderable>()->registerClass<RenderableSphericalGrid>(
		"RenderableSphericalGrid");
	_manager->factory<Renderable>()->registerClass<RenderableWavefrontObject>(
		"RenderableWavefrontObject");
    //_manager->factory<Renderable>()->registerClass<RenderableVolumeCL>(
    //      "RenderableVolumeCL");
    _manager->factory<Renderable>()->registerClass<RenderablePlane>(
          "RenderablePlane");
    _manager->factory<Renderable>()->registerClass<RenderableVolumeGL>(
          "RenderableVolumeGL");
    _manager->factory<Renderable>()->registerClass<RenderableFieldlines>("RenderableFieldlines");

    // Add Ephimerides
    _manager->addFactory(new ghoul::TemplateFactory<Ephemeris>);
    _manager->factory<Ephemeris>()->registerClass<StaticEphemeris>("Static");
    _manager->factory<Ephemeris>()->registerClass<SpiceEphemeris>("Spice");

    // Add PlanetGeometry
    _manager->addFactory(new ghoul::TemplateFactory<planetgeometry::PlanetGeometry>);
    _manager->factory<planetgeometry::PlanetGeometry>()
          ->registerClass<planetgeometry::SimpleSphereGeometry>("SimpleSphere");
}

void FactoryManager::deinitialize()
{
    assert(_manager != nullptr);
    delete _manager;
    _manager = nullptr;
}

FactoryManager& FactoryManager::ref()
{
    assert(_manager != nullptr);
    return *_manager;
}

FactoryManager::FactoryManager()
{
}
FactoryManager::~FactoryManager()
{
    for (ghoul::TemplateFactoryBase* factory : _factories)
        delete factory;
    _factories.clear();
}

void FactoryManager::addFactory(ghoul::TemplateFactoryBase* factory) {
    _factories.push_back(factory);

}

}  // namespace openspace
