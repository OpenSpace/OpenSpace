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
#include <openspace/rendering/renderableplanet.h>
#include <openspace/rendering/renderablevolumeexpert.h>
#include <openspace/rendering/renderablevolumecl.h>
#include <openspace/rendering/renderablevolumegl.h>
#include <openspace/flare/flare.h>

// positioninformation
#include <openspace/scenegraph/constantpositioninformation.h>
#include <openspace/scenegraph/spicepositioninformation.h>

namespace openspace {

// Template specializations for the different factories
template<>
ghoul::TemplateFactory<Renderable>* FactoryManager::factoryByType() {
    return &_renderableFactory;
}
template<>
ghoul::TemplateFactory<PositionInformation>* FactoryManager::factoryByType() {
    return &_positionInformationFactory;
}
    
FactoryManager* FactoryManager::_manager = nullptr;

void FactoryManager::initialize() {
    assert(_manager == nullptr);
    if (_manager == nullptr)
        _manager = new FactoryManager;
    assert(_manager != nullptr);
    
    // Add Renderables
    _manager->factoryByType<Renderable>()->
        registerClass<RenderablePlanet>("RenderablePlanet");
    _manager->factoryByType<Renderable>()->
        registerClass<RenderableVolumeCL>("RenderableVolumeCL");
    _manager->factoryByType<Renderable>()->
        registerClass<RenderableVolumeGL>("RenderableVolumeGL");
    _manager->factoryByType<Renderable>()->
        registerClass<RenderableVolumeExpert>("RenderableVolumeExpert");
    _manager->factoryByType<Renderable>()->
        registerClass<Flare>("RenderableFlare");
    
    // Add PositionInformations
    _manager->factoryByType<PositionInformation>()->
        registerClass<ConstantPositionInformation>("Static");
    _manager->factoryByType<PositionInformation>()->
        registerClass<SpicePositionInformation>("Spice");

}

void FactoryManager::deinitialize() {
    assert(_manager != nullptr);
    delete _manager;
    _manager = nullptr;
}

FactoryManager& FactoryManager::ref() {
    assert(_manager != nullptr);
    return *_manager;
}
     
FactoryManager::FactoryManager() {
    
}
FactoryManager::~FactoryManager() {
    
}

} // namespace openspace