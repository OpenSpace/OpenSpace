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

#include <openspace/util/factorymanager.h>

#include <openspace/rendering/renderable.h>
#include <openspace/scene/ephemeris.h>

#include <ghoul/misc/assert.h>

namespace openspace {

FactoryManager* FactoryManager::_manager = nullptr;

void FactoryManager::initialize() {
    ghoul_assert(!_manager, "Factory Manager already initialized");
    if (_manager == nullptr)
        _manager = new FactoryManager;
    ghoul_assert(_manager, "Factory Manager was not correctly initialized");

    _manager->addFactory(new ghoul::TemplateFactory<Renderable>);
    _manager->addFactory(new ghoul::TemplateFactory<Ephemeris>);
}

void FactoryManager::deinitialize() {
    ghoul_assert(_manager, "No Factory Manager to deinitialize");
    delete _manager;
    _manager = nullptr;
}

FactoryManager& FactoryManager::ref() {
    ghoul_assert(_manager, "No Factory Manager to dereference");
    return *_manager;
}

FactoryManager::~FactoryManager() {
    for (ghoul::TemplateFactoryBase* factory : _factories)
        delete factory;
    _factories.clear();
}

void FactoryManager::addFactory(ghoul::TemplateFactoryBase* factory) {
    _factories.push_back(factory);

}

}  // namespace openspace
