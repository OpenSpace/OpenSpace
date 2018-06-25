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

#include <modules/digitaluniverse/digitaluniversemodule.h>

#include <modules/digitaluniverse/rendering/renderablebillboardscloud.h>
#include <modules/digitaluniverse/rendering/renderabledumeshes.h>
#include <modules/digitaluniverse/rendering/renderableplanescloud.h>
#include <modules/digitaluniverse/rendering/renderablepoints.h>
#include <openspace/documentation/documentation.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/templatefactory.h>

namespace openspace {

ghoul::opengl::ProgramObjectManager DigitalUniverseModule::ProgramObjectManager;

DigitalUniverseModule::DigitalUniverseModule()
    : OpenSpaceModule(DigitalUniverseModule::Name)
{}

void DigitalUniverseModule::internalInitialize(const ghoul::Dictionary&) {
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "Renderable factory was not created");

    fRenderable->registerClass<RenderablePoints>("RenderablePoints");
    fRenderable->registerClass<RenderableBillboardsCloud>("RenderableBillboardsCloud");
    fRenderable->registerClass<RenderablePlanesCloud>("RenderablePlanesCloud");
    fRenderable->registerClass<RenderableDUMeshes>("RenderableDUMeshes");
}

void DigitalUniverseModule::internalDeinitializeGL() {
    ProgramObjectManager.releaseAll(ghoul::opengl::ProgramObjectManager::Warnings::Yes);
}

std::vector<documentation::Documentation> DigitalUniverseModule::documentations() const {
    return {
        RenderablePoints::Documentation(),
        RenderableBillboardsCloud::Documentation(),
        RenderablePlanesCloud::Documentation(),
        RenderableDUMeshes::Documentation()
    };
}

} // namespace openspace
