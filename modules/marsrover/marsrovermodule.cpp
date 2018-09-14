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

#include <modules/marsrover/marsrovermodule.h>

#include <openspace/rendering/renderable.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/misc/assert.h>

#include <modules/marsrover/rendering/renderableheightmap.h>
#include <modules/marsrover/rotation/advancedspicerotation.h>
#include <modules/marsrover/rotation/rksmlrotation.h>
#include <modules/marsrover/surfaceprojection/spacecraftprojection.h>
#include <modules/marsrover/surfaceprojection/projectionprovider.h>
#include <modules/marsrover/surfaceprojection/wheeldataprovider.h>
#include <openspace/util/timeline.h>

#include <ghoul/logging/logmanager.h>

namespace {
 const std::string _loggerCat = "MarsroverModule";
}

namespace openspace {

ghoul::opengl::ProgramObjectManager MarsroverModule::ProgramObjectManager;

MarsroverModule::MarsroverModule() : OpenSpaceModule(MarsroverModule::Name) {}   

void MarsroverModule::internalInitialize(const ghoul::Dictionary&) {
    //to later
    auto fRenderable = FactoryManager::ref().factory<Renderable>();    
    ghoul_assert(fRenderable, "Renderable factory was not created");    

    //fRenderable->registerClass<RenderableMarsrover>("RenderableMarsrover");
    
    //if we need the heightmap code to be a renderable
    fRenderable->registerClass<RenderableHeightMap>("RenderableHeightMap");

    auto fRotation = FactoryManager::ref().factory<Rotation>();
    //(fRotation, "Rotation factory was not created");
    fRotation->registerClass<AdvancedSpiceRotation>("AdvancedSpiceRotation");
    fRotation->registerClass<RksmlRotation>("RksmlRotation");
    
    auto fTranslation = FactoryManager::ref().factory<Translation>();
    fTranslation->registerClass<SpacecraftProjection>("SpacecraftProjection");
   

    WheelData = std::make_unique<WheelDataProvider>();
    //fwheelDataProvider->registerClass<WheelDataProvider>("WheelDataProvider");
    //FactoryManager::ref().addFactory(std::move(fProjectionProvider));

    WheelData->initialize();
    //WheelData2 = WheelData;
    //fProjectionProvider->WheelDataProvider::loadData("./OpenSpace/");

    //slutade här!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    //std::string filepath = ".\sync\http\marscuriosity_wheeldata\1\rksml";
    //fProjectionProvider->loadData(filepath);


    //Read heightmap once
    //MslTerrain terrainMap;
    //terrainMap.createHeightMap(); 

    //auto fTranslation = FactoryManager::ref().factory<Translation>();
    //fTranslation->registerClass<MarsProjection>("MarsProjection");
}

//Timeline<WheelDataProvider::Node> &MarsroverModule::getFrameData(const std::string f, Timeline<WheelDataProvider::Node> obj)

//void MarsroverModule::getFrameData(const std::string f)
Timeline<WheelDataProvider::Node> &MarsroverModule::getFrameData(const std::string f)
{
    LERROR(fmt::format("(getFrameData marsrovermodule) f: '{}'", f));
    return WheelData->getNode(f);
}

} // namespace openspace
