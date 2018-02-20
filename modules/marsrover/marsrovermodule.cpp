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

#include <openspace/documentation/documentation.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/util/factorymanager.h>

#include <ghoul/misc/assert.h>

#include <modules/marsrover/dashboard/dashboarditemangle.h>
#include <modules/marsrover/dashboard/dashboarditemdate.h>
#include <modules/marsrover/dashboard/dashboarditemdistance.h>
#include <modules/marsrover/dashboard/dashboarditemframerate.h>
#include <modules/marsrover/dashboard/dashboarditemmission.h>
#include <modules/marsrover/dashboard/dashboarditemparallelconnection.h>
#include <modules/marsrover/dashboard/dashboarditemsimulationincrement.h>
#include <modules/marsrover/dashboard/dashboarditemspacing.h>

#include <modules/marsrover/rendering/renderablemodel.h>
#include <modules/marsrover/rendering/renderablesphere.h>
#include <modules/marsrover/rendering/renderablesphericalgrid.h>
#include <modules/marsrover/rendering/renderabletrailorbit.h>
#include <modules/marsrover/rendering/renderabletrailtrajectory.h>
#include <modules/marsrover/rendering/renderableplane.h>
#include <modules/marsrover/rendering/modelgeometry.h>
#include <modules/marsrover/rendering/multimodelgeometry.h>
#include <modules/marsrover/rendering/screenspacedashboard.h>
#include <modules/marsrover/rendering/screenspaceimagelocal.h>
#include <modules/marsrover/rendering/screenspaceimageonline.h>
#include <modules/marsrover/rendering/screenspaceframebuffer.h>

#include <modules/marsrover/translation/luatranslation.h>
#include <modules/marsrover/translation/statictranslation.h>

#include <modules/marsrover/rotation/fixedrotation.h>
#include <modules/marsrover/rotation/luarotation.h>
#include <modules/marsrover/rotation/staticrotation.h>

#include <modules/marsrover/scale/luascale.h>
#include <modules/marsrover/scale/staticscale.h>

#include <modules/marsrover/rendering/renderablemarsrover.h>

#include <ghoul/filesystem/filesystem>

namespace openspace {

MarsroverModule::MarsroverModule() : OpenSpaceModule(MarsroverModule::Name) {}

void MarsroverModule::internalInitialize(const ghoul::Dictionary&) {
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<modelgeometry::ModelGeometry>>(),
        "ModelGeometry"
    );
    FactoryManager::ref().addFactory(
        std::make_unique<ghoul::TemplateFactory<ScreenSpaceRenderable>>(),
        "ScreenSpaceRenderable"
    );

    auto fSsRenderable = FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fSsRenderable, "ScreenSpaceRenderable factory was not created");

    fSsRenderable->registerClass<ScreenSpaceDashboard>("ScreenSpaceDashboard");
    fSsRenderable->registerClass<ScreenSpaceImageLocal>("ScreenSpaceImageLocal");
    fSsRenderable->registerClass<ScreenSpaceImageOnline>("ScreenSpaceImageOnline");
    fSsRenderable->registerClass<ScreenSpaceFramebuffer>("ScreenSpaceFramebuffer");

    auto fDashboard = FactoryManager::ref().factory<DashboardItem>();
    ghoul_assert(fDashboard, "Dashboard factory was not created");

    fDashboard->registerClass<DashboardItemAngle>("DashboardItemAngle");
    fDashboard->registerClass<DashboardItemDate>("DashboardItemDate");
    fDashboard->registerClass<DashboardItemDistance>("DashboardItemDistance");
    fDashboard->registerClass<DashboardItemFramerate>("DashboardItemFramerate");
    fDashboard->registerClass<DashboardItemMission>("DashboardItemMission");
    fDashboard->registerClass<DashboardItemParallelConnection>(
        "DashboardItemParallelConnection"
    );
    fDashboard->registerClass<DashboardItemSimulationIncrement>(
        "DashboardItemSimulationIncrement"
    );
    fDashboard->registerClass<DashboardItemSpacing>("DashboardItemSpacing");

    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "Renderable factory was not created");

    fRenderable->registerClass<RenderableModel>("RenderableModel");
    fRenderable->registerClass<RenderablePlane>("RenderablePlane");
    fRenderable->registerClass<RenderableSphere>("RenderableSphere");
    fRenderable->registerClass<RenderableSphericalGrid>("RenderableSphericalGrid");
    fRenderable->registerClass<RenderableTrailOrbit>("RenderableTrailOrbit");
    fRenderable->registerClass<RenderableTrailTrajectory>("RenderableTrailTrajectory");
    fRenderable->registerClass<RenderableMarsrover>("RenderableMarsrover");

    auto fTranslation = FactoryManager::ref().factory<Translation>();
    ghoul_assert(fTranslation, "Ephemeris factory was not created");

    fTranslation->registerClass<LuaTranslation>("LuaTranslation");
    fTranslation->registerClass<StaticTranslation>("StaticTranslation");

    auto fRotation = FactoryManager::ref().factory<Rotation>();
    ghoul_assert(fRotation, "Rotation factory was not created");

    fRotation->registerClass<FixedRotation>("FixedRotation");
    fRotation->registerClass<LuaRotation>("LuaRotation");
    fRotation->registerClass<StaticRotation>("StaticRotation");

    auto fScale = FactoryManager::ref().factory<Scale>();
    ghoul_assert(fScale, "Scale factory was not created");

    fScale->registerClass<LuaScale>("LuaScale");
    fScale->registerClass<StaticScale>("StaticScale");

    auto fGeometry = FactoryManager::ref().factory<modelgeometry::ModelGeometry>();
    ghoul_assert(fGeometry, "Model geometry factory was not created");
    fGeometry->registerClass<modelgeometry::MultiModelGeometry>("MultiModelGeometry");
}

std::vector<documentation::Documentation> MarsroverModule::documentations() const {
    return {
        DashboardItemDate::Documentation(),
        DashboardItemDistance::Documentation(),
        DashboardItemFramerate::Documentation(),
        DashboardItemMission::Documentation(),
        DashboardItemParallelConnection::Documentation(),
        DashboardItemSimulationIncrement::Documentation(),
        DashboardItemSpacing::Documentation(),

        RenderableModel::Documentation(),
        RenderablePlane::Documentation(),
        RenderableSphere::Documentation(),
        RenderableTrailOrbit::Documentation(),
        RenderableTrailTrajectory::Documentation(),

        ScreenSpaceDashboard::Documentation(),
        ScreenSpaceFramebuffer::Documentation(),
        ScreenSpaceImageLocal::Documentation(),
        ScreenSpaceImageOnline::Documentation(),

        FixedRotation::Documentation(),
        LuaRotation::Documentation(),
        StaticRotation::Documentation(),

        LuaScale::Documentation(),
        StaticScale::Documentation(),

        LuaTranslation::Documentation(),
        StaticTranslation::Documentation(),

        modelgeometry::ModelGeometry::Documentation(),
    };
}

std::vector<scripting::LuaLibrary> MarsroverModule::luaLibraries() const {
    return {
        ScreenSpaceDashboard::luaLibrary()
    };
}

} // namespace openspace
