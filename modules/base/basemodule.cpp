/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/base/dashboard/dashboarditemangle.h>
#include <modules/base/dashboard/dashboarditemdate.h>
#include <modules/base/dashboard/dashboarditemdistance.h>
#include <modules/base/dashboard/dashboarditemelapsedtime.h>
#include <modules/base/dashboard/dashboarditemframerate.h>
#include <modules/base/dashboard/dashboarditeminputstate.h>
#include <modules/base/dashboard/dashboarditemmission.h>
#include <modules/base/dashboard/dashboarditemparallelconnection.h>
#include <modules/base/dashboard/dashboarditempropertyvalue.h>
#include <modules/base/dashboard/dashboarditemsimulationincrement.h>
#include <modules/base/dashboard/dashboarditemspacing.h>
#include <modules/base/dashboard/dashboarditemtext.h>
#include <modules/base/dashboard/dashboarditemvelocity.h>
#include <modules/base/lightsource/cameralightsource.h>
#include <modules/base/lightsource/scenegraphlightsource.h>
#include <modules/base/rendering/grids/renderableboxgrid.h>
#include <modules/base/rendering/grids/renderablegrid.h>
#include <modules/base/rendering/grids/renderableradialgrid.h>
#include <modules/base/rendering/grids/renderablesphericalgrid.h>
#include <modules/base/rendering/pointcloud/renderableinterpolatedpoints.h>
#include <modules/base/rendering/pointcloud/renderablepointcloud.h>
#include <modules/base/rendering/pointcloud/renderablepolygoncloud.h>
#include <modules/base/rendering/pointcloud/sizemappingcomponent.h>
#include <modules/base/rendering/renderablecartesianaxes.h>
#include <modules/base/rendering/renderabledisc.h>
#include <modules/base/rendering/renderablelabel.h>
#include <modules/base/rendering/renderablemodel.h>
#include <modules/base/rendering/renderablenodearrow.h>
#include <modules/base/rendering/renderablenodeline.h>
#include <modules/base/rendering/renderablesphereimagelocal.h>
#include <modules/base/rendering/renderablesphereimageonline.h>
#include <modules/base/rendering/renderabletrailorbit.h>
#include <modules/base/rendering/renderabletrailtrajectory.h>
#include <modules/base/rendering/renderableplaneimagelocal.h>
#include <modules/base/rendering/renderableplaneimageonline.h>
#include <modules/base/rendering/renderableplanetimevaryingimage.h>
#include <modules/base/rendering/renderableprism.h>
#include <modules/base/rendering/renderabletimevaryingsphere.h>
#include <modules/base/rendering/screenspacedashboard.h>
#include <modules/base/rendering/screenspaceimagelocal.h>
#include <modules/base/rendering/screenspaceimageonline.h>
#include <modules/base/rendering/screenspaceframebuffer.h>
#include <modules/base/rotation/constantrotation.h>
#include <modules/base/rotation/fixedrotation.h>
#include <modules/base/rotation/luarotation.h>
#include <modules/base/rotation/staticrotation.h>
#include <modules/base/rotation/timelinerotation.h>
#include <modules/base/scale/luascale.h>
#include <modules/base/scale/nonuniformstaticscale.h>
#include <modules/base/scale/staticscale.h>
#include <modules/base/scale/timedependentscale.h>
#include <modules/base/translation/timelinetranslation.h>
#include <modules/base/translation/luatranslation.h>
#include <modules/base/translation/statictranslation.h>
#include <modules/base/timeframe/timeframeinterval.h>
#include <modules/base/timeframe/timeframeunion.h>
#include <openspace/documentation/documentation.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/templatefactory.h>

namespace openspace {

ghoul::opengl::ProgramObjectManager BaseModule::ProgramObjectManager;
ghoul::opengl::TextureManager BaseModule::TextureManager;

BaseModule::BaseModule() : OpenSpaceModule(BaseModule::Name) {}

void BaseModule::internalInitialize(const ghoul::Dictionary&) {
    FactoryManager::ref().addFactory<ScreenSpaceRenderable>("ScreenSpaceRenderable");

    ghoul::TemplateFactory<ScreenSpaceRenderable>* fSsRenderable =
        FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fSsRenderable, "ScreenSpaceRenderable factory was not created");

    fSsRenderable->registerClass<ScreenSpaceDashboard>("ScreenSpaceDashboard");
    fSsRenderable->registerClass<ScreenSpaceImageLocal>("ScreenSpaceImageLocal");
    fSsRenderable->registerClass<ScreenSpaceImageOnline>("ScreenSpaceImageOnline");
    fSsRenderable->registerClass<ScreenSpaceFramebuffer>("ScreenSpaceFramebuffer");

    ghoul::TemplateFactory<DashboardItem>* fDashboard =
        FactoryManager::ref().factory<DashboardItem>();
    ghoul_assert(fDashboard, "Dashboard factory was not created");

    fDashboard->registerClass<DashboardItemAngle>("DashboardItemAngle");
    fDashboard->registerClass<DashboardItemDate>("DashboardItemDate");
    fDashboard->registerClass<DashboardItemDistance>("DashboardItemDistance");
    fDashboard->registerClass<DashboardItemElapsedTime>("DashboardItemElapsedTime");
    fDashboard->registerClass<DashboardItemFramerate>("DashboardItemFramerate");
    fDashboard->registerClass<DashboardItemInputState>("DashboardItemInputState");
    fDashboard->registerClass<DashboardItemMission>("DashboardItemMission");
    fDashboard->registerClass<DashboardItemParallelConnection>(
        "DashboardItemParallelConnection"
    );
    fDashboard->registerClass<DashboardItemPropertyValue>(
        "DashboardItemPropertyValue"
    );
    fDashboard->registerClass<DashboardItemSimulationIncrement>(
        "DashboardItemSimulationIncrement"
    );
    fDashboard->registerClass<DashboardItemSpacing>("DashboardItemSpacing");
    fDashboard->registerClass<DashboardItemText>("DashboardItemText");
    fDashboard->registerClass<DashboardItemVelocity>("DashboardItemVelocity");

    ghoul::TemplateFactory<Renderable>* fRenderable =
        FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "Renderable factory was not created");

    fRenderable->registerClass<RenderableBoxGrid>("RenderableBoxGrid");
    fRenderable->registerClass<RenderableCartesianAxes>("RenderableCartesianAxes");
    fRenderable->registerClass<RenderableDisc>("RenderableDisc");
    fRenderable->registerClass<RenderableGrid>("RenderableGrid");
    fRenderable->registerClass<RenderableLabel>("RenderableLabel");
    fRenderable->registerClass<RenderableModel>("RenderableModel");
    fRenderable->registerClass<RenderableNodeArrow>("RenderableNodeArrow");
    fRenderable->registerClass<RenderableNodeLine>("RenderableNodeLine");
    fRenderable->registerClass<RenderablePlaneImageLocal>("RenderablePlaneImageLocal");
    fRenderable->registerClass<RenderablePlaneImageOnline>("RenderablePlaneImageOnline");
    fRenderable->registerClass<RenderablePlaneTimeVaryingImage>(
        "RenderablePlaneTimeVaryingImage"
    );
    fRenderable->registerClass<RenderableInterpolatedPoints>(
        "RenderableInterpolatedPoints"
    );
    fRenderable->registerClass<RenderablePointCloud>("RenderablePointCloud");
    fRenderable->registerClass<RenderablePolygonCloud>("RenderablePolygonCloud");
    fRenderable->registerClass<RenderablePrism>("RenderablePrism");
    fRenderable->registerClass<RenderableTimeVaryingSphere>(
        "RenderableTimeVaryingSphere"
    );
    fRenderable->registerClass<RenderableRadialGrid>("RenderableRadialGrid");
    fRenderable->registerClass<RenderableSphereImageLocal>("RenderableSphereImageLocal");
    fRenderable->registerClass<RenderableSphereImageOnline>(
        "RenderableSphereImageOnline"
    );
    fRenderable->registerClass<RenderableSphericalGrid>("RenderableSphericalGrid");
    fRenderable->registerClass<RenderableTrailOrbit>("RenderableTrailOrbit");
    fRenderable->registerClass<RenderableTrailTrajectory>("RenderableTrailTrajectory");

    ghoul::TemplateFactory<Translation>* fTranslation =
        FactoryManager::ref().factory<Translation>();
    ghoul_assert(fTranslation, "Ephemeris factory was not created");

    fTranslation->registerClass<TimelineTranslation>("TimelineTranslation");
    fTranslation->registerClass<LuaTranslation>("LuaTranslation");
    fTranslation->registerClass<StaticTranslation>("StaticTranslation");

    ghoul::TemplateFactory<Rotation>* fRotation =
        FactoryManager::ref().factory<Rotation>();
    ghoul_assert(fRotation, "Rotation factory was not created");

    fRotation->registerClass<ConstantRotation>("ConstantRotation");
    fRotation->registerClass<FixedRotation>("FixedRotation");
    fRotation->registerClass<LuaRotation>("LuaRotation");
    fRotation->registerClass<StaticRotation>("StaticRotation");
    fRotation->registerClass<TimelineRotation>("TimelineRotation");


    ghoul::TemplateFactory<Scale>* fScale = FactoryManager::ref().factory<Scale>();
    ghoul_assert(fScale, "Scale factory was not created");

    fScale->registerClass<LuaScale>("LuaScale");
    fScale->registerClass<NonUniformStaticScale>("NonUniformStaticScale");
    fScale->registerClass<StaticScale>("StaticScale");
    fScale->registerClass<TimeDependentScale>("TimeDependentScale");

    ghoul::TemplateFactory<TimeFrame>* fTimeFrame =
        FactoryManager::ref().factory<TimeFrame>();
    ghoul_assert(fTimeFrame, "Scale factory was not created");

    fTimeFrame->registerClass<TimeFrameInterval>("TimeFrameInterval");
    fTimeFrame->registerClass<TimeFrameUnion>("TimeFrameUnion");

    ghoul::TemplateFactory<LightSource>* fLightSource =
        FactoryManager::ref().factory<LightSource>();
    ghoul_assert(fLightSource, "Light Source factory was not created");

    fLightSource->registerClass<CameraLightSource>("CameraLightSource");
    fLightSource->registerClass<SceneGraphLightSource>("SceneGraphLightSource");
}

void BaseModule::internalDeinitializeGL() {
    ProgramObjectManager.releaseAll(ghoul::opengl::ProgramObjectManager::Warnings::Yes);
    TextureManager.releaseAll(ghoul::opengl::TextureManager::Warnings::Yes);
}

std::vector<documentation::Documentation> BaseModule::documentations() const {
    return {
        DashboardItemAngle::Documentation(),
        DashboardItemDate::Documentation(),
        DashboardItemDistance::Documentation(),
        DashboardItemFramerate::Documentation(),
        DashboardItemMission::Documentation(),
        DashboardItemParallelConnection::Documentation(),
        DashboardItemPropertyValue::Documentation(),
        DashboardItemSimulationIncrement::Documentation(),
        DashboardItemSpacing::Documentation(),
        DashboardItemText::Documentation(),
        DashboardItemVelocity::Documentation(),

        RenderableBoxGrid::Documentation(),
        RenderableCartesianAxes::Documentation(),
        RenderableDisc::Documentation(),
        RenderableGrid::Documentation(),
        RenderableInterpolatedPoints::Documentation(),
        RenderableLabel::Documentation(),
        RenderableModel::Documentation(),
        RenderableNodeArrow::Documentation(),
        RenderableNodeLine::Documentation(),
        RenderablePlaneImageLocal::Documentation(),
        RenderablePlaneImageOnline::Documentation(),
        RenderablePlaneTimeVaryingImage::Documentation(),
        RenderablePointCloud::Documentation(),
        RenderablePolygonCloud::Documentation(),
        RenderablePrism::Documentation(),
        RenderableRadialGrid::Documentation(),
        RenderableSphereImageLocal::Documentation(),
        RenderableSphereImageOnline::Documentation(),
        RenderableSphericalGrid::Documentation(),
        RenderableTimeVaryingSphere::Documentation(),
        RenderableTrailOrbit::Documentation(),
        RenderableTrailTrajectory::Documentation(),

        SizeMappingComponent::Documentation(),

        ScreenSpaceDashboard::Documentation(),
        ScreenSpaceFramebuffer::Documentation(),
        ScreenSpaceImageLocal::Documentation(),
        ScreenSpaceImageOnline::Documentation(),

        ConstantRotation::Documentation(),
        FixedRotation::Documentation(),
        LuaRotation::Documentation(),
        StaticRotation::Documentation(),
        TimelineRotation::Documentation(),

        LuaScale::Documentation(),
        NonUniformStaticScale::Documentation(),
        StaticScale::Documentation(),
        TimeDependentScale::Documentation(),

        LuaTranslation::Documentation(),
        StaticTranslation::Documentation(),
        TimelineTranslation::Documentation(),

        TimeFrameInterval::Documentation(),
        TimeFrameUnion::Documentation(),

        CameraLightSource::Documentation(),
        SceneGraphLightSource::Documentation()
    };
}

std::vector<scripting::LuaLibrary> BaseModule::luaLibraries() const {
    return {
        ScreenSpaceDashboard::luaLibrary()
    };
}

} // namespace openspace
