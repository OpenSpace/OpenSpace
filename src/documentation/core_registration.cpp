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

#include <openspace/documentation/core_registration.h>

#include <openspace/data/datamapping.h>
#include <openspace/documentation/documentationengine.h>
#include <openspace/engine/logfactory.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/events/eventengine.h>
#include <openspace/interaction/actionmanager.h>
#include <openspace/interaction/keybindingmanager.h>
#include <openspace/interaction/sessionrecording.h>
#include <openspace/mission/mission.h>
#include <openspace/mission/missionmanager.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/navigationstate.h>
#include <openspace/navigation/path.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/rendering/colormappingcomponent.h>
#include <openspace/rendering/dashboard.h>
#include <openspace/rendering/dashboarditem.h>
#include <openspace/rendering/labelscomponent.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scene/lightsource.h>
#include <openspace/scene/profile.h>
#include <openspace/scene/rotation.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scale.h>
#include <openspace/scene/translation.h>
#include <openspace/scene/timeframe.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scripting/scriptscheduler.h>
#include <openspace/scripting/systemcapabilitiesbinding.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/task.h>
#include <openspace/util/time.h>
#include <openspace/util/timerange.h>

namespace openspace {

void registerCoreClasses(documentation::DocumentationEngine& engine) {
    engine.addDocumentation(LogFactoryDocumentation());

    engine.addDocumentation(DashboardItem::Documentation());

    engine.addDocumentation(ColorMappingComponent::Documentation());
    engine.addDocumentation(LabelsComponent::Documentation());
    engine.addDocumentation(LightSource::Documentation());
    engine.addDocumentation(Mission::Documentation());
    engine.addDocumentation(Renderable::Documentation());
    engine.addDocumentation(Rotation::Documentation());
    engine.addDocumentation(Scale::Documentation());
    engine.addDocumentation(SceneGraphNode::Documentation());
    engine.addDocumentation(ScreenSpaceRenderable::Documentation());
    engine.addDocumentation(Task::documentation());
    engine.addDocumentation(TimeRange::Documentation());
    engine.addDocumentation(Translation::Documentation());
    engine.addDocumentation(TimeFrame::Documentation());

    engine.addDocumentation(dataloader::DataMapping::Documentation());

    engine.addDocumentation(interaction::NavigationState::Documentation());
    engine.addDocumentation(interaction::Path::Documentation());
}

// NOTE: should this be in the documentation/core_reg.cpp file? Seems to be here just
//       because it has the same method name (and similar implementaiton) as the
//       documentation version.
void registerCoreClasses(scripting::ScriptEngine& engine) {
    engine.addLibrary(Dashboard::luaLibrary());
    engine.addLibrary(EventEngine::luaLibrary());
    engine.addLibrary(MissionManager::luaLibrary());
    engine.addLibrary(ModuleEngine::luaLibrary());
    engine.addLibrary(OpenSpaceEngine::luaLibrary());
    engine.addLibrary(ParallelPeer::luaLibrary());
    engine.addLibrary(Profile::luaLibrary());
    engine.addLibrary(RenderEngine::luaLibrary());
    engine.addLibrary(SpiceManager::luaLibrary());
    engine.addLibrary(Scene::luaLibrary());
    engine.addLibrary(Time::luaLibrary());
    engine.addLibrary(interaction::ActionManager::luaLibrary());
    engine.addLibrary(interaction::KeybindingManager::luaLibrary());
    engine.addLibrary(interaction::NavigationHandler::luaLibrary());
    engine.addLibrary(interaction::OrbitalNavigator::luaLibrary());
    engine.addLibrary(interaction::PathNavigator::luaLibrary());
    engine.addLibrary(interaction::SessionRecording::luaLibrary());
    engine.addLibrary(scripting::ScriptScheduler::luaLibrary());
    engine.addLibrary(scripting::generalSystemCapabilities());
    engine.addLibrary(scripting::openglSystemCapabilities());
}

} // namespace openspace
