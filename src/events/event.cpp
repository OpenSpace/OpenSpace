/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <openspace/events/event.h>

#include <openspace/properties/property.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scene/profile.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>
#include <openspace/util/tstring.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <functional>

namespace {
    constexpr const char _loggerCat[] = "EventInfo";
} // namespace

namespace openspace::events {

void log(int i, const EventSceneGraphNodeAdded& e) {
    ghoul_assert(e.type == EventSceneGraphNodeAdded::Type, "Wrong type");
    LINFO(fmt::format("[{}] SceneGraphNodeAdded: {}", i, e.node));
}

void log(int i, const EventSceneGraphNodeRemoved& e) {
    ghoul_assert(e.type == EventSceneGraphNodeRemoved::Type, "Wrong type");
    LINFO(fmt::format("[{}] SceneGraphNodeRemoved: {}", i, e.node));
}

void log(int i, const EventParallelConnectionEstablished& e) {
    ghoul_assert(e.type == EventParallelConnectionEstablished::Type, "Wrong type");
    LINFO(fmt::format("[{}] ParallelConnectionEstablished", i));
}

void log(int i, const EventParallelConnectionLost& e) {
    ghoul_assert(e.type == EventParallelConnectionLost::Type, "Wrong type");
    LINFO(fmt::format("[{}] ParallelConnectionLost", i));
}

void log(int i, const EventParallelConnectionHostshipGained& e) {
    ghoul_assert(e.type == EventParallelConnectionHostshipGained::Type, "Wrong type");
    LINFO(fmt::format("[{}] ParallelConnectionHostshipGained", i));
}

void log(int i, const EventParallelConnectionHostshipLost& e) {
    ghoul_assert(e.type == EventParallelConnectionHostshipLost::Type, "Wrong type");
    LINFO(fmt::format("[{}] ParallelConnectionHostshipLost", i));
}

void log(int i, const EventProfileLoadingFinished& e) {
    ghoul_assert(e.type == EventProfileLoadingFinished::Type, "Wrong type");
    LINFO(fmt::format("[{}] ProfileLoadingFinished", i));
}

void log(int i, const EventApplicationShutdownStarted& e) {
    ghoul_assert(e.type == EventApplicationShutdownStarted::Type, "Wrong type");
    LINFO(fmt::format("[{}] ApplicationShutdownStarted", i));
}

void log(int i, const EventScreenSpaceRenderableAdded& e) {
    ghoul_assert(e.type == EventScreenSpaceRenderableAdded::Type, "Wrong type");
    LINFO(fmt::format("[{}] ScreenSpaceRenderableAdded: {}", i, e.renderable));

}

void log(int i, const EventScreenSpaceRenderableRemoved& e) {
    ghoul_assert(e.type == EventScreenSpaceRenderableRemoved::Type, "Wrong type");
    LINFO(fmt::format("[{}] ScreenSpaceRenderableRemoved: {}", i, e.renderable));
}

void log(int i, const EventCameraApproachedSceneGraphNode& e) {
    ghoul_assert(e.type == EventCameraApproachedSceneGraphNode::Type, "Wrong type");
    LINFO(fmt::format(
        "[{}] CameraApproachedSceneGraphNode: {} -> {}",
        i, reinterpret_cast<const void*>(e.camera), e.node
    ));
}

void log(int i, const EventCameraMovedAwayFromSceneGraphNode& e) {
    ghoul_assert(e.type == EventCameraMovedAwayFromSceneGraphNode::Type, "Wrong type");
    LINFO(fmt::format(
        "[{}] CameraMovedAwayFromSceneGraphNode: {} -> {}",
        i, reinterpret_cast<const void*>(e.camera), e.node
    ));
}

void log(int i, const EventTimeOfInterestReached& e) {
    ghoul_assert(e.type == EventTimeOfInterestReached::Type, "Wrong type");
    LINFO(fmt::format(
        "[{}] TimeOfInterestReached: {},  {}",
        i, e.time->UTC(), reinterpret_cast<const void*>(e.camera)
    ));
}

void log(int i, const EventMissionEventReached& e) {
    ghoul_assert(e.type == EventMissionEventReached::Type, "Wrong type");
    LINFO(fmt::format("[{}] MissionEventReached", i));
}

void log(int i, const EventPlanetEclipsed& e) {
    ghoul_assert(e.type == EventPlanetEclipsed::Type, "Wrong type");
    LINFO(fmt::format("[{}] PlanetEclipsed: {} -> {}", i, e.eclipsee, e.eclipser));
}

void log(int i, const CustomEvent& e) {
    ghoul_assert(e.type == CustomEvent::Type, "Wrong type");
    LINFO(fmt::format("[{}] CustomEvent: {} ({})", i, e.subtype, e.payload));
}

void logAllEvents(const Event* e) {
    int i = 0;
    while (e) {
        switch (e->type) {
            case Event::Type::SceneGraphNodeAdded:
                log(i, *static_cast<const EventSceneGraphNodeAdded*>(e));
                break;
            case Event::Type::SceneGraphNodeRemoved:
                log(i, *static_cast<const EventSceneGraphNodeRemoved*>(e));
                break;
            case Event::Type::ParallelConnectionEstablished:
                log(i, *static_cast<const EventParallelConnectionEstablished*>(e));
                break;
            case Event::Type::ParallelConnectionLost:
                log(i, *static_cast<const EventParallelConnectionLost*>(e));
                break;
            case Event::Type::ParallelConnectionHostshipGained:
                log(i, *static_cast<const EventParallelConnectionHostshipGained*>(e));
                break;
            case Event::Type::ParallelConnectionHostshipLost:
                log(i, *static_cast<const EventParallelConnectionHostshipLost*>(e));
                break;
            case Event::Type::ProfileLoadingFinished:
                log(i, *static_cast<const EventProfileLoadingFinished*>(e));
                break;
            case Event::Type::ApplicationShutdownStarted:
                log(i, *static_cast<const EventApplicationShutdownStarted*>(e));
                break;
            case Event::Type::ScreenSpaceRenderableAdded:
                log(i, *static_cast<const EventScreenSpaceRenderableAdded*>(e));
                break;
            case Event::Type::ScreenSpaceRenderableRemoved:
                log(i, *static_cast<const EventScreenSpaceRenderableRemoved*>(e));
                break;
            case Event::Type::CameraApproachedSceneGraphNode:
                log(i, *static_cast<const EventCameraApproachedSceneGraphNode*>(e));
                break;
            case Event::Type::CameraMovedAwayFromSceneGraphNode:
                log(i, *static_cast<const EventCameraMovedAwayFromSceneGraphNode*>(e));
                break;
            case Event::Type::TimeOfInterestReached:
                log(i, *static_cast<const EventTimeOfInterestReached*>(e));
                break;
            case Event::Type::MissionEventReached:
                log(i, *static_cast<const EventMissionEventReached*>(e));
                break;
            case Event::Type::PlanetEclipsed:
                log(i, *static_cast<const EventPlanetEclipsed*>(e));
                break;
            case Event::Type::Custom:
                log(i, *static_cast<const CustomEvent*>(e));
                break;
            default:
                LINFO(fmt::format("[{}]: Unknown {}", typeid(e).name()));
                break;
        }

        i++;
        e = e->next;
    }
}

EventSceneGraphNodeAdded::EventSceneGraphNodeAdded(const SceneGraphNode* node_)
    : Event(Type)
{
    node = temporaryString(node_->identifier());
}

EventSceneGraphNodeRemoved::EventSceneGraphNodeRemoved(const SceneGraphNode* node_)
    : Event(Type)
{
    node = temporaryString(node_->identifier());
}

EventParallelConnectionEstablished::EventParallelConnectionEstablished()
    : Event(Type)
{}

EventParallelConnectionLost::EventParallelConnectionLost()
    : Event(Type)
{}

EventParallelConnectionHostshipGained::EventParallelConnectionHostshipGained()
    : Event(Type)
{}

EventParallelConnectionHostshipLost::EventParallelConnectionHostshipLost()
    : Event(Type)
{}

EventProfileLoadingFinished::EventProfileLoadingFinished()
    : Event(Type)
{}

EventApplicationShutdownStarted::EventApplicationShutdownStarted()
    : Event(Type)
{}

EventScreenSpaceRenderableAdded::EventScreenSpaceRenderableAdded(
                                                 const ScreenSpaceRenderable* renderable_)
    : Event(Type)
{
    renderable = temporaryString(renderable_->identifier());
}

EventScreenSpaceRenderableRemoved::EventScreenSpaceRenderableRemoved(
                                                 const ScreenSpaceRenderable* renderable_)
    : Event(Type)
{
    renderable = temporaryString(renderable_->identifier());
}

EventCameraApproachedSceneGraphNode::EventCameraApproachedSceneGraphNode(
                                                                    const Camera* camera_,
                                                              const SceneGraphNode* node_)
    : Event(Type)
    , camera(camera_)
{
    node = temporaryString(node_->identifier());
}

EventCameraMovedAwayFromSceneGraphNode::EventCameraMovedAwayFromSceneGraphNode(
                                                                    const Camera* camera_,
                                                              const SceneGraphNode* node_)
    : Event(Type)
    , camera(camera_)
{
    node = temporaryString(node_->identifier());
}

EventTimeOfInterestReached::EventTimeOfInterestReached(const Time* time_,
                                                       const Camera* camera_)
    : Event(Type)
    , time(time_)
    , camera(camera_)
{}

EventMissionEventReached::EventMissionEventReached()
    : Event(Type)
{}

EventPlanetEclipsed::EventPlanetEclipsed(const SceneGraphNode* eclipsee_,
                                         const SceneGraphNode* eclipser_)
    : Event(Type)
{
    eclipsee = temporaryString(eclipsee_->identifier());
    eclipser = temporaryString(eclipser_->identifier());
}

CustomEvent::CustomEvent(std::string_view subtype_, const void* payload_)
    : Event(Type)
    , subtype(subtype_)
    , payload(payload_)
{}

} // namespace openspace::events
