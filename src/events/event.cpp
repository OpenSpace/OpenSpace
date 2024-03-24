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
    constexpr std::string_view _loggerCat = "EventInfo";
} // namespace

using namespace std::string_literals;

namespace openspace::events {

void log(int i, const EventSceneGraphNodeAdded& e) {
    ghoul_assert(e.type == EventSceneGraphNodeAdded::Type, "Wrong type");
    LINFO(std::format("[{}] SceneGraphNodeAdded: {}", i, e.node));
}

void log(int i, const EventSceneGraphNodeRemoved& e) {
    ghoul_assert(e.type == EventSceneGraphNodeRemoved::Type, "Wrong type");
    LINFO(std::format("[{}] SceneGraphNodeRemoved: {}", i, e.node));
}

void log(int i, const EventParallelConnection& e) {
    ghoul_assert(e.type == EventParallelConnection::Type, "Wrong type");
    std::string_view state = [](EventParallelConnection::State s) {
        switch (s) {
            case EventParallelConnection::State::Established:    return "Established";
            case EventParallelConnection::State::Lost:           return "Lost";
            case EventParallelConnection::State::HostshipGained: return "HostshipGained";
            case EventParallelConnection::State::HostshipLost:   return "HostshipLost";
            default:                                  throw ghoul::MissingCaseException();
        }
    }(e.state);
    LINFO(std::format("[{}] ParallelConnection ({})", i, state));
}

void log(int i, [[ maybe_unused ]] const EventProfileLoadingFinished& e) {
    ghoul_assert(e.type == EventProfileLoadingFinished::Type, "Wrong type");
    LINFO(std::format("[{}] ProfileLoadingFinished", i));
}

void log(int i, const EventApplicationShutdown& e) {
    ghoul_assert(e.type == EventApplicationShutdown::Type, "Wrong type");
    const std::string t = [](EventApplicationShutdown::State state) {
        switch (state) {
            case EventApplicationShutdown::State::Started:  return "started";
            case EventApplicationShutdown::State::Aborted:  return "aborted";
            case EventApplicationShutdown::State::Finished: return "finished";
            default:                                  throw ghoul::MissingCaseException();
        }
    }(e.state);
    LINFO(std::format("[{}] ApplicationShutdown", i));
}

void log(int i, const EventScreenSpaceRenderableAdded& e) {
    ghoul_assert(e.type == EventScreenSpaceRenderableAdded::Type, "Wrong type");
    LINFO(std::format("[{}] ScreenSpaceRenderableAdded: {}", i, e.renderable));
}

void log(int i, const EventScreenSpaceRenderableRemoved& e) {
    ghoul_assert(e.type == EventScreenSpaceRenderableRemoved::Type, "Wrong type");
    LINFO(std::format("[{}] ScreenSpaceRenderableRemoved: {}", i, e.renderable));
}

void log(int i, const EventCameraFocusTransition& e) {
    ghoul_assert(e.type == EventCameraFocusTransition::Type, "Wrong type");
    std::string_view t = [](EventCameraFocusTransition::Transition transition) {
        switch (transition) {
            case EventCameraFocusTransition::Transition::Approaching:
                return "Approaching";
            case EventCameraFocusTransition::Transition::Reaching:
                return "Reaching";
            case EventCameraFocusTransition::Transition::Receding:
                return "Receding";
            case EventCameraFocusTransition::Transition::Exiting:
                return "Exiting";
            default:
                throw ghoul::MissingCaseException();
        }
    }(e.transition);

    LINFO(std::format(
        "[{}] CameraTransition: {}, {} ({})",
        i, reinterpret_cast<const void*>(e.camera), e.node, t
    ));
}

void log(int i, const EventTimeOfInterestReached& e) {
    ghoul_assert(e.type == EventTimeOfInterestReached::Type, "Wrong type");
    LINFO(std::format(
        "[{}] TimeOfInterestReached: {},  {}",
        i, e.time->UTC(), reinterpret_cast<const void*>(e.camera)
    ));
}

void log(int i, [[ maybe_unused ]] const EventMissionEventReached& e) {
    ghoul_assert(e.type == EventMissionEventReached::Type, "Wrong type");
    LINFO(std::format("[{}] MissionEventReached", i));
}

void log(int i, const EventPlanetEclipsed& e) {
    ghoul_assert(e.type == EventPlanetEclipsed::Type, "Wrong type");
    LINFO(std::format("[{}] PlanetEclipsed: {} -> {}", i, e.eclipsee, e.eclipser));
}

void log(int i, [[ maybe_unused ]] const EventInterpolationFinished& e) {
    ghoul_assert(e.type == EventInterpolationFinished::Type, "Wrong type");
    LINFO(std::format("[{}] InterpolationFinished", i));
}

void log(int i, const EventFocusNodeChanged& e) {
    ghoul_assert(e.type == EventFocusNodeChanged::Type, "Wrong type");
    LINFO(std::format("[{}] FocusNodeChanged: {} -> {}", i, e.oldNode, e.newNode));
}

void log(int i, const EventLayerAdded& e) {
    ghoul_assert(e.type == EventLayerAdded::Type, "Wrong type");
    LINFO(std::format("[{}] LayerAdded: {}", i, e.layer));
}

void log(int i, const EventLayerRemoved& e) {
    ghoul_assert(e.type == EventLayerRemoved::Type, "Wrong type");
    LINFO(std::format("[{}] LayerRemoved: {}", i, e.layer));
}

void log(int i, const EventSessionRecordingPlayback& e) {
    ghoul_assert(e.type == EventSessionRecordingPlayback::Type, "Wrong type");

    std::string_view state = [](EventSessionRecordingPlayback::State s) {
        switch (s) {
            case EventSessionRecordingPlayback::State::Started:  return "Started";
            case EventSessionRecordingPlayback::State::Paused:   return "Paused";
            case EventSessionRecordingPlayback::State::Resumed:  return "Resumed";
            case EventSessionRecordingPlayback::State::Finished: return "Finished";
            default:                                  throw ghoul::MissingCaseException();
        }
    }(e.state);

    LINFO(std::format("[{}] SessionRecordingPlayback: {}", i, state));
}

void log(int i, const EventPointSpacecraft& e) {
    ghoul_assert(e.type == EventPointSpacecraft::Type, "Wrong type");
    LINFO(std::format(
        "[{}] PointSpacecraft: Ra: {}, Dec: {}, Duration: {}", i, e.ra, e.dec,
        e.duration
    ));
}

void log(int i, const EventRenderableEnabled& e) {
    ghoul_assert(e.type == EventRenderableEnabled::Type, "Wrong type");
    LINFO(std::format("[{}] EventRenderableEnabled: {}", i, e.node));
}

void log(int i, const EventRenderableDisabled& e) {
    ghoul_assert(e.type == EventRenderableDisabled::Type, "Wrong type");
    LINFO(std::format("[{}] EventRenderableDisabled: {}", i, e.node));
}

void log(int i, const EventCameraPathStarted& e) {
    ghoul_assert(e.type == EventCameraPathStarted::Type, "Wrong type");
    LINFO(std::format(
        "[{}] EventCameraPathStarted:  Origin: '{}'  Destination: '{}'",
        i, e.origin, e.destination
    ));
}

void log(int i, const EventCameraPathFinished& e) {
    ghoul_assert(e.type == EventCameraPathFinished::Type, "Wrong type");
    LINFO(std::format(
        "[{}] EventCameraPathFinished:  Origin: '{}'  Destination: '{}'",
        i, e.origin, e.destination
    ));
}

void log(int i, const EventCameraMovedPosition& e) {
    ghoul_assert(e.type == EventCameraMovedPosition::Type, "Wrong type");
    LINFO(std::format("[{}] EventCameraMovedPosition", i));
}

void log(int i, const CustomEvent& e) {
    ghoul_assert(e.type == CustomEvent::Type, "Wrong type");
    LINFO(std::format("[{}] CustomEvent: {} ({})", i, e.subtype, e.payload));
}

std::string_view toString(Event::Type type) {
    switch (type) {
        case Event::Type::SceneGraphNodeAdded: return "SceneGraphNodeAdded";
        case Event::Type::SceneGraphNodeRemoved: return "SceneGraphNodeRemoved";
        case Event::Type::ParallelConnection: return "ParallelConnection";
        case Event::Type::ProfileLoadingFinished: return "ProfileLoadingFinished";
        case Event::Type::ApplicationShutdown: return "ApplicationShutdown";
        case Event::Type::ScreenSpaceRenderableAdded: return "ScreenSpaceRenderableAdded";
        case Event::Type::ScreenSpaceRenderableRemoved:
            return "ScreenSpaceRenderableRemoved";
        case Event::Type::CameraFocusTransition: return "CameraFocusTransition";
        case Event::Type::TimeOfInterestReached: return "TimeOfInterestReached";
        case Event::Type::MissionEventReached: return "MissionEventReached";
        case Event::Type::PlanetEclipsed: return "PlanetEclipsed";
        case Event::Type::InterpolationFinished: return "InterpolationFinished";
        case Event::Type::FocusNodeChanged: return "FocusNodeChanged";
        case Event::Type::LayerAdded: return "LayerAdded";
        case Event::Type::LayerRemoved: return "LayerRemoved";
        case Event::Type::SessionRecordingPlayback: return "SessionRecordingPlayback";
        case Event::Type::PointSpacecraft: return "PointSpacecraft";
        case Event::Type::RenderableEnabled: return "RenderableEnabled";
        case Event::Type::RenderableDisabled: return "RenderableDisabled";
        case Event::Type::CameraPathStarted: return "CameraPathStarted";
        case Event::Type::CameraPathFinished: return "CameraPathFinished";
        case Event::Type::CameraMovedPosition: return "CameraMovedPosition";
        case Event::Type::Custom: return "Custom";
        default:
            throw ghoul::MissingCaseException();
    }
}

Event::Type fromString(std::string_view str) {
    if (str == "SceneGraphNodeAdded") {
        return Event::Type::SceneGraphNodeAdded;
    }
    else if (str == "SceneGraphNodeRemoved") {
        return Event::Type::SceneGraphNodeRemoved;
    }
    else if (str == "ParallelConnection") {
        return Event::Type::ParallelConnection;
    }
    else if (str == "ProfileLoadingFinished") {
        return Event::Type::ProfileLoadingFinished;
    }
    else if (str == "ApplicationShutdown") {
        return Event::Type::ApplicationShutdown;
    }
    else if (str == "ScreenSpaceRenderableAdded") {
        return Event::Type::ScreenSpaceRenderableAdded;
    }
    else if (str == "ScreenSpaceRenderableRemoved") {
        return Event::Type::ScreenSpaceRenderableRemoved;
    }
    else if (str == "CameraFocusTransition") {
        return Event::Type::CameraFocusTransition;
    }
    else if (str == "TimeOfInterestReached") {
        return Event::Type::TimeOfInterestReached;
    }
    else if (str == "MissionEventReached") {
        return Event::Type::MissionEventReached;
    }
    else if (str == "PlanetEclipsed") {
        return Event::Type::PlanetEclipsed;
    }
    else if (str == "InterpolationFinished") {
        return Event::Type::InterpolationFinished;
    }
    else if (str == "FocusNodeChanged") {
        return Event::Type::FocusNodeChanged;
    }
    else if (str == "LayerAdded") {
        return Event::Type::LayerAdded;
    }
    else if (str == "LayerRemoved") {
        return Event::Type::LayerRemoved;
    }
    else if (str == "SessionRecordingPlayback") {
        return Event::Type::SessionRecordingPlayback;
    }
    else if (str == "PointSpacecraft") {
        return Event::Type::PointSpacecraft;
    }
    else if (str == "RenderableEnabled") {
        return Event::Type::RenderableEnabled;
    }
    else if (str == "RenderableDisabled") {
        return Event::Type::RenderableDisabled;
    }
    else if (str == "CameraPathStarted") {
        return Event::Type::CameraPathStarted;
    }
    else if (str == "CameraPathFinished") {
        return Event::Type::CameraPathFinished;
    }
    else if (str == "CameraMovedPosition") {
        return Event::Type::CameraMovedPosition;
    }
    else if (str == "Custom") {
        return Event::Type::Custom;
    }

    throw ghoul::RuntimeError(std::format("Unknown event type '{}'", str));
}

ghoul::Dictionary toParameter(const Event& e) {
    ghoul::Dictionary d;
    switch (e.type) {
        case Event::Type::SceneGraphNodeAdded:
            d.setValue(
                "Node",
                std::string(static_cast<const EventSceneGraphNodeAdded&>(e).node)
            );
            break;
        case Event::Type::SceneGraphNodeRemoved:
            d.setValue(
                "Node",
                std::string(static_cast<const EventSceneGraphNodeRemoved&>(e).node)
            );
            break;
        case Event::Type::ParallelConnection:
            switch (static_cast<const EventParallelConnection&>(e).state) {
                case EventParallelConnection::State::Established:
                    d.setValue("State", "Established"s);
                    break;
                case EventParallelConnection::State::Lost:
                    d.setValue("State", "Lost"s);
                    break;
                case EventParallelConnection::State::HostshipGained:
                    d.setValue("State", "HostshipGained"s);
                    break;
                case EventParallelConnection::State::HostshipLost:
                    d.setValue("State", "HostshipLost"s);
                    break;
            }
            break;
        case Event::Type::ApplicationShutdown:
            switch (static_cast<const EventApplicationShutdown&>(e).state) {
                case EventApplicationShutdown::State::Started:
                    d.setValue("State", "Started"s);
                    break;
                case EventApplicationShutdown::State::Aborted:
                    d.setValue("State", "Aborted"s);
                    break;
                case EventApplicationShutdown::State::Finished:
                    d.setValue("State", "Finished"s);
                    break;
            }
            break;
        case Event::Type::ScreenSpaceRenderableAdded:
            d.setValue(
                "Renderable",
                std::string(
                    static_cast<const EventScreenSpaceRenderableAdded&>(e).renderable
                )
            );
            break;
        case Event::Type::ScreenSpaceRenderableRemoved:
            d.setValue(
                "Renderable",
                std::string(
                    static_cast<const EventScreenSpaceRenderableRemoved&>(e).renderable
                )
            );
            break;
        case Event::Type::CameraFocusTransition:
            d.setValue(
                "Node",
                std::string(static_cast<const EventCameraFocusTransition&>(e).node)
            );
            switch (static_cast<const EventCameraFocusTransition&>(e).transition) {
                case EventCameraFocusTransition::Transition::Approaching:
                    d.setValue("Transition", "Approaching"s);
                    break;
                case EventCameraFocusTransition::Transition::Reaching:
                    d.setValue("Transition", "Reaching"s);
                    break;
                case EventCameraFocusTransition::Transition::Receding:
                    d.setValue("Transition", "Receding"s);
                    break;
                case EventCameraFocusTransition::Transition::Exiting:
                    d.setValue("Transition", "Exiting"s);
                    break;
            }
            break;
        case Event::Type::PlanetEclipsed:
            d.setValue(
                "Eclipsee",
                std::string(static_cast<const EventPlanetEclipsed&>(e).eclipsee)
            );
            d.setValue(
                "Eclipser",
                std::string(static_cast<const EventPlanetEclipsed&>(e).eclipser)
            );
            break;
        case Event::Type::InterpolationFinished:
            d.setValue(
                "Property",
                std::string(static_cast<const EventInterpolationFinished&>(e).property)
            );
            break;
        case Event::Type::FocusNodeChanged:
            d.setValue(
                "OldNode",
                std::string(static_cast<const EventFocusNodeChanged&>(e).oldNode)
            );
            d.setValue(
                "NewNode",
                std::string(static_cast<const EventFocusNodeChanged&>(e).newNode)
            );
            break;
        case Event::Type::LayerAdded:
            d.setValue(
                "Globe",
                std::string(static_cast<const EventLayerAdded&>(e).node)
            );
            d.setValue(
                "Group",
                std::string(static_cast<const EventLayerAdded&>(e).layerGroup)
            );
            d.setValue(
                "Layer",
                std::string(static_cast<const EventLayerAdded&>(e).layer)
            );
            break;
        case Event::Type::LayerRemoved:
            d.setValue(
                "Globe",
                std::string(static_cast<const EventLayerRemoved&>(e).node)
            );
            d.setValue(
                "Group",
                std::string(static_cast<const EventLayerRemoved&>(e).layerGroup)
            );
            d.setValue(
                "Layer",
                std::string(static_cast<const EventLayerRemoved&>(e).layer)
            );
            break;
        case Event::Type::SessionRecordingPlayback:
            switch (static_cast<const EventSessionRecordingPlayback&>(e).state) {
                case EventSessionRecordingPlayback::State::Started:
                    d.setValue("State", "Started"s);
                    break;
                case EventSessionRecordingPlayback::State::Paused:
                    d.setValue("State", "Paused"s);
                    break;
                case EventSessionRecordingPlayback::State::Resumed:
                    d.setValue("State", "Resumed"s);
                    break;
                case EventSessionRecordingPlayback::State::Finished:
                    d.setValue("State", "Finished"s);
                    break;
            }
            break;
        case Event::Type::PointSpacecraft:
            d.setValue("Ra", static_cast<const EventPointSpacecraft&>(e).ra);
            d.setValue("Dec", static_cast<const EventPointSpacecraft&>(e).dec);
            d.setValue("Duration", static_cast<const EventPointSpacecraft&>(e).duration);
            break;
        case Event::Type::RenderableEnabled:
            d.setValue(
                "Node",
                std::string(static_cast<const EventRenderableEnabled&>(e).node)
            );
            break;
        case Event::Type::RenderableDisabled:
            d.setValue(
                "Node",
                std::string(static_cast<const EventRenderableDisabled&>(e).node)
            );
            break;
        case Event::Type::CameraPathStarted:
            d.setValue(
                "Origin",
                std::string(static_cast<const EventCameraPathStarted&>(e).origin)
            );
            d.setValue(
                "Destination",
                std::string(static_cast<const EventCameraPathStarted&>(e).destination)
            );
            break;
        case Event::Type::CameraPathFinished:
            d.setValue(
                "Origin",
                std::string(static_cast<const EventCameraPathFinished&>(e).origin)
            );
            d.setValue(
                "Destination",
                std::string(static_cast<const EventCameraPathFinished&>(e).destination)
            );
            break;
        case Event::Type::Custom:
            d.setValue(
                "Subtype", std::string(static_cast<const CustomEvent&>(e).subtype)
            );
            d.setValue(
                "Payload", std::string(static_cast<const CustomEvent&>(e).payload)
            );
            break;
        default:
            break;
    }
    return d;
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
            case Event::Type::ParallelConnection:
                log(i, *static_cast<const EventParallelConnection*>(e));
                break;
            case Event::Type::ProfileLoadingFinished:
                log(i, *static_cast<const EventProfileLoadingFinished*>(e));
                break;
            case Event::Type::ApplicationShutdown:
                log(i, *static_cast<const EventApplicationShutdown*>(e));
                break;
            case Event::Type::ScreenSpaceRenderableAdded:
                log(i, *static_cast<const EventScreenSpaceRenderableAdded*>(e));
                break;
            case Event::Type::ScreenSpaceRenderableRemoved:
                log(i, *static_cast<const EventScreenSpaceRenderableRemoved*>(e));
                break;
            case Event::Type::CameraFocusTransition:
                log(i, *static_cast<const EventCameraFocusTransition*>(e));
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
            case Event::Type::InterpolationFinished:
                log(i, *static_cast<const EventInterpolationFinished*>(e));
                break;
            case Event::Type::FocusNodeChanged:
                log(i, *static_cast<const EventFocusNodeChanged*>(e));
                break;
            case Event::Type::LayerAdded:
                log(i, *static_cast<const EventLayerAdded*>(e));
                break;
            case Event::Type::LayerRemoved:
                log(i, *static_cast<const EventLayerRemoved*>(e));
                break;
            case Event::Type::SessionRecordingPlayback:
                log(i, *static_cast<const EventSessionRecordingPlayback*>(e));
                break;
            case Event::Type::PointSpacecraft:
                log(i, *static_cast<const EventPointSpacecraft*>(e));
                break;
            case Event::Type::RenderableEnabled:
                log(i, *static_cast<const EventRenderableEnabled*>(e));
                break;
            case Event::Type::RenderableDisabled:
                log(i, *static_cast<const EventRenderableDisabled*>(e));
                break;
            case Event::Type::CameraPathStarted:
                log(i, *static_cast<const EventCameraPathStarted*>(e));
                break;
            case Event::Type::CameraPathFinished:
                log(i, *static_cast<const EventCameraPathFinished*>(e));
                break;
            case Event::Type::CameraMovedPosition:
                log(i, *static_cast<const EventCameraMovedPosition*>(e));
                break;
            case Event::Type::Custom:
                log(i, *static_cast<const CustomEvent*>(e));
                break;
            default:
                break;
        }

        i++;
        e = e->next;
    }
}

EventSceneGraphNodeAdded::EventSceneGraphNodeAdded(const SceneGraphNode* node_)
    : Event(Type)
    , node(temporaryString(node_->identifier()))
{}

EventSceneGraphNodeRemoved::EventSceneGraphNodeRemoved(const SceneGraphNode* node_)
    : Event(Type)
    , node(temporaryString(node_->identifier()))
{}

EventParallelConnection::EventParallelConnection(State state_)
    : Event(Type)
    , state(state_)
{}

EventProfileLoadingFinished::EventProfileLoadingFinished()
    : Event(Type)
{}

EventApplicationShutdown::EventApplicationShutdown(State state_)
    : Event(Type)
    , state(state_)
{}

EventScreenSpaceRenderableAdded::EventScreenSpaceRenderableAdded(
                                                 const ScreenSpaceRenderable* renderable_)
    : Event(Type)
    , renderable(temporaryString(renderable_->identifier()))
{}

EventScreenSpaceRenderableRemoved::EventScreenSpaceRenderableRemoved(
                                                 const ScreenSpaceRenderable* renderable_)
    : Event(Type)
    , renderable(temporaryString(renderable_->identifier()))
{}

EventCameraFocusTransition::EventCameraFocusTransition(const Camera* camera_,
                                                       const SceneGraphNode* node_,
                                                       Transition transition_)
    : Event(Type)
    , camera(camera_)
    , node(temporaryString(node_->identifier()))
    , transition(transition_)
{}

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
    , eclipsee(temporaryString(eclipsee_->identifier()))
    , eclipser(temporaryString(eclipser_->identifier()))
{}

EventInterpolationFinished::EventInterpolationFinished(
                                                    const properties::Property* property_)
    : Event(Type)
    , property(temporaryString(property_->fullyQualifiedIdentifier()))
{}

EventFocusNodeChanged::EventFocusNodeChanged(const SceneGraphNode* oldNode_,
                                            const SceneGraphNode* newNode_)
    : Event(Type)
    , oldNode(oldNode_ ? temporaryString(oldNode_->identifier()) : "")
    , newNode(temporaryString(newNode_->identifier()))
{
    ghoul_assert(newNode_, "There must be a new node");
}

EventLayerAdded::EventLayerAdded(std::string_view node_, std::string_view layerGroup_,
                                 std::string_view layer_)
    : Event(Type)
    , node(temporaryString(node_))
    , layerGroup(temporaryString(layerGroup_))
    , layer(temporaryString(layer_))
{}

EventLayerRemoved::EventLayerRemoved(std::string_view node_, std::string_view layerGroup_,
                                     std::string_view layer_)
    : Event(Type)
    , node(temporaryString(node_))
    , layerGroup(temporaryString(layerGroup_))
    , layer(temporaryString(layer_))
{}

EventSessionRecordingPlayback::EventSessionRecordingPlayback(State state_)
    : Event(Type)
    , state(state_)
{}

EventPointSpacecraft::EventPointSpacecraft(double ra_, double dec_, double duration_)
    : Event(Type)
    , ra(ra_)
    , dec(dec_)
    , duration(duration_)
{}

EventRenderableEnabled::EventRenderableEnabled(const SceneGraphNode* node_)
    : Event(Type)
    , node(temporaryString(node_->identifier()))
{}

EventRenderableDisabled::EventRenderableDisabled(const SceneGraphNode* node_)
    : Event(Type)
    , node(temporaryString(node_->identifier()))
{}

EventCameraPathStarted::EventCameraPathStarted(const SceneGraphNode* origin_,
                                               const SceneGraphNode* destination_)
    : Event(Type)
    , origin(temporaryString(origin_->identifier()))
    , destination(temporaryString(destination_->identifier()))
{}

EventCameraPathFinished::EventCameraPathFinished(const SceneGraphNode* origin_,
                                                 const SceneGraphNode* destination_)
    : Event(Type)
    , origin(temporaryString(origin_->identifier()))
    , destination(temporaryString(destination_->identifier()))
{}

EventCameraMovedPosition::EventCameraMovedPosition()
    : Event(Type)
{}

CustomEvent::CustomEvent(std::string_view subtype_, std::string_view payload_)
    : Event(Type)
    , subtype(subtype_)
    , payload(payload_)
{}

} // namespace openspace::events
