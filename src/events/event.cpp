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

void log(int i, const EventApplicationShutdown& e) {
    ghoul_assert(e.type == EventApplicationShutdown::Type, "Wrong type");
    std::string t = [](EventApplicationShutdown::State state) {
        switch (state) {
            case EventApplicationShutdown::State::Started:  return "started";
            case EventApplicationShutdown::State::Aborted:  return "aborted";
            case EventApplicationShutdown::State::Finished: return "finished";
            default:                                  throw ghoul::MissingCaseException();
        }
    }(e.state);
    LINFO(fmt::format("[{}] ApplicationShutdown", i));
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

void log(int i, const EventInterpolationFinished& e) {
    ghoul_assert(e.type == EventInterpolationFinished::Type, "Wrong type");
    LINFO(fmt::format("[{}] InterpolationFinished", i));
}

void log(int i, const CustomEvent& e) {
    ghoul_assert(e.type == CustomEvent::Type, "Wrong type");
    LINFO(fmt::format("[{}] CustomEvent: {} ({})", i, e.subtype, e.payload));
}

std::string_view toString(Event::Type type) {
    switch (type) {
        case Event::Type::SceneGraphNodeAdded: return "SceneGraphNodeAdded";
        case Event::Type::SceneGraphNodeRemoved: return "SceneGraphNodeRemoved";
        case Event::Type::ParallelConnectionEstablished:
            return "ParallelConnectionEstablished";
        case Event::Type::ParallelConnectionLost: return "ParallelConnectionLost";
        case Event::Type::ParallelConnectionHostshipGained:
            return "ParallelConnectionHostshipGained";
        case Event::Type::ParallelConnectionHostshipLost:
            return "ParallelConnectionHostshipLost";
        case Event::Type::ProfileLoadingFinished: return "ProfileLoadingFinished";
        case Event::Type::ApplicationShutdown: return "ApplicationShutdown";
        case Event::Type::ScreenSpaceRenderableAdded: return "ScreenSpaceRenderableAdded";
        case Event::Type::ScreenSpaceRenderableRemoved:
            return "ScreenSpaceRenderableRemoved";
        case Event::Type::CameraApproachedSceneGraphNode:
            return "CameraApproachedSceneGraphNode";
        case Event::Type::CameraMovedAwayFromSceneGraphNode:
            return "CameraMovedAwayFromSceneGraphNode";
        case Event::Type::TimeOfInterestReached: return "TimeOfInterestReached";
        case Event::Type::MissionEventReached: return "MissionEventReached";
        case Event::Type::PlanetEclipsed: return "PlanetEclipsed";
        case Event::Type::InterpolationFinished: return "InterpolationFinished";
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
    else if (str == "ParallelConnectionEstablished") {
        return Event::Type::ParallelConnectionEstablished;
    }
    else if (str == "ParallelConnectionLost") {
        return Event::Type::ParallelConnectionLost;
    }
    else if (str == "ParallelConnectionHostshipGained") {
        return Event::Type::ParallelConnectionHostshipGained;
    }
    else if (str == "ParallelConnectionHostshipLost") {
        return Event::Type::ParallelConnectionHostshipLost;
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
    else if (str == "CameraApproachedSceneGraphNode") {
        return Event::Type::CameraApproachedSceneGraphNode;
    }
    else if (str == "CameraMovedAwayFromSceneGraphNode") {
        return Event::Type::CameraMovedAwayFromSceneGraphNode;
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
    else if (str == "Custom") {
        return Event::Type::Custom;
    }

    throw ghoul::MissingCaseException();
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
        case Event::Type::ApplicationShutdown:
            switch (static_cast<const EventApplicationShutdown&>(e).state) {
                case EventApplicationShutdown::State::Started:
                    d.setValue("State", std::string("Started"));
                    break;
                case EventApplicationShutdown::State::Aborted:
                    d.setValue("State", std::string("Aborted"));
                    break;
                case EventApplicationShutdown::State::Finished:
                    d.setValue("State", std::string("Finished"));
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
        case Event::Type::CameraApproachedSceneGraphNode:
            d.setValue(
                "Node",
                std::string(
                    static_cast<const EventCameraApproachedSceneGraphNode&>(e).node
                )
            );
            break;
        case Event::Type::CameraMovedAwayFromSceneGraphNode:
            d.setValue(
                "Node",
                std::string(
                    static_cast<const EventCameraMovedAwayFromSceneGraphNode&>(e).node
                )
            );
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
        case Event::Type::Custom:
            d.setValue(
                "Subtype", std::string(static_cast<const CustomEvent&>(e).subtype)
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
            case Event::Type::ApplicationShutdown:
                log(i, *static_cast<const EventApplicationShutdown*>(e));
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
            case Event::Type::InterpolationFinished:
                log(i, *static_cast<const EventInterpolationFinished*>(e));
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
    , node(temporaryString(node_->identifier()))
{}

EventSceneGraphNodeRemoved::EventSceneGraphNodeRemoved(const SceneGraphNode* node_)
    : Event(Type)
    , node(temporaryString(node_->identifier()))
{}

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

EventCameraApproachedSceneGraphNode::EventCameraApproachedSceneGraphNode(
                                                                    const Camera* camera_,
                                                              const SceneGraphNode* node_)
    : Event(Type)
    , camera(camera_)
    , node(temporaryString(node_->identifier()))
{}

EventCameraMovedAwayFromSceneGraphNode::EventCameraMovedAwayFromSceneGraphNode(
                                                                    const Camera* camera_,
                                                              const SceneGraphNode* node_)
    : Event(Type)
    , camera(camera_)
    , node(temporaryString(node_->identifier()))
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

CustomEvent::CustomEvent(std::string_view subtype_, const void* payload_)
    : Event(Type)
    , subtype(subtype_)
    , payload(payload_)
{}

} // namespace openspace::events
