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

#ifndef __OPENSPACE_CORE___EVENT___H__
#define __OPENSPACE_CORE___EVENT___H__

#include <openspace/util/tstring.h>
#include <ghoul/misc/assert.h>

namespace openspace {
    namespace properties { class Property; }

    class Camera;
    class Layer;
    class Profile;
    class SceneGraphNode;
    class ScreenSpaceRenderable;
    class Time;
} // namepsace opensppace

namespace openspace::events {

struct Event {
    enum class Type {
        SceneGraphNodeAdded,
        SceneGraphNodeRemoved,
        ParallelConnectionEstablished,
        ParallelConnectionLost,
        ParallelConnectionHostshipGained,
        ParallelConnectionHostshipLost,
        ProfileLoadingFinished,
        ApplicationShutdown,
        ScreenSpaceRenderableAdded,
        ScreenSpaceRenderableRemoved,
        CameraApproachedSceneGraphNode,
        CameraMovedAwayFromSceneGraphNode,
        TimeOfInterestReached,
        MissionEventReached,
        PlanetEclipsed,
        InterpolationFinished,
        Custom
    };
    Event(Type type_) : type(type_) {}

    const Type type;
    const Event* next = nullptr;
};

template <typename T>
T* asType(Event* e) {
    ghoul_assert(e->type == T::Type, "Wrong type requested, check 'isType'");
    return static_cast<T*>(e);
}

template <typename T>
bool isType(Event* e) {
    return e->type == T::Type;
}

void logAllEvents(const Event* e);

//
//  Events
//

struct EventSceneGraphNodeAdded : public Event {
    static const Type Type = Event::Type::SceneGraphNodeAdded;

    EventSceneGraphNodeAdded(const SceneGraphNode* node_);
    const tstring node;
};


struct EventSceneGraphNodeRemoved : public Event {
    static const Type Type = Event::Type::SceneGraphNodeRemoved;
    
    EventSceneGraphNodeRemoved(const SceneGraphNode* node_);
    const tstring node;
};


struct EventParallelConnectionEstablished : public Event {
    static const Type Type = Event::Type::ParallelConnectionEstablished;

    EventParallelConnectionEstablished();
};


struct EventParallelConnectionLost : public Event {
    static const Type Type = Event::Type::ParallelConnectionLost;

    EventParallelConnectionLost();
};


struct EventParallelConnectionHostshipGained : public Event {
    static const Type Type = Event::Type::ParallelConnectionHostshipGained;

    EventParallelConnectionHostshipGained();
};


struct EventParallelConnectionHostshipLost : public Event {
    static const Type Type = Event::Type::ParallelConnectionHostshipLost;

    EventParallelConnectionHostshipLost();
};


struct EventProfileLoadingFinished : public Event {
    static const Type Type = Event::Type::ProfileLoadingFinished;

    EventProfileLoadingFinished();
};


struct EventApplicationShutdown : public Event {
    static const Type Type = Event::Type::ApplicationShutdown;

    enum class State : uint8_t {
        Started,
        Aborted,
        Finished
    };

    EventApplicationShutdown(State state_);
    const State state;
};


struct EventScreenSpaceRenderableAdded : public Event {
    static const Type Type = Event::Type::ScreenSpaceRenderableAdded;

    EventScreenSpaceRenderableAdded(const ScreenSpaceRenderable* renderable_);
    const tstring renderable;
};


struct EventScreenSpaceRenderableRemoved : public Event {
    static const Type Type = Event::Type::ScreenSpaceRenderableRemoved;

    EventScreenSpaceRenderableRemoved(const ScreenSpaceRenderable* renderable_);
    const tstring renderable;
};


struct EventCameraApproachedSceneGraphNode : public Event {
    static const Type Type = Event::Type::CameraApproachedSceneGraphNode;

    EventCameraApproachedSceneGraphNode(const Camera* camera_,
        const SceneGraphNode* node_);

    const Camera* camera = nullptr;
    const tstring node;
};


struct EventCameraMovedAwayFromSceneGraphNode : public Event {
    static const Type Type = Event::Type::CameraMovedAwayFromSceneGraphNode;

    EventCameraMovedAwayFromSceneGraphNode(const Camera* camera_,
        const SceneGraphNode* node_);

    const Camera* camera = nullptr;
    const tstring node;
};


// Not used right now
struct EventTimeOfInterestReached : public Event {
    static const Type Type = Event::Type::TimeOfInterestReached;

    EventTimeOfInterestReached(const Time* time_, const Camera* camera_);
    const Time* time = nullptr;
    const Camera* camera = nullptr;
};


// Not used right now
struct EventMissionEventReached : public Event {
    static const Type Type = Event::Type::MissionEventReached;

    // Not sure which kind of parameters we want to pass here
    EventMissionEventReached();
};


// Not used right now
struct EventPlanetEclipsed : public Event {
    static const Type Type = Event::Type::PlanetEclipsed;

    EventPlanetEclipsed(const SceneGraphNode* eclipsee_, const SceneGraphNode* eclipser);
    const tstring eclipsee;
    const tstring eclipser;
};


struct EventInterpolationFinished : public Event {
    static const Type Type = Event::Type::InterpolationFinished;

    EventInterpolationFinished(const properties::Property* property_);
    const tstring property;
};


struct CustomEvent : public Event {
    static const Type Type = Event::Type::Custom;

    CustomEvent(std::string_view subtype_, const void* payload_);

    const tstring subtype;
    const void* payload = nullptr;
};

} // namespace openspace::events

#endif // __OPENSPACE_CORE___EVENT___H__
