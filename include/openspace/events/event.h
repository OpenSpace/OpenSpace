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
#include <ghoul/misc/dictionary.h>

namespace openspace {
    namespace properties { class Property; }

    class Camera;
    class Layer;
    class Profile;
    class SceneGraphNode;
    class ScreenSpaceRenderable;
    class Time;
} // namespace openspace

namespace openspace::events {

struct Event {
    // Steps to add a new event type:
    //  1. Add a new entry into this enum list
    //  2. Create a new subclass of Event in this file with a constructor that sets the
    //     Event's `type` to this new enum entry
    //  3. In the cpp file, add a new `log` message that takes the new type as an argument
    //     and that prints something useful when the log is encountered and the user wants
    //     to see all events.
    //  4. Add a new case into the logAllEvents function that handles the new enum entry
    //  5. If the new event type has any parameters it takes in its constructor, go into
    //     the `toParameter` function and add a case label for the new enum type and
    //     return a dictionary with these parameters. This dictionary is passed to actions
    //     if they are triggered by events
    //  6. Add the new enum entry into the `toString` and `fromString` methods
    enum class Type {
        SceneGraphNodeAdded,
        SceneGraphNodeRemoved,
        ParallelConnection,
        ProfileLoadingFinished,
        ApplicationShutdown,
        ScreenSpaceRenderableAdded,
        ScreenSpaceRenderableRemoved,
        CameraFocusTransition,
        TimeOfInterestReached,
        MissionEventReached,
        PlanetEclipsed,
        InterpolationFinished,
        FocusNodeChanged,
        LayerAdded,
        LayerRemoved,
        SessionRecordingPlayback,
        Custom
    };
    constexpr explicit Event(Type type_) : type(type_) {}

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

std::string_view toString(Event::Type type);
Event::Type fromString(std::string_view str);

ghoul::Dictionary toParameter(const Event& e);

void logAllEvents(const Event* e);

//
//  Events
//

/**
 * This event is created whenever a new scene graph node is added to the system.  By the
 * time this event is signalled, the scene graph node has already been created and added
 * to the scene.
 *
 * \param Node The identifier of the node that was added
 */
struct EventSceneGraphNodeAdded : public Event {
    static const Type Type = Event::Type::SceneGraphNodeAdded;

    explicit EventSceneGraphNodeAdded(const SceneGraphNode* node_);
    const tstring node;
};

/**
 * This event is created whenever a scene graph node was removed.  By the time this event
 * is signalled, the scene graph node has already been removed.
 *
 * \param Node The identifier of that node that was removed
 */
struct EventSceneGraphNodeRemoved : public Event {
    static const Type Type = Event::Type::SceneGraphNodeRemoved;

    explicit EventSceneGraphNodeRemoved(const SceneGraphNode* node_);
    const tstring node;
};

/**
 * This event is created whenever something in the parallel connection subsystem changes.
 * The new state is sent as an argument with this event.
 *
 * \param State The new state of the parallel connection system;  is one of `Established`,
 *        `Lost`, `HostshipGained`, or `HostshipLost`
 */
struct EventParallelConnection : public Event {
    static const Type Type = Event::Type::ParallelConnection;

    enum class State : uint8_t {
        Established,
        Lost,
        HostshipGained,
        HostshipLost
    };
    explicit EventParallelConnection(State state_);
    State state;
};

/**
 * This event is created when the loading of a profile is finished. This is emitted
 * regardless of whether it is the initial profile, or any subsequent profile is loaded.
 */
struct EventProfileLoadingFinished : public Event {
    static const Type Type = Event::Type::ProfileLoadingFinished;

    EventProfileLoadingFinished();
};

/**
 * This event is created whenever some information about the application shutdown sequence
 * changes. This can either be that the seqeuence started, was aborted, or is finished,
 * which means that OpenSpace is just about the shutdown.
 *
 * \param State The next state of the application shutdown sequence;  is one of `Started`,
 *        `Aborted`,  or `Finished`
 */
struct EventApplicationShutdown : public Event {
    static const Type Type = Event::Type::ApplicationShutdown;

    enum class State : uint8_t {
        Started,
        Aborted,
        Finished
    };

    explicit EventApplicationShutdown(State state_);
    const State state;
};

/**
 * This event is created when a new screenspace renderable has been created.  By the time
 * this event is craeted, the screenspace renderable is already registered and available.
 *
 * \param Renderable The identifier of the new screenspace renderable that was just added
 *        to the system
 */
struct EventScreenSpaceRenderableAdded : public Event {
    static const Type Type = Event::Type::ScreenSpaceRenderableAdded;

    explicit EventScreenSpaceRenderableAdded(const ScreenSpaceRenderable* renderable_);
    const tstring renderable;
};

/**
 * This event is created when a screenspace renderable has been removed from the system.
 * When this event is created, the screenspace renderable has already been removed and is
 * no longer available
 *
 * \param Renderable The identifier of the screenspace renderable that was removed
 */
struct EventScreenSpaceRenderableRemoved : public Event {
    static const Type Type = Event::Type::ScreenSpaceRenderableRemoved;

    explicit EventScreenSpaceRenderableRemoved(const ScreenSpaceRenderable* renderable_);
    const tstring renderable;
};

/**
 * This event is created when the camera transitions between different interaction sphere
 * distances. Right now, only movement relative to camera's focus node is considered.
 * Each scene graph node has an interaction sphere radius that serves as the reference
 * distance for all spheres.
```
Diagram of events for a camera moving from right-to-left. Interaction sphere is 'O' in
middle, and ')' are spherical boundaries. The approach factor, reach factor, and
interaction sphere radius are all taken from the current focus node.

|<------------------->|  Approach factor * Interaction sphere
             |<------>|  Reach Factor * Interaction sphere

(                       (           O          )                       )
^                       ^                      ^                       ^
Exiting                 Receding               Reaching                Approaching
```
 *
 * \param Node The name of the node the camera is transitioning relative to. Currently is
 *        always the same as the camera's focus node
 * \param Transition The transition type that the camera just finished; is one of
 *        `Approaching`, `Reaching`, `Receding`, or `Exiting`
 */
struct EventCameraFocusTransition : public Event {
    static const Type Type = Event::Type::CameraFocusTransition;

    enum class Transition {
        Approaching,
        Reaching,
        Receding,
        Exiting
    };

    EventCameraFocusTransition(const Camera* camera_, const SceneGraphNode* node_,
        Transition transition_);

    const Camera* camera = nullptr;
    const tstring node;
    const Transition transition;
};


/**
 * This event is created with a specific time of interest is reached. This event is
 * currently unused.
 */
struct EventTimeOfInterestReached : public Event {
    static const Type Type = Event::Type::TimeOfInterestReached;

    EventTimeOfInterestReached(const Time* time_, const Camera* camera_);
    const Time* time = nullptr;
    const Camera* camera = nullptr;
};


/**
 * This event is created when the end of a mission phase is reached. This event is
 * currently unused.
 */
struct EventMissionEventReached : public Event {
    static const Type Type = Event::Type::MissionEventReached;

    // Not sure which kind of parameters we want to pass here
    EventMissionEventReached();
};

/**
 * This event is created when a planet is eclipsed by a moon or a different planet. This
 * event is currently unused.
 *
 * \param Eclipsee The identifier of the scene graph node that is eclipsed by another
 *        object
 * \param Eclipser The identifier of the scene graph node that is eclipsing the other
 *        object
 */
struct EventPlanetEclipsed : public Event {
    static const Type Type = Event::Type::PlanetEclipsed;

    EventPlanetEclipsed(const SceneGraphNode* eclipsee_, const SceneGraphNode* eclipser_);
    const tstring eclipsee;
    const tstring eclipser;
};

/**
 * This event is created when the interpolation of a property value is finished. If the
 * interpolation time of a property change is 0s, this event is not fired
 *
 * \param Property The URI of the property whose interpolation has finished
 */
struct EventInterpolationFinished : public Event {
    static const Type Type = Event::Type::InterpolationFinished;

    EventInterpolationFinished(const properties::Property* property_);
    const tstring property;
};

/**
 * This event is created when the camera changes focus nodes. Even if the camera position
 * is interpolated, the node change happens instantaneously and the event is fired at the
 * same time.
 *
 * \param OldNode The identifier of the scene graph node which was the old focus node
 * \param NewNode The identifier of the scene graph node that is the new focus node
 */
struct EventFocusNodeChanged : public Event {
    static const Type Type = Event::Type::FocusNodeChanged;

    EventFocusNodeChanged(const SceneGraphNode* oldNode_, const SceneGraphNode* newNode_);
    const tstring oldNode;
    const tstring newNode;
};

/**
 * This event is created when a layer is added to to a globe.
 *
 * \param Globe The identifier of the globe to which the layer is added
 * \param Group The identifier of the layer group to which the layer is added
 * \param Layer The identifier of the layer that was added
 */
struct EventLayerAdded : public Event {
    static const Type Type = Event::Type::LayerAdded;

    explicit EventLayerAdded(std::string_view node_, std::string_view layerGroup_,
        std::string_view layer_);
    const tstring node;
    const tstring layerGroup;
    const tstring layer;
};

/**
 * This event is created when a layer is removed from a globe.
 *
 * \param Globe The identifier of the globe from which the layer is removed
 * \param Group The identifier of the layer group from which the layer is removed
 * \param Layer The identifier of the layer that was removed
 */
struct EventLayerRemoved : public Event {
    static const Type Type = Event::Type::LayerRemoved;

    explicit EventLayerRemoved(std::string_view node_, std::string_view layerGroup_,
        std::string_view layer_);
    const tstring node;
    const tstring layerGroup;
    const tstring layer;
};

/**
 * This event is created when something regarding a session recording playback changes.
 * The event contains information about the new state of the session recording subsystem.
 *
 * \param State The new state of the session recording; one of `Started`, `Paused`,
 *        `Resumed`, `Finished`
 */
struct EventSessionRecordingPlayback : public Event {
    static const Type Type = Event::Type::SessionRecordingPlayback;

    enum class State {
        Started,
        Paused,
        Resumed,
        Finished
    };

    EventSessionRecordingPlayback(State state_);
    const State state;
};

/**
 * A custom event type that can be used in a pinch when no explicit event type is
 * available. This should only be used in special circumstances and it should be
 * transitioned to a specific event type, if it is deemed to be useful.
 */
struct CustomEvent : public Event {
    static const Type Type = Event::Type::Custom;

    CustomEvent(std::string_view subtype_, const void* payload_);

    const tstring subtype;
    const void* payload = nullptr;
};

} // namespace openspace::events

#endif // __OPENSPACE_CORE___EVENT___H__
