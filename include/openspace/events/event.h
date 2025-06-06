/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
    class Renderable;
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
    enum class Type : uint8_t {
        ParallelConnection,
        ProfileLoadingFinished,
        AssetLoadingFinished,
        ApplicationShutdown,
        CameraFocusTransition,
        TimeOfInterestReached,
        MissionAdded,
        MissionRemoved,
        MissionEventReached,
        PlanetEclipsed,
        InterpolationFinished,
        FocusNodeChanged,
        PropertyTreeUpdated,
        PropertyTreePruned,
        ActionAdded,
        ActionRemoved,
        SessionRecordingPlayback,
        PointSpacecraft,
        RenderableEnabled,
        RenderableDisabled,
        CameraPathStarted,
        CameraPathFinished,
        CameraMovedPosition,
        ScheduledScriptExecuted,
        GuiTreeUpdated,
        Custom,
        Last // sentinel value
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
 * This event is created whenever something in the parallel connection subsystem changes.
 * The new state is sent as an argument with this event.
 */
struct EventParallelConnection : public Event {
    static constexpr Type Type = Event::Type::ParallelConnection;

    enum class State : uint8_t {
        Established,
        Lost,
        HostshipGained,
        HostshipLost
    };

    /**
     * Creates an instance of an EventParallelConnection event.
     *
     * \param state_ The new state of the parallel connection system; is one of
     *        `Established`, `Lost`, `HostshipGained`, or `HostshipLost`
     */
    explicit EventParallelConnection(State state_);
    State state;
};

/**
 * This event is created when the loading of a profile is finished. This is emitted
 * regardless of whether it is the initial profile, or any subsequent profile is loaded.
 */
struct EventProfileLoadingFinished : public Event {
    static constexpr Type Type = Event::Type::ProfileLoadingFinished;

    /**
     * Creates an instance of an EventProfileLoadingFinished event.
     */
    EventProfileLoadingFinished();
};

/**
* This event is created when the loading of all assets are finished. This is emitted
* regardless of whether it is the initial startup of a profile, or any subsequent asset
* being loaded e.g., through add or drag-and-drop.
*/
struct EventAssetLoadingFinished : public Event {
    static constexpr Type Type = Event::Type::AssetLoadingFinished;

    /**
     * Creates an instance of an AssetLoadingFinished event.
     */
    EventAssetLoadingFinished();
};

/**
 * This event is created whenever some information about the application shutdown sequence
 * changes. This can either be that the seqeuence started, was aborted, or is finished,
 * which means that OpenSpace is just about the shutdown.
 */
struct EventApplicationShutdown : public Event {
    static constexpr Type Type = Event::Type::ApplicationShutdown;

    enum class State : uint8_t {
        Started,
        Aborted,
        Finished
    };

    /**
     * Creates an instance of an EventApplicationShutdown event.
     *
     * \param state_ The next state of the application shutdown sequence; is one of
     *        `Started`, `Aborted`, or `Finished`
     */
    explicit EventApplicationShutdown(State state_);
    const State state;
};

/**
 * This event is created when the camera transitions between different interaction sphere
 * distances. Right now, only movement relative to camera's focus node is considered.
 * Each scene graph node has an interaction sphere radius that serves as the reference
 * distance for all spheres.
 * ```
 * Diagram of events for a camera moving from right-to-left. Interaction sphere is 'O' in
 * middle, and ')' are spherical boundaries. The approach factor, reach factor, and
 * interaction sphere radius are all taken from the current focus node.
 *
 * |<------------------->|  Approach factor * Interaction sphere
 *              |<------>|  Reach Factor * Interaction sphere
 *
 * (                       (           O          )                       )
 * ^                       ^                      ^                       ^
 * Exiting                 Receding               Reaching                Approaching
 * ```
 */
struct EventCameraFocusTransition : public Event {
    static constexpr Type Type = Event::Type::CameraFocusTransition;

    enum class Transition {
        Approaching,
        Reaching,
        Receding,
        Exiting
    };

    /**
     * Creates an instance of an EventCameraFocusTransition event.
     *
     * \param camera_ The camera object that caused the transition
     * \param node_ The name of the node the camera is transitioning relative to.
     *        Currently is always the same as the camera's focus node
     * \param transition_ The transition type that the camera just finished; is one of
     *        `Approaching`, `Reaching`, `Receding`, or `Exiting`
     *
     * \pre camera_ must not be nullptr
     * \pre node_ must not be nullptr
     */
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
    static constexpr Type Type = Event::Type::TimeOfInterestReached;

    /**
     * Creates an instance of an EventTimeOfInterestReached event.
     *
     * \param time_ The time of interest that has been reached
     * \param camera_ Information about the camera for the specific transition
     */
    EventTimeOfInterestReached(const Time* time_, const Camera* camera_);

    const Time* time = nullptr;
    const Camera* camera = nullptr;
};


/**
 * This event is created when a mission is added.
 */
struct EventMissionAdded : public Event {
    static constexpr Type Type = Event::Type::MissionAdded;

    /**
     * Creates an instance of an EventMissionAdded event.
     *
     * \param identifier The identifier of the mission added
     */
    EventMissionAdded(std::string_view identifier);

    const tstring identifier;
};

/**
 * This event is created when a mission is removed.
 */
struct EventMissionRemoved : public Event {
    static constexpr Type Type = Event::Type::MissionRemoved;

    /**
     * Creates an instance of an EventMissionRemoved event.
     *
     * \param identifier The identifier of the mission removed
     */
    EventMissionRemoved(std::string_view identifier);

    const tstring identifier;
};

/**
 * This event is created when the end of a mission phase is reached. This event is
 * currently unused.
 */
struct EventMissionEventReached : public Event {
    static constexpr Type Type = Event::Type::MissionEventReached;

    // Not sure which kind of parameters we want to pass here.
    EventMissionEventReached();
};

/**
 * This event is created when a planet is eclipsed by a moon or a different planet. This
 * event is currently unused.
 */
struct EventPlanetEclipsed : public Event {
    static constexpr Type Type = Event::Type::PlanetEclipsed;

    /**
     * Creates an instance of an EventPlanetEclipsed event.

     * \param eclipsee_ The scene graph node that is eclipsed by another object
     * \param eclipser_ The scene graph node that is eclipsing the other object
     *
     * \pre eclipsee_ must not be nullptr
     * \pre eclipser_ must not be nullptr
     */
    EventPlanetEclipsed(const SceneGraphNode* eclipsee_, const SceneGraphNode* eclipser_);

    const tstring eclipsee;
    const tstring eclipser;
};

/**
 * This event is created when the interpolation of a property value is finished. If the
 * interpolation time of a property change is 0s, this event is not fired.
 */
struct EventInterpolationFinished : public Event {
    static constexpr Type Type = Event::Type::InterpolationFinished;

    /**
     * Creates an instance of an EventInterpolationFinished event.
     *
     * \param property_ The property whose interpolation has finished
     *
     * \pre property_ must not be nullptr
     */
    EventInterpolationFinished(const properties::Property* property_);
    const tstring property;
};

/**
 * This event is created when the camera changes focus nodes. Even if the camera position
 * is interpolated, the node change happens instantaneously and the event is fired at the
 * same time.
 */
struct EventFocusNodeChanged : public Event {
    static constexpr Type Type = Event::Type::FocusNodeChanged;

    /**
     * Creates an instance of an EventFocusNodeChanged event.
     *
     * \param oldNode_ The scene graph node which was the old focus node
     * \param newNode_ The scene graph node that is the new focus node
     *
     * \pre oldNode_ must not be nullptr
     * \pre newNode_ must not be nullptr
     */
    EventFocusNodeChanged(const SceneGraphNode* oldNode_, const SceneGraphNode* newNode_);

    const tstring oldNode;
    const tstring newNode;
};

/**
 * This event is created a property owner or property has been added or has changed.
 */
struct EventPropertyTreeUpdated : public Event {
    static constexpr Type Type = Event::Type::PropertyTreeUpdated;

    /**
     * Creates an instance of an EventPropertyTreeUpdated event.
     *
     * \param uri_ A string with the uri of the property or property owner that was added
     *
     * \pre uri_ must be a valid uri
     */
    explicit EventPropertyTreeUpdated(std::string_view uri_);

    const tstring uri;
};

/**
 * This event is created when a property owner or property is removed from a the property
 * tree.
 */
struct EventPropertyTreePruned : public Event {
    static constexpr Type Type = Event::Type::PropertyTreePruned;

    /**
     * Creates an instance of an EventPropertyTreePruned event.
     *
     * \param uri_ The uri of the property or property owner that was removed
     *
     * \pre uri_ must be a valid uri
     */
    explicit EventPropertyTreePruned(std::string_view uri_);

    const tstring uri;
};

/**
 * This event is created when an action is added.
 */
struct EventActionAdded : public Event {
    static constexpr Type Type = Event::Type::ActionAdded;

    /**
     * Creates an instance of an EventActionAdded event.
     *
     * \param uri_ A string with the uri of the action that was added
     *
     * \pre uri_ must be a valid uri
     */
    explicit EventActionAdded(std::string_view uri_);

    const tstring uri;
};

/**
 * This event is created when an action is removed.
 */
struct EventActionRemoved : public Event {
    static constexpr Type Type = Event::Type::ActionRemoved;

    /**
     * Creates an instance of an EventActionRemoved event.
     *
     * \param uri_ The uri of the action that was removed
     *
     * \pre uri_ must be a valid uri
     */
    explicit EventActionRemoved(std::string_view uri_);

    const tstring uri;
};

/**
 * This event is created when something regarding a session recording playback changes.
 * The event contains information about the new state of the session recording subsystem.
 */
struct EventSessionRecordingPlayback : public Event {
    static constexpr Type Type = Event::Type::SessionRecordingPlayback;

    enum class State {
        Started,
        Paused,
        Resumed,
        Finished
    };

    /**
     * Creates an instance of an EventSessionRecordingPlayback event.
     *
     * \param state_ The new state of the session recording; one of `Started`, `Paused`,
     *        `Resumed`, `Finished`
     */
    EventSessionRecordingPlayback(State state_);

    const State state;
};

/**
 * This event is created when a request for pointing a spacecraft towards a Ra Dec
 * coordinate in the sky is issued. The event contains information about the sky
 * coordinate to point the spacecraft towards, and an optional argument for the duration
 * it should do the pointing.
 */
struct EventPointSpacecraft : public Event {
    static constexpr Type Type = Event::Type::PointSpacecraft;

    /**
     * Creates an instance of an EventSessionRecordingPlayback event.
     *
     * \param ra_ The Ra part of the sky coordinate in decimal degrees to point towards
     * \param dec_ The Dec part of the sky coordinate in decimal degrees to point towards
     * \param duration_ The duration of time in seconds that the spacecraft should
     *        redirect itself to the coordinate. Default is 3 seconds
     */
    EventPointSpacecraft(double ra_, double dec_, double duration_ = 3.0);

    const double ra;
    const double dec;
    const double duration;
};

/**
 * This event is created whenever a renderable is enabled. By the time this event is
 * signalled, the renderable has already been enabled.
 */
struct EventRenderableEnabled : public Event {
    static constexpr Type Type = Event::Type::RenderableEnabled;

    /**
     * Creates an instance of an EventRenderableEnabled event.
     *
     * \param node_ The identifier of the node that contains the renderable
     *
     * \pre node_ must not be nullptr
     */
    explicit EventRenderableEnabled(const SceneGraphNode* node_);

    const tstring node;
};

/**
 * This event is created whenever a renderable is disabled. By the time this event is
 * signalled, the renderable has already been disabled.
 */
struct EventRenderableDisabled : public Event {
    static constexpr Type Type = Event::Type::RenderableDisabled;

    /**
     * Creates an instance of an EventRenderableDisabled event.
     *
     * \param node_ The identifier of that node that contains the renderable
     *
     * \pre node_ must not be nullptr
     */
    explicit EventRenderableDisabled(const SceneGraphNode* node_);

    const tstring node;
};

/**
 * This event is created when the a camera path is started
 */
struct EventCameraPathStarted : public Event {
    static constexpr Type Type = Event::Type::CameraPathStarted;

    /**
     * Creates an instance of an EventCameraPathStarted event.
     *
     * \param origin_ The scene graph node from which the path started
     * \param destination_ The scene graph node at which the path ends
     */
    EventCameraPathStarted(const SceneGraphNode* origin_,
        const SceneGraphNode* destination_);

    const tstring origin;
    const tstring destination;
};

/**
 * This event is created when the a camera path is finished
 */
struct EventCameraPathFinished : public Event {
    static constexpr Type Type = Event::Type::CameraPathFinished;

    /**
     * Creates an instance of an EventCameraPathStarted event.
     *
     * \param origin_ The scene graph node from which the path started
     * \param destination_ The scene graph node where the path ended
     */
    EventCameraPathFinished(const SceneGraphNode* origin_,
        const SceneGraphNode* destination_);

    const tstring origin;
    const tstring destination;
};

/**
 * This event is created when the a camera moves location.
 */
struct EventCameraMovedPosition : public Event {
    static constexpr Type Type = Event::Type::CameraMovedPosition;

    /**
     * Creates an instance of an EventCameraMovedPosition event.
     */
    EventCameraMovedPosition();
};

/**
 * This event is created when a scheduled script is executed.
 */
struct EventScheduledScriptExecuted : public Event {
    static constexpr Type Type = Event::Type::ScheduledScriptExecuted;

    /**
     * Creates an instance of an ScheduledScriptExecuted event.
     */
    EventScheduledScriptExecuted(std::string_view script_);

    const tstring script;
};

/**
 * This event is created when the custom ordering for a specific branch in the Scene
 * GUI tree is changed. It signals to the UI that the tree should be updated.
 */
struct EventGuiTreeUpdated : public Event {
    static constexpr Type Type = Event::Type::GuiTreeUpdated;

    /**
     * Creates an instance of an EventGuiTreeUpdated event.
     */
    EventGuiTreeUpdated();
};

/**
 * A custom event type that can be used in a pinch when no explicit event type is
 * available. This should only be used in special circumstances and it should be
 * transitioned to a specific event type, if it is deemed to be useful.
 */
struct CustomEvent : public Event {
    static constexpr Type Type = Event::Type::Custom;

    /**
     * Creates an instance of a CustomEvent event.
     *
     * \param subtype_ A textual description of the custom subtype that is emitted
     * \param payload_ The payload in a string form
     *
     * \pre subtype_ must not be empty
     */
    CustomEvent(std::string_view subtype_, std::string_view payload_);

    const tstring subtype;
    const tstring payload;
};

} // namespace openspace::events

#endif // __OPENSPACE_CORE___EVENT___H__
