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
        CameraTransition,
        TimeOfInterestReached,
        MissionEventReached,
        PlanetEclipsed,
        InterpolationFinished,
        FocusNodeChanged,
        LayerAdded,
        LayerRemoved,
        SessionRecordingStartedPlayback,
        SessionRecordingFinishedPlayback,
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

struct EventSceneGraphNodeAdded : public Event {
    static const Type Type = Event::Type::SceneGraphNodeAdded;

    explicit EventSceneGraphNodeAdded(const SceneGraphNode* node_);
    const tstring node;
};


struct EventSceneGraphNodeRemoved : public Event {
    static const Type Type = Event::Type::SceneGraphNodeRemoved;
    
    explicit EventSceneGraphNodeRemoved(const SceneGraphNode* node_);
    const tstring node;
};


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

    explicit EventApplicationShutdown(State state_);
    const State state;
};


struct EventScreenSpaceRenderableAdded : public Event {
    static const Type Type = Event::Type::ScreenSpaceRenderableAdded;

    explicit EventScreenSpaceRenderableAdded(const ScreenSpaceRenderable* renderable_);
    const tstring renderable;
};


struct EventScreenSpaceRenderableRemoved : public Event {
    static const Type Type = Event::Type::ScreenSpaceRenderableRemoved;

    explicit EventScreenSpaceRenderableRemoved(const ScreenSpaceRenderable* renderable_);
    const tstring renderable;
};


struct EventCameraTransition : public Event {
    static const Type Type = Event::Type::CameraTransition;

    enum class Location {
        Outside,
        ApproachSphere,
        ReachedSphere
    };
    EventCameraTransition(const Camera* camera_, const SceneGraphNode* node_,
        Location before_, Location after_);

    const Camera* camera = nullptr;
    const tstring node;
    const Location before;
    const Location after;
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

    EventPlanetEclipsed(const SceneGraphNode* eclipsee_, const SceneGraphNode* eclipser_);
    const tstring eclipsee;
    const tstring eclipser;
};


struct EventInterpolationFinished : public Event {
    static const Type Type = Event::Type::InterpolationFinished;

    EventInterpolationFinished(const properties::Property* property_);
    const tstring property;
};


struct EventFocusNodeChanged : public Event {
    static const Type Type = Event::Type::FocusNodeChanged;

    EventFocusNodeChanged(const SceneGraphNode* oldNode_, const SceneGraphNode* newNode_);
    const tstring oldNode;
    const tstring newNode;
};


struct EventLayerAdded : public Event {
    static const Type Type = Event::Type::LayerAdded;

    explicit EventLayerAdded(std::string_view node_, std::string_view layerGroup_, 
        std::string_view layer_);
    const tstring node;
    const tstring layerGroup;
    const tstring layer;
};


struct EventLayerRemoved : public Event {
    static const Type Type = Event::Type::LayerRemoved;

    explicit EventLayerRemoved(std::string_view node_, std::string_view layerGroup_, 
        std::string_view layer_);
    const tstring node;
    const tstring layerGroup;
    const tstring layer;
};


struct EventSessionRecordingStartedPlayback : public Event {
    static const Type Type = Event::Type::SessionRecordingStartedPlayback;

    EventSessionRecordingStartedPlayback();
};


struct EventSessionRecordingFinishedPlayback : public Event {
    static const Type Type = Event::Type::SessionRecordingFinishedPlayback;

    EventSessionRecordingFinishedPlayback();
};


struct CustomEvent : public Event {
    static const Type Type = Event::Type::Custom;

    CustomEvent(std::string_view subtype_, const void* payload_);

    const tstring subtype;
    const void* payload = nullptr;
};

} // namespace openspace::events

#endif // __OPENSPACE_CORE___EVENT___H__
