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

#include <ghoul/misc/assert.h>

namespace openspace {
    class Camera;
    class Layer;
    class SceneGraphNode;
    class ScreenSpaceRenderable;
} // namepsace opensppace

namespace openspace::events {

struct Event {
    enum class Type {
        SceneGraphNodeAdded,
        SceneGraphNodeRemoved,
        PropertyAdded,
        PropertyRemoved,
        ParallelConnectionEstablished,
        ParallelConnectionLost,
        ParallelConnectionHostshipGained,
        ParallelConnectionHostshipLost,
        ProfileLoadingFinished,
        ApplicationShutdownStarted,
        ScreenSpaceRenderableAdded,
        ScreenSpaceRenderableRemoved,
        CameraApproachedSceneGraphNode,
        CameraMoveAwayFromSceneGraphNode,
        TimeOfInterestReached,
        LayerAdded,
        LayerRemoved,
        TemporalLayerUpdated,
        MissionEventReached,
        PlanetEclipsed,
        Custom
    };
    Event(Type type_) : type(type_) {}

    const Type type;
    Event* next = nullptr;
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


struct EventSceneGraphNodeAdded : public Event {
    static const Type Type = Event::Type::SceneGraphNodeAdded;

    EventSceneGraphNodeAdded(const SceneGraphNode* node_)
        : Event(Type)
        , node(node_)
    {}

    const SceneGraphNode* node = nullptr;
};


struct EventSceneGraphNodeRemoved : public Event {
    static const Type Type = Event::Type::SceneGraphNodeRemoved;
    
    EventSceneGraphNodeRemoved(const SceneGraphNode* node_)
        : Event(Type)
        , node(node_)
    {}

    const SceneGraphNode* node = nullptr;
};


struct EventPropertyAdded : public Event {
    static const Type Type = Event::Type::PropertyAdded;

    EventPropertyAdded(const Property* property_)
        : Event(Type)
        , property(property_)
    {}

    const Property* property = nullptr;
};


struct EventPropertyRemoved : public Event {
    static const Type Type = Event::Type::PropertyRemoved;

    EventPropertyRemoved(const Property* property_)
        : Event(Type)
        , property(property_)
    {}

    const Property* property = nullptr;
};


struct EventParallelConnectionEstablished : public Event {
    static const Type Type = Event::Type::ParallelConnectionEstablished;

    EventParallelConnectionEstablished() : Event(Type) {}
};


struct EventParallelConnectionLost : public Event {
    static const Type Type = Event::Type::ParallelConnectionLost;

    EventParallelConnectionLost() : Event(Type) {}
};


struct EventParallelConnectionHostshipGained : public Event {
    static const Type Type = Event::Type::ParallelConnectionHostshipGained;

    EventParallelConnectionHostshipGained() : Event(Type) {}
};


struct EventParallelConnectionHostshipLost : public Event {
    static const Type Type = Event::Type::ParallelConnectionHostshipLost;

    EventParallelConnectionHostshipLost() : Event(Type) {}
};


struct EventProfileLoadingFinished : public Event {
    static const Type Type = Event::Type::ProfileLoadingFinished;

    EventProfileLoadingFinished() : Event(Type) {}
};


struct EventApplicationShutdownStarted : public Event {
    static const Type Type = Event::Type::ApplicationShutdownStarted;

    EventApplicationShutdownStarted() : Event(Type) {}
};


struct EventScreenSpaceRenderableAdded : public Event {
    static const Type Type = Event::Type::ScreenSpaceRenderableAdded;

    EventScreenSpaceRenderableAdded(const ScreenSpaceRenderable* renderable_)
        : Event(Type)
        , renderable(renderable_)
    {}

    const ScreenSpaceRenderable* renderable = nullptr;
};


struct EventScreenSpaceRenderableRemoved : public Event {
    static const Type Type = Event::Type::ScreenSpaceRenderableRemoved;

    EventScreenSpaceRenderableRemoved(const ScreenSpaceRenderable* renderable_)
        : Event(Type)
        , renderable(renderable_)
    {}

    const ScreenSpaceRenderable* renderable = nullptr;
};


struct EventCameraApproachedSceneGraphNode : public Event {
    static const Type Type = Event::Type::CameraApproachedSceneGraphNode;

    EventCameraApproachedSceneGraphNode(const Camera* camera_,
                                        const SceneGraphNode* node_)
        : Event(Type)
        , camera(camera_)
        , node(node_)
    {}

    const Camera* camera = nullptr;
    const SceneGraphNode* node = nullptr;
};


struct EventCameraApproachedSceneGraphNode : public Event {
    static const Type Type = Event::Type::CameraApproachedSceneGraphNode;

    EventCameraApproachedSceneGraphNode(const Camera* camera_,
                                        const SceneGraphNode* node_)
        : Event(Type)
        , camera(camera_)
        , node(node_)
    {}

    const Camera* camera = nullptr;
    const SceneGraphNode* node = nullptr;
};


struct EventTimeOfInterestReached : public Event {

};


struct EventLayerAdded : public Event {
    static const Type Type = Event::Type::LayerAdded;

    // Maybe needs RenderableGlobe object as well?
    EventLayerAdded(const Layer* layer_)
        : Event(Type)
        , layer(layer_)
    {}

    const Layer* layer = nullptr;
};


struct EventLayerRemoved : public Event {
    static const Type Type = Event::Type::LayerRemoved;

    // Maybe needs RenderableGlobe object as well?
    EventLayerRemoved(const Layer* layer_)
        : Event(Type)
        , layer(layer_)
    {}

    const Layer* layer = nullptr;
};


struct EventTemporalLayerUpdated : public Event {
    static const Type Type = Event::Type::TemporalLayerUpdated;

    EventTemporalLayerUpdated(const Layer* layer_)
        : Event(Type)
        , layer(layer_)
    {}

    const Layer* layer = nullptr;
};


struct EventMissionEventReached : public Event {
    static const Type Type = Event::Type::MissionEventReached;

    // Not sure which kind of parameters we want to pass here
    EventMissionEventReached()
        : Event(Type)
    {}
};


struct EventPlanetEclipsed : public Event {
    static const Type Type = Event::Type::PlanetEclipsed;

    EventPlanetEclipsed(const SceneGraphNode* eclipsee_, const SceneGraphNode* eclipser)
        : Event(Type)
        , eclipsee(eclipsee_)
        , eclipser(eclipser)
    {}

    const SceneGraphNode* eclipsee = nullptr;
    const SceneGraphNode* eclipser = nullptr;
};


template <typename T>
struct CustomEvent : public Event {
    static const Type Type = Event::Type::Custom;

    CustomEvent(std::string_view type_, const T* payload_)
        : Event(Type)
        , type(type_),
        , payload(payload_)
    {}

    const std:string_view type;
    const T* payload = nullptr;
};

} // namespace openspace::events

#endif // __OPENSPACE_CORE___EVENT___H__
