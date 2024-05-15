#include <modules/base/rendering/renderableswitch.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/logging/logmanager.h>


namespace {
    struct [[codegen::Dictionary(RenderableSwitch)]] Parameters {
        ghoul::Dictionary Renderable1;
        ghoul::Dictionary Renderable2;
        float DistanceThreshold;
    };
#include "renderableswitch_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableSwitch::Documentation() {
    return codegen::doc<Parameters>(
        "base_renderable_switch"
    );
}

RenderableSwitch::RenderableSwitch(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _renderable1 = std::make_unique<RenderablePlaneImageLocal>(p.Renderable1);
    _renderable2 = std::make_unique<RenderablePlaneImageLocal>(p.Renderable2);
    _distanceThreshold = p.DistanceThreshold;
}

void RenderableSwitch::initializeGL() {
    _renderable1->initializeGL();
    _renderable2->initializeGL();
}

void RenderableSwitch::deinitializeGL() {
    _renderable1->deinitializeGL();
    _renderable2->deinitializeGL();
}

bool RenderableSwitch::isReady() const {
    return _renderable1->isReady() && _renderable2->isReady();
}

void RenderableSwitch::update(const UpdateData& data) {
    _renderable1->update(data);
    _renderable2->update(data);
}

void RenderableSwitch::render(const RenderData& data, RendererTasks& tasks) {
    if (_enabled) {
        glm::dvec3 cameraPosition = data.camera.positionVec3();
        glm::dvec3 modelPosition = data.modelTransform.translation;
        //std::cout << "Distance: " << glm::distance(cameraPosition, modelPosition) << std::endl;
        if (glm::distance(cameraPosition, modelPosition) < _distanceThreshold) {
            _renderable1->render(data, tasks);
        }
        else {
            _renderable2->render(data, tasks);
        }
    }
}
} // namespace openspace
