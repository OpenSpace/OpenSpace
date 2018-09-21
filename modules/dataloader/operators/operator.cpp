#include <modules/dataloader/operators/operator.h>
#include <modules/dataloader/dataloadermodule.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/timemanager.h>
#include <openspace/scene/scene.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>

namespace openspace::dataloader {

DataLoaderModule* Operator::module() {
    return global::moduleEngine.module<DataLoaderModule>();
}

Scene* Operator::scene() {
    return global::renderEngine.scene();
}

void Operator::setTime(std::string value) {
    Time newTime;
    newTime.setTime(value);
    global::timeManager.setTimeNextFrame(newTime);
}

} // namespace openspace::dataloader
