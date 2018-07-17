#include <modules/dataloader/operators/operator.h>
#include <modules/dataloader/dataloadermodule.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/timemanager.h>
#include <openspace/scene/scene.h>
#include <openspace/rendering/renderengine.h>

namespace openspace::dataloader {

DataLoaderModule* Operator::module() {
    return OsEng.moduleEngine().module<DataLoaderModule>();
}

Scene* Operator::scene() {
    return OsEng.renderEngine().scene();
}

void Operator::setTime(std::string value) {
    Time newTime;
    newTime.setTime(value);
    OsEng.timeManager().setTimeNextFrame(newTime);
}

} // namespace openspace::dataloader
