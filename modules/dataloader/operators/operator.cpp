
#include <modules/dataloader/operators/operator.h>
#include <modules/dataloader/dataloadermodule.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/timemanager.h>
#include <openspace/scene/scene.h>

namespace openspace::dataloader {

DataLoaderModule* Operator::module() {
    return OsEng.moduleEngine().module<DataLoaderModule>();
}

Scene* Operator::scene() {
    return OsEng.renderEngine().scene();
}

Time& Operator::time() {
    return OsEng.timeManager().time();
}

} // namespace openspace::dataloader
