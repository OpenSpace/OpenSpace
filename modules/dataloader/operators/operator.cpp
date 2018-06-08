
#include <modules/dataloader/operators/operator.h>
#include <modules/dataloader/dataloadermodule.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>

namespace openspace::dataloader {

DataLoaderModule* Operator::getModule() {
    return OsEng.moduleEngine().module<DataLoaderModule>();
}

}
