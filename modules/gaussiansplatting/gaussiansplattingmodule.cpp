#include <modules/gaussiansplatting/gaussiansplattingmodule.h>

#include <modules/gaussiansplatting/rendering/renderablegaussiansplat.h>
#include <openspace/documentation/documentation.h>
#include <openspace/misc/templatefactory.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/factorymanager.h>
#include <cassert>

namespace openspace {

GaussianSplattingModule::GaussianSplattingModule()
    : OpenSpaceModule(Name)
{}

void GaussianSplattingModule::internalInitialize(const Dictionary&) {
    TemplateFactory<Renderable>* fRenderable =
        FactoryManager::ref().factory<Renderable>();
    assert(fRenderable && "Renderable factory was not created");

    fRenderable->registerClass<RenderableGaussianSplat>("RenderableGaussianSplat");
}

} // namespace openspace