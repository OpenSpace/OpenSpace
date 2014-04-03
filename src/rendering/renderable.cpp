
// open space includes
#include <openspace/rendering/renderable.h>

namespace openspace {
    
Renderable::Renderable() {}
	
Renderable::Renderable(const ghoul::Dictionary& dictionary) {}

Renderable::~Renderable() {

}

void Renderable::setBoundingSphere(const pss &boundingSphere) {
	boundingSphere_ = boundingSphere;
}

const pss& Renderable::getBoundingSphere() {
	return boundingSphere_;
}

void Renderable::update() {
}


	
} // namespace openspace