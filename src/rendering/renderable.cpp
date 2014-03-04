
// open space includes
#include "renderable.h"

namespace openspace {
	
Renderable::Renderable() {

}

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