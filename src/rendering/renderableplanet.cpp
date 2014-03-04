
// open space includes
#include "renderableplanet.h"

namespace openspace {

RenderablePlanet::RenderablePlanet(): programObject_(nullptr), texture_(nullptr) {}

RenderablePlanet::~RenderablePlanet() {
	delete planet_;
}

    
void RenderablePlanet::initialize(ghoul::Dictionary* dictionary) {
    
}

void RenderablePlanet::setProgramObject(ghoul::opengl::ProgramObject *programObject = nullptr) {
	assert(programObject) ;
	programObject_ = programObject;
}

void RenderablePlanet::setTexture(ghoul::opengl::Texture *texture) {
	assert(texture);
	texture_ = texture;
}

void RenderablePlanet::render(const Camera *camera, const psc &thisPosition) {

	// check so that the shader is set
	assert(programObject_);
	assert(texture_);

	// activate shader
	programObject_->activate();

	// fetch data
	psc currentPosition = thisPosition;
	psc campos = camera->getPosition();
	glm::mat4 camrot = camera->getViewRotationMatrix();

	// scale the planet to appropriate size since the planet is a unit sphere
	glm::mat4 transform = glm::mat4(1);
		
	// setup the data to the shader
	programObject_->setUniform("ViewProjection", camera->getViewProjectionMatrix());
	programObject_->setUniform("ModelTransform", transform);
	programObject_->setUniform("campos", campos.getVec4f());
	programObject_->setUniform("objpos", currentPosition.getVec4f());
	programObject_->setUniform("camrot", camrot);
	programObject_->setUniform("scaling", camera->getScaling());
		
	//// if texture is availible, use it
    glActiveTexture(GL_TEXTURE0);
    texture_->bind();
    programObject_->setUniform("texture1", 0);
		
	// render
	planet_->render();

	// disable shader
	programObject_->deactivate();
	
}

void RenderablePlanet::update() {

}
	
} // namespace openspace