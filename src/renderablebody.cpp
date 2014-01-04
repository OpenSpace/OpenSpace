
// open space includes
#include "renderablebody.h"

namespace openspace {

RenderableBody::RenderableBody(const pss &radius):Renderable(radius) {
	programObject_ = nullptr;
	texture_ = nullptr;
	rad_ = radius[0] * pow(10,radius[1]);

	// setup a unit sphere
	planet_ = new gl4::Sphere(1.0f,30);
	planet_->init();
}

RenderableBody::~RenderableBody() {
	delete planet_;
}

void RenderableBody::setProgramObject(ghoul::opengl::ProgramObject *programObject = nullptr) {
	assert(programObject) ;
	programObject_ = programObject;
}

void RenderableBody::setTexture(ghoul::opengl::Texture *texture) {
	assert(texture);
	texture_ = texture;
}

void RenderableBody::render(const Camera *camera, const psc &thisPosition) {

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
	transform = glm::scale(transform, glm::vec3(rad_,rad_,rad_));
		
	// setup the data to the shader
	programObject_->setUniform("ViewProjection", camera->getViewProjectionMatrix());
	programObject_->setUniform("ModelTransform", transform);
	programObject_->setUniform("campos", campos.getVec4f());
	programObject_->setUniform("objpos", currentPosition.getVec4f());
	programObject_->setUniform("camrot", camrot);
	programObject_->setUniform("scaling", camera->getScaling());
		
	// if texture is availible, use it
	glActiveTexture(GL_TEXTURE0);
	texture_->bind();
	programObject_->setUniform("texture1", 0);
		
	// render
	planet_->render();

	// disable shader
	programObject_->deactivate();
	
}

void RenderableBody::update() {

}
	
} // namespace openspace