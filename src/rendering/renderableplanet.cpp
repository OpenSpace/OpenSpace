
// open space includes
#include "renderableplanet.h"

#include <ghoul/opengl/texturereader.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspaceengine.h>
#include <sgct.h>

namespace {
    std::string _loggerCat = "RenderablePlanet";
}

namespace openspace {

RenderablePlanet::RenderablePlanet(): programObject_(nullptr), texture_(nullptr),
                                      planet_(nullptr) {}

RenderablePlanet::~RenderablePlanet() {
	delete planet_;
}

bool RenderablePlanet::initialize() {
    
    if (programObject_ == nullptr) {
        OsEng.ref().configurationManager().getValue("pscShader", programObject_);
    }
    if (programObject_ == nullptr) {
        return false;
    }
    
    return true;
}

bool RenderablePlanet::initializeWithDictionary(ghoul::Dictionary* dictionary) {

    if ( ! initialize()) {
        return false;
    }

    double value = 1.0f, exponent= 0.0f;
    double segments = 20.0;
    
    if(dictionary->hasKey("Geometry.Radius.1"))
        dictionary->getValue("Geometry.Radius.1", value);
    
    if(dictionary->hasKey("Geometry.Radius.2"))
        dictionary->getValue("Geometry.Radius.2", exponent);
    
    if(dictionary->hasKey("Geometry.Segments"))
        dictionary->getValue("Geometry.Segments", segments);
    
    // create the power scaled scalar
    pss planetSize(value, exponent);
    setBoundingSphere(planetSize);
    
    // get path if available
    std::string path;
    dictionary->getValue("Path", path);
    
    if(dictionary->hasKey("Textures.Color")) {
        std::string texturePath;
        dictionary->getValue("Textures.Color", texturePath);
        std::string fullpath = path + "/" + texturePath;
        texture_ = ghoul::opengl::loadTexture(fullpath);
        if (texture_) {
            LDEBUG("Loaded texture from '" << fullpath <<"'");
            texture_->uploadTexture();
        }
    }
    
    planet_ = new PowerScaledSphere(pss(value, exponent), static_cast<int>(segments));
    
    return true;
}

void RenderablePlanet::setProgramObject(ghoul::opengl::ProgramObject *programObject = nullptr) {
	assert(programObject);
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
    pss scaling = camera->getScaling();

	// scale the planet to appropriate size since the planet is a unit sphere
	glm::mat4 transform = glm::mat4(1);
    transform = glm::rotate(transform, 4.1f*static_cast<float>(sgct::Engine::instance()->getTime()), glm::vec3(0.0f, 1.0f, 0.0f));
		
	// setup the data to the shader
	programObject_->setUniform("ViewProjection", camera->getViewProjectionMatrix());
	programObject_->setUniform("ModelTransform", transform);
	programObject_->setUniform("campos", campos.getVec4f());
	programObject_->setUniform("objpos", currentPosition.getVec4f());
	programObject_->setUniform("camrot", camrot);
	programObject_->setUniform("scaling", scaling.getVec2f());
		
	// Bind texture
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