
// open space includes
#include <openspace/rendering/renderableplanet.h>

#include <ghoul/opengl/texturereader.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/engine/openspaceengine.h>
#include <sgct.h>

namespace {
    std::string _loggerCat = "RenderablePlanet";
}

namespace openspace {

RenderablePlanet::RenderablePlanet(const ghoul::Dictionary& dictionary): programObject_(nullptr),
                                                                   _texturePath(""),
                                                                   texture_(nullptr),
                                                                   planet_(nullptr)
{
    double value = 1.0f, exponent= 0.0f;
    double segments = 20.0;
    
    if(dictionary.hasKey("Geometry.Radius.1"))
        dictionary.getValue("Geometry.Radius.1", value);
    
    if(dictionary.hasKey("Geometry.Radius.2"))
        dictionary.getValue("Geometry.Radius.2", exponent);
    
    if(dictionary.hasKey("Geometry.Segments"))
        dictionary.getValue("Geometry.Segments", segments);
    
    // create the power scaled scalar
    pss planetSize(value, exponent);
    setBoundingSphere(planetSize);
    
    // get path if available
    std::string path = "";
    if(dictionary.hasKey("Path")) {
       dictionary.getValue("Path", path);
       path += "/";
    }
    
    if(dictionary.hasKey("Textures.Color")) {
        std::string texturePath;
        dictionary.getValue("Textures.Color", texturePath);
        _texturePath = path + texturePath;
    }
    
    planet_ = new PowerScaledSphere(pss(value, exponent), static_cast<int>(segments));
}

RenderablePlanet::~RenderablePlanet() {
    deinitialize();
}

bool RenderablePlanet::initialize() {
    
    bool completeSuccess = true;
    if (programObject_ == nullptr) {
        completeSuccess = OsEng.ref().configurationManager().getValue("pscShader", programObject_);
    }
    
    if(_texturePath != "") {
        texture_ = ghoul::opengl::loadTexture(_texturePath);
        if (texture_) {
            LDEBUG("Loaded texture from '" << _texturePath <<"'");
            texture_->uploadTexture();
        } else {
            completeSuccess = false;
        }
    }
    planet_->initialize();
    
    return completeSuccess;
}



bool RenderablePlanet::deinitialize() {
    if(planet_)
        delete planet_;
    
    if(texture_)
        delete texture_;
    
    return true;
}

void RenderablePlanet::setProgramObject(ghoul::opengl::ProgramObject *programObject) {
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