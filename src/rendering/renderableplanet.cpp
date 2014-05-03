
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

RenderablePlanet::RenderablePlanet(const ghoul::Dictionary& dictionary): _programObject(nullptr),
                                                                   _texturePath(""),
                                                                   _texture(nullptr),
                                                                   _planet(nullptr)
{
    double value = 1.0f, exponent = 0.0f;
    double segments = 20.0;

    if (dictionary.hasKey("Geometry.Radius.1"))
        dictionary.getValue("Geometry.Radius.1", value);

    if (dictionary.hasKey("Geometry.Radius.2"))
        dictionary.getValue("Geometry.Radius.2", exponent);

    if (dictionary.hasKey("Geometry.Segments"))
        dictionary.getValue("Geometry.Segments", segments);

    // create the power scaled scalar
    pss planetSize(value, exponent);
    setBoundingSphere(planetSize);

    // get path if available
    std::string path = "";
    if (dictionary.hasKey("Path")) {
        dictionary.getValue("Path", path);
        path += "/";
    }

    if (dictionary.hasKey("Textures.Color")) {
        std::string texturePath;
        dictionary.getValue("Textures.Color", texturePath);
        _texturePath = path + texturePath;
    }

    _planet = new PowerScaledSphere(pss(value, exponent), static_cast<int>(segments));
}

RenderablePlanet::~RenderablePlanet() {
    deinitialize();
}

bool RenderablePlanet::initialize() {
    
    bool completeSuccess = true;
    if (_programObject == nullptr) {
        completeSuccess = OsEng.ref().configurationManager().getValue("pscShader", _programObject);
    }
    
    if(_texturePath != "") {
        _texture = ghoul::opengl::loadTexture(_texturePath);
        if (_texture) {
            LDEBUG("Loaded texture from '" << _texturePath <<"'");
            _texture->uploadTexture();
        } else {
            completeSuccess = false;
        }
    }
    _planet->initialize();
    
    return completeSuccess;
}



bool RenderablePlanet::deinitialize() {
    if(_planet)
        delete _planet;
    
    if(_texture)
        delete _texture;
    
    return true;
}

void RenderablePlanet::setProgramObject(ghoul::opengl::ProgramObject *programObject) {
	assert(programObject);
	_programObject = programObject;
}

void RenderablePlanet::setTexture(ghoul::opengl::Texture *texture) {
	assert(texture);
	_texture = texture;
}

void RenderablePlanet::render(const Camera *camera, const psc &thisPosition) {

	// check so that the shader is set
	assert(_programObject);
	assert(_texture);
    
	// activate shader
	_programObject->activate();

	// fetch data
	psc currentPosition = thisPosition;
	psc campos = camera->getPosition();
	glm::mat4 camrot = camera->getViewRotationMatrix();
    pss scaling = camera->getScaling();

	// scale the planet to appropriate size since the planet is a unit sphere
	glm::mat4 transform = glm::mat4(1);
//    transform = glm::rotate(transform, 4.1f*static_cast<float>(sgct::Engine::instance()->getTime()), glm::vec3(0.0f, 1.0f, 0.0f));

	// setup the data to the shader
	_programObject->setUniform("ViewProjection", camera->getViewProjectionMatrix());
	_programObject->setUniform("ModelTransform", transform);
	_programObject->setUniform("campos", campos.getVec4f());
	_programObject->setUniform("objpos", currentPosition.getVec4f());
	_programObject->setUniform("camrot", camrot);
	_programObject->setUniform("scaling", scaling.getVec2f());
		
	// Bind texture
    glActiveTexture(GL_TEXTURE0);
    _texture->bind();
    _programObject->setUniform("texture1", 0);
		
	// render
	_planet->render();

	// disable shader
	_programObject->deactivate();
	
}

void RenderablePlanet::update() {

}

void RenderablePlanet::setName(std::string name) {
    _name = std::move(name);
}

const std::string& RenderablePlanet::name() const {
    return _name;
}



} // namespace openspace
