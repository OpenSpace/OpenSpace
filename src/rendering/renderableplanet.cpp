/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014                                                                    *
*                                                                                       *
* Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
* software and associated documentation files (the "Software"), to deal in the Software *
* without restriction, including without limitation the rights to use, copy, modify,    *
* merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
* permit persons to whom the Software is furnished to do so, subject to the following   *
* conditions:                                                                           *
*                                                                                       *
* The above copyright notice and this permission notice shall be included in all copies *
* or substantial portions of the Software.                                              *
*                                                                                       *
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
* INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
* PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
* HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
* CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
* OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
****************************************************************************************/

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

RenderablePlanet::RenderablePlanet(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _radius("radius", "Radius", glm::vec2(1.f, 0.f), glm::vec2(-10.f, -20.f),
              glm::vec2(10.f, 20.f))
    , _segments("segments", "Segments", 20, 1, 1000)
    , _colorTexturePath("colorTexture", "Color Texture")
    , _programObject(nullptr)
    , _texture(nullptr)
    , _planet(nullptr)
{
    double value = 1.0f, exponent = 0.0f;
    int segments = 20;

    if (dictionary.hasKey("Geometry.Radius.1"))
        dictionary.getValue("Geometry.Radius.1", value);

    if (dictionary.hasKey("Geometry.Radius.2"))
        dictionary.getValue("Geometry.Radius.2", exponent);

    _radius = glm::vec2(value, exponent);

    if (dictionary.hasKey("Geometry.Segments"))
        dictionary.getValue("Geometry.Segments", segments);

    _segments = segments;

    // get path if available
    std::string path = "";
    if (dictionary.hasKey("Path")) {
        dictionary.getValue("Path", path);
        path += "/";
    }

    std::string texturePath = "";
    if (dictionary.hasKey("Textures.Color"))
        dictionary.getValue("Textures.Color", texturePath);

    _colorTexturePath = path + texturePath;


    addProperty(_radius);
    _radius.onChange(std::bind(&RenderablePlanet::createSphere, this));
    addProperty(_segments);
    _segments.onChange(std::bind(&RenderablePlanet::createSphere, this));

    addProperty(_colorTexturePath);
    _colorTexturePath.onChange(std::bind(&RenderablePlanet::loadTexture, this));

    createSphere();
}

RenderablePlanet::~RenderablePlanet() {
    deinitialize();
}

bool RenderablePlanet::initialize() {
    bool completeSuccess = true;
    if (_programObject == nullptr)
        completeSuccess &= OsEng.ref().configurationManager().getValue("pscShader", _programObject);
    
    loadTexture();
    completeSuccess &= (_texture != nullptr);

    _planet->initialize();
    
    return completeSuccess;
}

bool RenderablePlanet::deinitialize() {
    delete _planet;
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

void RenderablePlanet::createSphere() {
    // create the power scaled scalar

    pss planetSize(_radius);
    setBoundingSphere(planetSize);

    delete _planet;
    _planet = new PowerScaledSphere(planetSize, _segments);
    _planet->initialize();
}

void RenderablePlanet::loadTexture() {
    delete _texture;
    _texture = nullptr;
    if (_colorTexturePath.value() != "") {
        _texture = ghoul::opengl::loadTexture(_colorTexturePath);
        if (_texture) {
            LDEBUG("Loaded texture from '" << _colorTexturePath.value() << "'");
            _texture->uploadTexture();
        }
    }
}

} // namespace openspace
