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
#include <openspace/rendering/planets/renderableplanet.h>
#include <openspace/util/constants.h>
#include <openspace/rendering/planets/planetgeometry.h>

#include <ghoul/opengl/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/engine/openspaceengine.h>
#include <sgct.h>

namespace {
const std::string _loggerCat = "RenderablePlanet";
}

namespace openspace {

RenderablePlanet::RenderablePlanet(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _colorTexturePath("colorTexture", "Color Texture")
    , _programObject(nullptr)
    , _texture(nullptr)
    //, _planet(nullptr)
    , _geometry(nullptr)
{
    std::string path;
    dictionary.getValue(constants::scenegraph::keyPathModule, path);

    if (dictionary.hasKey(constants::renderableplanet::keyGeometry)) {
        ghoul::Dictionary geometryDictionary;
        dictionary.getValue(constants::renderableplanet::keyGeometry, geometryDictionary);
        geometryDictionary.setValue(constants::scenegraph::keyPathModule, path);
        geometryDictionary.setValue(constants::scenegraphnode::keyName, name());

        _geometry
              = planetgeometry::PlanetGeometry::createFromDictionary(geometryDictionary);
    }

    // TODO: textures need to be replaced by a good system similar to the geometry as soon
    // as the requirements are fixed (ab)
    std::string texturePath = "";
    if (dictionary.hasKey("Textures.Color")) {
        dictionary.getValue("Textures.Color", texturePath);
        _colorTexturePath = path + "/" + texturePath;
    }

    for (properties::Property* p : _geometry->properties())
        addProperty(p);

     addProperty(_colorTexturePath);
    _colorTexturePath.onChange(std::bind(&RenderablePlanet::loadTexture, this));
}

RenderablePlanet::~RenderablePlanet()
{
    deinitialize();
}

bool RenderablePlanet::initialize()
{
    bool completeSuccess = true;
    if (_programObject == nullptr)
        completeSuccess
              &= OsEng.ref().configurationManager().getValue("pscShader", _programObject);

    loadTexture();
    completeSuccess &= (_texture != nullptr);

    completeSuccess &= _geometry->initialize(this);

    return completeSuccess;
}

bool RenderablePlanet::deinitialize()
{
    _geometry->deinitialize();
    delete _geometry;
    _geometry = nullptr;
    delete _texture;
    _texture = nullptr;
    return true;
}

void RenderablePlanet::render(const Camera* camera, const psc& thisPosition)
{
    // TODO replace with more robust assert
    // check so that the shader is set
    assert(_programObject);
    assert(_texture);

    // activate shader
    _programObject->activate();

    // fetch data
    psc currentPosition = thisPosition;
    psc campos = camera->position();
    glm::mat4 camrot = camera->viewRotationMatrix();
    pss scaling = camera->scaling();

    // scale the planet to appropriate size since the planet is a unit sphere
    glm::mat4 transform = glm::mat4(1);
    transform = glm::rotate(
          transform, 4.1f * static_cast<float>(sgct::Engine::instance()->getTime()),
          glm::vec3(0.0f, 1.0f, 0.0f));

    // setup the data to the shader
    _programObject->setUniform("ViewProjection", camera->viewProjectionMatrix());
    _programObject->setUniform("ModelTransform", transform);
    _programObject->setUniform("campos", campos.getVec4f());
    _programObject->setUniform("objpos", currentPosition.getVec4f());
    _programObject->setUniform("camrot", camrot);
    _programObject->setUniform("scaling", scaling.getVec2f());

    // Bind texture
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _programObject->setUniform("texture1", unit);

    // render
    _geometry->render();

    // disable shader
    _programObject->deactivate();
}

void RenderablePlanet::update()
{
}

void RenderablePlanet::loadTexture()
{
    delete _texture;
    _texture = nullptr;
    if (_colorTexturePath.value() != "") {
        _texture = ghoul::opengl::loadTexture(absPath(_colorTexturePath));
        if (_texture) {
            LDEBUG("Loaded texture from '" << absPath(_colorTexturePath) << "'");
            _texture->uploadTexture();
        }
    }
}

}  // namespace openspace
