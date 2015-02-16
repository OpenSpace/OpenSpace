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

#include <openspace/rendering/renderablesphere.h>
#include <openspace/util/constants.h>

#include <openspace/engine/openspaceengine.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/util/powerscaledsphere.h>


namespace {
	const std::string _loggerCat = "RenderableSphere";

    const std::string keySize = "Size";
    const std::string keySegments = "Segments";
    const std::string keyTexture = "Texture";
    const std::string keyOrientation = "Orientation";

    enum Orientation {
        Outside = 1,
        Inside = 2
    };
}

namespace openspace {

RenderableSphere::RenderableSphere(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _texturePath("texture", "Texture")
    , _orientation("orientation", "Orientation")
    , _size("size", "Size", glm::vec2(1.f, 1.f), glm::vec2(0.f), glm::vec2(100.f))
    , _segments("segments", "Segments", 8, 4, 100)
    , _transparency("transparency", "Transparency", 1.f, 0.f, 1.f)
	, _shader(nullptr)
	, _texture(nullptr)
	, _sphere(nullptr)
    , _programIsDirty(false)
    , _sphereIsDirty(false)
{
    std::string path;
    bool success = dictionary.getValue(constants::scenegraph::keyPathModule, path);
    ghoul_assert(success,
            "RenderablePlanet need the '" << constants::scenegraph::keyPathModule << "' be specified");

    if (dictionary.hasKeyAndValue<glm::vec2>(keySize)) {
        glm::vec2 size;
        dictionary.getValue(keySize, size);
        _size = size;
    }

    if (dictionary.hasKeyAndValue<glm::vec2>(keySegments)) {
        int segments;
        dictionary.getValue(keySegments, segments);
        _segments = segments;
    }

    if (dictionary.hasKeyAndValue<std::string>(keyTexture)) {
        std::string texture;
        dictionary.getValue(keyTexture, texture);
        _texturePath = path + '/' + texture;
    }

    _orientation.addOption(Outside, "Outside");
    _orientation.addOption(Inside, "Inside");
    _orientation.addOption(Outside | Inside, "Outside + Inside");

    if (dictionary.hasKeyAndValue<std::string>(keyOrientation)) {
        std::string orientation;
        dictionary.getValue(keyOrientation, orientation);
        if (orientation == "Outside")
            _orientation = Outside;
        else if (orientation == "Inside")
            _orientation = Inside;
        else
            _orientation = Outside | Inside;
    }

    addProperty(_orientation);
    addProperty(_size);
    _size.onChange([this](){ _sphereIsDirty = true; });
    addProperty(_segments);
    _segments.onChange([this](){ _sphereIsDirty = true; });

    addProperty(_transparency);

	addProperty(_texturePath);
	_texturePath.onChange(std::bind(&RenderableSphere::loadTexture, this));
}

RenderableSphere::~RenderableSphere() {
}

bool RenderableSphere::isReady() const {
    return (_sphere != nullptr) && (_shader != nullptr) && (_texture != nullptr);
}

bool RenderableSphere::initialize() {
    _sphere = new PowerScaledSphere(_size.value(), _segments);
    _sphere->initialize();

    // pscstandard
    _shader = ghoul::opengl::ProgramObject::Build("Sphere",
        "${SHADERS}/modules/sphere/sphere_vs.glsl",
        "${SHADERS}/modules/sphere/sphere_fs.glsl");
    if (!_shader)
        return false;
    _shader->setProgramObjectCallback([&](ghoul::opengl::ProgramObject*){ _programIsDirty = true; });

    loadTexture();

	return isReady();
}

bool RenderableSphere::deinitialize() {
    delete _sphere;
	delete _texture;
	return true;
}

void RenderableSphere::render(const RenderData& data) {

	glm::mat4 transform = glm::mat4(1.0);

    transform = glm::rotate(transform, 90.f, glm::vec3(1, 0, 0));


    // Activate shader
	_shader->activate();
    _shader->setIgnoreUniformLocationError(true);

	_shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	_shader->setUniform("ModelTransform", transform);
	setPscUniforms(_shader, &data.camera, data.position);

    _shader->setUniform("alpha", _transparency);

	ghoul::opengl::TextureUnit unit;
	unit.activate();
	_texture->bind();
	_shader->setUniform("texture1", unit);

    _sphere->render();

    _shader->setIgnoreUniformLocationError(false);
	_shader->deactivate();
}

void RenderableSphere::update(const UpdateData& data) {
    if (_programIsDirty) {
        _shader->rebuildFromFile();
        _programIsDirty = false;
    }

    if (_sphereIsDirty) {
        delete _sphere;
        _sphere = new PowerScaledSphere(_size.value(), _segments);
        _sphere->initialize();
        _sphereIsDirty = false;
    }
}

void RenderableSphere::loadTexture() {
	LDEBUG("loadTexture");
	if (_texturePath.value() != "") {
		LDEBUG("loadTexture2");
		ghoul::opengl::Texture* texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_texturePath));
		if (texture) {
			LDEBUG("Loaded texture from '" << absPath(_texturePath) << "'");
			texture->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
			texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);

			if (_texture)
				delete _texture;
			_texture = texture;
		}
	}
}

} // namespace openspace
