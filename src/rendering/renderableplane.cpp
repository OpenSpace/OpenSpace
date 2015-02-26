/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <openspace/rendering/renderableplane.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/constants.h>

#include <ghoul/filesystem/filesystem>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureunit.h>

namespace {
	const std::string _loggerCat = "RenderablePlane";

    ghoul::filesystem::File* _colorTextureFile;

	const std::string keyFieldlines = "Fieldlines";
	const std::string keyFilename = "File";
	const std::string keyHints = "Hints";
	const std::string keyShaders = "Shaders";
	const std::string keyVertexShader = "VertexShader";
	const std::string keyFragmentShader = "FragmentShader";
}

namespace openspace {

RenderablePlane::RenderablePlane(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _texturePath("texture", "Texture")
	, _billboard("billboard", "Billboard", false)
	, _size("size", "Size", glm::vec2(1,1), glm::vec2(0.f), glm::vec2(1.f, 25.f))
	, _origin(Origin::Center)
	, _shader(nullptr)
    , _programIsDirty(false)
	, _texture(nullptr)
    , _textureIsDirty(false)
	, _quad(0)
	, _vertexPositionBuffer(0)
{
    glm::vec2 size;
	dictionary.getValue("Size", size);
    _size = size;

	std::string origin;
	if (dictionary.getValue("Origin", origin)) {
		if (origin == "LowerLeft") {
			_origin = Origin::LowerLeft;
		}
		else if (origin == "LowerRight") {
			_origin = Origin::LowerRight;
		}
		else if (origin == "UpperLeft") {
			_origin = Origin::UpperLeft;
		}
		else if (origin == "UpperRight") {
			_origin = Origin::UpperRight;
		}
		else if (origin == "Center") {
			_origin = Origin::Center;
		}
	}

	// Attempt to get the billboard value
	bool billboard = false;
	if (dictionary.getValue("Billboard", billboard)) {
		_billboard = billboard;
	}

	std::string texturePath = "";
	bool success = dictionary.getValue("Texture", texturePath);
	if (success) {
		_texturePath = findPath(texturePath);
        _colorTextureFile = new ghoul::filesystem::File(_texturePath);
    }

    addProperty(_billboard);
	addProperty(_texturePath);
    _texturePath.onChange(std::bind(&RenderablePlane::loadTexture, this));
    _colorTextureFile->setCallback([&](const ghoul::filesystem::File&) { _textureIsDirty = true; });

    addProperty(_size);
    //_size.onChange(std::bind(&RenderablePlane::createPlane, this));
    _size.onChange([this](){ _planeIsDirty = true; });

	setBoundingSphere(_size.value());
}

RenderablePlane::~RenderablePlane() {
    delete _colorTextureFile;
}

bool RenderablePlane::isReady() const {
	bool ready = true;
	if (!_shader)
		ready &= false;
	if(!_texture)
		ready &= false;
	return ready;
}

bool RenderablePlane::initialize() {
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    createPlane();

    // Plane program
    _shader = ghoul::opengl::ProgramObject::Build("Plane",
        "${SHADERS}/modules/plane/plane_vs.glsl",
        "${SHADERS}/modules/plane/plane_fs.glsl");
    if (!_shader)
        return false;
    _shader->setProgramObjectCallback([&](ghoul::opengl::ProgramObject*){ _programIsDirty = true; });

	loadTexture();

	return isReady();
}

bool RenderablePlane::deinitialize() {
	glDeleteVertexArrays(1, &_quad);
	_quad = 0;
	glDeleteBuffers(1, &_vertexPositionBuffer);
	_vertexPositionBuffer = 0;
	delete _texture;
    delete _shader;
	return true;
}

void RenderablePlane::render(const RenderData& data) {
	glm::mat4 transform = glm::mat4(1.0);
	if (_billboard)
		transform = glm::inverse(data.camera.viewRotationMatrix());

	// Activate shader
	_shader->activate();

	_shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	_shader->setUniform("ModelTransform", transform);
	setPscUniforms(_shader, &data.camera, data.position);

	ghoul::opengl::TextureUnit unit;
	unit.activate();
	_texture->bind();
	_shader->setUniform("texture1", unit);

	glBindVertexArray(_quad);
	glDrawArrays(GL_TRIANGLES, 0, 6);

	_shader->deactivate();
}

void RenderablePlane::update(const UpdateData& data) {
    if (_programIsDirty) {
        _shader->rebuildFromFile();
        _programIsDirty = false;
    }

    if (_planeIsDirty)
        createPlane();

    if (_textureIsDirty) {
        loadTexture();
        _textureIsDirty = false;
    }
}

void RenderablePlane::loadTexture() {
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

            delete _colorTextureFile;
            _colorTextureFile = new ghoul::filesystem::File(_texturePath);
            _colorTextureFile->setCallback([&](const ghoul::filesystem::File&) { _textureIsDirty = true; });
		}
	}
}

void RenderablePlane::createPlane() {
    // ============================
    // 		GEOMETRY (quad)
    // ============================
    const GLfloat size = _size.value()[0];
    const GLfloat w = _size.value()[1];
    const GLfloat vertex_data[] = { // square of two triangles (sigh)
        //	  x      y     z     w     s     t
        -size, -size, 0.0f, w, 0, 1,
        size, size, 0.0f, w, 1, 0,
        -size, size, 0.0f, w, 0, 0,
        -size, -size, 0.0f, w, 0, 1,
        size, -size, 0.0f, w, 1, 1,
        size, size, 0.0f, w, 1, 0,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));
}

} // namespace openspace
