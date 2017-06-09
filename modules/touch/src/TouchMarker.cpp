/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/touch/include/TouchMarker.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#include <ghoul/logging/logmanager.h>

namespace {
	const std::string _loggerCat = "TouchMarker";
}
namespace openspace {

TouchMarker::TouchMarker()
	: properties::PropertyOwner("TouchMarker")
	, _visible("TouchMarkers visible", "Toggle visibility of markers", false)
	, _radiusSize("Marker size", "Set marker size in radius", 0.5, 0, 1)
	, _texturePath("texturePath", "Color Texture")
	, _shader(nullptr)
	, _texture(nullptr)
	, _listIsDirty(false)
	, _textureIsDirty(false)
	, _numFingers(0)
{
	addProperty(_visible);
	addProperty(_radiusSize);
	addProperty(_texturePath);
	_texturePath.onChange(std::bind(&TouchMarker::loadTexture, this));

	
}

bool TouchMarker::isReady() const {
	if (_shader == nullptr) {
		std::cout << "something went wrong\n";
	}
	return (_shader != nullptr); // && _texture;
}

bool TouchMarker::initialize(const std::vector<TUIO::TuioCursor> list) {
	glGenVertexArrays(1, &_quad); // generate array
	glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
	createVertexList(list);

	_shader = OsEng.renderEngine().buildRenderProgram("MarkerProgram",
		"${MODULE_TOUCH}/shaders/marker_vs.glsl",
		"${MODULE_TOUCH}/shaders/marker_fs.glsl"
	);

	//loadTexture();

	return isReady();
}

bool TouchMarker::deinitialize() {
	glDeleteVertexArrays(1, &_quad);
	_quad = 0;

	glDeleteBuffers(1, &_vertexPositionBuffer);
	_vertexPositionBuffer = 0;

	_texture = nullptr;

	RenderEngine& renderEngine = OsEng.renderEngine();
	if (_shader) {
		renderEngine.removeRenderProgram(_shader);
		_shader = nullptr;
	}

	return true;
}

void TouchMarker::render(const std::vector<TUIO::TuioCursor> list) {
	if (_visible) {
		createVertexList(list);
		_shader->activate();

		// Bind texture
		/*ghoul::opengl::TextureUnit unit;
		unit.activate();
		_texture->bind();
		_shader->setUniform("texture1", unit);*/
		_shader->setUniform("radius", _radiusSize);
		
		glEnable(GL_PROGRAM_POINT_SIZE); // Enable gl_PointSize in vertex shader
		glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
		glBindVertexArray(_quad);
		glDrawArrays(GL_POINTS, 0, _numFingers);

		_shader->deactivate();
	}
}

void TouchMarker::update() {
	if (_shader->isDirty())
		_shader->rebuildFromFile();

	if (_listIsDirty)
		//createFingerList();

	if (_textureIsDirty) {
		loadTexture();
		_textureIsDirty = false;
	}
}

void TouchMarker::loadTexture() {
	if (_texturePath.value() != "") {
		std::unique_ptr<ghoul::opengl::Texture> texture =
			ghoul::io::TextureReader::ref().loadTexture(absPath(_texturePath));

		if (texture) {
			LDEBUGC(
				"TouchMarker",
				"Loaded texture from '" << absPath(_texturePath) << "'"
			);
			texture->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
			texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);

			_texture = std::move(texture);
		}
	}
}

void TouchMarker::createVertexList(const std::vector<TUIO::TuioCursor> list) {
	_numFingers = list.size();
	std::vector<GLfloat> vertexData(_numFingers * 2, 0);
	int i = 0;
	for (const TUIO::TuioCursor& c : list) {
		vertexData.at(i) = 2 * (c.getX() - 0.5);
		vertexData.at(i + 1) = -2 * (c.getY() - 0.5);
		i += 2;
	}

	glBindVertexArray(_quad);
	glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(vertexData.data()), vertexData.data(), GL_STATIC_DRAW);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(
		0,
		2,
		GL_FLOAT,
		GL_FALSE,
		0,
		reinterpret_cast<void*>(0)
	);
}

} // openspace namespace