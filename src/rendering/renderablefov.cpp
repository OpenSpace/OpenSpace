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
#include <openspace/rendering/renderablefov.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/constants.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/util/spicemanager.h>
#include <iomanip>
#include <utility>

namespace {
	const std::string _loggerCat = "RenderableFov";
	//constants
	const std::string keyBody                = "Body";
	const std::string keyObserver            = "Observer";
	const std::string keyFrame               = "Frame";
	const std::string keyPathModule          = "ModulePath";
	const std::string keyColor               = "RGB";
}

//#define DEBUG
namespace openspace{
RenderableFov::RenderableFov(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _colorTexturePath("colorTexture", "Color Texture")
	, _programObject(nullptr)
	, _texture(nullptr)
	, _vaoID(0)
	, _vBufferID(0)
	, _iBufferID(0)
{
	// @TODO Uncomment when used again, do not depend on assert in constructor --jonasstrandstedt
	//dictionary.getValue(keyBody, _target);
	//dictionary.getValue(keyObserver, _observer);
	//dictionary.getValue(keyFrame, _frame);

	if (!dictionary.getValue(keyColor, _c)){
		_c = glm::vec3(0.0);
	}else{
		_r = 1 / _c[0];
		_g = 1 / _c[1];
		_b = 1 / _c[2];
	}
}
void RenderableFov::fullYearSweep(){
	
	int points = 8;
	_stride = 8;
	_isize = points;
	_iarray.clear();

	for (int i = 0; i < points; i++){
		for (int j = 0; j < 4; j++){
			_varray.push_back(0); // pos
		}
		for (int j = 0; j < 4; j++){
			_varray.push_back(0); // col
		}
		_iarray.push_back(i);
	}

	_stride = 8;
	_vsize = static_cast<unsigned int>(_varray.size());
	_vtotal = static_cast<int>(_vsize / _stride);
}

RenderableFov::~RenderableFov(){
}

bool RenderableFov::isReady() const {
	bool ready = true;
	ready &= (_programObject != nullptr);

	return ready;
}

void RenderableFov::sendToGPU(){
	// Initialize and upload to graphics card
	glGenVertexArrays(1, &_vaoID);
	glGenBuffers(1, &_vBufferID);
	glGenBuffers(1, &_iBufferID);

	glBindVertexArray(_vaoID);
	glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
	glBufferData(GL_ARRAY_BUFFER, _vsize * sizeof(GLfloat), NULL, GL_STREAM_DRAW); // orphaning the buffer, sending NULL data.
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize * sizeof(GLfloat), &_varray[0]);

	GLsizei st = sizeof(GLfloat) * _stride;

	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, st, (void*)0);
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, st, (void*)(4 * sizeof(GLfloat)));

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iBufferID);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, _isize * sizeof(int), _iarray.data(), GL_STATIC_DRAW);

	glBindVertexArray(0);
}


bool RenderableFov::initialize(){
	bool completeSuccess = true;
	if (_programObject == nullptr)
		completeSuccess &= OsEng.ref().configurationManager().getValue("EphemerisProgram", _programObject);
	
	 SpiceManager::ref().getETfromDate("2007 feb 26 20:00:00", _startTrail);

	 fullYearSweep();
	 sendToGPU();

	return completeSuccess;
}

bool RenderableFov::deinitialize(){
	if (_texture)
		delete _texture;
	_texture = nullptr;

	glDeleteVertexArrays(1, &_vaoID);
	glDeleteBuffers(1, &_vBufferID);
	glDeleteBuffers(1, &_iBufferID);

	return true;
}

void RenderableFov::updateData(){
	glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
	glBufferSubData(GL_ARRAY_BUFFER, 0, _vsize * sizeof(GLfloat), &_varray[0]);
}

void RenderableFov::render(const RenderData& data){
	_programObject->activate();

	// fetch data
	glm::mat4 tmat = glm::mat4(1);

	glm::mat4 transform(1);

	glm::mat4 tmp = glm::mat4(1);
	glm::mat4 rot = glm::rotate(transform, 90.f, glm::vec3(0, 1, 0));

	for (int i = 0; i < 3; i++){
		for (int j = 0; j < 3; j++){
			tmp[i][j] = static_cast<float>(_stateMatrix[i][j]);
		}
	}
	transform = tmp*rot;

	// setup the data to the shader
	_programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	_programObject->setUniform("ModelTransform", transform);
	setPscUniforms(_programObject, &data.camera, data.position);


    //boresight vector
	std::string shape, name;
	shape.resize(32);
	name.resize(32);
	std::vector<glm::dvec3> bounds;
	glm::dvec3 boresight;

	bool found = openspace::SpiceManager::ref().getFieldOfView("NH_LORRI", shape, name, boresight, bounds);

	size_t size = 4 * sizeof(float);
	float *begin = &_varray[0];

	glm::vec4 origin(0);
	glm::vec4 col_start(1.00, 0.89, 0.00, 1);
	glm::vec4 col_end(1.00, 0.29, 0.00, 1);
	glm::vec4 bsight_t(boresight[0], boresight[1], boresight[2], data.position[3]-3);

	float sc = 2.2f;
	glm::vec4 corner1(bounds[0][0], bounds[0][1], bounds[0][2], data.position[3]-sc);
	memcpy(begin, glm::value_ptr(origin), size);
	memcpy(begin + 4, glm::value_ptr(col_start), size);
	memcpy(begin + 8, glm::value_ptr(corner1), size);
	memcpy(begin + 12, glm::value_ptr(col_end), size);

	glm::vec4 corner2(bounds[1][0], bounds[1][1], bounds[1][2], data.position[3]-sc);
	memcpy(begin + 16, glm::value_ptr(origin), size);
	memcpy(begin + 20, glm::value_ptr(col_start), size);
	memcpy(begin + 24, glm::value_ptr(corner2), size);
	memcpy(begin + 28, glm::value_ptr(col_end), size);

	glm::vec4 corner3(bounds[2][0], bounds[2][1], bounds[2][2], data.position[3]-sc);
	memcpy(begin + 32, glm::value_ptr(origin), size);
	memcpy(begin + 36, glm::value_ptr(col_start), size);
	memcpy(begin + 40, glm::value_ptr(corner3), size);
	memcpy(begin + 44, glm::value_ptr(col_end), size);

	glm::vec4 corner4(bounds[3][0], bounds[3][1], bounds[3][2], data.position[3]-sc);
	memcpy(begin + 48, glm::value_ptr(origin), size);
	memcpy(begin + 52, glm::value_ptr(col_start), size);
	memcpy(begin + 56, glm::value_ptr(corner4), size);
	memcpy(begin + 60, glm::value_ptr(col_end), size);

	updateData();

	glBindVertexArray(_vaoID); 
	glDrawArrays(GL_LINE_STRIP, 0, _vtotal);
	glBindVertexArray(0);

	_programObject->deactivate();
}

void RenderableFov::update(const UpdateData& data){

	_time  = data.time;
	_delta = static_cast<int>(data.delta);

	openspace::SpiceManager::ref().getPositionTransformMatrix("NH_SPACECRAFT", "GALACTIC", data.time, _stateMatrix);
}

void RenderableFov::loadTexture()
{
	delete _texture;
	_texture = nullptr;
	if (_colorTexturePath.value() != "") {
		_texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath));
		if (_texture) {
			LDEBUG("Loaded texture from '" << absPath(_colorTexturePath) << "'");
			_texture->uploadTexture();
		}
	}
}

}