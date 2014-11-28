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

#include <openspace/rendering/stars/renderablestars.h>

#include <openspace/util/constants.h>

#include <ghoul/filesystem/filesystem>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/opengl/texturereader.h>
#include <ghoul/opengl/textureunit.h>

#include <array>
#include <fstream>
#include <stdint.h>

namespace {
	const std::string _loggerCat = "RenderableStars";

	const int8_t CurrentCacheVersion = 1;

	struct ColorVBOLayout {
		std::array<float, 4> position; // (x,y,z,e)

		float bvColor; // B-V color value
		float luminance;
		float absoluteMagnitude;
	};

	struct VelocityVBOLayout {
		std::array<float, 4> position; // (x,y,z,e)

		float bvColor; // B-V color value
		float luminance;
		float absoluteMagnitude;

		float vx; // v_x
		float vy; // v_y
		float vz; // v_z
	};
}

namespace openspace {

RenderableStars::RenderableStars(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _colorTexturePath("colorTexture", "Color Texture")
	, _colorOption("colorOption", "Color Option")
	, _haloProgram(nullptr)
	, _programIsDirty(false)
	, _texture(nullptr)
	, _dataIsDirty(true)
	, _nValuesPerStar(0)
	, _vaoID(0)
	, _vboID(0)
{
	std::string texturePath = "";
	if (dictionary.hasKey(constants::renderablestars::keyTexture)) {
		dictionary.getValue(constants::renderablestars::keyTexture, texturePath);
		_colorTexturePath = absPath(texturePath);
	}

	bool success = dictionary.getValue(constants::renderablestars::keyFile, _speckPath);
	if (!success) {
		LERROR("SpeckDataSource did not contain key '" <<
			constants::renderablestars::keyFile << "'");
		return;
	}
	_speckPath = absPath(_speckPath);

	_colorOption.addOption({ColorOption::Color, "Color"});
	_colorOption.addOption({ColorOption::Velocity, "Velocity"});
	addProperty(_colorOption);
	_colorOption.onChange([&]{ _dataIsDirty = true;});

	addProperty(_colorTexturePath);
	_colorTexturePath.onChange([&]{ _textureIsDirty = true;});
}

RenderableStars::~RenderableStars() {
}

bool RenderableStars::isReady() const {
	return (_haloProgram != nullptr) && (_fullData.size() > 0);
}

bool RenderableStars::initialize() {
	bool completeSuccess = true;


	// Star program
	_haloProgram = ghoul::opengl::ProgramObject::Build("Star",
		"${SHADERS}/modules/stars/star_vs.glsl",
		"${SHADERS}/modules/stars/star_fs.glsl",
		"${SHADERS}/modules/stars/star_ge.glsl",
		[&](ghoul::opengl::ProgramObject*){ _programIsDirty = true;});

	completeSuccess = (_haloProgram != nullptr);
	completeSuccess &= loadData();
	completeSuccess &= (_texture != nullptr);

	return completeSuccess;
}

bool RenderableStars::deinitialize() {
	glDeleteBuffers(1, &_vboID);
	_vboID = 0;
	glDeleteVertexArrays(1, &_vaoID);
	_vaoID = 0;

	delete _texture;
	_texture = nullptr;
	return true;	
}

void RenderableStars::render(const RenderData& data) {
	if(!_haloProgram)
		return;
	if(!_texture)
		return;
	assert(_haloProgram);
	_haloProgram->activate();

	glm::vec2 scaling = glm::vec2(1, -19);  

	glDisable(GL_DEPTH_TEST);
	glEnable(GL_BLEND);
	glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ONE);

	glm::mat4 modelMatrix      = data.camera.modelMatrix();
	glm::mat4 viewMatrix       = data.camera.viewMatrix();
	glm::mat4 projectionMatrix = data.camera.projectionMatrix();

	_haloProgram->setUniform("model", modelMatrix);
	_haloProgram->setUniform("view", viewMatrix);
	_haloProgram->setUniform("projection", projectionMatrix);

	_haloProgram->setUniform("colorOption", _colorOption.value());
	
	//_haloProgram->setUniform("ViewProjection", camera->viewProjectionMatrix());
	_haloProgram->setUniform("ModelTransform", glm::mat4(1));
	setPscUniforms(_haloProgram, &data.camera, data.position);
	_haloProgram->setUniform("scaling", scaling);

	ghoul::opengl::TextureUnit unit;
	unit.activate();
	_texture->bind();
	_haloProgram->setIgnoreUniformLocationError(true);
	_haloProgram->setUniform("texture1", unit);
	_haloProgram->setIgnoreUniformLocationError(false);

	glBindVertexArray(_vaoID);
	const GLsizei nStars = static_cast<GLsizei>(_fullData.size() / _nValuesPerStar);
	glDrawArrays(GL_POINTS, 0, nStars);  
	glBindVertexArray(0);
	_haloProgram->deactivate();

	glDisable(GL_BLEND);
}

void RenderableStars::update(const UpdateData& data) {
	if (_programIsDirty) {
		_haloProgram->rebuildFromFile();
		_dataIsDirty = true;

		_programIsDirty = false;

	}
	
	if (_dataIsDirty) {
		const int value = _colorOption;
		LDEBUG("Regenerating data");

		createDataSlice(ColorOption(value));

		int size = static_cast<int>(_slicedData.size());

		if (_vaoID == 0)
			glGenVertexArrays(1, &_vaoID);
		if (_vboID == 0)
			glGenBuffers(1, &_vboID);
		glBindVertexArray(_vaoID);
		glBindBuffer(GL_ARRAY_BUFFER, _vboID);
		glBufferData(GL_ARRAY_BUFFER, size*sizeof(GLfloat), &_slicedData[0], GL_STATIC_DRAW);

		GLint positionAttrib = _haloProgram->attributeLocation("in_position");
		GLint brightnessDataAttrib = _haloProgram->attributeLocation("in_brightness");

		const size_t nStars = _fullData.size() / _nValuesPerStar;
		const size_t nValues = _slicedData.size() / nStars;

		GLsizei stride = static_cast<GLsizei>(sizeof(GLfloat) * nValues);

		glEnableVertexAttribArray(positionAttrib);
		glEnableVertexAttribArray(brightnessDataAttrib);
		const int colorOption = _colorOption;
		switch (colorOption) {
		case ColorOption::Color:
			glVertexAttribPointer(positionAttrib, 4, GL_FLOAT, GL_FALSE, stride, reinterpret_cast<void*>(offsetof(ColorVBOLayout, position)));
			glVertexAttribPointer(brightnessDataAttrib, 3, GL_FLOAT, GL_FALSE, stride, reinterpret_cast<void*>(offsetof(ColorVBOLayout, bvColor)));
			
			break;
		case ColorOption::Velocity:
			{
			glVertexAttribPointer(positionAttrib, 4, GL_FLOAT, GL_FALSE, stride, reinterpret_cast<void*>(offsetof(VelocityVBOLayout, position)));
			glVertexAttribPointer(brightnessDataAttrib, 3, GL_FLOAT, GL_FALSE, stride, reinterpret_cast<void*>(offsetof(VelocityVBOLayout, bvColor)));

			GLint velocityAttrib = _haloProgram->attributeLocation("in_velocity");
			glEnableVertexAttribArray(velocityAttrib);
			glVertexAttribPointer(velocityAttrib, 3, GL_FLOAT, GL_TRUE, stride, reinterpret_cast<void*>(offsetof(VelocityVBOLayout, vx)));
			
			break;
			}
		}

		glBindBuffer(GL_ARRAY_BUFFER, 0);
		glBindVertexArray(0);

		_dataIsDirty = false;
	}	

	if (_textureIsDirty) {
		LDEBUG("Reloading texture due to changed texture");
		delete _texture;
		_texture = nullptr;
		if (_colorTexturePath.value() != "") {
			_texture = ghoul::opengl::loadTexture(absPath(_colorTexturePath));
			if (_texture) {
				LDEBUG("Loaded texture from '" << absPath(_colorTexturePath) << "'");
				_texture->uploadTexture();
			}
		}
		_textureIsDirty = false;
	}
}

bool RenderableStars::loadData() {
	std::string _file = _speckPath;
	std::string cachedFile = "";
	FileSys.cacheManager()->getCachedFile(_file, cachedFile, true);

	bool hasCachedFile = FileSys.fileExists(cachedFile);
	if (hasCachedFile) {
		LINFO("Cached file '" << cachedFile << "' used for Speck file '" << _file << "'");

		bool success = loadCachedFile(cachedFile);
		if (success)
			return true;
		else
			FileSys.cacheManager()->removeCacheFile(_file);
			// Intentional fall-through to the 'else' computation to generate the cache
			// file for the next run
	}
	else {
		LINFO("Cache for Speck file '" << _file << "' not found");
	}
	LINFO("Loading Speck file '" << _file << "'");

	bool success = readSpeckFile();
	if (!success)
		return false;

	LINFO("Saving cache");
	success = saveCachedFile(cachedFile);

	return success;
}

bool RenderableStars::readSpeckFile() {
	std::string _file = _speckPath;
	std::ifstream file(_file);
	if (!file.good()) {
		LERROR("Failed to open Speck file '" << _file << "'");
		return false;
	}

	_nValuesPerStar = 0;

	// The beginning of the speck file has a header that either contains comments
	// (signaled by a preceding '#') or information about the structure of the file
	// (signaled by the keywords 'datavar', 'texturevar', and 'texture')
	std::string line = "";
	while (true) {
		std::ifstream::streampos position = file.tellg();
		std::getline(file, line);

		if (line[0] == '#')
			continue;

		if (line.substr(0, 7) != "datavar" &&
			line.substr(0, 10) != "texturevar" &&
			line.substr(0, 7) != "texture")
		{
			// we read a line that doesn't belong to the header, so we have to jump back
			// before the beginning of the current line
			file.seekg(position);
			break;
		}

		if (line.substr(0, 7) == "datavar") {
			// datavar lines are structured as follows:
			// datavar # description
			// where # is the index of the data variable; so if we repeatedly overwrite
			// the 'nValues' variable with the latest index, we will end up with the total
			// number of values (+3 since X Y Z are not counted in the Speck file index)
			std::stringstream str(line);

			std::string dummy;
			str >> dummy;
			str >> _nValuesPerStar;
			_nValuesPerStar += 1; // We want the number, but the index is 0 based
		}
	}

	_nValuesPerStar += 3; // X Y Z are not counted in the Speck file indices

	do {
		std::vector<float> values(_nValuesPerStar);

		std::getline(file, line);
		std::stringstream str(line);

		for (int i = 0; i < _nValuesPerStar; ++i)
			str >> values[i];

		_fullData.insert(_fullData.end(), values.begin(), values.end());
	} while (!file.eof());

	return true;
}

bool RenderableStars::loadCachedFile(const std::string& file) {
	std::ifstream fileStream(file, std::ifstream::binary);
	if (fileStream.good()) {
		int8_t version = 0;
		fileStream.read(reinterpret_cast<char*>(&version), sizeof(int8_t));
		if (version != CurrentCacheVersion) {
			LINFO("The format of the cached file has changed, deleted old cache");
			fileStream.close();
			FileSys.deleteFile(file);
			return false;
		}

		int32_t nValues = 0;
		fileStream.read(reinterpret_cast<char*>(&nValues), sizeof(int32_t));
		fileStream.read(reinterpret_cast<char*>(&_nValuesPerStar), sizeof(int32_t));

		_fullData.resize(nValues);
		fileStream.read(reinterpret_cast<char*>(&_fullData[0]), nValues * sizeof(_fullData[0]));

		bool success = fileStream.good();
		return success;
	}
	else {
		LERROR("Error opening file '" << file << "' for loading cache file");
		return false;
	}
}

bool RenderableStars::saveCachedFile(const std::string& file) const {
	std::ofstream fileStream(file, std::ofstream::binary);
	if (fileStream.good()) {
		fileStream.write(reinterpret_cast<const char*>(&CurrentCacheVersion), sizeof(int8_t));

		int32_t nValues = static_cast<int32_t>(_fullData.size());
		if (nValues == 0) {
			LERROR("Error writing cache: No values were loaded");
			return false;
		}
		fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));

		int32_t nValuesPerStar = static_cast<int32_t>(_nValuesPerStar);
		fileStream.write(reinterpret_cast<const char*>(&nValuesPerStar), sizeof(int32_t));

		size_t nBytes = nValues * sizeof(_fullData[0]);
		fileStream.write(reinterpret_cast<const char*>(&_fullData[0]), nBytes);

		bool success = fileStream.good();
		return success;
	}
	else {
		LERROR("Error opening file '" << file << "' for save cache file");
		return false;
	}
}

void RenderableStars::createDataSlice(ColorOption option) {
	_slicedData.clear();
	for (size_t i = 0; i < _fullData.size(); i+=_nValuesPerStar) {
		psc position = PowerScaledCoordinate::CreatePowerScaledCoordinate(
							_fullData[i + 0],
							_fullData[i + 1],
							_fullData[i + 2]
						);
		// Convert parsecs -> meter
		PowerScaledScalar parsecsToMetersFactor = PowerScaledScalar(0.308567758f, 17.f);
		position[0] *= parsecsToMetersFactor[0];
		position[1] *= parsecsToMetersFactor[0];
		position[2] *= parsecsToMetersFactor[0];
		position[3] += parsecsToMetersFactor[1];

		switch (option) {
		case ColorOption::Color:
			{
				union {
					ColorVBOLayout value;
					std::array<float, sizeof(ColorVBOLayout)> data;
				} layout;

				layout.value.position = { {
					position[0], position[1], position[2], position[3]
				} };
					
				layout.value.bvColor = _fullData[i + 3];
				layout.value.luminance = _fullData[i + 4];
				layout.value.absoluteMagnitude = _fullData[i + 5];

				_slicedData.insert(_slicedData.end(),
								   layout.data.begin(),
								   layout.data.end());

				break;
			}
		case ColorOption::Velocity:
			{
				union {
					VelocityVBOLayout value;
					std::array<float, sizeof(VelocityVBOLayout)> data;
				} layout;

				layout.value.position = { {
						position[0], position[1], position[2], position[3]
					} };

				layout.value.bvColor = _fullData[i + 3];
				layout.value.luminance = _fullData[i + 4];
				layout.value.absoluteMagnitude = _fullData[i + 5];

				layout.value.vx = _fullData[i + 12];
				layout.value.vy = _fullData[i + 13];
				layout.value.vz = _fullData[i + 14];

				_slicedData.insert(_slicedData.end(),
								   layout.data.begin(),
								   layout.data.end());
				break;
			}
		}
	}
}

} // namespace openspace
