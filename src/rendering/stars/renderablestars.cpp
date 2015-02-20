/*****************************************************************************************
*                                                                                        *
* OpenSpace                                                                              *
*                                                                                        *
* Copyright (c) 2014                                                                     *
*                                                                                        *
* Permission is hereby granted, free of charge, to any person obtaining a copy of this   *
* software and associated documentation files (the "Software"), to deal in the Software  *
* without restriction, including without limitation the rights to use, copy, modify,     *
* merge, publish, distribute, sublicense, and/or sell copies of the Software, and to     *
* permit persons to whom the Software is furnished to do so, subject to the following    *
* conditions:                                                                            *
*                                                                                        *
* The above copyright notice and this permission notice shall be included in all copies  *
* or substantial portions of the Software.                                               *
*                                                                                        *
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,    *
* INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A          *
* PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT     *
* HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF   *
* CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE   *
* OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                          *
*****************************************************************************************/

#include <openspace/rendering/stars/renderablestars.h>

#include <openspace/util/constants.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/filesystem/filesystem>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/io/texture/texturereader.h>
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

	struct SpeedVBOLayout {
		std::array<float, 4> position; // (x,y,z,e)

		float bvColor; // B-V color value
		float luminance;
		float absoluteMagnitude;

		float speed;
	};
}

namespace openspace {

RenderableStars::RenderableStars(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _pointSpreadFunctionTexturePath("psfTexture", "Point Spread Function Texture")
	, _pointSpreadFunctionTexture(nullptr)
	, _pointSpreadFunctionTextureIsDirty(true)
	, _colorTexturePath("colorTexture", "ColorBV Texture")
	, _colorTexture(nullptr)
	, _colorTextureIsDirty(true)
	, _colorOption("colorOption", "Color Option")
	, _dataIsDirty(true)
    , _magnitudeClamp(
        "magnitudeClamp",
        "Magnitude Clamping",
        glm::vec2(1.f, 4.f),
        glm::vec2(-15.f),
        glm::vec2(15.f)
      )
    , _exponentialOffset("exponentialOffset", "Exponential Offset", 5.f, 0.f, 10.f)
    , _exponentialDampening("exponentialDampening", "Exponential Dampening", 0.871f, 0.f, 1.f)
    , _scaleFactor("scaleFactor", "Scale Factor", 1.f, 0.f, 10.f)
	, _program(nullptr)
	, _programIsDirty(false)
	, _speckFile("")
	, _nValuesPerStar(0)
	, _vao(0)
	, _vbo(0)
{
	std::string texturePath = "";
	dictionary.getValue(constants::renderablestars::keyTexture, texturePath);
	_pointSpreadFunctionTexturePath = absPath(texturePath);

	dictionary.getValue(constants::renderablestars::keyColorMap, texturePath);
	_colorTexturePath = absPath(texturePath);

	bool success = dictionary.getValue(constants::renderablestars::keyFile, _speckFile);
	if (!success) {
		LERROR("SpeckDataSource did not contain key '" <<
			constants::renderablestars::keyFile << "'");
		return;
	}
	_speckFile = absPath(_speckFile);

	_colorOption.addOption(ColorOption::Color, "Color");
	_colorOption.addOption(ColorOption::Velocity, "Velocity");
	_colorOption.addOption(ColorOption::Speed, "Speed");
	addProperty(_colorOption);
	_colorOption.onChange([&]{ _dataIsDirty = true;});

	addProperty(_pointSpreadFunctionTexturePath);
	_pointSpreadFunctionTexturePath.onChange([&]{ _pointSpreadFunctionTextureIsDirty = true;});
	 
	addProperty(_colorTexturePath);
	_colorTexturePath.onChange([&]{ _colorTextureIsDirty = true; });

    addProperty(_magnitudeClamp);
    addProperty(_exponentialOffset);
    addProperty(_exponentialDampening);
    addProperty(_scaleFactor);
}

RenderableStars::~RenderableStars() {
}

bool RenderableStars::isReady() const {
	return (_program != nullptr) && (!_fullData.empty());
}

bool RenderableStars::initialize() {
	bool completeSuccess = true;

	_program = ghoul::opengl::ProgramObject::Build("Star",
		"${SHADERS}/modules/stars/star_vs.glsl",
		"${SHADERS}/modules/stars/star_fs.glsl",
		"${SHADERS}/modules/stars/star_ge.glsl");
	if (!_program)
		return false;
	_program->setProgramObjectCallback([&](ghoul::opengl::ProgramObject*){ _programIsDirty = true; });
	completeSuccess &= loadData();
	completeSuccess &= (_pointSpreadFunctionTexture != nullptr);

	return completeSuccess;
}

bool RenderableStars::deinitialize() {
	glDeleteBuffers(1, &_vbo);
	_vbo = 0;
	glDeleteVertexArrays(1, &_vao);
	_vao = 0;

	delete _pointSpreadFunctionTexture;
	_pointSpreadFunctionTexture = nullptr;

	if(_program)
 		delete _program;
	_program = nullptr;
	return true;	
}

void RenderableStars::render(const RenderData& data) {
	_program->activate();

	// @Check overwriting the scaling from the camera; error as parsec->meter conversion
	// is done twice? ---abock
	glm::vec2 scaling = glm::vec2(1, -19);  

	glm::mat4 modelMatrix = glm::mat4(1.0);
	glm::mat4 viewMatrix       = data.camera.viewMatrix();
	glm::mat4 projectionMatrix = data.camera.projectionMatrix();

	_program->setIgnoreUniformLocationError(true);
	//_program->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	//_program->setUniform("ModelTransform", glm::mat4(1.f));
	_program->setUniform("model", modelMatrix);
	_program->setUniform("view", viewMatrix);
	_program->setUniform("projection", projectionMatrix);

	_program->setUniform("colorOption", _colorOption);
    _program->setUniform("magnitudeClamp", _magnitudeClamp);
    _program->setUniform("exponentialOffset", _exponentialOffset);
    _program->setUniform("exponentialDampening", _exponentialDampening);
    _program->setUniform("scaleFactor", _scaleFactor);
	
	setPscUniforms(_program, &data.camera, data.position);
	_program->setUniform("scaling", scaling);

	ghoul::opengl::TextureUnit psfUnit;
	psfUnit.activate();
	if (_pointSpreadFunctionTexture)
		_pointSpreadFunctionTexture->bind();
	_program->setUniform("psfTexture", psfUnit);

	ghoul::opengl::TextureUnit colorUnit;
	colorUnit.activate();
	if (_colorTexture)
		_colorTexture->bind();
	_program->setUniform("colorTexture", colorUnit);

	glBindVertexArray(_vao);
	const GLsizei nStars = static_cast<GLsizei>(_fullData.size() / _nValuesPerStar);
	glDrawArrays(GL_POINTS, 0, nStars);

	glBindVertexArray(0);
	_program->setIgnoreUniformLocationError(false);
	_program->deactivate();
}

void RenderableStars::update(const UpdateData& data) {
	if (_programIsDirty) {
		_program->rebuildFromFile();
		_dataIsDirty = true;
		_programIsDirty = false;
	}
	
	if (_dataIsDirty) {
		const int value = _colorOption;
		LDEBUG("Regenerating data");

		createDataSlice(ColorOption(value));

		int size = static_cast<int>(_slicedData.size());

		if (_vao == 0) {
			glGenVertexArrays(1, &_vao);
			LDEBUG("Generating Vertex Array id '" << _vao << "'");
		}
		if (_vbo == 0) {
			glGenBuffers(1, &_vbo);
			LDEBUG("Generating Vertex Buffer Object id '" << _vbo << "'");
		}
		glBindVertexArray(_vao);
		glBindBuffer(GL_ARRAY_BUFFER, _vbo);
		glBufferData(GL_ARRAY_BUFFER,
			size*sizeof(GLfloat),
			&_slicedData[0],
			GL_STATIC_DRAW);

		GLint positionAttrib = _program->attributeLocation("in_position");
		GLint brightnessDataAttrib = _program->attributeLocation("in_brightness");

		const size_t nStars = _fullData.size() / _nValuesPerStar;
		const size_t nValues = _slicedData.size() / nStars;

		GLsizei stride = static_cast<GLsizei>(sizeof(GLfloat) * nValues);

		glEnableVertexAttribArray(positionAttrib);
		glEnableVertexAttribArray(brightnessDataAttrib);
		const int colorOption = _colorOption;
		switch (colorOption) {
		case ColorOption::Color:
			glVertexAttribPointer(positionAttrib, 4, GL_FLOAT, GL_FALSE, stride,
				reinterpret_cast<void*>(offsetof(ColorVBOLayout, position)));
			glVertexAttribPointer(brightnessDataAttrib, 3, GL_FLOAT, GL_FALSE, stride,
				reinterpret_cast<void*>(offsetof(ColorVBOLayout, bvColor)));
			
			break;
		case ColorOption::Velocity:
			{
				glVertexAttribPointer(positionAttrib, 4, GL_FLOAT, GL_FALSE, stride,
					reinterpret_cast<void*>(offsetof(VelocityVBOLayout, position)));
				glVertexAttribPointer(brightnessDataAttrib, 3, GL_FLOAT, GL_FALSE, stride,
					reinterpret_cast<void*>(offsetof(VelocityVBOLayout, bvColor)));

				GLint velocityAttrib = _program->attributeLocation("in_velocity");
				glEnableVertexAttribArray(velocityAttrib);
				glVertexAttribPointer(velocityAttrib, 3, GL_FLOAT, GL_TRUE, stride,
					reinterpret_cast<void*>(offsetof(VelocityVBOLayout, vx)));

				break;
			}
		case ColorOption::Speed:
			{
				glVertexAttribPointer(positionAttrib, 4, GL_FLOAT, GL_FALSE, stride,
					reinterpret_cast<void*>(offsetof(SpeedVBOLayout, position)));
				glVertexAttribPointer(brightnessDataAttrib, 3, GL_FLOAT, GL_FALSE, stride,
					reinterpret_cast<void*>(offsetof(SpeedVBOLayout, bvColor)));

				GLint speedAttrib = _program->attributeLocation("in_speed");
				glEnableVertexAttribArray(speedAttrib);
				glVertexAttribPointer(speedAttrib, 1, GL_FLOAT, GL_TRUE, stride,
					reinterpret_cast<void*>(offsetof(SpeedVBOLayout, speed)));

			}
		}

		glBindBuffer(GL_ARRAY_BUFFER, 0);
		glBindVertexArray(0);

		_dataIsDirty = false;
	}	

	if (_pointSpreadFunctionTextureIsDirty) {
		LDEBUG("Reloading Point Spread Function texture");
		delete _pointSpreadFunctionTexture;
		_pointSpreadFunctionTexture = nullptr;
		if (_pointSpreadFunctionTexturePath.value() != "") {
			_pointSpreadFunctionTexture = ghoul::io::TextureReader::ref().loadTexture(absPath(_pointSpreadFunctionTexturePath));
			if (_pointSpreadFunctionTexture) {
				LDEBUG("Loaded texture from '" << absPath(_pointSpreadFunctionTexturePath) << "'");
				_pointSpreadFunctionTexture->uploadTexture();
			}
		}
		_pointSpreadFunctionTextureIsDirty = false;
	}

	if (_colorTextureIsDirty) {
		LDEBUG("Reloading Color Texture");
		delete _colorTexture;
		_colorTexture = nullptr;
		if (_colorTexturePath.value() != "") {
			_colorTexture = ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath));
			if (_colorTexture) {
				LDEBUG("Loaded texture from '" << absPath(_colorTexturePath) << "'");
				_colorTexture->uploadTexture();
			}
		}
		_colorTextureIsDirty = false;
	}
}

bool RenderableStars::loadData() {
	std::string _file = _speckFile;
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
	std::string _file = _speckFile;
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
		fileStream.read(reinterpret_cast<char*>(&_fullData[0]),
			nValues * sizeof(_fullData[0]));

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
		fileStream.write(reinterpret_cast<const char*>(&CurrentCacheVersion),
			sizeof(int8_t));

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

    // This is only temporary until the scalegraph is in place ---abock
    float minDistance = std::numeric_limits<float>::max();
    float maxDistance = -std::numeric_limits<float>::max();

    for (size_t i = 0; i < _fullData.size(); i+=_nValuesPerStar) {
        float distLy = _fullData[i + 6];
        //if (distLy < 20.f) {
            minDistance = std::min(minDistance, distLy);
            maxDistance = std::max(maxDistance, distLy);
        //}
    }

	for (size_t i = 0; i < _fullData.size(); i+=_nValuesPerStar) {
        glm::vec3 p = glm::vec3(_fullData[i + 0], _fullData[i + 1], _fullData[i + 2]);

        // This is only temporary until the scalegraph is in place. It places all stars
        // on a sphere with a small variation in the distance to account for blending
        // issues ---abock
        //if (p != glm::vec3(0.f))
        //    p = glm::normalize(p);

        //float distLy = _fullData[i + 6];
        //float normalizedDist = (distLy - minDistance) / (maxDistance - minDistance);
        //float distance = 18.f - normalizedDist / 1.f ;


        //psc position = psc(glm::vec4(p, distance));

        // Convert parsecs -> meter
        psc position = psc(glm::vec4(p * 0.308567756f, 17));

		//position[1] *= parsecsToMetersFactor[0];
		//position[2] *= parsecsToMetersFactor[0];
		//position[3] += parsecsToMetersFactor[1];

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
                //layout.value.absoluteMagnitude = _fullData[i + 6];

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
		case ColorOption::Speed:
			{
				union {
					SpeedVBOLayout value;
					std::array<float, sizeof(SpeedVBOLayout)> data;
				} layout;

				layout.value.position = { {
						position[0], position[1], position[2], position[3]
					} };

				layout.value.bvColor = _fullData[i + 3];
				layout.value.luminance = _fullData[i + 4];
				layout.value.absoluteMagnitude = _fullData[i + 5];

				layout.value.speed = _fullData[i + 15];

				_slicedData.insert(_slicedData.end(),
								   layout.data.begin(),
								   layout.data.end());
				break;
			}
		}
	}
}

} // namespace openspace
