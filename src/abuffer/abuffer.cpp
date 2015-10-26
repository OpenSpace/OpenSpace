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

#include <openspace/abuffer/abuffer.h>
#include <openspace/engine/openspaceengine.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

#include <iostream>
#include <fstream>
#include <string>

namespace {

	const std::string generatedSettingsPath     = "${SHADERS_GENERATED}/ABufferSettings.hglsl";
	const std::string generatedHeadersPath      = "${SHADERS_GENERATED}/ABufferHeaders.hglsl";
	const std::string generatedSamplerCallsPath = "${SHADERS_GENERATED}/ABufferSamplerCalls.hglsl";
	const std::string generatedTransferFunctionVisualizerPath =
                        "${SHADERS_GENERATED}/ABufferTransferFunctionVisualizer.hglsl";
	const std::string generatedSamplersPath     = "${SHADERS_GENERATED}/ABufferSamplers.hglsl";

	const std::string _loggerCat = "ABuffer";

}

namespace openspace {

ABuffer::ABuffer()
	: _validShader(false)
	, _resolveShader(nullptr)
	, _volumeStepFactor(0.f)
{

	updateDimensions();
}

ABuffer::~ABuffer() {
    delete _resolveShader;
	
	for (auto file: _samplerFiles)
		delete file;
}

bool ABuffer::initializeABuffer() {
	// ============================
    // 			SHADERS
    // ============================
	auto shaderCallback = [this](ghoul::opengl::ProgramObject* program) {
		// Error for visibility in log
		_validShader = false;
	};

	generateShaderSource();
	_resolveShader = ghoul::opengl::ProgramObject::Build(
		"ABufferResolve",
		"${SHADERS}/ABuffer/abufferResolveVertex.glsl",
		"${SHADERS}/ABuffer/abufferResolveFragment.glsl");
	if (!_resolveShader)
		return false;
	_resolveShader->setProgramObjectCallback(shaderCallback);
    // Remove explicit callback and use programobject isDirty instead ---abock
    
    // ============================
    // 		GEOMETRY (quad)
    // ============================
	const GLfloat size = 1.0f;
    const GLfloat vertex_data[] = {
        //	  x      y     s     t
        -size, -size, 0.0f, 1.0f,
        size,	size, 0.0f, 1.0f, 
        -size,  size, 0.0f, 1.0f, 
        -size, -size, 0.0f, 1.0f, 
        size, -size, 0.0f, 1.0f, 
        size,	size, 0.0f, 1.0f,
    };
	GLuint vertexPositionBuffer;
	glGenVertexArrays(1, &_screenQuad); // generate array
	glBindVertexArray(_screenQuad); // bind array
	glGenBuffers(1, &vertexPositionBuffer); // generate buffer
	glBindBuffer(GL_ARRAY_BUFFER, vertexPositionBuffer); // bind buffer
	glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat)*4, reinterpret_cast<void*>(0));
	glEnableVertexAttribArray(0);
	return true;
}

bool ABuffer::reinitialize() {
	// set the total resolution for all viewports
	updateDimensions();
	return reinitializeInternal();
}

void ABuffer::resolve(float blackoutFactor) {
	if (!_validShader) {
		generateShaderSource();
		updateShader();
		_validShader = true;
	}

	if (!_resolveShader)
		return;

	_resolveShader->activate();
    _resolveShader->setUniform("blackoutFactor", blackoutFactor);
	int startAt = 0;
	for (int i = 0; i < _volumes.size(); ++i) {
		glActiveTexture(GL_TEXTURE0 + i);
		_volumes.at(i).second->bind();
		startAt = i + 1;
	}
	for (int i = 0; i < _transferFunctions.size(); ++i) {
		glActiveTexture(GL_TEXTURE0 + startAt + i);
		_transferFunctions.at(i).second->bind();
	}

	glBindVertexArray(_screenQuad);
	glDrawArrays(GL_TRIANGLES, 0, 6);

    _resolveShader->deactivate();
}

void ABuffer::addVolume(const std::string& tag,ghoul::opengl::Texture* volume) {
	_volumes.push_back(std::make_pair(tag, volume));
}

void ABuffer::addTransferFunction(const std::string& tag,ghoul::opengl::Texture* transferFunction) {
	_transferFunctions.push_back(std::make_pair(tag, transferFunction));
}

int ABuffer::addSamplerfile(const std::string& filename) {
	if( ! FileSys.fileExists(filename))
		return -1;
    
  	auto fileCallback = [this](const ghoul::filesystem::File& file) {
        _validShader = false;
    };
	ghoul::filesystem::File* file = new ghoul::filesystem::File(filename);
	file->setCallback(fileCallback);
	_samplerFiles.push_back(file);
	_samplers.push_back("");

	// ID is one more than "actual" position since ID=0 is considered geometry
	//return 1 << (_samplers.size()-1);
	return static_cast<int>(_samplers.size());
}

bool ABuffer::updateShader() {
    if (_resolveShader == nullptr)
        return false;
	bool s = _resolveShader->rebuildFromFile();
	if (s) {
		int startAt = 0;
		for (int i = 0; i < _volumes.size(); ++i) {
			_resolveShader->setUniform(_volumes.at(i).first, i);
			startAt = i + 1;
		}
		for (int i = 0; i < _transferFunctions.size(); ++i) {
			_resolveShader->setUniform(_transferFunctions.at(i).first, startAt + i);
		}
		LINFO("Successfully updated ABuffer resolve shader!");
	}
	else {
		LWARNING("Couldn't update ABuffer resolve shader");
	}
	return s;
}

void ABuffer::generateShaderSource() {
	for (int i = 0; i < _samplerFiles.size(); ++i) {
		std::string line, source = "";
		std::ifstream samplerFile(_samplerFiles.at(i)->path());
		if(samplerFile.is_open()) {
			while(std::getline(samplerFile, line)) {
				source += line + "\n";
			}
		}
		samplerFile.close();
		_samplers.at(i) = source;
	}

	LDEBUG("Generating shader includes");
	openspaceHeaders();
	openspaceSamplerCalls();
	openspaceSamplers();
	openspaceTransferFunction();
}

void ABuffer::openspaceHeaders() {
	std::ofstream f(absPath(generatedHeadersPath));
	f << "#define MAX_VOLUMES " << std::to_string(_samplers.size()) << "\n"
		<< "#define MAX_TF " << _transferFunctions.size() << "\n";
	for (int i = 0; i < _volumes.size(); ++i) {
		f << "uniform sampler3D " << _volumes.at(i).first << ";\n";
	}
	for (int i = 0; i < _transferFunctions.size(); ++i) {
		f << "uniform sampler1D " << _transferFunctions.at(i).first << ";\n";
	}

	for (int i = 0; i < _samplers.size(); ++i) {
		auto found = _samplers.at(i).find_first_of('{');
		if (found != std::string::npos) {
			f << _samplers.at(i).substr(0, found) << ";\n";
		}
	}

	if (_volumes.size() < 1) {
		f.close();
		return;
	}

	size_t maxLoop = 0;
	f << "const vec3 volume_dim[] = {\n";
	for (int i = 0; i < _volumes.size(); ++i) {
		glm::size3_t size = _volumes.at(i).second->dimensions();
		for (int k = 0; k < 3; ++k)
			maxLoop = glm::max(maxLoop, size[k]);
		f << "    vec3(" << std::to_string(size[0]) << ".0," + std::to_string(size[1]) << ".0,"
			<< std::to_string(size[2]) + ".0),\n";
	}
	f << "};\n";

	f << "#define LOOP_LIMIT " << maxLoop << "\n";

	f << "float volumeStepSize[] = {\n";
	for (int i = 0; i < _volumes.size(); ++i) {
		glm::size3_t size = _volumes.at(i).second->dimensions();
		f << "    stepSize,\n";
	}
	f << "};\n";

	f << "float volumeStepSizeOriginal[] = {\n";
	for (int i = 0; i < _volumes.size(); ++i) {
		glm::size3_t size = _volumes.at(i).second->dimensions();
		f << "    stepSize,\n";
	}
	f << "};\n";

	f.close();
}

void ABuffer::openspaceSamplerCalls() {
	std::ofstream f(absPath(generatedSamplerCallsPath));
	for (int i = 0; i < _samplers.size(); ++i) {
		auto found1 = _samplers.at(i).find_first_not_of("vec4 ");
		auto found2 = _samplers.at(i).find_first_of("(", found1);
		if (found1 != std::string::npos && found2 != std::string::npos) {
			std::string functionName = _samplers.at(i).substr(found1, found2 - found1);
			f << "#ifndef SKIP_VOLUME_" << i << "\n"
				<< "if((currentVolumeBitmask & (1 << " << i << ")) == " << std::to_string(1 << i) << ") {\n"
				<< "    vec4 c = " << functionName << "(final_color,volume_position[" << i << "]);\n"
				<< "    blendStep(final_color, c, volumeStepSize[" << i << "]);\n"
				<< "    volume_position[" << i << "] += volume_direction[" << i << "]*volumeStepSize[" << i << "];\n"
				<< "}\n"
				<< "#endif\n";
		}
	}
	f.close();
}

void ABuffer::openspaceSamplers() {
	std::ofstream f(absPath(generatedSamplersPath));
	for (const std::string& sampler : _samplers)
		f << sampler << std::endl;
	f.close();
}

void ABuffer::openspaceTransferFunction() {
	std::ofstream f(absPath(generatedTransferFunctionVisualizerPath));
	f	<< "float showfunc_size = 20.0;\n"
		<< "float SCREEN_HEIGHTf = float(SCREEN_HEIGHT);\n"
		<< "float SCREEN_WIDTHf = float(SCREEN_WIDTH);\n";
	for (int i = 0; i < _transferFunctions.size(); ++i) {
		f << "if( gl_FragCoord.y > SCREEN_HEIGHTf-showfunc_size*" << i + 1
			<< " && gl_FragCoord.y < SCREEN_HEIGHTf-showfunc_size*" << std::to_string(i) << ") {\n"
			<< "    float normalizedIntensity = gl_FragCoord.x / (SCREEN_WIDTHf-1) ;\n"
			<< "    vec4 tfc = texture(" << _transferFunctions.at(i).first << ", normalizedIntensity);\n"
			<< "    final_color = tfc;\n"
			<< "    float cmpf = SCREEN_HEIGHTf-showfunc_size*" << i + 1 << " + tfc.a*showfunc_size;\n"
			<< "    if(gl_FragCoord.y > cmpf) {\n"
			<< "        final_color = vec4(0,0,0,0);\n"
			<< "    } else {\n"
			<< "        final_color.a = 1.0;\n"
			<< "    }\n"
			<< "} else if(ceil(gl_FragCoord.y) == SCREEN_HEIGHTf - showfunc_size*" << i + 1 << ") {\n"
			<< "    const float intensity = 0.4;\n"
			<< "    final_color = vec4(intensity,intensity,intensity,1.0);\n"
			<< "}\n";
	}
	f.close();
}

void ABuffer::invalidateABuffer() {
	LDEBUG("Shader invalidated");
	_validShader = false;
}

void ABuffer::updateDimensions() {
    glm::ivec2 res = OsEng.windowWrapper().currentWindowResolution();
    _width = res.x;
    _height = res.y;
    
	_totalPixels = _width * _height;
}


} // openspace