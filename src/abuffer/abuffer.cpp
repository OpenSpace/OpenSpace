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

#include <openspace/abuffer/abuffer.h>
#include <openspace/engine/openspaceengine.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <sgct.h>

#include <iostream>
#include <fstream>
#include <string>

namespace {
	std::string _loggerCat = "ABuffer";

std::string padGeneratedString(const std::string& content) {
	std::string _content_ = "// GENERATED CONTENT\n" + content + "\n// END GENERATED CONTENT";
	return _content_;
}

}

namespace openspace {

ABuffer::ABuffer(): _validShader(true) {
	int x1, xSize, y1, ySize;
    sgct::Engine::instance()->getActiveWindowPtr()->getCurrentViewportPixelCoords(x1, y1, xSize, ySize);
    _width = xSize;
    _height = ySize;
    _totalPixels = _width * _height;
	const std::string fragmentShaderSourcePath = absPath("${SHADERS}/ABuffer/abufferResolveFragment.glsl");
	_fragmentShaderFile = new ghoul::filesystem::File(fragmentShaderSourcePath, true);
	_fragmentShaderPath = fragmentShaderSourcePath.substr(0, fragmentShaderSourcePath.length()-4) + "gglsl";
}

ABuffer::~ABuffer() {
	if(_fragmentShaderFile)
		delete _fragmentShaderFile;

	if(_resolveShader)
		delete _resolveShader;
}

bool ABuffer::initializeABuffer() {
	// ============================
    // 			SHADERS
    // ============================
  	auto shaderCallback = [this](const ghoul::filesystem::File& file) {
        _validShader = false;
    };
    _fragmentShaderFile->setCallback(shaderCallback);

    _resolveShader = nullptr;
    generateShaderSource();
    updateShader();

    // ============================
    // 		GEOMETRY (quad)
    // ============================
	const GLfloat size = 1.0f;
    const GLfloat vertex_data[] = { // square of two triangles (sigh)
        //	  x      y     z     w     s     t
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

void ABuffer::resolve() {
	if( ! _validShader) {
		_validShader = true;
		generateShaderSource();
		updateShader();

	}

	if(_resolveShader) {
		_resolveShader->activate();
	    int startAt = 0;
		for(int i = 0; i < _volumes.size(); ++i) {
			glActiveTexture(GL_TEXTURE0 + i);
			_volumes.at(i).second->bind();
			startAt = i + 1;
		}
		for(int i = 0; i < _transferFunctions.size(); ++i) {
			glActiveTexture(GL_TEXTURE0 + startAt + i);
			_transferFunctions.at(i).second->bind();
		}

	    glBindVertexArray(_screenQuad);
	    glDrawArrays(GL_TRIANGLES, 0, 6);

		_resolveShader->deactivate();
	}
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
	return _samplers.size();
}

bool ABuffer::updateShader() {

	using ghoul::opengl::ShaderObject;
    using ghoul::opengl::ProgramObject;
    ShaderObject* vs
          = new ShaderObject(ShaderObject::ShaderType::ShaderTypeVertex,
                             absPath("${SHADERS}/ABuffer/abufferResolveVertex.glsl"), "Vertex");
    ShaderObject* fs
          = new ShaderObject(ShaderObject::ShaderType::ShaderTypeFragment,_fragmentShaderPath, "Fragment");

    ghoul::opengl::ProgramObject* resolveShader = new ProgramObject;
    resolveShader->attachObject(vs);
    resolveShader->attachObject(fs);

    if (!resolveShader->compileShaderObjects()) {
    	LERROR("Could not compile shader");
        return false;
    }
    if (!resolveShader->linkProgramObject()){
    	LERROR("Could not link shader");
        return false;
    }

    int startAt = 0;
	for(int i = 0; i < _volumes.size(); ++i) {
		resolveShader->setUniform(_volumes.at(i).first, i);
		startAt = i + 1;
	}
	for(int i = 0; i < _transferFunctions.size(); ++i) {
		resolveShader->setUniform(_transferFunctions.at(i).first, startAt + i);
	}

	resolveShader->setUniform("SCREEN_WIDTH", static_cast<int>(_width));
	resolveShader->setUniform("SCREEN_HEIGHT", static_cast<int>(_height));

	if(_resolveShader)
		delete _resolveShader;

	_resolveShader = resolveShader;
	LDEBUG("Successfully updated shader!");
	return true;
}

void ABuffer::generateShaderSource() {

	for(int i = 0; i < _samplerFiles.size(); ++i) {
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

	std::string line, source = "";
	std::ifstream fragmentShaderFile(_fragmentShaderFile->path());
	if(fragmentShaderFile.is_open()) {
		while(std::getline(fragmentShaderFile, line)) {
			if(line == "#pragma openspace insert HEADERS") {
				line = padGeneratedString(openspaceHeaders());
			} else if(line == "#pragma openspace insert SAMPLERCALLS") {
				line = padGeneratedString(openspaceSamplerCalls());
			} else if(line == "#pragma openspace insert SAMPLERS") {
				line = padGeneratedString(openspaceSamplers());
			} else if(line == "#pragma openspace insert SETTINGS") {
				line = padGeneratedString(settings());
			}
			source += line + "\n";
		}
	}
	fragmentShaderFile.close();

	std::ofstream fragmentShaderOut(_fragmentShaderPath);
	fragmentShaderOut << source;
	fragmentShaderOut.close();
}

std::string ABuffer::openspaceHeaders() {

	std::string headers;
	headers += "#define MAX_VOLUMES " + std::to_string(_samplers.size()) + "\n";
	for (int i = 0; i < _volumes.size(); ++i) {
		headers += "uniform sampler3D " + _volumes.at(i).first + ";\n";
	}
	for (int i = 0; i < _transferFunctions.size(); ++i) {
		headers += "uniform sampler1D " + _transferFunctions.at(i).first + ";\n";
	}

	for (int i = 0; i < _samplers.size(); ++i) {
		auto found = _samplers.at(i).find_first_of('{');
		if(found!=std::string::npos) {
			headers += _samplers.at(i).substr(0, found) + ";\n";
		}
	}

	headers += "const vec3 volume_dim[] = {\n";
	for (int i = 0; i < _volumes.size(); ++i) {
		glm::size3_t size = _volumes.at(i).second->dimensions();
		headers += "    vec3(" + std::to_string(size[0]) + ".0," + std::to_string(size[1]) + ".0," 
			    + std::to_string(size[2]) + ".0),\n";
	}
	headers += "};\n";

	headers += "float volumeStepSize[] = {\n";
	for (int i = 0; i < _volumes.size(); ++i) {
		glm::size3_t size = _volumes.at(i).second->dimensions();
		headers += "    stepSize,\n";
	}
	headers += "};\n";

	return headers;
}

std::string ABuffer::openspaceSamplerCalls() {
	std::string samplercalls;
	for (int i = 0; i < _samplers.size(); ++i) {

		auto found1 = _samplers.at(i).find_first_not_of("void ");
		auto found2 = _samplers.at(i).find_first_of("(",found1);
		if(found1 != std::string::npos && found2 != std::string::npos) {
			std::string functionName = _samplers.at(i).substr(found1, found2 - found1);
			samplercalls += "if((currentVolumeBitmask & (1 << " + std::to_string(i) + ")) == 1) {\n";
			samplercalls += functionName + "(final_color,volume_position[" + std::to_string(i) + "]);\n";
			samplercalls += "volume_position[" + std::to_string(i) + "] += volume_direction[" + std::to_string(i) + "]*volumeStepSize[" + std::to_string(i) + "];;\n";
			samplercalls += "}\n";
		}

		
	}
	return samplercalls;
}

std::string ABuffer::openspaceSamplers() {
	std::string samplers;
	for (int i = 0; i < _samplers.size(); ++i) {
		samplers += _samplers.at(i) + "\n";
	}
	return samplers;
}


} // openspace