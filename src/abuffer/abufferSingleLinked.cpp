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

#include <openspace/abuffer/abufferSingleLinked.h>
#include <openspace/engine/openspaceengine.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <sgct.h>

#include <iostream>
#include <fstream>

#define MAX_LAYERS 10
#define BUFFER_OFFSET(i) ((char *)NULL + (i))

namespace {
	std::string _loggerCat = "ABufferSingleLinked";

const std::string openspaceHeaders = R"(
#define MAX_VOLUMES 1
// VOLUME1
uniform sampler3D volume;
uniform sampler1D transferFunction;
void sampleVolume1(inout vec4 finalColor, vec3 position);


const vec3 volume_dim[] = {
	vec3(300.0,100.0,100.0)
};
)";

const std::string openspaceSamplerCalls = R"(
if((currentVolumeBitmask & (1 << 0)) == 1)
    sampleVolume1(final_color,volume_position[volID]);
)";

const std::string openspaceSamplers = R"(
void sampleVolume1(inout vec4 finalColor, vec3 position) {
	float intensity = texture(volume, position).x;
	//intensity *=25.0;
	//intensity = clamp(intensity*100.0, 0.0, 1.0);
	vec4 color = texture(transferFunction, intensity);
	blendStep(finalColor, color, stepSize);
}
)";

}

namespace openspace {

ABufferSingleLinked::ABufferSingleLinked() {
	int x1, xSize, y1, ySize;
    sgct::Engine::instance()->getActiveWindowPtr()->getCurrentViewportPixelCoords(x1, y1, xSize, ySize);
    width = xSize;
    height = ySize;
    totalPixels = width * height;
	maxFragments = totalPixels * MAX_LAYERS;
	data = 0;
	anchorPointerTexture = 0;
	anchorPointerTextureInitializer = 0;
	atomicCounterBuffer = 0;
	fragmentBuffer = 0;
	fragmentTexture = 0;

	const std::string fragmentShaderSourcePath = absPath("${SHADERS}/ABuffer/abufferResolveFragment.glsl");
	_fragmentShaderFile = new ghoul::filesystem::File(fragmentShaderSourcePath, true);
	_updateShader = false;
	_fragmentShaderPath = fragmentShaderSourcePath + ".ABuffer.gglsl";

	generateShaderSource();
	

}

ABufferSingleLinked::~ABufferSingleLinked() {
	if(data != 0)
		delete data;

	if(_fragmentShaderFile)
		delete _fragmentShaderFile;

	if(_resolveShader)
		delete _resolveShader;

	glDeleteTextures(1,&anchorPointerTexture);
	glDeleteTextures(1,&fragmentTexture);
	glDeleteBuffers(1,&anchorPointerTextureInitializer);
	glDeleteBuffers(1,&atomicCounterBuffer);
	glDeleteBuffers(1,&anchorPointerTextureInitializer);
}

bool ABufferSingleLinked::initialize() {
	// ============================
	//          BUFFERS
	// ============================
	glGenTextures(1, &anchorPointerTexture);
	glBindTexture(GL_TEXTURE_2D, anchorPointerTexture);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_R32UI, width, height, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, NULL);

	glGenBuffers(1, &anchorPointerTextureInitializer);
	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, anchorPointerTextureInitializer);
	glBufferData(GL_PIXEL_UNPACK_BUFFER, totalPixels * sizeof(GLuint), NULL, GL_STATIC_DRAW);

	data = (GLuint*)glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY);
	memset(data, 0x00, totalPixels * sizeof(GLuint));
	glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

	glGenBuffers(1, &atomicCounterBuffer);
	glBindBuffer(GL_ATOMIC_COUNTER_BUFFER, atomicCounterBuffer);
	glBufferData(GL_ATOMIC_COUNTER_BUFFER, sizeof(GLuint), NULL, GL_DYNAMIC_COPY);

	glGenBuffers(1, &fragmentBuffer);
	glBindBuffer(GL_TEXTURE_BUFFER, fragmentBuffer);
	glBufferData(GL_TEXTURE_BUFFER, MAX_LAYERS*totalPixels*sizeof(GLfloat)*4, NULL, GL_DYNAMIC_COPY);

    glGenTextures(1, &fragmentTexture);
    glBindTexture(GL_TEXTURE_BUFFER, fragmentTexture);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_RGBA32UI, fragmentBuffer);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glBindImageTexture(1, fragmentTexture, 0, GL_FALSE, 0, GL_WRITE_ONLY, GL_RGBA32UI);

    // ============================
    // 			SHADERS
    // ============================
  	auto shaderCallback = [this](const ghoul::filesystem::File& file) {
        _updateShader = true;
    };
    _fragmentShaderFile->setCallback(shaderCallback);

    _resolveShader = nullptr;
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
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat)*4, BUFFER_OFFSET(0));
	glEnableVertexAttribArray(0);

	return true;
}

void ABufferSingleLinked::clear() {
	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, anchorPointerTextureInitializer);
	glBindTexture(GL_TEXTURE_2D, anchorPointerTexture);

	glTexImage2D(GL_TEXTURE_2D, 0, GL_R32UI, width, height, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, NULL);
	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

	static const GLuint zero = 1;
	glBindBufferBase(GL_ATOMIC_COUNTER_BUFFER, 0, atomicCounterBuffer);
	glBufferSubData(GL_ATOMIC_COUNTER_BUFFER, 0, sizeof(zero), &zero);
	glBindBufferBase(GL_ATOMIC_COUNTER_BUFFER, 0, 0);
}

void ABufferSingleLinked::preRender() {

	// Bind head-pointer image for read-write
	glBindBufferBase(GL_ATOMIC_COUNTER_BUFFER, 0, atomicCounterBuffer);
    glBindImageTexture(0, anchorPointerTexture, 0, GL_FALSE, 0, GL_READ_WRITE, GL_R32UI);
    glBindImageTexture(1, fragmentTexture, 0, GL_FALSE, 0, GL_READ_WRITE, GL_RGBA32UI);
}

void ABufferSingleLinked::postRender() {

	if(_updateShader) {
		_updateShader = false;
		generateShaderSource();
		updateShader();

	}
}

void ABufferSingleLinked::resolve() {

	if(_resolveShader) {
		ghoul::opengl::Texture* volume 	= nullptr;
		ghoul::opengl::Texture* tf 		= nullptr;
	    OsEng.configurationManager().getValue("firstVolume", volume);
	    OsEng.configurationManager().getValue("firstTransferFunction", tf);
		_resolveShader->activate();

		if(volume) {
			glActiveTexture(GL_TEXTURE0);
			volume->bind();
		}

		if(tf) {
			glActiveTexture(GL_TEXTURE1);
			tf->bind();
		}
		//LDEBUG("SCREEN_WIDTH" << width);
	    glBindVertexArray(_screenQuad);
	    glDrawArrays(GL_TRIANGLES, 0, 6);
		_resolveShader->deactivate();
	}
	
}

bool ABufferSingleLinked::updateShader() {

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
	resolveShader->setUniform("volume", 0);
	resolveShader->setUniform("transferFunction", 1);
	resolveShader->setUniform("SCREEN_WIDTH", static_cast<int>(width));
	resolveShader->setUniform("SCREEN_HEIGHT", static_cast<int>(height));

	if(_resolveShader)
		delete _resolveShader;

	_resolveShader = resolveShader;
	LDEBUG("Successfully updated shader!");
	return true;
}

void ABufferSingleLinked::generateShaderSource() {
	std::string line, source = "";
	std::ifstream fragmentShaderFile(_fragmentShaderFile->path());
	if(fragmentShaderFile.is_open()) {
		while(std::getline(fragmentShaderFile, line)) {
			if(line == "#pragma openspace insert HEADERS") {
				line = openspaceHeaders;
			} else if(line == "#pragma openspace insert SAMPLERCALLS") {
				line = openspaceSamplerCalls;
			} else if(line == "#pragma openspace insert SAMPLERS") {
				line = openspaceSamplers;
			}
			source += line + "\n";
		}
	}
	fragmentShaderFile.close();

	std::ofstream fragmentShaderOut(_fragmentShaderPath);
	fragmentShaderOut << source;
	fragmentShaderOut.close();
}

} // openspace