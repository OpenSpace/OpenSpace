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

#ifndef __ABUFFER_H__
#define __ABUFFER_H__

#include <openspace/abuffer/abuffer_i.h>

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/glm.h>

#include <ghoul/opengl/programobject.h>
#include <ghoul/filesystem/file.h>

namespace ghoul {
	namespace opengl {
		class Texture;
	}
}

namespace openspace {

class ABuffer: public ABuffer_I {
public:

	ABuffer();
	virtual ~ABuffer();
	virtual void resolve();

	void addVolume(const std::string& tag,ghoul::opengl::Texture* volume);
	void addTransferFunction(const std::string& tag,ghoul::opengl::Texture* transferFunction);
	int addSamplerfile(const std::string& filename);

protected:
	virtual std::string settings() = 0;

	bool initializeABuffer();

	void generateShaderSource();
	bool updateShader();

	std::string openspaceHeaders();
	std::string openspaceSamplerCalls();
	std::string openspaceSamplers();
	std::string openspaceTransferFunction();

	unsigned int _width, _height, _totalPixels;

private:
	GLuint _screenQuad;

	bool _validShader;
	std::string _fragmentShaderPath;
	ghoul::filesystem::File* _fragmentShaderFile;
	ghoul::opengl::ProgramObject* _resolveShader;

	std::vector<std::pair<std::string,ghoul::opengl::Texture*> > _volumes;
	std::vector<std::pair<std::string,ghoul::opengl::Texture*> > _transferFunctions;
	std::vector<ghoul::filesystem::File*> _samplerFiles;
	std::vector<std::string> _samplers;

	// Development functionality to update shader for changes in several files
	std::vector<ghoul::filesystem::File*> _shaderFiles;

	float _volumeStepFactor;



};		// ABuffer
}		// openspace

#endif 	// __ABUFFER_H__