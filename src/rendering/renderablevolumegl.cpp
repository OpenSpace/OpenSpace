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

#include <openspace/rendering/renderablevolumegl.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/kameleonwrapper.h>

#include <ghoul/opengl/texturereader.h>
#include <ghoul/opencl/clworksize.h>
#include <ghoul/filesystem/filesystem.h>

#include <algorithm>

#include <openspace/engine/openspaceengine.h>
#include <openspace/util/constants.h>

namespace {
    const std::string _loggerCat = "RenderableVolumeGL";
}

namespace openspace {

RenderableVolumeGL::RenderableVolumeGL(const ghoul::Dictionary& dictionary)
	: RenderableVolume(dictionary)
	, _transferFunctionName("")
	, _volumeName("")
	, _boxScaling(1.0, 1.0, 1.0)
	, _w(0.f)
	, _updateTransferfunction(false)
	, _id(-1)
{
	std::string name;
	bool success = dictionary.getValue(constants::scenegraphnode::keyName, name);
	assert(success);
    
    _filename = "";
	success = dictionary.getValueSafe(constants::renderablevolumegl::keyVolume,
		_filename);
	if (!success) {
		LERROR("Node '" << name << "' did not contain a valid '" << 
			constants::renderablevolumegl::keyVolume << "'");
		return;
	}
	_filename = findPath(_filename);

    LDEBUG("Volume Filename: " << _filename);

	dictionary.getValueSafe(constants::renderablevolumegl::keyHints, _hintsDictionary);

    _transferFunction = nullptr;
    _transferFunctionFile = nullptr;
    _transferFunctionPath = "";
	success = dictionary.getValueSafe(
		constants::renderablevolumegl::keyTransferFunction, _transferFunctionPath);
	if (!success) {
		LERROR("Node '" << name << "' did not contain a valid '" <<
			constants::renderablevolumegl::keyTransferFunction << "'");
		return;
	}
	_transferFunctionPath = findPath(_transferFunctionPath);
	_transferFunctionFile = new ghoul::filesystem::File(_transferFunctionPath, true);
    
	_samplerFilename = "";
	success = dictionary.getValueSafe(constants::renderablevolumegl::keySampler,
		_samplerFilename);
	if (!success) {
		LERROR("Node '" << name << "' did not contain a valid '" <<
			constants::renderablevolumegl::keySampler << "'");
		return;
	}
    _samplerFilename = findPath(_samplerFilename);

	glm::vec4 scalingVec4(_boxScaling, _w);
	success = dictionary.getValueSafe(constants::renderablevolumegl::keyBoxScaling,
		scalingVec4);
	if (success) {
		_boxScaling = scalingVec4.xyz;
		_w = scalingVec4.w;
	}
	else {
		success = dictionary.getValueSafe(constants::renderablevolumegl::keyBoxScaling,
			_boxScaling);
		if (!success) {
			LERROR("Node '" << name << "' did not contain a valid '" <<
				constants::renderablevolumegl::keyBoxScaling << "'");
			return;
		}
	}

	dictionary.getValueSafe(constants::renderablevolumegl::keyVolumeName, _volumeName);
	dictionary.getValueSafe(constants::renderablevolumegl::keyTransferFunctionName,
		_transferFunctionName);

    setBoundingSphere(PowerScaledScalar::CreatePSS(glm::length(_boxScaling)*pow(10,_w)));
}

RenderableVolumeGL::~RenderableVolumeGL() {
    deinitialize();
    if(_volume)
        delete _volume;
    if(_transferFunctionFile)
        delete _transferFunctionFile;
    if(_transferFunction)
        delete _transferFunction;
}

bool RenderableVolumeGL::initialize() {
    // assert(_filename != "");
    //	------ VOLUME READING ----------------
// <<<<<<< HEAD
// =======
// 	_volume = loadVolume(_filename, _hintsDictionary);
// 	_volume->uploadTexture();
//     _transferFunction = loadTransferFunction(_transferFunctionPath);
//     _transferFunction->uploadTexture();

// >>>>>>> feature/fieldlines
    // TODO: fix volume an transferfunction names
    if(_filename != "") {
        _volume = loadVolume(_filename, _hintsDictionary);
        _boxOffset = getVolumeOffset(_filename, _hintsDictionary);
        _volume->uploadTexture();
        OsEng.renderEngine().abuffer()->addVolume(_volumeName, _volume);
    }

    if(_transferFunctionPath != "") {
        _transferFunction = loadTransferFunction(_transferFunctionPath);
        _transferFunction->uploadTexture();
        OsEng.renderEngine().abuffer()->addTransferFunction(_transferFunctionName, _transferFunction);

        auto textureCallback = [this](const ghoul::filesystem::File& file) {
            _updateTransferfunction = true;
        };
        _transferFunctionFile->setCallback(textureCallback);
    }

    // add the sampler and get the ID
    _id = OsEng.renderEngine().abuffer()->addSamplerfile(_samplerFilename);

    OsEng.configurationManager().getValue("RaycastProgram", _boxProgram);
    _MVPLocation = _boxProgram->uniformLocation("modelViewProjection");
    _modelTransformLocation = _boxProgram->uniformLocation("modelTransform");
    _typeLocation = _boxProgram->uniformLocation("volumeType");

    // ============================
    //      GEOMETRY (quad)
    // ============================
    const GLfloat size = 0.5f;
    const GLfloat vertex_data[] = { // square of two triangles (sigh)
        //  x,     y,     z,     s,
        -size, -size,  size,  _w,
         size,  size,  size,  _w,
        -size,  size,  size,  _w,
        -size, -size,  size,  _w,
         size, -size,  size,  _w,
         size,  size,  size,  _w,

        -size, -size, -size,  _w,
         size,  size, -size,  _w,
        -size,  size, -size,  _w,
        -size, -size, -size,  _w,
         size, -size, -size,  _w,
         size,  size, -size,  _w,

         size, -size, -size,  _w,
         size,  size,  size,  _w,
         size, -size,  size,  _w,
         size, -size, -size,  _w,
         size,  size, -size,  _w,
         size,  size,  size,  _w,

        -size, -size, -size,  _w,
        -size,  size,  size,  _w,
        -size, -size,  size,  _w,
        -size, -size, -size,  _w,
        -size,  size, -size,  _w,
        -size,  size,  size,  _w,

        -size,  size, -size,  _w,
         size,  size,  size,  _w,
        -size,  size,  size,  _w,
        -size,  size, -size,  _w,
         size,  size, -size,  _w,
         size,  size,  size,  _w,

        -size, -size, -size,  _w,
         size, -size,  size,  _w,
        -size, -size,  size,  _w,
        -size, -size, -size,  _w,
         size, -size, -size,  _w,
         size, -size,  size,  _w,
    };
    GLuint vertexPositionBuffer;
    glGenVertexArrays(1, &_boxArray); // generate array
    glBindVertexArray(_boxArray); // bind array
    glGenBuffers(1, &vertexPositionBuffer); // generate buffer
    glBindBuffer(GL_ARRAY_BUFFER, vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat)*4, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(0);
    
    return true;
}

bool RenderableVolumeGL::deinitialize() {
    return true;
}

void RenderableVolumeGL::render(const Camera *camera, const psc &thisPosition, RuntimeData* runtimeData) {
    if(_updateTransferfunction) {
        _updateTransferfunction = false;
        ghoul::opengl::Texture* transferFunction = loadTransferFunction(_transferFunctionPath);
        if(transferFunction) {
            const void* data = transferFunction->pixelData();
            glBindBuffer(GL_COPY_READ_BUFFER, *transferFunction);
            _transferFunction->bind();
            glTexImage1D(   GL_TEXTURE_1D, 0, _transferFunction->internalFormat(), 
                            _transferFunction->width(),0, _transferFunction->format(), 
                            _transferFunction->dataType(), data);
            delete transferFunction;
            LDEBUG("Updated transferfunction!");
        }
    }

    glm::mat4 transform = glm::mat4(1.0);
    transform = glm::scale(transform, _boxScaling);

    // fetch data
    psc currentPosition         = thisPosition;

    // psc addon(-1.1,0.0,0.0,0.0);
    // currentPosition += addon;
    psc addon(_boxOffset/100.0f); // TODO: Proper scaling/units
    currentPosition += addon; // Move box to model barycenter

    _boxProgram->activate();
	_boxProgram->setUniform(_typeLocation, _id);
	_boxProgram->setUniform("modelViewProjection", camera->viewProjectionMatrix());
	_boxProgram->setUniform("modelTransform", transform);
	setPscUniforms(_boxProgram, camera, currentPosition);

    // make sure GL_CULL_FACE is enabled (it should be)
    glEnable(GL_CULL_FACE);

    //  Draw backface
    glCullFace(GL_FRONT);
    glBindVertexArray(_boxArray);
    glDrawArrays(GL_TRIANGLES, 0, 6*6);

    //  Draw frontface (now the normal cull face is is set)
    glCullFace(GL_BACK);
    glDrawArrays(GL_TRIANGLES, 0, 6*6);

    _boxProgram->deactivate();
}

void RenderableVolumeGL::update() {
}

} // namespace openspace
