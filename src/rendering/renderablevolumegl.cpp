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

// open space includes
#include <openspace/rendering/renderablevolumegl.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/powerscaledcoordinate.h>

#include <ghoul/opengl/texturereader.h>
#include <ghoul/opencl/clworksize.h>
#include <ghoul/filesystem/filesystem.h>

#include <algorithm>

#include <openspace/engine/openspaceengine.h>

namespace {
    std::string _loggerCat = "RenderableVolumeGL";
}

namespace openspace {

RenderableVolumeGL::RenderableVolumeGL(const ghoul::Dictionary& dictionary):
    RenderableVolume(dictionary), _boxScaling(1.0, 1.0, 1.0),
    _updateTransferfunction(false) {
        
    
    _filename = "";
    if(dictionary.hasKey("Volume")) {
        if(dictionary.getValue("Volume", _filename)) {
            _filename = findPath(_filename);
        }
    }

    LDEBUG("filename: " << _filename);
    
    if(dictionary.hasKey("Hints"))
        dictionary.getValue("Hints", _hintsDictionary);

    _transferFunction = nullptr;
    _transferFunctionFile = nullptr;
    if (dictionary.hasKey("TransferFunction")) {
        std::string transferFunctionPath = "";
        if(dictionary.getValue("TransferFunction", transferFunctionPath)) {
            _transferFunctionPath = findPath(transferFunctionPath);
        }
    }
    _samplerFilename = "";
    if (dictionary.hasKey("Sampler")) {
        if(dictionary.getValue("Sampler", _samplerFilename)) {
            _samplerFilename = findPath(_samplerFilename);
        }
    }
    if( _transferFunctionPath == "") {
        LERROR("No transferFunction!");
    } else {
        _transferFunctionFile = new ghoul::filesystem::File(_transferFunctionPath, true);
    }
    if( _samplerFilename == "") {
        LERROR("No samplerfile!");
    } 


    double tempValue;
    if(dictionary.hasKey("BoxScaling.1") && dictionary.getValue("BoxScaling.1", tempValue)) {
    	if(tempValue > 0.0)
    		_boxScaling[0] = tempValue;
    }
    if(dictionary.hasKey("BoxScaling.2") && dictionary.getValue("BoxScaling.2", tempValue)) {
    	if(tempValue > 0.0)
    		_boxScaling[1] = tempValue;
    }
    if(dictionary.hasKey("BoxScaling.3") && dictionary.getValue("BoxScaling.3", tempValue)) {
    	if(tempValue > 0.0)
    		_boxScaling[2] = tempValue;
    }

    _colorBoxRenderer = new VolumeRaycasterBox();
    setBoundingSphere(PowerScaledScalar::CreatePSS(glm::length(_boxScaling)));
}

RenderableVolumeGL::~RenderableVolumeGL() {
    deinitialize();
    if(_volume)
        delete _volume;
    if(_colorBoxRenderer)
        delete _colorBoxRenderer;
    if(_transferFunctionFile)
        delete _transferFunctionFile;
    if(_transferFunction)
        delete _transferFunction;
}

bool RenderableVolumeGL::initialize() {
    assert(_filename != "");
    //	------ VOLUME READING ----------------
	_volume = loadVolume(_filename, _hintsDictionary);
	_volume->uploadTexture();
    _transferFunction = loadTransferFunction(_transferFunctionPath);
    _transferFunction->uploadTexture();
    OsEng.configurationManager().setValue("firstVolume", _volume);
    OsEng.configurationManager().setValue("firstTransferFunction", _transferFunction);
    OsEng.configurationManager().setValue("firstSampler", _samplerFilename);

    auto textureCallback = [this](const ghoul::filesystem::File& file) {
        _updateTransferfunction = true;
    };
    _transferFunctionFile->setCallback(textureCallback);

	_colorBoxRenderer->initialize();
    
    
    return true;
}

bool RenderableVolumeGL::deinitialize() {
    return true;
}

void RenderableVolumeGL::render(const Camera *camera, const psc &thisPosition) {
    if(_updateTransferfunction) {
        _updateTransferfunction = false;
        ghoul::opengl::Texture* transferFunction = loadTransferFunction(_transferFunctionPath);
        if(transferFunction) {
            const void* data = transferFunction->pixelData();
            glBindBuffer(GL_COPY_READ_BUFFER, *transferFunction);
            _transferFunction->bind();
            glTexImage1D(GL_TEXTURE_1D, 0, _transferFunction->internalFormat(), _transferFunction->width(),0, _transferFunction->format(), _transferFunction->dataType(), data);
            //delete data;
            delete transferFunction;
            LDEBUG("Updated transferfunction!");

        }
    }

    glm::mat4 transform ;
    glm::mat4 camTransform = camera->viewRotationMatrix();
    psc relative = thisPosition-camera->position();

    transform = camTransform;
    transform = glm::translate(transform, relative.vec3());
    transform = glm::translate(transform, glm::vec3(-1.1,0.0,0.0));
    transform = glm::scale(transform, _boxScaling);

    _colorBoxRenderer->render(camera->viewProjectionMatrix(), transform);

}

void RenderableVolumeGL::update() {
    
}


} // namespace openspace
