/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/iswa/rendering/datasphere.h>

#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/powerscaledscalar.h>
#include <openspace/util/powerscaledsphere.h>
#include <modules/iswa/util/dataprocessorjson.h>
#include <modules/iswa/rendering/iswabasegroup.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/glm.h>

namespace openspace {

DataSphere::DataSphere(const ghoul::Dictionary& dictionary)
    : DataCygnet(dictionary)
{
    _radius = dictionary.value<float>("Radius");
}

DataSphere::~DataSphere() {}

void DataSphere::initializeGL() {
    IswaCygnet::initializeGL();

    if (!_shader) {
        _shader = global::renderEngine.buildRenderProgram(
            "DataSphereProgram",
            absPath("${MODULE_ISWA}/shaders/datasphere_vs.glsl"),
            absPath("${MODULE_ISWA}/shaders/datasphere_fs.glsl")
        );
    }

    // Rotate 90 degrees because of the texture coordinates in PowerScaledSphere
    _rotation = glm::rotate(_rotation, glm::half_pi<float>(), glm::vec3(1.f, 0.f, 0.f));

    if (_group) {
        _dataProcessor = _group->dataProcessor();
        subscribeToGroup();
    } else {
        _dataProcessor = std::make_shared<DataProcessorJson>();
        //If autofiler is on, background values property should be hidden
        _autoFilter.onChange([this]() {
            // If autofiler is selected, use _dataProcessor to set backgroundValues
            // and unregister backgroundvalues property.
            if (_autoFilter) {
                _backgroundValues = _dataProcessor->filterValues();
                _backgroundValues.setVisibility(properties::Property::Visibility::Hidden);
                //_backgroundValues.setVisible(false);
            // else if autofilter is turned off, register backgroundValues
            } else {
                _backgroundValues.setVisibility(properties::Property::Visibility::All);
                //_backgroundValues.setVisible(true);
            }
        });
    }

    readTransferFunctions(_transferFunctionsFile);

    setPropertyCallbacks();
    _useHistogram = true;
    _autoFilter = true;
}

bool DataSphere::createGeometry() {
    PowerScaledScalar radius =  PowerScaledScalar(6.371f * _radius, 6.0);
    int segments = 100;
    _sphere = std::make_unique<PowerScaledSphere>(radius, segments);
    _sphere->initialize();
    return true;
}

bool DataSphere::destroyGeometry() {
    _sphere = nullptr;
    return true;
}

void DataSphere::renderGeometry() const {
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    _sphere->render();
}

std::vector<float*> DataSphere::textureData() {
    // if the buffer in the datafile is empty, do not proceed
    if (_dataBuffer.empty()) {
        return std::vector<float*>();
    }

    if (!_dataOptions.options().empty()) { // load options for value selection
        fillOptions(_dataBuffer);
        _dataProcessor->addDataValues(_dataBuffer, _dataOptions);

        // if this datacygnet has added new values then reload texture
        // for the whole group, including this datacygnet, and return after.
        if (_group) {
            _group->updateGroup();
            return std::vector<float*>();
        }
    }
    // _textureDimensions = _dataProcessor->setDimensions();
    return _dataProcessor->processData(_dataBuffer, _dataOptions, _textureDimensions);
}

void DataSphere::setUniforms() {
    // set both data texture and transfer function texture
    setTextureUniforms();
    _shader->setUniform("backgroundValues", _backgroundValues);
    _shader->setUniform("transparency", _alpha);
}

} //namespace openspace
