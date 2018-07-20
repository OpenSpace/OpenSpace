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

#include <modules/iswa/rendering/dataplane.h>

#include <modules/iswa/rendering/iswabasegroup.h>
#include <modules/iswa/util/dataprocessortext.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>

namespace openspace {

DataPlane::DataPlane(const ghoul::Dictionary& dictionary) : DataCygnet(dictionary) {}

void DataPlane::initializeGL() {
    IswaCygnet::initialize();

    if (!_shader) {
        _shader = global::renderEngine.buildRenderProgram(
            "DataPlaneProgram",
            absPath("${MODULE_ISWA}/shaders/dataplane_vs.glsl"),
            absPath("${MODULE_ISWA}/shaders/dataplane_fs.glsl")
        );
    }

    if (_group) {
        _dataProcessor = _group->dataProcessor();
        subscribeToGroup();
    } else {
        _dataProcessor = std::make_shared<DataProcessorText>();

        //If autofiler is on, background values property should be hidden
        _autoFilter.onChange([this]() {
            // If autofiler is selected, use _dataProcessor to set backgroundValues
            // and unregister backgroundvalues property.
            if (_autoFilter) {
                _backgroundValues = _dataProcessor->filterValues();
                _backgroundValues.setVisibility(properties::Property::Visibility::Hidden);
            // else if autofilter is turned off, register backgroundValues
            } else {
                _backgroundValues.setVisibility(properties::Property::Visibility::All);
            }
        });
    }

    readTransferFunctions(_transferFunctionsFile);
    setPropertyCallbacks();
    _autoFilter = true;
}

bool DataPlane::createGeometry() {
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer

    // ============================
    //         GEOMETRY (quad)
    // ============================
    // GLfloat x,y, z;
    float s = _data.spatialScale.x;
    const GLfloat x =  s *_data.scale.x / 2.f;
    const GLfloat y = s * _data.scale.y / 2.f;
    const GLfloat z = s * _data.scale.z / 2.f;
    const GLfloat w = _data.spatialScale.w;

    const GLfloat vertex_data[] = { // square of two triangles (sigh)
    //   x   y               z   w  s  t
        -x, -y,             -z,  w, 0, 1,
         x,  y,              z,  w, 1, 0,
        -x,  ((x>0)?y:-y),   z,  w, 0, 0,
        -x, -y,             -z,  w, 0, 1,
         x,  ((x>0)?-y:y),  -z,  w, 1, 1,
         x,  y,              z,  w, 1, 0,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        4,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 6,
        nullptr
    );
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 6,
        reinterpret_cast<void*>(sizeof(GLfloat) * 4)
    );

    return true;
}

bool DataPlane::destroyGeometry() {
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    return true;
}

void DataPlane::renderGeometry() const {
    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
}

void DataPlane::setUniforms() {
    // set both data texture and transfer function texture
    setTextureUniforms();
    _shader->setUniform("backgroundValues", _backgroundValues);
    _shader->setUniform("transparency", _alpha);
}

std::vector<float*> DataPlane::textureData(){
    // if the buffer in the datafile is empty, do not proceed
    if (_dataBuffer.empty()) {
        return std::vector<float*>();
    }

    if(!_dataOptions.options().size()) { // load options for value selection
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

    std::vector<float*> d = _dataProcessor->processData(
        _dataBuffer,
        _dataOptions,
        _textureDimensions
    );

    return d;
}

} // namespace openspace
