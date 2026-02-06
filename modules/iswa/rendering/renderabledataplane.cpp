/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/iswa/rendering/renderabledataplane.h>

#include <modules/iswa/rendering/iswabasegroup.h>
#include <modules/iswa/util/dataprocessortext.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>

namespace openspace {

documentation::Documentation RenderableDataPlane::Documentation() {
    documentation::Documentation doc = RenderableDataCygnet::Documentation();
    doc.name = "RenderableDataPlane";
    doc.id = "iswa_renderable_dataplane";
    return doc;
}

RenderableDataPlane::RenderableDataPlane(const ghoul::Dictionary& dictionary)
    : RenderableDataCygnet(dictionary)
{}

void RenderableDataPlane::initializeGL() {
    RenderableDataCygnet::initializeGL();

    if (!_shader) {
        _shader = global::renderEngine->buildRenderProgram(
            "DataPlaneProgram",
            absPath("${MODULE_ISWA}/shaders/dataplane_vs.glsl"),
            absPath("${MODULE_ISWA}/shaders/dataplane_fs.glsl")
        );
    }

    if (_group) {
        _dataProcessor = _group->dataProcessor();
        subscribeToGroup();
    }
    else {
        _dataProcessor = std::make_shared<DataProcessorText>();

        //If autofiler is on, background values property should be hidden
        _autoFilter.onChange([this]() {
            // If autofiler is selected, use _dataProcessor to set backgroundValues
            // and unregister backgroundvalues property.
            if (_autoFilter) {
                _backgroundValues = _dataProcessor->filterValues();
                _backgroundValues.setVisibility(properties::Property::Visibility::Hidden);
            // else if autofilter is turned off, register backgroundValues
            }
            else {
                _backgroundValues.setVisibility(properties::Property::Visibility::Always);
            }
        });
    }

    readTransferFunctions(_transferFunctionsFile);
    setPropertyCallbacks();
    _autoFilter = true;
}

void RenderableDataPlane::deinitializeGL() {
    _shader = nullptr;
    RenderableDataCygnet::deinitializeGL();
}

bool RenderableDataPlane::createGeometry() {
    struct Vertex {
        GLfloat x;
        GLfloat y;
        GLfloat z;
        GLfloat w;
        GLfloat s;
        GLfloat t;
    };

    glCreateBuffers(1, &_vbo);
    float s = _data.spatialScale.x;
    const GLfloat x = s * _data.scale.x / 2.f;
    const GLfloat y = s * _data.scale.y / 2.f;
    const GLfloat z = s * _data.scale.z / 2.f;
    const GLfloat w = _data.spatialScale.w;
    const Vertex VertexData[] = {
        { -x, -y,             -z,  w, 0, 1, },
        {  x,  y,              z,  w, 1, 0, },
        { -x, ((x > 0) ? y : -y),   z,  w, 0, 0, },
        { -x, -y,             -z,  w, 0, 1, },
        { x,  ((x > 0) ? -y : y),  -z,  w, 1, 1, },
        { x,  y,              z,  w, 1, 0 }
    };
    glNamedBufferStorage(_vbo, sizeof(VertexData), VertexData, GL_NONE_BIT);

    glCreateVertexArrays(1, &_vao);
    glVertexArrayVertexBuffer(_vao, 0, _vbo, 0, sizeof(Vertex));

    glEnableVertexArrayAttrib(_vao, 0);
    glVertexArrayAttribFormat(_vao, 0, 4, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(_vao, 0, 0);

    glEnableVertexArrayAttrib(_vao, 1);
    glVertexArrayAttribFormat(_vao, 1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 4);
    glVertexArrayAttribBinding(_vao, 1, 0);

    return true;
}

bool RenderableDataPlane::destroyGeometry() {
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;

    glDeleteBuffers(1, &_vbo);
    _vbo = 0;

    return true;
}

void RenderableDataPlane::renderGeometry() const {
    glBindVertexArray(_vao);
    glDrawArrays(GL_TRIANGLES, 0, 6);
}

void RenderableDataPlane::setUniforms() {
    // set both data texture and transfer function texture
    setTextureUniforms();
    _shader->setUniform("backgroundValues", _backgroundValues);
    _shader->setUniform("transparency", _alpha);
}

std::vector<float*> RenderableDataPlane::textureData() {
    // if the buffer in the datafile is empty, do not proceed
    if (_dataBuffer.empty()) {
        return std::vector<float*>();
    }

    if (!_dataOptions.options().size()) { // load options for value selection
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
