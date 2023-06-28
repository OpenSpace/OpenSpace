/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/fieldlinessequence/rendering/renderablecutplane.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
 
#include <fstream>
#include <optional>
#include <iostream>
#include <ostream>
#include <algorithm>
#include <cmath>

#include <modules/fieldlinessequence/fieldlinessequencemodule.h>

namespace {
    enum BlendMode {
        Normal = 0,
        Additive
    };
  constexpr openspace::properties::Property::PropertyInfo FilePathInfo = {
    "FilePath",
    "Hejhejs",
    "text",
    // @VISIBILITY(2.25)
    openspace::properties::Property::Visibility::User
  };
constexpr openspace::properties::Property::PropertyInfo DataPropertyInfo = {
    "DataProperty",
    "Dataproperty to create a plane of from slice",
    " ",
    // @VISIBILITY(2.67)
    openspace::properties::Property::Visibility::User
};
constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
    "Size",
    "Size (in meters)",
    "This value specifies the size of the plane in meters",
    openspace::properties::Property::Visibility::AdvancedUser
};
constexpr openspace::properties::Property::PropertyInfo ColorTablePathInfo = {
       "ColorTablePath",
       "Path to Color Table",
       "Color Table/Transfer Function to use for 'By Quantity' coloring",
       openspace::properties::Property::Visibility::AdvancedUser
};
  struct [[codegen::Dictionary(RenderableCutPlane)]] Parameters {
    // [[codegen::verbatim(FilePathInfo.description)]]
    std::string filePath;
    // Size of all dimensions
    std::variant<float, glm::vec3> size;
    // Axis to slice on
    std::string axis;
    // Value to what axis
    float cutValue;
    // [[codegen::verbatim(DataPropertyInfo.description)]]
    std::string dataProperty;
    // List of ranges for which their corresponding parameters values will be
    // colorized by. Should be entered as {min value, max value} per range
    std::optional<std::vector<glm::vec2>> colorTableRanges;
    // A list of paths to transferfunction .txt files containing color tables
    // used for colorizing the fieldlines according to different parameters
    std::optional<std::vector<std::string>> colorTablePaths;
      
  };
#include "renderablecutplane_codegen.cpp"
} // namespace
namespace openspace {
documentation::Documentation RenderableCutPlane::Documentation() {
    return codegen::doc<Parameters>(
                                    "base_renderablecutplane",
                                    RenderablePlane::Documentation()
                                    );
}

RenderableCutPlane::RenderableCutPlane(const ghoul::Dictionary& dictionary)
: RenderablePlane(dictionary),
_filePath(FilePathInfo),
_size(SizeInfo, glm::vec3(10.f), glm::vec3(0.f), glm::vec3(1e25f)),
_colorTablePath(ColorTablePathInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);   
    addProperty(_blendMode);

    if (std::holds_alternative<float>(p.size)) {
        _size = glm::vec3(std::get<float>(p.size));
    }
    else {
        _size = std::get<glm::vec3>(p.size);
    }

    _filePath = p.filePath;
    _axis = p.axis;
    _cutValue = p.cutValue;
    _dataProperty = p.dataProperty;
   _colorTableRanges = *p.colorTableRanges;

    if (p.colorTablePaths.has_value()) {
        _colorTablePaths = p.colorTablePaths.value();
    }
    else {
        // Set a default color table, just in case the (optional) user defined paths are
        // corrupt or not provided
        _colorTablePaths.push_back(FieldlinesSequenceModule::DefaultTransferFunctionFile);
    }
    if (p.colorTableRanges.has_value()) {
        _colorTableRanges = *p.colorTableRanges;
    }
    else {
        _colorTableRanges.push_back(glm::vec2(0.f, 1.f));
    }
}

bool RenderableCutPlane::isReady() const {
    return RenderablePlane::isReady();
}

void RenderableCutPlane::initialize() {
    _transferFunction = std::make_unique<TransferFunction>(
        absPath(_colorTablePaths[0]).string()
    );
}

void RenderableCutPlane::initializeGL() {
    ZoneScoped;

    loadTexture();

    _xAxisSize *= _size.value().x;
    _yAxisSize *= _size.value().y;
    _zAxisSize *= _size.value().z;
    _size = { _xAxisSize, _yAxisSize, _zAxisSize };

    RenderablePlane::initializeGL();
    createPlane();

    _shader = BaseModule::ProgramObjectManager.request(
        "CutPlane",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "CutPlane",
                absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/cutplane_vs.glsl"),
                absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/cutplane_fs.glsl")
            );
        }
    );
}

void RenderableCutPlane::deinitializeGL() {
 ZoneScoped;

 RenderablePlane::deinitializeGL();

 BaseModule::ProgramObjectManager.release(
     "CutPlane",
     [](ghoul::opengl::ProgramObject* p) {
         global::renderEngine->removeRenderProgram(p);
     }
 );
 _shader = nullptr;

}

void RenderableCutPlane::render(const RenderData& data, RendererTasks& t) {
    ZoneScoped;

    _shader->activate();
    _shader->setUniform("opacity", opacity());

    _shader->setUniform("mirrorBackside", _mirrorBackside);

    glm::dvec3 objectPositionWorld = glm::dvec3(
        glm::translate(
            glm::dmat4(1.0),
            data.modelTransform.translation) * glm::dvec4(0.0, 0.0, 0.0, 1.0)
    );
   
    glm::dvec3 normal = glm::normalize(data.camera.positionVec3() - objectPositionWorld);
    glm::dvec3 newRight = glm::normalize(
        glm::cross(data.camera.lookUpVectorWorldSpace(), normal)
    );
    glm::dvec3 newUp = glm::cross(normal, newRight);

    glm::dmat4 cameraOrientedRotation = glm::dmat4(1.0);
    cameraOrientedRotation[0] = glm::dvec4(newRight, 0.0);
    cameraOrientedRotation[1] = glm::dvec4(newUp, 0.0);
    cameraOrientedRotation[2] = glm::dvec4(normal, 0.0);

    const glm::dmat4 rotationTransform = _billboard ?
        cameraOrientedRotation :
        glm::dmat4(data.modelTransform.rotation);

    std::cout << axisCutValueX << "X \n";
    std::cout << axisCutValueX << "Y \n";
    std::cout << axisCutValueX << "Z \n";

   
    const glm::dmat4 translationMatrixX = glm::translate(glm::dmat4(1.0),
        glm::dvec3((alignOnX + axisCutValueX) * 2 * fls::ReToMeter, 0.0, 0.0));
    const glm::dmat4 translationMatrixY = glm::translate(glm::dmat4(1.0),
        glm::dvec3(0.0, (alignOnY + axisCutValueY) * 2 * fls::ReToMeter, 0.0));
    const glm::dmat4 translationMatrixZ = glm::translate(glm::dmat4(1.0),
        glm::dvec3(0.0, 0.0 , 2 * (alignOnZ + axisCutValueZ) * fls::ReToMeter));

    const glm::dmat4 modelTransform = 
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        rotationTransform *
        translationMatrixX * translationMatrixY * translationMatrixZ *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));
    const glm::dmat4 modelViewTransform =
        data.camera.combinedViewMatrix() * modelTransform;

    _shader->setUniform("modelViewProjectionTransform",
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform));

    _shader->setUniform("modelViewTransform", glm::mat4(modelViewTransform));

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    bindTexture();
    defer{ unbindTexture(); };
    _shader->setUniform("texture1", unit);

    ghoul::opengl::TextureUnit textureUnit;
    textureUnit.activate();
    _transferFunction->bind(); // Calls update internally
    _shader->setUniform("colorTable", textureUnit);
    _shader->setUniform("colorTableRange", _colorTableRanges[0]);

    bool additiveBlending = (_blendMode == static_cast<int>(BlendMode::Additive));
    if (additiveBlending) {
        glDepthMask(false);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    }
    glDisable(GL_CULL_FACE);

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    if (additiveBlending) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glDepthMask(true);
    }

    _shader->deactivate();
}

void RenderableCutPlane::update(const UpdateData& data) {
    ZoneScoped;
    createPlane();
}

void RenderableCutPlane::bindTexture() {
    _texture->bind();
}

void RenderableCutPlane::loadTexture()
{
    _slice.getSlice(_filePath, _axis, _cutValue);

    // z as default 
     dataProperyIndex = 2;
     
    // Set the index of the axis to scale on
    if (_axis.compare("x") == 0) dataProperyIndex = 0; 
    else if (_axis.compare("y") == 0) dataProperyIndex = 1; 
    else dataProperyIndex = 2;
 
    std::vector<std::vector<float>> volumeDimensions = _slice.volumeDimensions();

    std::vector<std::vector<float>> axisDim;
    axisDim.reserve(volumeDimensions.size()); // Reserve space for the new vector

    for (const auto& row : volumeDimensions) {
        std::vector<float> modifiedRow = row;

        if (&row == &(volumeDimensions[dataProperyIndex])) {
            // Modify the specific row in the new vector
            std::fill(modifiedRow.begin(), modifiedRow.end(), 0.0f);
        }

        // Add the modified row to the new vector
        axisDim.push_back(modifiedRow);
    }

    _xAxisSize = abs(axisDim[0][0]) + abs(axisDim[0][1]);
    _yAxisSize = abs(axisDim[1][0]) + abs(axisDim[1][1]);
    _zAxisSize = abs(axisDim[2][0]) + abs(axisDim[2][1]);

    alignOnX = ((abs(axisDim[0][0]) + abs(axisDim[0][1])) / 2) + axisDim[0][0]; 
    alignOnY = ((abs(axisDim[1][0]) + abs(axisDim[1][1])) / 2) + axisDim[1][0];
    alignOnZ = ((abs(axisDim[2][0]) + abs(axisDim[2][1])) / 2) + axisDim[2][0];

    switch (dataProperyIndex) {
    case 0: { axisCutValueX += _cutValue; }
    case 1: { axisCutValueY += _cutValue; }
    case 2: { axisCutValueZ += _cutValue; }

    }
    std::cout << abs(axisDim[0][0]) << ", " << abs(axisDim[0][1]) << ", " << (abs(axisDim[0][0]) + abs(axisDim[0][1])) / 2 << "hej1";
    std::cout << alignOnX << ", " << alignOnY << ", " << alignOnZ << "hej2 ";

    std::vector<std::string> s = _slice.quantitiesNames();
 
    auto it = std::find(s.begin(), s.end(), _dataProperty);
    if (it == s.end())
    {
        std::cout << "Name not in vector " << "\n";
    } else
    {
        auto pos =  std::distance(s.begin(), it);
        _dataPropertyIndex = static_cast<int>(pos);
    }

    //Create texture object of the slice
    _texture = createFloatTexture(_slice.data()[_dataPropertyIndex]);

}
// Function to create a floating-point texture from a double vector
std::unique_ptr<ghoul::opengl::Texture> RenderableCutPlane::createFloatTexture(const std::vector<std::vector<float>>& data)
{
    std::vector<float> flatData = {};
    for (size_t i = 0; i < _slice.data()[_dataPropertyIndex].size(); i++)
    {
        for (size_t j = 0; j < _slice.data()[_dataPropertyIndex][i].size(); j++) {

            flatData.push_back(_slice.data()[_dataPropertyIndex][i][j]);
        }
    }

    // Create a floating-point texture
    GLenum type = GL_TEXTURE_2D;
    ghoul::opengl::Texture::Format format = ghoul::opengl::Texture::Format::Red;
    GLenum internalFormat = GL_R32F; //32 bit float
    glm::uvec3 size(data[0].size(), data.size(), 1);
    std::unique_ptr<ghoul::opengl::Texture> texture =
        std::make_unique<ghoul::opengl::Texture>(
            flatData.data(),
            size,
            type,
            format,
            internalFormat,
            GL_FLOAT
        );
    // Set texture parameters
    texture->uploadTexture();
    texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
    texture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToEdge);
    return texture;
}
void RenderableCutPlane::createPlane() {

    const GLfloat sizeX = _size.value().x;
    const GLfloat sizeY = _size.value().y;
    const GLfloat sizeZ = _size.value().z;
    GLfloat vertexData[36];  // 6 vertices with 6 components each

    switch (dataProperyIndex) {
    case 0: {
        // Vertex data with X-axis set to zero
        const GLfloat verticesXZero[] = {
            0.f, -sizeY, -sizeZ, 0.f, 0.f, 0.f,
            0.f,  sizeY,  sizeZ, 0.f, 1.f, 1.f,
            0.f,  sizeY, -sizeZ, 0.f, 0.f, 1.f,
            0.f, -sizeY, -sizeZ, 0.f, 0.f, 0.f,
            0.f, -sizeY,  sizeZ, 0.f, 1.f, 0.f,
            0.f,  sizeY,  sizeZ, 0.f, 1.f, 1.f
        };
;        memcpy(vertexData, verticesXZero, sizeof(vertexData));
        break;
    }
    case 1: {
        // Vertex data with Y-axis set to zero
        const GLfloat verticesYZero[] = {
            -sizeX, 0.f, -sizeZ, 0.f, 0.f, 0.f,
             sizeX, 0.f,  sizeZ, 0.f, 1.f, 1.f,
            -sizeX, 0.f,  sizeZ, 0.f, 0.f, 1.f,
            -sizeX, 0.f, -sizeZ, 0.f, 0.f, 0.f,
             sizeX, 0.f, -sizeZ, 0.f, 1.f, 0.f,
             sizeX, 0.f,  sizeZ, 0.f, 1.f, 1.f
        };
        memcpy(vertexData, verticesYZero, sizeof(vertexData));
        break;
    }
    case 2: {
        // Vertex data with Z-axis set to zero
        const GLfloat verticesZZero[] = {
            -sizeX, -sizeY, 0.f, 0.f, 0.f, 0.f,
             sizeX,  sizeY, 0.f, 0.f, 1.f, 1.f,
            -sizeX,  sizeY, 0.f, 0.f, 0.f, 1.f,
            -sizeX, -sizeY, 0.f, 0.f, 0.f, 0.f,
             sizeX, -sizeY, 0.f, 0.f, 1.f, 0.f,
             sizeX,  sizeY, 0.f, 0.f, 1.f, 1.f
        };
        memcpy(vertexData, verticesZZero, sizeof(vertexData));
        break;
    }
    default:
        return;  // Invalid axis, exit without modifying the vertex data
    }

    glBindVertexArray(_quad);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertexData), vertexData, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, nullptr);

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 6,
        reinterpret_cast<void*>(sizeof(GLfloat) * 4)
    );
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

}// namespace openspace










