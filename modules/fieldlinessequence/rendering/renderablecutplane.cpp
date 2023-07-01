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
    constexpr openspace::properties::Property::PropertyInfo AxisInfo = {
        "Axis",
        "The x, y or z axis",
        "Axis to cut the volume on",
        openspace::properties::Property::Visibility::User
    };  
    constexpr openspace::properties::Property::PropertyInfo CutValueInfo = {
        "CutValue",
        "A value within the volume dimension",
        "A value to cut the plane on within the dimension of the selected axis",
        openspace::properties::Property::Visibility::User
    }; 
    constexpr openspace::properties::Property::PropertyInfo ColorTablePathsInfo = {
        "ColorTablePaths",
        "A local varibale of a local color transfer function",
        "A list of paths to transferfunction .txt files containing color tables used for colorizing the cutplane according to different data properties",
        openspace::properties::Property::Visibility::User
    };  
    constexpr openspace::properties::Property::PropertyInfo ColorTableRangesInfo = {
        "ColorTableRanges",
        "Values of a range",
        "List of ranges for which their corresponding data property values will be colorized by. Should be entered as {min value, max value} per range",
        openspace::properties::Property::Visibility::User
    };
    constexpr openspace::properties::Property::PropertyInfo DataPropertyInfo = {
        "DataProperty",
        "Name of the data property",
        "Data property to color the cutplane by",
        openspace::properties::Property::Visibility::User
    };
    constexpr openspace::properties::Property::PropertyInfo FilePathInfo = {
        "FilePath",
        "Filepath to the file to create texture from",
        " ",
        openspace::properties::Property::Visibility::User
    };
    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size (in meters)",
        "This value specifies the size unit",
        openspace::properties::Property::Visibility::User
    };

  struct [[codegen::Dictionary(RenderableCutPlane)]] Parameters {
    // [[codegen::verbatim(AxisInfo.description)]]
    std::string axis;
    // [[codegen::verbatim(CutValueInfo.description)]]
    float cutValue;
    // [[codegen::verbatim(DataPropertyInfo.description)]]
    std::string dataProperty;
    // [[codegen::verbatim(FilePathInfo.description)]]
    std::string filePath;
    // [[codegen::verbatim(SizeInfo.description)]]
    std::variant<float, glm::vec3> size;
    // [[codegen::verbatim(ColorTablePathsInfo.description)]]
    std::optional<std::vector<std::string>> colorTablePaths;
    // [[codegen::verbatim(ColorTableRangesInfo.description)]]
    std::optional<std::vector<glm::vec2>> colorTableRanges; 
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
_size(SizeInfo, glm::vec3(10.f), glm::vec3(0.f), glm::vec3(1e25f))
{
    const Parameters p = codegen::bake<Parameters>(dictionary);   

    if (std::holds_alternative<float>(p.size)) {
        _size = glm::vec3(std::get<float>(p.size));
    }
    else {
        _size = std::get<glm::vec3>(p.size);
    }

    _axis = p.axis;
    _cutValue = p.cutValue;
    _dataProperty = p.dataProperty;
    _filePath = p.filePath;

    if (p.colorTablePaths.has_value()) {
        _colorTablePaths = p.colorTablePaths.value();
    }
    else {
        // Set a default color table
        _colorTablePaths.push_back(FieldlinesSequenceModule::DefaultTransferFunctionFile);
    }

    if (p.colorTableRanges.has_value()) {
        _colorTableRanges = *p.colorTableRanges;
    }
    else {
        _colorTableRanges.push_back(glm::vec2(0.f, 1.f));
    }
}

void RenderableCutPlane::initialize() {
    _transferFunction = std::make_unique<TransferFunction>(
        absPath(_colorTablePaths[0]).string()
    );
}

void RenderableCutPlane::initializeGL() {
    ZoneScoped;

    // Create texture of the selected slice
    loadDataFromSlice();

    // Set length of the axis of the cutplane
    _xAxisLength *= _size.value().x;
    _yAxisLength *= _size.value().y;
    _zAxisLength *= _size.value().z;
    _size = { _xAxisLength, _yAxisLength, _zAxisLength };

    RenderablePlane::initializeGL();
    createPlane();

    // Setup shader program
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
   
    // Translation matrixes to get correct orientation with respect to the Earth
    const glm::dmat4 translationMatrixX = glm::translate(glm::dmat4(1.0),
        glm::dvec3((_alignOnX + _axisCutValueX) * 2 * fls::ReToMeter, 0.0, 0.0));
    const glm::dmat4 translationMatrixY = glm::translate(glm::dmat4(1.0),
        glm::dvec3(0.0, (_alignOnY + _axisCutValueY) * 2 * fls::ReToMeter, 0.0));
    const glm::dmat4 translationMatrixZ = glm::translate(glm::dmat4(1.0),
        glm::dvec3(0.0, 0.0 , 2 * (_alignOnZ + _axisCutValueZ) * fls::ReToMeter));

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

    // Add texture rendered from data
    ghoul::opengl::TextureUnit textureUnit1;
    textureUnit1.activate();
    bindTexture();
    defer{ unbindTexture(); };
    _shader->setUniform("texture1", textureUnit1);

    ghoul::opengl::TextureUnit textureUnit2;
    textureUnit2.activate();
    _transferFunction->bind(); 
    _shader->setUniform("colorTable", textureUnit2);
    _shader->setUniform("colorTableRange", _colorTableRanges[0]);
 
    glDisable(GL_CULL_FACE);

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    _shader->deactivate();
}

void RenderableCutPlane::bindTexture() {
    _texture->bind();
}

void RenderableCutPlane::update(const UpdateData& data) {
    ZoneScoped;
    createPlane();
}

void RenderableCutPlane::loadDataFromSlice()
{
    // Create slice
    _slice.getSlice(_filePath, _axis, _cutValue);

    // Z as default 
    axisIndex = 2;
     
    // Set the index of the axis to scale on
    if (_axis.compare("x") == 0) axisIndex = 0; 
    else if (_axis.compare("y") == 0) axisIndex = 1; 
 
    std::vector<std::vector<float>> volumeDimensions = _slice.volumeDimensions();

    // The dimension of the cutplane's axes 
    std::vector<std::vector<float>> axisDim;
    axisDim.reserve(volumeDimensions.size()); 

    for (const auto& row : volumeDimensions) {
        std::vector<float> cutAxis = row;

        if (&row == &(volumeDimensions[axisIndex])) {
            std::fill(cutAxis.begin(), cutAxis.end(), 0.0f);
        }

        axisDim.push_back(cutAxis);
    }

    // Calculate the length of the axes
    _xAxisLength = abs(axisDim[0][0]) + abs(axisDim[0][1]);
    _yAxisLength = abs(axisDim[1][0]) + abs(axisDim[1][1]);
    _zAxisLength = abs(axisDim[2][0]) + abs(axisDim[2][1]);

    // Calculate where the cutplane should be rendered with respect of Earth
    _alignOnX = ((abs(axisDim[0][0]) + abs(axisDim[0][1])) / 2) + axisDim[0][0]; 
    _alignOnY = ((abs(axisDim[1][0]) + abs(axisDim[1][1])) / 2) + axisDim[1][0];
    _alignOnZ = ((abs(axisDim[2][0]) + abs(axisDim[2][1])) / 2) + axisDim[2][0];

    // Add the the selected value to the axis to cut on
    switch (axisIndex) {
    case 0: { _axisCutValueX += _cutValue; break; }
    case 1: { _axisCutValueY += _cutValue; break; }
    case 2: { _axisCutValueZ += _cutValue; break; }
    default: break;
    }

    // Get all property names of the slice
    std::vector<std::string> s = _slice.dataPropertyNames();
 
    // Give all data property names a index
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
    _texture = createTexture(_slice.data()[_dataPropertyIndex]);

}
// Function to create a floating-point texture from a double vector
std::unique_ptr<ghoul::opengl::Texture> RenderableCutPlane::createTexture(const std::vector<std::vector<float>>& data)
{
    // Insert the data of the slice into 1D vector
    std::vector<float> sliceData1D = {};
    for (size_t i = 0; i < _slice.data()[_dataPropertyIndex].size(); i++)
    {
        for (size_t j = 0; j < _slice.data()[_dataPropertyIndex][i].size(); j++) {

            sliceData1D.push_back(_slice.data()[_dataPropertyIndex][i][j]);
        }
    }

    // Create a floating-point texture
    GLenum type = GL_TEXTURE_2D;
    ghoul::opengl::Texture::Format format = ghoul::opengl::Texture::Format::Red;
    GLenum internalFormat = GL_R32F; //32 bit float
    glm::uvec3 size(data[0].size(), data.size(), 1);
    std::unique_ptr<ghoul::opengl::Texture> texture =
        std::make_unique<ghoul::opengl::Texture>(
            sliceData1D.data(),
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
    GLfloat vertexData[36];  

    // Choose the type of plane to be rendered depending on what axis to cut on 
    switch (axisIndex) {
    case 0: {
        // Vertex data with cut on X-axis 
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
        // Vertex data with cut on Y-axis
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
        // Vertex data with cut on Z-axis 
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
        return;
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










