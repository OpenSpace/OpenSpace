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

#include <modules/fieldlinessequence/fieldlinessequencemodule.h>


namespace {
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
constexpr openspace::properties::Property::PropertyInfo ColorTablePathInfo = {
       "ColorTablePath",
       "Path to Color Table",
       "Color Table/Transfer Function to use for 'By Quantity' coloring",
       openspace::properties::Property::Visibility::AdvancedUser
};
  struct [[codegen::Dictionary(RenderableCutPlane)]] Parameters {
    // [[codegen::verbatim(FilePathInfo.description)]]
    std::string filePath;
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
_colorTablePath(ColorTablePathInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

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
   // RenderablePlane::initializeGL();
   // glGenVertexArrays(1, &_quad); // generate array
   // glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
  
    loadTexture();

    _axis1 *= _size.value().x;
    _axis2 *= _size.value().y;
    _size = { _axis1,_axis2 };
    std::cout << _axis1 << ", " << _axis2 << "\n";

    _colorTablePaths.resize(4, _colorTablePaths.back());
    _colorTablePath = _colorTablePaths[0];
    _colorTableRanges.resize(4, _colorTableRanges.back());

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

     _colorTablePath.onChange([this]() {
            _transferFunction->setPath(_colorTablePath);
        });

    // Needed for additive blending
   // setRenderBin(Renderable::RenderBin::Overlay); 
}

void RenderableCutPlane::deinitializeGL() {
 ZoneScoped;

    glDeleteBuffers(1, &vaoHandle);
    vaoHandle = 0;

   glDeleteBuffers(1, &_vertexPositionBuffer);
   _vertexPositionBuffer = 0;

   glDeleteBuffers(1, &_vertexColorBuffer);
   _vertexColorBuffer = 0;


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

    // CAMERA
    glm::dmat4 cameraOrientedRotation = glm::dmat4(1.0);
    cameraOrientedRotation[0] = glm::dvec4(newRight, 0.0);
    cameraOrientedRotation[1] = glm::dvec4(newUp, 0.0);
    cameraOrientedRotation[2] = glm::dvec4(normal, 0.0);

    const glm::dmat4 rotationTransform =
        glm::dmat4(data.modelTransform.rotation);

    const glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        rotationTransform *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));
    const glm::dmat4 modelViewTransform =
        data.camera.combinedViewMatrix() * modelTransform;

    _shader->setUniform("modelViewProjectionTransform",
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform));

    _shader->setUniform("modelViewTransform", glm::mat4(modelViewTransform));


    ghoul::opengl::TextureUnit textureUnit;
    textureUnit.activate();
    _transferFunction->bind(); // Calls update internally
    _shader->setUniform("colorTable", textureUnit);
    _shader->setUniform("colorTableRange", _colorTableRanges[_dataPropertyIndex]);
   // _texture->bind();
   // defer{ unbindTexture(); };
  // _shader->setUniform("texture1", textureUnit);

  //  glDisable(GL_CULL_FACE);
   
   glBindVertexArray(vaoHandle);
   glDrawArrays(GL_TRIANGLES, 0, 6);
   glBindVertexArray(0);

   /* glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);  
     glBindVertexArray(0);*/

    _shader->deactivate();
}

void RenderableCutPlane::update(const UpdateData& data) {
    ZoneScoped;
   // if (_planeIsDirty) {
        createPlane();
   // }
   // RenderablePlane::update(data);
    //updateVertexColorBuffer();
}

void RenderableCutPlane::bindTexture() {
    //ghoul::opengl::Texture* rawTexture = _texture.get();
    //ghoul_assert(_texture, "Texture must be loaded before binding");
    //rawTexture->bind();
    if (_texture) {
        _texture->bind();
    }
}

void RenderableCutPlane::createPlane()
{
    const GLfloat sizeX = _size.value().x;
    const GLfloat sizeY = _size.value().y;
    const GLfloat vertexData[] = {
        //   x       y    z    w    s    t
        -sizeX, -sizeY, 0.f, 0.f, 0.f, 0.f,
         sizeX,  sizeY, 0.f, 0.f, 1.f, 1.f,
        -sizeX,  sizeY, 0.f, 0.f, 0.f, 1.f,
        -sizeX, -sizeY, 0.f, 0.f, 0.f, 0.f,
         sizeX, -sizeY, 0.f, 0.f, 1.f, 0.f,
         sizeX,  sizeY, 0.f, 0.f, 1.f, 1.f
    };

    std::vector<float> flatData = {};
    for (size_t i = 0; i < _slice.data()[_dataPropertyIndex].size(); i++)
    {
        for (size_t j = 0; j < _slice.data()[_dataPropertyIndex][i].size(); j++) {

            flatData.push_back(_slice.data()[_dataPropertyIndex][i][j]);
        }
    }
  
    const std::vector<float>& colorData = flatData;

    GLuint vboHandles[2];
    glGenBuffers(2, vboHandles);

     _vertexPositionBuffer = vboHandles[0];
     _vertexColorBuffer = vboHandles[1];

    // Populate the position buffer
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertexData), vertexData, GL_STATIC_DRAW);
    // Populate the color buffer
    glBindBuffer(GL_ARRAY_BUFFER, _vertexColorBuffer);
    glBufferData(GL_ARRAY_BUFFER, colorData.size() * sizeof(float), colorData.data(), GL_STATIC_DRAW);

    glGenVertexArrays(1, &vaoHandle);
    glBindVertexArray(vaoHandle);

    // Enable the vertex attribute arrays
    glEnableVertexAttribArray(0);  // Vertex position
    glEnableVertexAttribArray(1);  // Vertex color

    // Map index 0 to the position buffer
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, NULL);

    // Map index 1 to the color buffer
    glBindBuffer(GL_ARRAY_BUFFER, _vertexColorBuffer);
    glVertexAttribPointer(1, 1, GL_FLOAT, GL_FALSE, 0, 0);

   // delete[] colorData;
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
 
}

void RenderableCutPlane::loadTexture()
{
    _slice.getSlice(_filePath, _axis, _cutValue);

    int axisIndex = 2;
    // Set the index of the axis to scale on
    if (_axis.compare("x") == 0) axisIndex = 0;
    else if (_axis.compare("y") == 0) axisIndex = 1;

    std::vector<std::vector<float>> volumeDimensions = _slice.volumeDimensions();
    // Copy elements except the second element
    std::copy_if(volumeDimensions.begin(), volumeDimensions.end(), std::back_inserter(_axisDim),
          [volumeDimensions, axisIndex](std::vector<float> vec) { return vec != volumeDimensions[axisIndex]; });

    _axis1 = abs(_axisDim[0][0]) + abs(_axisDim[0][1]);
    _axis2 = abs(_axisDim[1][0]) + abs(_axisDim[1][1]); 

    std::cout << "The Volume dimensions x, y, z " << "\n";
    std::cout << _slice.volumeDimensions()[0][0] << " - " << _slice.volumeDimensions()[0][1] << "\n";
    std::cout << _slice.volumeDimensions()[1][0] << " - " << _slice.volumeDimensions()[1][1] << "\n";
    std::cout << _slice.volumeDimensions()[2][0] << " - " << _slice.volumeDimensions()[2][1] << "\n";

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
    // Convert the 2D vector to a flat float array
    std::vector<float> flatData1; //en slice men platt
    for (const auto& row : data) {
        flatData1.insert(flatData1.end(), row.begin(), row.end());
    }

    for (const auto& element : flatData1) {
        std::cout << element << " ";
    }

    // Create a floating-point texture
    GLenum type = GL_TEXTURE_2D;
    ghoul::opengl::Texture::Format format = ghoul::opengl::Texture::Format::Red;
    GLenum internalFormat = GL_R32F; //32 bit float
    glm::uvec3 size(data[0].size(), data.size(), 1);
    std::unique_ptr<ghoul::opengl::Texture> texture =
        std::make_unique<ghoul::opengl::Texture>(
            flatData1.data(),
            size,
            type,
            format,
            internalFormat,
            GL_FLOAT
        );
    // Set texture parameters
    texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
    texture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToEdge);
    return texture;
}

void RenderableCutPlane::updateVertexColorBuffer() {
    //if (_activeStateIndex == -1) { return; }
    //glBindVertexArray(_quad);
    //glBindBuffer(GL_ARRAY_BUFFER, _vertexColorBuffer);

    //std::vector<float> flatVector;
    //for (const auto& innerVector : _slice.data()[_dataPropertyIndex]) {
    //    flatVector.insert(flatVector.end(), innerVector.begin(), innerVector.end());
    //}
    //// use colorquan as argument to get the wanted dataprop in the selected slice 
    //const std::vector<float>& quantities = flatVector;

    //// use colorquan as argument to get the wanted dataprop in the selected slice 
    //glBufferData(
    //    GL_ARRAY_BUFFER,
    //    quantities.size() * sizeof(float),
    //    quantities.data(),
    //    GL_STATIC_DRAW
    //);

    //glEnableVertexAttribArray(2);
    //glVertexAttribPointer(2, 1, GL_FLOAT, GL_FALSE, 0, 0);

    //glBindBuffer(GL_ARRAY_BUFFER, 0);
    //glBindVertexArray(0);


}

}// namespace openspace











