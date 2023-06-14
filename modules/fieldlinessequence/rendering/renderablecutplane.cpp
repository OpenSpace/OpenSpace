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
#include <modules/fieldlinessequence/util/gameravolumeslicer.cpp>

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

namespace {
//  constexpr std::string_view _loggerCat = “RenderableCutPlane”;
  constexpr openspace::properties::Property::PropertyInfo FilePathInfo = {
    "FilePath",
    "Hejhejs",
    "text",
    // @VISIBILITY(2.25)
    openspace::properties::Property::Visibility::User
  };
  //constexpr openspace::properties::Property::PropertyInfo PlaneEquationInfo = {
  //  "PlaneEquation",
  //    "Hejhejs",
  //    "text",
  //  // @VISIBILITY(2.25)
  //  openspace::properties::Property::Visibility::User
  //};

constexpr openspace::properties::Property::PropertyInfo ColorQuantityInfo = {
    "ColorQuantity",
    "Quantity to Color By",
    "Quantity used to color lines if the 'By Quantity' color method is selected",
    // @VISIBILITY(2.67)
    openspace::properties::Property::Visibility::User
};
  struct [[codegen::Dictionary(RenderableCutPlane)]] Parameters {
    // [[codegen::verbatim(FilePathInfo.description)]]
    std::string filePath;
    // Axis to slice on
    std::string axis;
    // Value to what axis
    float cutValue;
    // Quantity to color by 
    std::string colorQuantity;
    // Number of meters per data distance unit
   // float scaleToMeter;
      
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
_filePath(FilePathInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);
    addProperty(_blendMode);
    
    _axis = p.axis;
    _cutValue = p.cutValue;
    _colorQuantity = p.colorQuantity;
    
    setRenderBin(Renderable::RenderBin::Opaque);
}

bool RenderableCutPlane::isReady() const {
    return RenderablePlane::isReady();
}

void RenderableCutPlane::initialize() {
//    _transferFunction = std::make_unique<TransferFunction>(
//        absPath(_colorTablePaths[0]).string()
//    );
}

void RenderableCutPlane::initializeGL() {
    ZoneScoped;

    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    loadTexture();
    _axis1 *= _size.value().x;
    _axis2 *= _size.value().y;
    std::cout << "hej efter load " <<_axis1 << " " << _axis2;
    _size = {_axis1,_axis2};
    RenderablePlane::initializeGL();
    
    // TODO: Call shaders when implemented
    _shader = BaseModule::ProgramObjectManager.request(
        "CutPlane",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "CutPlane",
                absPath("${MODULE_BASE}/shaders/cutplane_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/cutplane_fs.glsl")
            );
        }
    );
}

void RenderableCutPlane::deinitializeGL() {
 ZoneScoped;
    
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    BaseModule::ProgramObjectManager.release(
        "CutPlane",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _shader = nullptr;
}

void RenderableCutPlane::render(const RenderData& data, RendererTasks&) {
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

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    bindTexture();
    defer { unbindTexture(); };

    _shader->setUniform("texture1", unit);

//    _shader->setUniform("multiplyColor", _multiplyColor);

//    bool additiveBlending = (_blendMode == static_cast<int>(BlendMode::Additive));
//    if (additiveBlending) {
//        glDepthMask(false);
//        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
//    }
    glDisable(GL_CULL_FACE);

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

//    if (additiveBlending) {
//        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
//        glDepthMask(true);
//    }

    _shader->deactivate();
   
}

void RenderableCutPlane::update(const UpdateData& data) {
    RenderablePlane::update(data);
}

//void RenderableCutPlane::bindTexture() {
//    ghoul::opengl::Texture* rawTexture = _texture.get();
//    ghoul_assert(_texture, "Texture must be loaded before binding");
//    rawTexture->bind();
//}


void RenderableCutPlane::loadTexture()
{
    // Create a 2D vector with dimensions 50x40
    std::vector<std::vector<float>> vec1(50, std::vector<float>(40));
    // Create a 2D vector with dimensions 50x40
    std::vector<std::vector<float>> vec2(50, std::vector<float>(40, 10));
    // Create vector that contains the outer values of the volume
    std::vector<std::vector<int>> volumeMinMax =
    {
        {-20, 30}, //Dim x går från pos -20 till 30 och är därmed 51 lång
        {-25, 15}, //Dim y går från pos -25 till 15 och är därmed 41 lång
        {-20, 30}, //Dim z går från pos -20 till 30 och är därmed 51 lång
    };
    // Create a vector with the names of the data
    std::vector<std::string> dataNames = { "multicolor", "singlecolor" };
    //Create vector that contains both data slices
    std::vector<std::vector<std::vector<float>>> slices;
    
    // Auto-fill the vector with numbers from 1 to 50 in a cyclic manner
    int count = 1;
    for (int i = 0; i < 50; i++) {
        for (int j = 0; j < 40; j++) {
            vec1[i][j] = count++;
            if (count > 40) {
                count = 1;  // Reset count to 1 if it exceeds 50
            }
        }
    }
    
    slices.push_back(vec1);
    slices.push_back(vec2);
    
    int axisIndex = 2;
      std::transform(_axis.begin(), _axis.end(), _axis.begin(),
                      [](unsigned char c) { return std::tolower(c); });
    
    // Set the scaling of the coordinates
    if (_axis.compare("x") == 0) axisIndex = 0;
    else if (_axis.compare("y") == 0) axisIndex = 1;

    // Copy elements except the second element
    std::copy_if(volumeMinMax.begin(), volumeMinMax.end(), std::back_inserter(_axisDim),
           [&volumeMinMax, axisIndex](const std::vector<int>& vec) { return &vec != &volumeMinMax[axisIndex]; });
    
     _axis1 = abs(_axisDim[0][0]) + abs(_axisDim[0][1]);
     _axis2 = abs(_axisDim[1][0]) + abs(_axisDim[1][1]);
    
    _texture = createFloatTexture(slices[0]);
    
//    CREATE TEXTURE OBJECT
//    unsigned int hash = ghoul::hashCRC32File(_filePath);
//    void* memory = static_cast<void*>(&slices[0]);
//
//    _texture = BaseModule::TextureManager.request(ss
//        std::to_string(hash),
//        [slice = memory]() -> std::unique_ptr<ghoul::opengl::Texture> {
//            std::unique_ptr<ghoul::opengl::Texture> texture =
//                ghoul::io::TextureReader::ref().loadTexture(
//                    slice,
//                    sizeof(slice),
//                    2
//                );
//            texture->uploadTexture();
//            texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
//            texture->purgeFromRAM();
//            return texture;
//        }
//    );

}
// Function to create a floating-point texture from a double vector
std::unique_ptr<ghoul::opengl::Texture> RenderableCutPlane::createFloatTexture(const std::vector<std::vector<float>>& data)
{
    // Convert the 2D vector to a flat float array
    std::vector<float> flatData; //en slice men platt
    for (const auto& row : data) {
        flatData.insert(flatData.end(), row.begin(), row.end());
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
    texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
    texture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToEdge);
    return texture;
}


}// namespace openspace











