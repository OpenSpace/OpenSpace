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

#include <modules/base/rendering/renderablecutplane.h>

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
  constexpr openspace::properties::Property::PropertyInfo PlaneEquationInfo = {
    "PlaneEquation",
      "Hejhejs",
      "text",
    // @VISIBILITY(2.25)
    openspace::properties::Property::Visibility::User
  };
  constexpr openspace::properties::Property::PropertyInfo ScalingUnitInfo = {
    "ScalingUnit",
      "Hejhejs",
      "text",
    // @VISIBILITY(2.25)
    openspace::properties::Property::Visibility::User
  };
  struct [[codegen::Dictionary(RenderableCutPlane)]] Parameters {
    // [[codegen::verbatim(FilePathInfo.description)]]
    std::string filePath;
    // [[codegen::verbatim(PlaneEquationInfo.description)]]
    std::optional<double> planeEquation[4];
    // [[codegen::verbatim(ScalingUnitInfo.description)]]
    std::optional<bool> lazyLoading;
    // Axis to slice on
    std::string axis;
    // Value to what axis
    float cutValue;
    // Quantity to color by 
    std::string colorQuantity;
    // Number of meters per data distance unit
    float scaleToMeter;
      
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
    RenderablePlane::initialize();
//    loadTexture();
}

void RenderableCutPlane::initializeGL() {
    ZoneScoped;

    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    loadTexture();
    std::cout << "hej efter load " <<_axis1 << " " << _axis2;
    _axis1 *= _size.value().x;
    _axis2 *= _size.value().y;
    std::cout << "hej efter load " <<_axis1 << " " << _axis2;
    _size = {_axis1,_axis2};
    RenderablePlane::initializeGL();
    
    // TODO: Call shaders when implemented
    _shader = BaseModule::ProgramObjectManager.request(
        "Plane",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "Plane",
                absPath("${MODULE_BASE}/shaders/plane_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/plane_fs.glsl")
            );
        }
    );
    
}

void RenderableCutPlane::deinitializeGL() {

//    BaseModule::TextureManager.release(_texture);
    RenderablePlane::deinitializeGL();
}

void RenderableCutPlane::update(const UpdateData& data) {
    RenderablePlane::update(data);
}

void RenderableCutPlane::bindTexture() {
//    _texture->bind();
}

//void RenderableCutPlane::render(const RenderData& data, RendererTasks& t) {
//    ZoneScoped;
//    RenderablePlane::render(data,t);
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
    
    unsigned int hash = ghoul::hashCRC32File(_filePath);
    
    void* memory = static_cast<void*>(&slices[0]);
    
    
    _texture = BaseModule::TextureManager.request(
        std::to_string(hash),
        [slice = memory]() -> std::unique_ptr<ghoul::opengl::Texture> {
            std::unique_ptr<ghoul::opengl::Texture> texture =
                ghoul::io::TextureReader::ref().loadTexture(
                    slice,
                    sizeof(slice),
                    2
                );
            texture->uploadTexture();
            texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
            texture->purgeFromRAM();
            return texture;
        }
    );
    
    BaseModule::TextureManager.release(t);
    
    // Shape the plane based on the aspect ration of the image
    glm::vec2 textureDim = glm::vec2(_texture->dimensions());
    if (_textureDimensions != textureDim) {
        float aspectRatio = textureDim.x / textureDim.y;
        float planeAspectRatio = _size.value().x / _size.value().y;

        if (std::abs(planeAspectRatio - aspectRatio) >
            std::numeric_limits<float>::epsilon())
        {
            glm::vec2 newSize =
                aspectRatio > 0.f ?
                glm::vec2(_size.value().x * aspectRatio, _size.value().y) :
                glm::vec2(_size.value().x, _size.value().y * aspectRatio);
            _size = newSize;
        }

        _textureDimensions = textureDim;
    }

}



}// namespace openspace











