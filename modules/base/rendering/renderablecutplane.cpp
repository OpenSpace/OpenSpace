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
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/texture.h>
#include <fstream>
#include <optional>
#include <iostream>



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
_filePath(_filePath)
{
  const Parameters p = codegen::bake<Parameters>(dictionary);
  addProperty(_blendMode);
  // FILE PATH
  _filePath = absPath(p.filePath).string();
    std::cout << "FILEPATH: " << &_filePath <<"\n";
    


  setRenderBin(Renderable::RenderBin::Opaque);
}

bool RenderableCutPlane::isReady() const {
    return RenderablePlane::isReady();
}

//void RenderableCutPlane::initialize() {
//  RenderablePlane::initialize();
//}

void RenderableCutPlane::initializeGL() {
    RenderablePlane::initializeGL();
    createPlane();
}

void RenderableCutPlane::deinitializeGL() {
    RenderablePlane::deinitializeGL();
}

void RenderableCutPlane::update(const UpdateData& data) {
    RenderablePlane::update(data);
    createPlane();
}

void RenderableCutPlane::createPlane()
    {
        const GLfloat sizeX = _size.value().x;
        const GLfloat sizeZ = _size.value().y;
//        data = readdata();
//      Create a 5x5 vector using a vector of vectors
//        const float sizeX = 10;
//        const float sizeZ = 7;
        std::vector<std::vector<float>> dataplane(10, std::vector<float>(7));
           // Initialize the vector
//          float count = 0;
//          for (int i = 0; i < sizeX; ++i) {
//              for (int j = 0; j < sizeZ; ++j) {
//                  dataplane[i][j] = count;
//                  count = count + 0.1;
//              }
//          }
//          // Print the dataplane vector
//          for (int i = 0; i < sizeX; ++i) {
//              for (int j = 0; j < sizeZ; ++j) {
//                  std::cout << dataplane[i][j] << " ";
//              }
//              std::cout << std::endl;
//          }
          const GLfloat vertexData[] = {
              //   x       y    z    w    s    t
              -sizeX, -sizeZ, 0.f, 0.f, 0.f, 0.f,
               sizeX,  sizeZ, 0.f, 0.f, 1.f, 1.f,
              -sizeX,  sizeZ, 0.f, 0.f, 0.f, 1.f,
              -sizeX, -sizeZ, 0.f, 0.f, 0.f, 0.f,
               sizeX, -sizeZ, 0.f, 0.f, 1.f, 0.f,
               sizeX,  sizeZ, 0.f, 0.f, 1.f, 1.f,
          };

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
       glBindVertexArray(0);
        // Remember to free the dynamically allocated memory
        //delete[] dataArray;
    }

} // namespace openspace


