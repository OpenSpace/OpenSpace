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
//#include <modules/fieldlinessequence/fieldlinessequencemodule.h>

#include <modules/fieldlinessequence/ext/HighFive/include/highfive/bits/H5Slice_traits.hpp>
#include <modules/fieldlinessequence/ext/HighFive/include/highfive/H5DataSet.hpp>
#include <modules/fieldlinessequence/ext/HighFive/include/highfive/H5Object.hpp>
#include <modules/fieldlinessequence/ext/HighFive/include/highfive/H5Group.hpp>
#include <cstring>
#include <cmath>


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
    float value;
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

// namespace openspace




void RenderableCutPlane::readHdfFile(std::string pathToHdf5File) {
    HighFive::File file(pathToHdf5File, HighFive::File::ReadOnly);
    
    std::vector<std::vector<std::vector<float>>> vCoordDataX;
    std::vector<std::vector<std::vector<float>>> vCoordDataY;
    std::vector<std::vector<std::vector<float>>> vCoordDataZ;
    
    std::vector<std::vector<std::vector<float>>> extraData;
    
    const HighFive::DataSet dsX = file.getDataSet("/X");
    const HighFive::DataSet dsY = file.getDataSet("/Y");
    const HighFive::DataSet dsZ = file.getDataSet("/Z");
    
    dsX.read(vCoordDataX);
    dsY.read(vCoordDataY);
    dsZ.read(vCoordDataZ);
    
    int all_z = vCoordDataZ.size(); // number of matrices
    int all_y = vCoordDataY[0].size(); // number of cols for each matrix
    int all_x = vCoordDataX[0][0].size(); // number of rows for each
    
    //    TODO gör alla temp_coord to glm::vec3
    _volumeCoordinates.resize(all_z, std::vector<std::vector<std::vector<float>>>(all_y, std::vector<std::vector<float>>(all_x)));
    
    for (int z = 0; z < all_z; z++) { //z ---- Every z value that exists
        for (int y = 0; y < all_y; y++) { //y ---- Every y value that exists on (z)
            for (int x = 0; x < all_x; x++) { //x ---- Every x value that exists on (z, y)
                
                std::vector<float> temp_coord;
                
                temp_coord.push_back(vCoordDataX[0][0][x]); //First matrix, first column, all rows
                temp_coord.push_back(vCoordDataY[0][y][0]); //First matrix, all columns, first rows
                temp_coord.push_back(vCoordDataZ[z][0][0]); //All matrices, first column, all rows
                
                _volumeCoordinates[z][y][x] = temp_coord;
            }
        }
    }
    
    const HighFive::Group timeStep = file.getGroup("/Step#1");
    _extraQuantatiesNames = timeStep.listObjectNames();
    
    for (int q = 0; q < _extraQuantatiesNames.size(); q++) {
        std::vector<float> temp;
        std::string quantityName = _extraQuantatiesNames[q];
        
        const HighFive::DataSet dsExtraQ = timeStep.getDataSet(quantityName);
        dsExtraQ.read(extraData);
        
        _extraQuantaties.push_back(extraData);
    }
    
}

void RenderableCutPlane::slicer(char axis, float value) {
    
    int all_z = _volumeCoordinates.size(); // number of matrices
    int all_y = _volumeCoordinates[0].size(); // number of cols for each matrix
    int all_x = _volumeCoordinates[0][0].size(); // number of rows for each
    
    int all_extraQ = _extraQuantaties.size(); // number of datasets
    int all_zq = _extraQuantaties[0].size(); // number of matrices
    int all_yq = _extraQuantaties[0][0].size(); // number of cols for each matrix
    int all_xq = _extraQuantaties[0][0][0].size(); // number of rows for each
    
    char axisInput = char(tolower(axis));
    
    std::vector<float> firstPoint = _volumeCoordinates.front().front().front();
    std::vector<float> lastPoint = _volumeCoordinates.back().back().back();
    
    //     These should probably be class members
    float firstX = firstPoint[0];
    float lastX = lastPoint[0];
    float firstY = firstPoint[1];
    float lastY = lastPoint[1];
    float firstZ = firstPoint[2];
    float lastZ = lastPoint[2];
    
    if (axisInput == 'x') {
        int index = abs(firstX) + std::round(value);; //start value + desired cutplane value
        
        for (size_t q = 0; q < all_extraQ; q++) {
            std::vector<std::vector<float>> temp1(all_zq, std::vector<float>(all_yq)); //make vec2
            std::vector<std::vector<float>> temp2(all_zq, std::vector<float>(all_yq)); //make vec2
            
            for (size_t z = 0; z < all_zq; z++) {
                for (size_t y = 0; y < all_yq; y++) {
                    temp1[z][y] = _extraQuantaties[q][z][y][index - 1]; // Gives the cutplane for all d1
                    temp2[z][y] = _extraQuantaties[q][z][y][index]; // Gives the cutplane for all d2
                }
            }
            _slicedDataBP.push_back(temp1);
            _slicedDataAP.push_back(temp2);
        }
    }
    else if (axisInput == 'y') {
        int index = abs(firstY) + std::round(value);; //start value + desired cutplane value
        
        for (size_t q = 0; q < all_extraQ; q++) {
            std::vector<std::vector<float>> temp1(all_zq, std::vector<float>(all_xq)); //make vec2
            std::vector<std::vector<float>> temp2(all_zq, std::vector<float>(all_xq)); //make vec2
            
            for (size_t z = 0; z < all_zq; z++) {
                for (size_t x = 0; x < all_xq; x++) {
                    temp1[z][x] = _extraQuantaties[q][z][index - 1][x]; // Gives the cutplane for all d1
                    temp2[z][x] = _extraQuantaties[q][z][index][x]; // Gives the cutplane for all d2
                }
            }
            _slicedDataBP.push_back(temp1);
            _slicedDataAP.push_back(temp2);
        }
    }
    else if (axisInput == 'z') {
        int index = abs(firstZ) + std::round(value); //start value + desired cutplane value
        
        for (size_t q = 0; q < all_extraQ; q++) {
            std::vector<std::vector<float>> temp1(all_yq, std::vector<float>(all_xq)); //make vec2
            std::vector<std::vector<float>> temp2(all_yq, std::vector<float>(all_xq)); //make vec2
            for (size_t y = 0; y < all_yq; y++) {
                for (size_t x = 0; x < all_xq; x++) {
                    temp1[y][x] = _extraQuantaties[q][index - 1][y][x]; // Gives the cutplane for all d1
                    temp2[y][x] = _extraQuantaties[q][index][y][x]; // Gives the cutplane for all d2
                }
            }
            _slicedDataBP.push_back(temp1);
            _slicedDataAP.push_back(temp2);
        }
        
    }
    else {
        std::cout << "Invalid axis input";
        return;
    }
}

void RenderableCutPlane::interpolator(float value) {
    
    int size_1 = _slicedDataAP[0].size();
    int size_2 = _slicedDataAP[0][0].size();
    
    double integerPart;
    float value_shifted = value + 0.5; // Caused by the shift 0.5 from grid to data (cell centered data)
    float decimalPart = modf(value_shifted, &integerPart);
    
    std::cout << "value: " << value << ", shifted value: " << value_shifted << ", decimal: " << decimalPart << "\n";
    
    if (decimalPart == 1) { // Sliced right on datapoint
        _interpolatedData = _slicedDataBP;
    }
    else {
        for (size_t q = 0; q < _extraQuantaties.size(); q++) {
            std::vector<std::vector<float>> temp(size_1, std::vector<float>(size_2)); //make vec2
            
            for (size_t i = 0; i < size_1; i++) {
                for (size_t j = 0; j < size_2; j++) {
                    float d1 = _slicedDataBP[q][i][j];
                    float d2 = _slicedDataAP[q][i][j];
                    
                    float v = (decimalPart - 1) * d1 + decimalPart * d2; // Interpolation algorithm : v = (t-1)*d1 + t*d2
                    
                    temp[i][j] = v;
                }
            }
            _interpolatedData.push_back(temp);
        }
    }
}



}
