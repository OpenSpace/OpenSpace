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

#ifndef __OPENSPACE_MODULE_SOLARBROWSING___SPACECRAFTCAMERAPLANE___H__
#define __OPENSPACE_MODULE_SOLARBROWSING___SPACECRAFTCAMERAPLANE___H__

#include <openspace/util/updatestructures.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>

#include <memory>

namespace ghoul::opengl { class Texture; }

namespace openspace {

class TransferFunction;

class SpacecraftCameraPlane {
public:
    SpacecraftCameraPlane(double moveDistance);

    void render(const RenderData& data, ghoul::opengl::Texture& imageryTexture,
        TransferFunction* lut, const glm::dvec3& sunPositionWorld, float planeOpacity,
        float multiplierValue, float contrastValue, float gammaValue, bool enableBorder,
        bool enableFrustum, const glm::vec2& currentCenterPixel, float currentScale,
        float multipleImageryOffset, bool isCoronaGraph);

    void update();
    void createPlaneAndFrustum(double moveDistance);
    // TODO(mnoven) : Pre process image, no need to set uniforms
    //void setUniforms();
    bool isReady();
    bool destroy();

    const glm::vec3& normal() const;
    const glm::dvec3& worldPosition() const;
    const glm::dmat4& worldRotation() const;

    //glm::dvec3 _planePosSpacecraftRefFrame;
    //glm::dmat4 _sunToSpacecraftTransform;

private:
    std::unique_ptr<ghoul::opengl::ProgramObject> _frustumShader;
    std::unique_ptr<ghoul::opengl::ProgramObject> _planeShader;

    GLuint _frustum;
    GLuint _frustumPositionBuffer;
    GLuint _quad;
    GLuint _vertexPositionBuffer;

//    glm::dvec3 _spacecraftPosition;
 //   glm::dmat4 _spacecraftRotation.
    glm::dvec3 _position;
    glm::dmat4 _rotation;
    glm::vec3 _normal;

    //glm::dvec2 _centerPixel;
    double _gaussianMoveFactor;
    float _size;
    //float _scaleFactor;

    void createFrustum();
    void createPlane();
    bool initialize();
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOLARBROWSING___SPACECRAFTCAMERAPLANE___H__
