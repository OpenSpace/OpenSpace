/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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
#ifndef __OPENSPACE_MODULE_BASE___SPACECRAFTCAMERAPLANE___H__
#define __OPENSPACE_MODULE_BASE___SPACECRAFTCAMERAPLANE___H__

#include <openspace/util/updatestructures.h>
#include <ghoul/opengl/texture.h>

namespace openspace {

class TransferFunction;
class SpacecraftCameraPlane {

public:
    SpacecraftCameraPlane(/*glm::vec2 centerPixel, float scaleFactor,*/ double moveDistance);

    void render(const RenderData& data, ghoul::opengl::Texture& imageryTexture,
                TransferFunction* lut, const glm::dvec3& sunPositionWorld,
                const float& planeOpacity, const float& contrastValue,
                const float& gammaValue, const bool& enableBorder,
                const bool& enableFrustum, const glm::vec2& currentCenterPixel,
                const float& currentScale, const float& multipleImageryOffset, const bool& isCoronaGraph);
    void update();
    void createPlaneAndFrustum(const double& moveDistance);
    // TODO(mnoven) : Pre process image, no need to set uniforms
    void setUniforms();
    bool isReady();
    bool destroy();

    const glm::vec3& normal() const { return _normal; };
    const glm::dvec3& worldPosition() const { return _position; }
    const glm::dmat4& worldRotation() const { return _rotation; }

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

#endif // __OPENSPACE_MODULE_BASE___RENDERABLESPACECRAFTCAMERAPLANE___H__
