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

#ifndef __OPENSPACE_MODULE_POSTPROCESSING___POSTPROCESSING_RENDERER___H__
#define __OPENSPACE_MODULE_POSTPROCESSING___POSTPROCESSING_RENDERER___H__

#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/camera/camera.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <ghoul/opengl/texture.h>

namespace openspace {

class PostprocessingRenderer : public properties::PropertyOwner {

public:
    PostprocessingRenderer();
    void initialize(const ghoul::Dictionary&);
    void initializeGL();
    void deinitializeGL();
    void update();
    void updateResolution();
    void render(Camera* camera);
    GLuint framebuffer();
    bool isEnabled();
    void bindFramebuffer();

private:
    
    glm::ivec2 _resolution = glm::ivec2(0);
    glm::ivec2 _resolutionLow = glm::ivec2(0);
    float _aspectRatio = 16.0f/9.0f;
    
    GLuint _screenQuad;
    GLuint _vertexPositionBuffer;

    std::unique_ptr<ghoul::opengl::ProgramObject> _lensflarePassProgram;
    UniformCache(mainColorTexture) _lensflarePassUniformCache;
    std::unique_ptr<ghoul::opengl::ProgramObject> _lensflareFeatureProgram;
    UniformCache(
        mainColorTexture, aspectRatio, chromaticDistortion, colorGradientAlpha, dustAlpha, dustTexture, ghostsAlpha,
        ghostsDispersal, ghostsNumber, ghostsWeight, gradientTexture, haloAlpha, haloWidth, haloWeight, starAlpha,
        starRotation, starTexture, thesholdBias, thesholdScale
        ) _lensflareFeatureUniformCache;
    
    std::unique_ptr<ghoul::opengl::ProgramObject> _bloomProgram;
    UniformCache(mainColorTexture, threshold) _bloomUniformCache;
    
    std::unique_ptr<ghoul::opengl::ProgramObject> _blurProgram;
    UniformCache(mainColorTexture, blurDirection, blurMagnitude) _blurUniformCache;
    
    std::unique_ptr<ghoul::opengl::ProgramObject> _blendProgram;
    UniformCache(mainColorTexture, effectsColorTexture, exposure, gamma) _blendUniformCache;

    GLuint _sceneFramebuffer;
    GLuint _sceneTexture;
    GLuint _postFramebuffer;
    static const int _postTexturesNumber = 3;
    GLuint _postTextures[_postTexturesNumber];

    std::unique_ptr<ghoul::opengl::Texture> _lensflareGradientTexture;
    std::unique_ptr<ghoul::opengl::Texture> _lensflareDustTexture;
    std::unique_ptr<ghoul::opengl::Texture> _lensflareStarTexture;
    
    properties::BoolProperty _enableLensFlareP;
    properties::FloatProperty _chromaticDistortionP;
    properties::FloatProperty _colorGradientAlphaP;
    properties::IntProperty _downsampleP;
    properties::FloatProperty _dustAlphaP;
    properties::FloatProperty _ghostsAlphaP;
    properties::FloatProperty _ghostsDispersalP;
    properties::IntProperty _ghostsNumberP;
    properties::FloatProperty _ghostsWeightP;
    properties::FloatProperty _haloAlphaP;
    properties::FloatProperty _haloWidthP;
    properties::FloatProperty _haloWeightP;
    properties::FloatProperty _starAlphaP;
    properties::FloatProperty _thesholdBiasP;
    properties::FloatProperty _thesholdScaleP;
    
    properties::BoolProperty _enableBloomP;
    properties::FloatProperty _bloomThresholdP;
    
    properties::IntProperty _blurPassNumberP;
    properties::FloatProperty _blurMagnitudeP;
    
    properties::FloatProperty _exposureP;
    properties::FloatProperty _gammaP;

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_POSTPROCESSING___POSTPROCESSING_RENDERER___H__
