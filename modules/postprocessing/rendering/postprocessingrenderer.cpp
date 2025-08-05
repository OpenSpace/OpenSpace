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

#include <modules/postprocessing/rendering/postprocessingrenderer.h>

#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

namespace {

    constexpr const char* _loggerCat = "Postprocessing";
    
    constexpr const std::array<const char*, 1> lensflarePassUniformNames = { "mainColorTexture" };
    constexpr const std::array<const char*, 19> lensflareFeatureUniformNames = {
        "mainColorTexture", "aspectRatio", "chromaticDistortion", "colorGradientAlpha", "dustAlpha",
        "dustTexture", "ghostsAlpha", "ghostsDispersal", "ghostsNumber", "ghostsWeight",
        "gradientTexture", "haloAlpha", "haloWidth", "haloWeight", "starAlpha",
        "starRotation", "starTexture", "thesholdBias", "thesholdScale"
    };
    constexpr const std::array<const char*, 3> blurUniformNames = { "mainColorTexture", "blurDirection", "blurMagnitude" };
    constexpr const std::array<const char*, 4> blendUniformNames = { "mainColorTexture", "effectsColorTexture", "exposure", "gamma" };
    constexpr const std::array<const char*, 2> bloomUniformNames = { "mainColorTexture", "threshold" };
    
    constexpr const char* PassthroughVertexShaderPath = "${MODULES}/postprocessing/shaders/pass_vs.glsl";
    constexpr const char* PassthroughFragmentShaderPath = "${MODULES}/postprocessing/shaders/pass_fs.glsl";
    constexpr const char* BloomShaderPath = "${MODULES}/postprocessing/shaders/bloom_fs.glsl";
    constexpr const char* LensflareShaderPath = "${MODULES}/postprocessing/shaders/lensflare_fs.glsl";
    constexpr const char* BlurShaderPath = "${MODULES}/postprocessing/shaders/blur_fs.glsl";
    constexpr const char* BlendShaderPath = "${MODULES}/postprocessing/shaders/blend_fs.glsl";
    constexpr const char* LenscolorImagePath = "${MODULES}/postprocessing/textures/lenscolor.png";
    constexpr const char* LensdirtImagePath = "${MODULES}/postprocessing/textures/lensdirt.png";
    constexpr const char* LensstarImagePath = "${MODULES}/postprocessing/textures/lensstar.png";
    
    constexpr openspace::properties::Property::PropertyInfo EnableLensFlareInfo = {
        "enableLensFlare",
        "Enable Lens Flare",
        ""
    };
    constexpr openspace::properties::Property::PropertyInfo ChromaticDistortionInfo = {
        "chromaticDistortion",
        "Chromatic Distortion",
        "The separation of red, green, and blue channels in lens flare"
    };
    constexpr openspace::properties::Property::PropertyInfo ColorGradientAlphaInfo = {
        "colorGradientAlpha",
        "Color Gradient Alpha",
        "Amount of radial gradient colorization applied globally to lens flare"
    };
    constexpr openspace::properties::Property::PropertyInfo DownsampleInfo = {
        "downsample",
        "Downsample",
        "Amount to downsample the lens flare effects"
    };
    constexpr openspace::properties::Property::PropertyInfo DustAlphaInfo = {
        "dustAlpha",
        "Dust Alpha",
        "Amount of dust and dirt applied globally to lens flare"
    };
    constexpr openspace::properties::Property::PropertyInfo GhostsAlphaInfo = {
        "ghostsAlpha",
        "Ghosts Alpha",
        "Intensity of lens flare internal reflections"
    };
    constexpr openspace::properties::Property::PropertyInfo GhostsDispersalInfo = {
        "ghostsDispersal",
        "Ghosts Dispersal",
        "Distance between lens flare internal reflections"
    };
    constexpr openspace::properties::Property::PropertyInfo GhostsNumberInfo = {
        "ghostsNumber",
        "Ghosts Number",
        "Number of lens flare internal reflections"
    };
    constexpr openspace::properties::Property::PropertyInfo GhostsWeightInfo = {
        "ghostsWeight",
        "Ghosts Weight",
        "Amount of radial falloff applied to lens flare internal reflections"
    };
    constexpr openspace::properties::Property::PropertyInfo HaloAlphaInfo = {
        "haloAlpha",
        "Halo Alpha",
        "Intensity of lens flare halo"
    };
    constexpr openspace::properties::Property::PropertyInfo HaloWidthInfo = {
        "haloWidth",
        "Halo Width",
        "Width of lens flare halo"
    };
    constexpr openspace::properties::Property::PropertyInfo HaloWeightInfo = {
        "haloWeight",
        "Halo Weight",
        "Amount of radial falloff applied to lens flare halo"
    };
    constexpr openspace::properties::Property::PropertyInfo StarAlphaInfo = {
        "starAlpha",
        "Star Alpha",
        "Intensity of lens flare starburst"
    };
    constexpr openspace::properties::Property::PropertyInfo ThresholdBiasInfo = {
        "thresholdBias",
        "Threshold Bias",
        "Bias of threshold that limits input to lens flare"
    };
    constexpr openspace::properties::Property::PropertyInfo ThresholdScaleInfo = {
        "thresholdScale",
        "Threshold Scale",
        "Scale of threshold that limits input to lens flare"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableBloomInfo = {
        "enableBloom",
        "Enable Bloom",
        ""
    };
    constexpr openspace::properties::Property::PropertyInfo BloomThresholdInfo = {
        "bloomThreshold",
        "Bloom threshold",
        "Threshold of the bloom effect"
    };

    constexpr openspace::properties::Property::PropertyInfo BlurPassNumberInfo = {
        "blurPassNumber",
        "Blur Pass Number",
        "Number of blur passes"
    };

    constexpr openspace::properties::Property::PropertyInfo BlurMagnitudeInfo = {
        "blurMagnitude",
        "Blur Magnitude",
        "Spread of blur"
    };

    constexpr openspace::properties::Property::PropertyInfo ExposureInfo = {
        "exposure",
        "Exposure",
        ""
    };
    constexpr openspace::properties::Property::PropertyInfo GammaInfo = {
        "gamma",
        "Gamma",
        ""
    };

} // namespace

namespace openspace {

PostprocessingRenderer::PostprocessingRenderer()
    : properties::PropertyOwner({ "PostprocessingRenderer", "Postprocessing", "Renderer for post-processing effects" })
    , _enableLensFlareP(EnableLensFlareInfo, true)
    , _chromaticDistortionP(ChromaticDistortionInfo, -2.25f, -100.0f, 100.0f)
    , _colorGradientAlphaP(ColorGradientAlphaInfo, 0.5f, 0.0f, 1.0f)
    , _downsampleP(DownsampleInfo, 1.0f, 1.0f, 8.0f)
    , _dustAlphaP(DustAlphaInfo, 0.75f, 0.0f, 1.0f)
    , _ghostsAlphaP(GhostsAlphaInfo, 10.0f, 0.0f, 100.0f)
    , _ghostsDispersalP(GhostsDispersalInfo, 0.7f, 0.0f, 2.0f)
    , _ghostsNumberP(GhostsNumberInfo, 3, 1, 9)
    , _ghostsWeightP(GhostsWeightInfo, 127.5f, 0.0f, 200.0f)
    , _haloAlphaP(HaloAlphaInfo, 0.125f, 0.0f, 10.0f)
    , _haloWidthP(HaloWidthInfo, 0.75f, -1.0f, 1.0f)
    , _haloWeightP(HaloWeightInfo, 0.375f, 0.0f, 2.0f)
    , _starAlphaP(StarAlphaInfo, 9.0f, 0.0f, 10.0f)
    , _thesholdBiasP(ThresholdBiasInfo, -0.6f, -1.0f, 1.0f)
    , _thesholdScaleP(ThresholdScaleInfo, 9.0f, 0.0f, 20.0f)
    , _enableBloomP(EnableBloomInfo, true)
    , _bloomThresholdP(BloomThresholdInfo, 0.75f, 0.0f, 1.0f)
    , _blurPassNumberP(BlurPassNumberInfo, 3, 0, 10)
    , _blurMagnitudeP(BlurMagnitudeInfo, 0.001f, 0.0f, 0.01f)
    , _exposureP(ExposureInfo, 0.0f, 0.0f, 10.0f)
    , _gammaP(GammaInfo, 1.0f, 0.0f, 10.0f)
{
    LDEBUG("PostprocessingRenderer::PostprocessingRenderer");
    
    addProperty(_enableLensFlareP);
    addProperty(_chromaticDistortionP);
    addProperty(_colorGradientAlphaP);
    _downsampleP.onChange([this]{ updateResolution(); });
    addProperty(_downsampleP);
    addProperty(_dustAlphaP);
    addProperty(_ghostsAlphaP);
    addProperty(_ghostsDispersalP);
    addProperty(_ghostsNumberP);
    addProperty(_ghostsWeightP);
    addProperty(_haloAlphaP);
    addProperty(_haloWidthP);
    addProperty(_haloWeightP);
    addProperty(_starAlphaP);
    addProperty(_thesholdBiasP);
    addProperty(_thesholdScaleP);
    
    addProperty(_enableBloomP);
    addProperty(_bloomThresholdP);
    
    addProperty(_blurPassNumberP);
    addProperty(_blurMagnitudeP);

    addProperty(_exposureP);
    addProperty(_gammaP);

}

bool PostprocessingRenderer::isEnabled(){
    bool enabled = false;
    enabled |= _enableBloomP;
    enabled |= _enableLensFlareP;
    return enabled;
}

void PostprocessingRenderer::bindFramebuffer(){
    glBindFramebuffer(GL_FRAMEBUFFER, _sceneFramebuffer);
    GLenum db[1] = { GL_COLOR_ATTACHMENT0 };
    glDrawBuffers(1, db);
}

void PostprocessingRenderer::setSceneTexture(GLuint sceneTexture)
{
    if (sceneTexture != _sceneTexture) {
        _sceneTexture = sceneTexture;
    }
}

GLuint PostprocessingRenderer::framebuffer(){
    return _sceneFramebuffer;
}

void PostprocessingRenderer::initialize(const ghoul::Dictionary&) {
    LDEBUG("PostprocessingRenderer::initialize");
    global::screenSpaceRootPropertyOwner->addPropertySubOwner(this);
}

void PostprocessingRenderer::initializeGL() {
    LDEBUG("PostprocessingRenderer::initializeGL");

    const GLfloat vertexData[] = {
        // x     y
        -1.f, -1.f,
         1.f,  1.f,
        -1.f,  1.f,
        -1.f, -1.f,
         1.f, -1.f,
         1.f,  1.f,
    };

    glGenVertexArrays(1, &_screenQuad);
    glBindVertexArray(_screenQuad);

    glGenBuffers(1, &_vertexPositionBuffer);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertexData), vertexData, GL_STATIC_DRAW);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 2, nullptr);
    glEnableVertexAttribArray(0);

    // Post processing framebuffer
    glGenTextures(1, &_sceneTexture);
    glGenFramebuffers(1, &_sceneFramebuffer);
    
    // Lens flare framebuffers
    glGenTextures(_postTexturesNumber, _postTextures);
    glGenFramebuffers(1, &_postFramebuffer);
    
    updateResolution();
    
    glBindFramebuffer(GL_FRAMEBUFFER, _sceneFramebuffer);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        _sceneTexture,
        0
    );
    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Post processing framebuffer is not complete");
    }

    glBindFramebuffer(GL_FRAMEBUFFER, _postFramebuffer);
    for (int i = 0; i < _postTexturesNumber; i++ ){
        glFramebufferTexture2D(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0+i,
            GL_TEXTURE_2D,
            _postTextures[i],
            0
        );
    }
    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Lensflare framebuffer is not complete");
    }

    _bloomProgram = ghoul::opengl::ProgramObject::Build(
        "Bloom",
        absPath(PassthroughVertexShaderPath),
        absPath(BloomShaderPath)
    );
    ghoul::opengl::updateUniformLocations(*_bloomProgram, _bloomUniformCache, bloomUniformNames);

    _lensflarePassProgram = ghoul::opengl::ProgramObject::Build(
        "Lens Flare Passthrough",
        absPath(PassthroughVertexShaderPath),
        absPath(PassthroughFragmentShaderPath)
    );
    ghoul::opengl::updateUniformLocations(*_lensflarePassProgram, _lensflarePassUniformCache, lensflarePassUniformNames);
    
    _lensflareFeatureProgram = ghoul::opengl::ProgramObject::Build(
        "Lens Flare Features",
        absPath(PassthroughVertexShaderPath),
        absPath(LensflareShaderPath)
    );
    ghoul::opengl::updateUniformLocations(*_lensflareFeatureProgram, _lensflareFeatureUniformCache, lensflareFeatureUniformNames);
    
    _blurProgram = ghoul::opengl::ProgramObject::Build(
        "Lens Flare Blur",
        absPath(PassthroughVertexShaderPath),
        absPath(BlurShaderPath)
    );
    ghoul::opengl::updateUniformLocations(*_blurProgram, _blurUniformCache, blurUniformNames);
    
    _blendProgram = ghoul::opengl::ProgramObject::Build(
        "Lens Flare Blend",
        absPath(PassthroughVertexShaderPath),
        absPath(BlendShaderPath)
    );
    ghoul::opengl::updateUniformLocations(*_blendProgram, _blendUniformCache, blendUniformNames);

    _lensflareGradientTexture = ghoul::io::TextureReader::ref().loadTexture( absPath(LenscolorImagePath), 2 );
    if (_lensflareGradientTexture) {
        LDEBUG(std::format("Loaded texture from '{}'", absPath(LenscolorImagePath) ));
        _lensflareGradientTexture->uploadTexture();
    }
    _lensflareDustTexture = ghoul::io::TextureReader::ref().loadTexture( absPath(LensdirtImagePath), 2 );
    if (_lensflareDustTexture) {
        LDEBUG(std::format("Loaded texture from '{}'", absPath(LensdirtImagePath) ));
        _lensflareDustTexture->uploadTexture();
    }
    _lensflareStarTexture = ghoul::io::TextureReader::ref().loadTexture( absPath(LensstarImagePath), 2 );
    if (_lensflareStarTexture) {
        LDEBUG(std::format("Loaded texture from '{}'", absPath(LensstarImagePath) ));
        _lensflareStarTexture->uploadTexture();
    }
    
}

void PostprocessingRenderer::deinitializeGL() {
    LDEBUG("PostprocessingRenderer::deinitializeGL");
    glDeleteTextures(1, &_sceneTexture);
    glDeleteFramebuffers(1, &_sceneFramebuffer);
    
    glDeleteTextures(_postTexturesNumber, _postTextures);
    glDeleteFramebuffers(1, &_postFramebuffer);
    
    glDeleteBuffers(1, &_vertexPositionBuffer);
    glDeleteVertexArrays(1, &_screenQuad);
}

void PostprocessingRenderer::updateResolution(){
    _resolution = global::renderEngine->renderingResolution();
    _resolutionLow = glm::ivec2( static_cast<float>(_resolution.x) / _downsampleP, static_cast<float>(_resolution.y) / _downsampleP );
    _aspectRatio = static_cast<float>(_resolution.x) / static_cast<float>(_resolution.y);
    
    glBindTexture(GL_TEXTURE_2D, _sceneTexture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, _resolution.x, _resolution.y, 0, GL_RGBA, GL_UNSIGNED_BYTE, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    
    for (int i = 0; i < _postTexturesNumber; i++ ){
        glBindTexture(GL_TEXTURE_2D, _postTextures[i]);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, _resolutionLow.x, _resolutionLow.y, 0, GL_RGBA, GL_UNSIGNED_BYTE, nullptr);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    }
}

void PostprocessingRenderer::update() {

    bool resolutionIsDirty = _resolution != global::renderEngine->renderingResolution();
    if (resolutionIsDirty){
        updateResolution();
    }

    if (_enableBloomP){

        if (_bloomProgram->isDirty()) {
            _bloomProgram->rebuildFromFile();

            ghoul::opengl::updateUniformLocations(
                *_bloomProgram,
                _bloomUniformCache,
                bloomUniformNames
            );
        }
    
    }

    if (_enableLensFlareP){

        if (_lensflarePassProgram->isDirty()) {
            _lensflarePassProgram->rebuildFromFile();

            ghoul::opengl::updateUniformLocations(
                *_lensflarePassProgram,
                _lensflarePassUniformCache,
                lensflarePassUniformNames
            );
        }
    
    }
    
    if (_enableBloomP || _enableLensFlareP){
        
        if (_blurProgram->isDirty()) {
            _blurProgram->rebuildFromFile();

            ghoul::opengl::updateUniformLocations(
                *_blurProgram,
                _blurUniformCache,
                blurUniformNames
            );
        }
        
        if (_blendProgram->isDirty()) {
            _blendProgram->rebuildFromFile();

            ghoul::opengl::updateUniformLocations(
                *_blendProgram,
                _blendUniformCache,
                blendUniformNames
            );
        }
        
    }
    
}

void PostprocessingRenderer::render(Camera* camera) {

    if (isEnabled()){
        
        GLint defaultFbo;
        glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);
        
        int bloomTex = 1;
        int lensflareTex = 2;
        int outputTex = 0;
        
        // if only rendering one or the other, skip blend step, so render to output texture
        if (_enableBloomP && !_enableLensFlareP) bloomTex = outputTex;
        if (_enableLensFlareP && !_enableBloomP) lensflareTex = outputTex;
        
        if (_enableBloomP){
            
            // bloom
        
            glBindFramebuffer(GL_FRAMEBUFFER, _postFramebuffer);
            glViewport(0,0,_resolutionLow.x,_resolutionLow.y);
            glDrawBuffer(GL_COLOR_ATTACHMENT0 + bloomTex);
            _bloomProgram->activate();

            ghoul::opengl::TextureUnit mainColorTextureUnit;
            mainColorTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _sceneTexture);
            _bloomProgram->setUniform(_bloomUniformCache.mainColorTexture, mainColorTextureUnit);

            _bloomProgram->setUniform(_bloomUniformCache.threshold, _bloomThresholdP );

            glBindVertexArray(_screenQuad);
            glDrawArrays(GL_TRIANGLES, 0, 6);
            glBindVertexArray(0);

            _bloomProgram->deactivate();
            
        }

        if (_enableLensFlareP){
        
            // lens flare features

            glBindFramebuffer(GL_FRAMEBUFFER, _postFramebuffer);
            glViewport(0,0,_resolutionLow.x,_resolutionLow.y);
            glDrawBuffer(GL_COLOR_ATTACHMENT0 + lensflareTex);
            _lensflareFeatureProgram->activate();

            ghoul::opengl::TextureUnit mainColorTextureUnit;
            mainColorTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _sceneTexture);
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.mainColorTexture, mainColorTextureUnit);

            ghoul::opengl::TextureUnit dustUnit;
            dustUnit.activate();
            _lensflareDustTexture->bind();
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.dustTexture, dustUnit );

            ghoul::opengl::TextureUnit gradientUnit;
            gradientUnit.activate();
            _lensflareGradientTexture->bind();
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.gradientTexture, gradientUnit );

            ghoul::opengl::TextureUnit starUnit;
            starUnit.activate();
            _lensflareStarTexture->bind();
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.starTexture, starUnit );
            
            glm::dvec3 cameraViewDirection = camera->viewDirectionWorldSpace();
            float starRotation = cameraViewDirection.x + cameraViewDirection.y + cameraViewDirection.z;
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.starRotation, starRotation );

            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.aspectRatio, _aspectRatio );
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.chromaticDistortion, _chromaticDistortionP );
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.colorGradientAlpha, _colorGradientAlphaP );
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.dustAlpha, _dustAlphaP );
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.ghostsAlpha, _ghostsAlphaP );
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.ghostsDispersal, _ghostsDispersalP );
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.ghostsNumber, _ghostsNumberP );
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.ghostsWeight, _ghostsWeightP );
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.haloAlpha, _haloAlphaP );
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.haloWidth, _haloWidthP );
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.haloWeight, _haloWeightP );
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.starAlpha, _starAlphaP );
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.thesholdBias, _thesholdBiasP );
            _lensflareFeatureProgram->setUniform(_lensflareFeatureUniformCache.thesholdScale, _thesholdScaleP );

            glBindVertexArray(_screenQuad);
            glDrawArrays(GL_TRIANGLES, 0, 6);
            glBindVertexArray(0);

            _lensflareFeatureProgram->deactivate();

        }
        
        if (_enableBloomP && _enableLensFlareP) {
        
            // blend effects with original texture
            
            glBindFramebuffer(GL_FRAMEBUFFER, _postFramebuffer);
            glViewport(0,0,_resolutionLow.x,_resolutionLow.y);
            glDrawBuffer(GL_COLOR_ATTACHMENT0 + outputTex);
            _blendProgram->activate();

            ghoul::opengl::TextureUnit mainColorTextureUnit;
            mainColorTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _postTextures[bloomTex]);
            _blendProgram->setUniform(_blendUniformCache.mainColorTexture, mainColorTextureUnit);

            ghoul::opengl::TextureUnit effectsColorTextureUnit;
            effectsColorTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _postTextures[lensflareTex]);
            _blendProgram->setUniform(_blendUniformCache.effectsColorTexture, effectsColorTextureUnit);
            
            _blendProgram->setUniform(_blendUniformCache.exposure, 0.0f );
            _blendProgram->setUniform(_blendUniformCache.gamma, 1.0f );

            glBindVertexArray(_screenQuad);
            glDrawArrays(GL_TRIANGLES, 0, 6);
            glBindVertexArray(0);

            _blendProgram->deactivate();

        }
        
        for (int i = 0; i < _blurPassNumberP; i++){
        
            { // blur horizontal

                int src = 0;
                int dst = 1;

                glBindFramebuffer(GL_FRAMEBUFFER, _postFramebuffer);
                glViewport(0,0,_resolutionLow.x,_resolutionLow.y);
                glDrawBuffer(GL_COLOR_ATTACHMENT0 + dst);
                _blurProgram->activate();

                ghoul::opengl::TextureUnit mainColorTextureUnit;
                mainColorTextureUnit.activate();
                glBindTexture(GL_TEXTURE_2D, _postTextures[src]);
                _blurProgram->setUniform(_blurUniformCache.mainColorTexture, mainColorTextureUnit);
                _blurProgram->setUniform(_blurUniformCache.blurDirection, glm::vec2(1.0f,0.0f) );
                _blurProgram->setUniform(_blurUniformCache.blurMagnitude, _blurMagnitudeP );

                glBindVertexArray(_screenQuad);
                glDrawArrays(GL_TRIANGLES, 0, 6);
                glBindVertexArray(0);

                _blurProgram->deactivate();

            }

            { // blur vertical

                int src = 1;
                int dst = 0;

                glBindFramebuffer(GL_FRAMEBUFFER, _postFramebuffer);
                glViewport(0,0,_resolutionLow.x,_resolutionLow.y);
                glDrawBuffer(GL_COLOR_ATTACHMENT0 + dst);
                _blurProgram->activate();

                ghoul::opengl::TextureUnit mainColorTextureUnit;
                mainColorTextureUnit.activate();
                glBindTexture(GL_TEXTURE_2D, _postTextures[src]);
                _blurProgram->setUniform(_blurUniformCache.mainColorTexture, mainColorTextureUnit);
                _blurProgram->setUniform(_blurUniformCache.blurDirection, glm::vec2(0.0f,1.0f) );
                _blurProgram->setUniform(_blurUniformCache.blurMagnitude, _blurMagnitudeP );

                glBindVertexArray(_screenQuad);
                glDrawArrays(GL_TRIANGLES, 0, 6);
                glBindVertexArray(0);

                _blurProgram->deactivate();

            }
            
        }

        { // blend effects with original texture
            
            int src = 0;

            glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
            glViewport(0,0,_resolution.x,_resolution.y);
            _blendProgram->activate();

            ghoul::opengl::TextureUnit mainColorTextureUnit;
            mainColorTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _sceneTexture);
            _blendProgram->setUniform(_blendUniformCache.mainColorTexture, mainColorTextureUnit);

            ghoul::opengl::TextureUnit effectsColorTextureUnit;
            effectsColorTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _postTextures[src]);
            _blendProgram->setUniform(_blendUniformCache.effectsColorTexture, effectsColorTextureUnit);
            
            _blendProgram->setUniform(_blendUniformCache.exposure, _exposureP );
            _blendProgram->setUniform(_blendUniformCache.gamma, _gammaP );

            glBindVertexArray(_screenQuad);
            glDrawArrays(GL_TRIANGLES, 0, 6);
            glBindVertexArray(0);

            _blendProgram->deactivate();

        }
        
    }
    
}

} // namespace openspace
