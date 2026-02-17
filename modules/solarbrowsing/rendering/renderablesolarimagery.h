/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_MODULE_SOLARBROWSING___RENDERABLESOLARIMAGERY___H__
#define __OPENSPACE_MODULE_SOLARBROWSING___RENDERABLESOLARIMAGERY___H__

#include <openspace/rendering/renderable.h>

#include <modules/solarbrowsing/util/structs.h>
#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <ghoul/opengl/uniformcache.h>
#include <memory>

namespace ghoul::opengl { class Texture; }

namespace openspace {

namespace documentation { struct Documentation; }

class TransferFunction;

namespace solarbrowsing {
    class AsyncImageDecoder;
} // namespace solarbrowsing

// @TODO (anden88 2026-02-04): Steps for streaming new image data from HelioViewer
// 1. Check if image exists in cache (since this will be at runtime we check the ram
// _imageMetadataMap)
// 2. If not -> spawn a thread to download data from HelioViewer
// 3. Once downloaded put it through the normal pipeline of storing the file in
// correct folder
// 4. Add the image data to the cache (file and in memory)

class RenderableSolarImagery : public Renderable {
public:
    RenderableSolarImagery(const ghoul::Dictionary& dictionary);
    ~RenderableSolarImagery() override = default;

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

    TransferFunction* transferFunction();
    const std::unique_ptr<ghoul::opengl::Texture>& imageryTexture() const;
    float contrastValue() const;
    float gammaValue() const;
    float scale() const;
    bool isCoronaGraph() const;
    glm::vec2 getCenterPixel() const;
    const glm::vec3& planeNormal() const;
    const glm::dvec3& planeWorldPosition() const;
    const glm::dmat4& planeWorldRotation() const;

private:
    void updateImageryTexture();
    void requestPredictiveFrames(const Keyframe<ImageMetadata>* keyframe,
        const UpdateData& data
    );

    void createPlaneAndFrustum(double moveDistance);
    void createPlane() const;
    void createFrustum() const;

    properties::OptionProperty _activeInstruments;
    properties::FloatProperty _contrastValue;
    properties::BoolProperty _enableBorder;
    properties::BoolProperty _enableFrustum;
    properties::FloatProperty _gammaValue;
    properties::DoubleProperty _moveFactor;
    properties::IntProperty _downsamplingLevel;

    properties::BoolProperty _verboseMode;
    properties::IntProperty _predictFramesAfter;
    properties::IntProperty _predictFramesBefore;

    // The decoded image texture
    std::unique_ptr<ghoul::opengl::Texture> _imageryTexture;
    const size_t NoActiveKeyframe = std::numeric_limits<size_t>::max();
    size_t _currentKeyframe = NoActiveKeyframe;
    // Data for the currently shown image
    float _currentScale = 0;
    bool _isCoronaGraph = false;
    glm::vec2 _currentCenterPixel = glm::vec2(2.f);

    // Image metadata
    InstrumentName _currentActiveInstrument;
    ImageMetadataMap _imageMetadataMap;
    std::unordered_map<InstrumentName, std::shared_ptr<TransferFunction>> _tfMap;

    // Decoder
    std::unique_ptr<solarbrowsing::AsyncImageDecoder> _asyncDecoder;
    size_t _lastPredictedKeyframe = NoActiveKeyframe;
    bool _predictionIsDirty = true;

    // Image plane and frustum
    UniformCache(isCoronaGraph, scale, centerPixel, imageryTexture, planeOpacity,
        gammaValue, contrastValue, modelViewProjectionTransform, hasLut,
        lut) _uniformCachePlane;

    UniformCache(planeOpacity, modelViewProjectionTransform,
        modelViewProjectionTransformPlane, scale, centerPixel) _uniformCacheFrustum;

    std::unique_ptr<ghoul::opengl::ProgramObject> _frustumShader;
    std::unique_ptr<ghoul::opengl::ProgramObject> _planeShader;
    GLuint _frustum = 0;
    GLuint _frustumPositionBuffer = 0;
    GLuint _quad = 0;
    GLuint _vertexPositionBuffer = 0;
    double _gaussianMoveFactor = 0.0;
    float _size = 0.f;

    glm::dvec3 _position;
    glm::dmat4 _rotation;
    glm::vec3 _normal;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOLARBROWSING___RENDERABLESOLARIMAGERY___H__
