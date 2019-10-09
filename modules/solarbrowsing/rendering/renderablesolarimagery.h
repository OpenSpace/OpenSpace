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

#ifndef __OPENSPACE_MODULE_SOLARBROWSING___RENDERABLESOLARIMAGERY___H__
#define __OPENSPACE_MODULE_SOLARBROWSING___RENDERABLESOLARIMAGERY___H__

#include <openspace/rendering/renderable.h>

#include <modules/solarbrowsing/util/spacecraftimagerymanager.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <memory>
#include <chrono>
#include <unordered_set>


namespace ghoul::opengl { class Texture; }

namespace openspace {

class PixelBufferObject;
class SpacecraftCameraPlane;
class TransferFunction;

class RenderableSolarImagery : public Renderable {
public:
    RenderableSolarImagery(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    void performImageTimestep(const double& osTime);

    TransferFunction* getTransferFunction();
    const std::unique_ptr<ghoul::opengl::Texture>& getImageryTexture();
    const SpacecraftCameraPlane& getCameraPlane();
    float getContrastValue();
    float getGammaValue();
    float getImageResolutionFactor();
    glm::vec2 getCenterPixel();
    float getScale();
    bool isCoronaGraph();

private:
    properties::OptionProperty _activeInstruments;
    properties::FloatProperty _contrastValue;
    properties::BoolProperty _enableBorder;
    properties::BoolProperty _enableFrustum;
    properties::FloatProperty _gammaValue;
    properties::IntProperty _minRealTimeUpdateInterval;
    properties::DoubleProperty _moveFactor;
    properties::FloatProperty _planeOpacity;
    properties::IntProperty _resolutionLevel;
    properties::BoolProperty _verboseMode;

    std::chrono::milliseconds _realTime;
    std::chrono::milliseconds _lastUpdateRealTime;

    TransferFunction* _lut;
    std::unique_ptr<ghoul::opengl::Texture> _texture;


    bool _timeToUpdateTexture = false;
    float _imagePlaneOffset = 0.0;
    double _realTimeDiff;
    double _currentActiveImageTime;

    bool _isWithinFrustum = false;
    bool _isWithinFrustumLast = true;
    unsigned int _bufferCountOffset = 1;
    unsigned int _imageSize;

    float _currentScale;
    glm::vec2 _currentCenterPixel;
    bool _isCoronaGraph;

    // For debugging
    unsigned int _frameSkipCount = 0;

    std::string _nodeName;
    std::unordered_map<std::string, std::shared_ptr<TransferFunction>> _tfMap;
    std::string _currentActiveInstrument;
    std::unordered_map<std::string, SpacecraftImageryManager::ImageMetadataStateSequence> _imageMetadataMap;
    std::unique_ptr<SpacecraftCameraPlane> _spacecraftCameraPlane;

    DecodeData getDecodeDataFromOsTime(int osTime);
    void updateTextureGPU(bool asyncUpload = true, bool resChanged = false);
    void listen();
    bool checkBoundaries(const RenderData& data);

    void decode(unsigned char* buffer, const std::string& fileame);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOLARBROWSING___RENDERABLESOLARIMAGERY___H__
