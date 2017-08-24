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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLESOLARIMAGERY___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLESOLARIMAGERY___H__

#define SOLAR_BUFFER_SIZE 10

#include <openspace/rendering/renderable.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/stringproperty.h>

#include <modules/solarbrowsing/util/spacecraftimagerymanager.h>
#include <openspace/rendering/transferfunction.h>
#include <memory>
#include <unordered_set>
#include <modules/solarbrowsing/util/streambuffer.h>
#include <modules/solarbrowsing/util/pixelbufferobject.h>
#include <modules/solarbrowsing/util/decodejob.h>
#include <modules/solarbrowsing/rendering/spacecraftcameraplane.h>

namespace ghoul { namespace opengl { class Texture; }}

namespace openspace {

class RenderableSolarImagery : public Renderable {

public:
    RenderableSolarImagery(const ghoul::Dictionary& dictionary);

    void render(const RenderData& data);
    void update(const UpdateData& data);
    void loadTexture();
    void performImageTimestep(const double& osTime);
    void updateTexture();
    void clearBuffer();

    TransferFunction* getTransferFunction() { return _lut; }
    const std::unique_ptr<ghoul::opengl::Texture>& getImageryTexture() { return _texture; }
    const SpacecraftCameraPlane& getCameraPlane() { return *_spacecraftCameraPlane; }
    float getContrastValue() { return _contrastValue; }
    float getGammaValue() { return _gammaValue; }
    float getImageResolutionFactor() { return _imageSize; }
    glm::vec2 getCenterPixel() { return _currentCenterPixel; }
    float getScale() { return _currentScale; }
    bool isCoronaGraph() { return _isCoronaGraph; }

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
    properties::BoolProperty _usePBO;
    properties::BoolProperty _verboseMode;

    std::chrono::milliseconds _realTime;
    std::chrono::milliseconds _lastUpdateRealTime;

    TransferFunction* _lut;
    std::unique_ptr<ghoul::opengl::Texture> _texture;
    std::array<std::unique_ptr<PixelBufferObject>, SOLAR_BUFFER_SIZE> _pbos;

    // TODO: Remove these?
    //bool _updatingCurrentActiveChannel = false;
    //bool _updatingCurrentLevelOfResolution = false;
    bool _initializePBO = true;
    bool _pboIsDirty = false;
    bool _timeToUpdateTexture = false;
    float _imagePlaneOffset = 0.0;

    double _deltaTimeLast = 0.0;
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

    PixelBufferObject* _currentPbo;
    std::queue<PixelBufferObject*> _pboQueue;
    std::unordered_set<int> _busyPbos;

    std::string _nodeName;
    StreamBuffer<SolarImageData> _streamBuffer;
    std::unordered_map<std::string, std::shared_ptr<TransferFunction>> _tfMap;
    std::string _currentActiveInstrument;
    std::unordered_map<std::string, TimedependentStateSequence<ImageMetadata>> _imageMetadataMap;
    std::unique_ptr<SpacecraftCameraPlane> _spacecraftCameraPlane;

    DecodeData getDecodeDataFromOsTime(const int& osTime);
    void uploadImageDataToPBO();
    void updateTextureGPU(bool asyncUpload = true, bool resChanged = false);
    void fillBuffer(const double& dt);
    void listen();
    void loadMetadata(const std::string& rootPath);
    PixelBufferObject* getAvailablePbo();

    std::string ISO8601(std::string& datetime);
    void decode(unsigned char* buffer, const std::string& fileame);
    bool initialize() override;
    bool deinitialize() override;
    bool isReady() const override;
    bool checkBoundaries(const RenderData& data);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLESOLARIMAGERY___H__
