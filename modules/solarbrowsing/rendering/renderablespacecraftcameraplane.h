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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLESPACECRAFTCAMERAPLANE___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLESPACECRAFTCAMERAPLANE___H__

#include <modules/base/rendering/renderableplane.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/engine/downloadmanager.h> // Make pointer & forward declare?
#include <modules/solarbrowsing/util/spacecraftimagerymanager.h>
#include <openspace/rendering/transferfunction.h>
#include <memory>
#include <modules/solarbrowsing/util/simplej2kcodec.h>

#include <openspace/util/powerscaledsphere.h>

namespace ghoul { namespace opengl { class Texture; }}

namespace openspace {

class RenderableSpacecraftCameraPlane : public Renderable {

public:
    RenderableSpacecraftCameraPlane(const ghoul::Dictionary& dictionary);

    void render(const RenderData& data);
    void update(const UpdateData& data);
    void loadTexture();
    void performImageTimestep(const double& osTime);
    void updateTexture();

private:
    properties::BoolProperty _asyncUploadPBO;
    properties::IntProperty _currentActiveChannel;
    properties::IntProperty _minRealTimeUpdateInterval;
    properties::DoubleProperty _moveFactor;
    properties::IntProperty _resolutionLevel;
    properties::StringProperty _target;
    properties::BoolProperty _usePBO;

    std::chrono::milliseconds _realTime;
    std::chrono::milliseconds _lastUpdateRealTime;
    std::unique_ptr<PowerScaledSphere> _sphere;

    std::unique_ptr<ghoul::opengl::ProgramObject> _frustumShader;
    std::unique_ptr<ghoul::opengl::ProgramObject> _sphereShader;
    std::unique_ptr<ghoul::opengl::ProgramObject> _planeShader;

    std::string _type;
    size_t _currentActiveImage;
    unsigned int _pboSize;
    GLuint _frustum;
    GLuint _frustumPositionBuffer;

    GLuint pboHandles[2];
    unsigned int _currentPBO;

    double _startTimeSequence;
    double _endTimeSequence;

    bool _updatingCurrentActiveChannel = false;
    bool _updatingCurrentLevelOfResolution = false;

    std::unique_ptr<std::future<void>> _future;
    bool _initializePBO;

    IMG_PRECISION* _pboBufferData;

    float _size;
    GLuint _quad;
    GLuint _vertexPositionBuffer;

    unsigned int _imageSize;
    double _move = 0.0;

    std::unique_ptr<ghoul::opengl::Texture> _texture;
    std::vector<std::unique_ptr<TransferFunction>> _transferFunctions;
    std::vector<std::vector<ImageMetadata>> _imageMetadata;

    void uploadImageDataToPBO(const int& image);
    void updateTextureGPU(bool asyncUpload = true, bool resChanged = false);

    void createFrustum();
    void createPlane();
    void updatePlane();

    void decode(unsigned char* buffer, const std::string& fileame,
                const int numThreads);
    void downloadTextureResource();
    bool initialize() override;
    bool deinitialize() override;
    bool isReady() const override;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLESPACECRAFTCAMERAPLANE___H__
