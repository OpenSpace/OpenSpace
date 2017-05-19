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
#include <modules/solarbrowsing/rendering/renderablespacecraftcameraplane.h>
#include <modules/solarbrowsing/rendering/renderablespacecraftcamerasphere.h>
#include <openspace/engine/openspaceengine.h>

#include <openspace/scene/scenegraphnode.h>
#include <openspace/rendering/renderengine.h>

// TODO(mnoven) CLEAN REDUNDANT STUFF
#include <ghoul/filesystem/filesystem>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#include <ghoul/filesystem/filesystem.h>

#include <openspace/util/time.h>
#include <chrono>
#include <math.h>

using namespace ghoul::opengl;
using namespace std::chrono;

typedef std::chrono::high_resolution_clock Clock;

namespace {
    static const std::string _loggerCat = "RenderableSpacecraftCameraPlane";
    double HALF_SUN_RADIUS = (1391600000.0 * 0.50); // Half sun radius divided by magic factor
    const double EPSILON = std::numeric_limits<double>::epsilon();
}

namespace openspace {

RenderableSpacecraftCameraPlane::RenderableSpacecraftCameraPlane(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _sharpenValue("sharpenValue", "Sharpen", 0.0, 0.0, 1.0)
    , _contrastValue("contrastValue", "Contrast", 0.0, -15.0, 15.0)
    , _planeOpacity("planeOpacity", "Plane Opacity", 1.0, 0.0, 1.0)
    , _disableFrustum("disableFrustum", "Disable Frustum", true)
    , _disableBorder("disableBorder", "Disable Border", true)
    , _gammaValue("gammaValue", "Gamma", 0.9, 0.1, 10.0)
    , _asyncUploadPBO("asyncUploadPBO", "Upload to PBO Async", true)
    , _activeInstruments("activeInstrument", "Active Instrument", properties::OptionProperty::DisplayType::Radio)
    , _bufferSize("bufferSize", "Buffer Size", 15, 1, 100)
    , _displayTimers("displayTimers", "Display Timers", false)
    , _lazyBuffering("lazyBuffering", "Lazy Buffering", true)
    , _minRealTimeUpdateInterval("minRealTimeUpdateInterval", "Min Update Interval", 85, 0, 300)
    , _moveFactor("movefactor", "Move Factor" , 1.0, 0.0, 1.0)
    , _resolutionLevel("resolutionlevel", "Level of detail", 3, 0, 5)
    , _target("target", "Target", "Sun")
    , _useBuffering("useBuffering", "Use Buffering", true)
    , _usePBO("usePBO", "Use PBO", true)
    , _planeSize("planeSize", "Plane Size", 50.0, 0.0, 1.0)
    , _concurrentJobManager(std::make_shared<globebrowsing::ThreadPool>(1))
    , _verboseMode("verboseMode", "Verbose Mode", false)
{
    std::string target;
    if (dictionary.getValue("Target", target)) {
        _target = target;
    }

    glm::dvec2 magicPlaneOffset;
    if (dictionary.getValue("MagicOffsetFromCenter", magicPlaneOffset)) {
        _magicPlaneOffset = magicPlaneOffset;
    }

    float tmpStartResolutionLevel;
    if (!dictionary.getValue("StartResolutionLevel", tmpStartResolutionLevel)) {
        throw ghoul::RuntimeError("Plane must have a starting resolution level");
    }
    _resolutionLevel = static_cast<int>(tmpStartResolutionLevel);

    if (!dictionary.getValue("MagicPlaneFactor", _magicPlaneFactor)) {
        throw ghoul::RuntimeError("Plane must at the moment have a magic factor");
    }

    std::string rootPath;
    if (!dictionary.getValue("RootPath", rootPath)) {
        throw ghoul::RuntimeError("RootPath has to be specified");
    }

    //TODO(mnoven): Can't pass in an int to dictionary?
    float tmpResolution;
    if (!dictionary.getValue("Resolution", tmpResolution)){
        throw ghoul::RuntimeError("Resolution has to be specified");
    }
    _fullResolution = static_cast<unsigned int>(tmpResolution);

    if (dictionary.hasKeyAndValue<ghoul::Dictionary>("Instruments")) {
        ghoul::Dictionary instrumentDic = dictionary.value<ghoul::Dictionary>("Instruments");
        for (size_t i = 1; i <= instrumentDic.size(); ++i) {
            if (!instrumentDic.hasKeyAndValue<std::string>(std::to_string(i))) {
                throw ghoul::RuntimeError("Instruments has to be an array-style table");
            }
            std::string instrument = instrumentDic.value<std::string>(std::to_string(i));
            std::transform(instrument.begin(), instrument.end(), instrument.begin(),
                           ::tolower);
            _instrumentFilter.insert(instrument);
        }
    }

    SpacecraftImageryManager::ref().loadImageMetadata(rootPath, _imageMetadataMap, _instrumentFilter);


    std::string tfRootPath;
    if (dictionary.getValue("TransferfunctionPath", tfRootPath)) {
        SpacecraftImageryManager::ref().loadTransferFunctions(tfRootPath, _tfMap, _instrumentFilter);
        //throw ghoul::RuntimeError("TransferfunctionPath has to be specified");
    }

    // Some sanity checks
    if (_imageMetadataMap.size() == 0) {
        if (_instrumentFilter.size() > 0) {
            LERROR("Could not find any instruments that match specified filter");
            for (auto& filter : _instrumentFilter) {
                LERROR(filter);
            }
        } else {
            LERROR("Images map is empty! Check your path");
        }
    }

    unsigned int i = 0;
    for (auto& el : _imageMetadataMap) {
        _activeInstruments.addOption(i++, el.first);
    }

    _currentActiveInstrument
          = _activeInstruments.getDescriptionByValue(_activeInstruments.value());

    // Have to figure out the times of the whole interval first
    ImageMetadata& start =_imageMetadataMap[_currentActiveInstrument][0];

    _startTimeSequence = start.timeObserved;
    //_endTimeSequence = end.timeObserved;

    //TODO(mnoven): Can't assume 1 plane
    Time::ref().setTime(_startTimeSequence - 10);

    _pboSize = (_fullResolution * _fullResolution * sizeof(IMG_PRECISION))
               / (pow(4, _resolutionLevel));
    _imageSize = _fullResolution / (pow(2, _resolutionLevel));

    // Generate PBO
    glGenBuffers(2, pboHandles);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboHandles[0]);
    glBufferData(GL_PIXEL_UNPACK_BUFFER, _pboSize, 0, GL_STREAM_DRAW);
    // glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboHandles[1]);
    // glBufferData(GL_PIXEL_UNPACK_BUFFER, _pboSize, 0, GL_STREAM_DRAW);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

    // TODO(mnoven): Faster to send GL_RGBA32F as internal format - GPU Pads anyways?
    _texture =  std::make_unique<Texture>(
                    nullptr, // Update pixel data later, is this really safe?
                    glm::size3_t(_imageSize, _imageSize, 1),
                    ghoul::opengl::Texture::Red, // Format of the pixeldata
                    GL_R8, // INTERNAL format. More preferable to give explicit precision here, otherwise up to the driver to decide
                    GL_UNSIGNED_BYTE, // Type of data
                    Texture::FilterMode::Linear,
                    Texture::WrappingMode::ClampToEdge
                );

    _future = nullptr;
    _texture->setDataOwnership(ghoul::Boolean::No);
    _texture->uploadTexture();

    _currentActiveImage = 0;
    _currentPBO = 0;

    // Initialize time
    _realTime = duration_cast<milliseconds>(system_clock::now().time_since_epoch());
    _lastUpdateRealTime = _realTime;

    _activeInstruments.onChange([this]() {
        _currentActiveInstrument
              = _activeInstruments.getDescriptionByValue(_activeInstruments.value());
        if (_verboseMode) {
            LDEBUG("Updating current active instrument" << _currentActiveInstrument);
        }
        _updatingCurrentActiveChannel = true;
        const double& osTime = Time::ref().j2000Seconds();
        const auto& imageList = _imageMetadataMap[_activeInstruments.getDescriptionByValue(_activeInstruments.value())];/*_imageMetadata[_currentActiveChannel]*/
        const auto& low = std::lower_bound(imageList.begin(), imageList.end(), osTime);
        _currentActiveImage = low - imageList.begin();
        if (_currentActiveImage == _imageMetadataMap[_activeInstruments.getDescriptionByValue(_activeInstruments.value())].size()) {
            _currentActiveImage = _currentActiveImage - 1;
        }
        updateTextureGPU(/*asyncUpload=*/false);
        _concurrentJobManager.reset();
        /*    while (_concurrentJobManager.numFinishedJobs() > 0) {
            _concurrentJobManager.popFinishedJob();
        }
        */
        if (_useBuffering) {
           // _initializePBO = true;
            fillBuffer(Time::ref().deltaTime());
        } else {
            uploadImageDataToPBO(_currentActiveImage);
        }
        _updatingCurrentActiveChannel = false;
    });

    _resolutionLevel.onChange([this]() {
        if (_verboseMode) {
            LDEBUG("Updating level of resolution " << _resolutionLevel);
        }
        _updatingCurrentLevelOfResolution = true;
        _pboSize = (_fullResolution * _fullResolution * sizeof(IMG_PRECISION)) / (pow(4, _resolutionLevel));
        _imageSize = _fullResolution / (pow(2, _resolutionLevel));
        updateTextureGPU(/*asyncUpload=*/false, /*resChanged=*/true);
        _concurrentJobManager.reset();
  /*      while (_concurrentJobManager.numFinishedJobs() > 0) {
            _concurrentJobManager.popFinishedJob();
        }
*/
        if (_useBuffering) {
            //_initializePBO = true;
            fillBuffer(Time::ref().deltaTime());
        } else {
            uploadImageDataToPBO(_currentActiveImage);
        }
        _updatingCurrentLevelOfResolution = false;
    });

    _moveFactor.onChange([this]() {
        updatePlane();
    });

    _deltaTimeLast = 1.0;

    if (_useBuffering) {
        fillBuffer(1.0);
    }

    // If no buffer is used this is needed
    _initializePBO = true;

    // Initialize PBO - not needed since buffer is filled async anyways
    // if (_usePBO) {
    //     LDEBUG("Initializing PBO with image " << _currentActiveImage);
    //     uploadImageDataToPBO(_currentActiveImage); // Begin fill PBO 1
    // }

    performImageTimestep(Time::ref().j2000Seconds());

    addProperty(_planeOpacity);
    addProperty(_disableBorder);
    addProperty(_disableFrustum);
    addProperty(_activeInstruments);
    addProperty(_sharpenValue);
    addProperty(_gammaValue);
    addProperty(_contrastValue);
    addProperty(_bufferSize);
    addProperty(_displayTimers);
    addProperty(_planeSize);
    addProperty(_resolutionLevel);
    addProperty(_lazyBuffering);
    addProperty(_minRealTimeUpdateInterval);
    addProperty(_asyncUploadPBO);
    addProperty(_useBuffering);
    addProperty(_usePBO);
    addProperty(_target);
    addProperty(_moveFactor);
    addProperty(_verboseMode);
}

void RenderableSpacecraftCameraPlane::fillBuffer(const double& dt) {
    _enqueuedImageIds.clear();
    if (_verboseMode) {
        LDEBUG("Refilling Buffer. dt: " << dt);
    }
    const auto& imageList = _imageMetadataMap[_activeInstruments.getDescriptionByValue(
          _activeInstruments.value())];
    const double& osTime = Time::ref().j2000Seconds();

    const auto& startIndex = std::lower_bound(
              imageList.begin(), imageList.end(), osTime);

    int startId = startIndex - imageList.begin();
    if (startId == imageList.size()) {
        startId = startId - 1;
    }

    const double& startTime = imageList[startId].timeObserved;

    for (int i = 0; i < _bufferSize; i++) {
        const auto& low = std::lower_bound(
              imageList.begin(), imageList.end(),
              startTime + (dt * i) * ((static_cast<double>(_minRealTimeUpdateInterval)) / 1000.0));

        const int nextImageIndex = low - imageList.begin();

        if (nextImageIndex == imageList.size()) {
            break;
        }

        const std::string& currentFilename = imageList[nextImageIndex].filename;
        auto job = std::make_shared<DecodeJob>(_imageSize, currentFilename,
                                               _resolutionLevel, _verboseMode);
        _concurrentJobManager.enqueueJob(job);
        _enqueuedImageIds.insert(currentFilename);
        if (_verboseMode) {
            LDEBUG("Enqueueing " << currentFilename);
        }
        _currentActiveImage = nextImageIndex;
    }

    if (!_lazyBuffering) {
        while (_concurrentJobManager.numFinishedJobs() < _bufferSize) {
            if (_verboseMode) {
                LDEBUG("Buffering .... ");
            }
        }
    }
    _initializePBO = true;
}

void RenderableSpacecraftCameraPlane::createPlane() {
    // ============================
    //         GEOMETRY (quad)
    // ============================
    const GLfloat size = _size;
    const GLfloat vertex_data[] = {
        //      x      y     z     w     s     t
        -size, -size, 0.f, 0.f, 0.f, 0.f,
        size, size, 0.f, 0.f, 1.f, 1.f,
        -size, size, 0.f, 0.f, 0.f, 1.f,
        -size, -size, 0.f, 0.f, 0.f, 0.f,
        size, -size, 0.f, 0.f, 1.f, 0.f,
        size, size, 0.f, 0.f, 1.f, 1.f,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6,
                          reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6,
                          reinterpret_cast<void*>(sizeof(GLfloat) * 4));
}

void RenderableSpacecraftCameraPlane::updatePlane() {
    //const double a = 1;
    //const double b = 0;
    //const double c = 0.31622776601; // sqrt(0.1)
    //_move = a * exp(-(pow((_moveFactor.value() - 1) - b, 2.0)) / (2.0 * pow(c, 2.0)));
    _move = /*a **/ exp(-(pow((_moveFactor.value() - 1) /*- b*/, 2.0)) / (2.0 /** pow(c, 2.0)*/));
    _size = _move * HALF_SUN_RADIUS / _magicPlaneFactor;
   // _size = _move * HALF_SUN_RADIUS / _planeSize;
    createPlane();
    createFrustum();
}

bool RenderableSpacecraftCameraPlane::isReady() const {
    return _planeShader && _frustumShader && _texture;
}

void RenderableSpacecraftCameraPlane::createFrustum() {
    // Vertex orders x, y, z, w
    // Where w indicates if vertex should be drawn in spacecraft
    // or planes coordinate system
    const GLfloat vertex_data[] = {
        0.f, 0.f, 0.f, 0.0,
        _size, _size, 0.f , 1.0,
        0.f, 0.f, 0.f, 0.0,
        -_size, -_size, 0.f , 1.0,
        0.f, 0.f, 0.f, 0.0,
        _size, -_size, 0.f , 1.0,
        0.f, 0.f, 0.f, 0.0,
        -_size, _size, 0.f , 1.0,
        // Borders
        // Left
        -_size, -_size, 0.f, 1.0,
        -_size, _size, 0.f, 1.0,
        // Top
        -_size, _size, 0.f, 1.0,
        _size, _size, 0.f, 1.0,
        // Right
        _size, _size, 0.f, 1.0,
        _size, -_size, 0.f, 1.0,
        // Bottom
        _size, -_size, 0.f, 1.0,
        -_size, -_size, 0.f, 1.0,
    };

    glGenVertexArrays(1, &_frustum);
    glGenBuffers(1, &_frustumPositionBuffer);  // generate buffer
    glBindVertexArray(_frustum);
    glBindBuffer(GL_ARRAY_BUFFER, _frustumPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, reinterpret_cast<void*>(0));
}

bool RenderableSpacecraftCameraPlane::initialize() {
    // Initialize plane buffer
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer);
    //_size.setValue(glm::vec2(FULL_PLANE_SIZE, 0.f));
    updatePlane();

    if (!_planeShader) {
        RenderEngine& renderEngine = OsEng.renderEngine();
        _planeShader = renderEngine.buildRenderProgram("SpacecraftImagePlaneProgram",
            "${MODULE_SOLARBROWSING}/shaders/spacecraftimageplane_vs.glsl",
            "${MODULE_SOLARBROWSING}/shaders/spacecraftimageplane_fs.glsl"
            );
        if (!_planeShader) {
            return false;
        }
    }

    if (!_frustumShader) {
        RenderEngine& renderEngine = OsEng.renderEngine();
        _frustumShader = renderEngine.buildRenderProgram("SpacecraftFrustumProgram",
            "${MODULE_SOLARBROWSING}/shaders/spacecraftimagefrustum_vs.glsl",
            "${MODULE_SOLARBROWSING}/shaders/spacecraftimagefrustum_fs.glsl"
            );
        if (!_frustumShader) {
            return false;
        }
    }

    // if (!_sphereShader) {
    //     RenderEngine& renderEngine = OsEng.renderEngine();
    //     _sphereShader = renderEngine.buildRenderProgram("SpacecraftImagePlaneProgram",
    //         "${MODULE_SOLARBROWSING}/shaders/spacecraftimagesphere_vs.glsl",
    //         "${MODULE_SOLARBROWSING}/shaders/spacecraftimagesphere_fs.glsl"
    //         );
    //     if (!_sphereShader) {
    //         return false;
    //     }
    // }

    // Fake sun
    //  PowerScaledScalar planetSize(glm::vec2(6.96701f, 8.f)); // 6.95701f
    // _sphere = std::make_unique<PowerScaledSphere>(PowerScaledSphere(planetSize, 100));
    // _sphere->initialize();

    // using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    // _planeShader->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    //_planeShader->setIgnoreUniformLocationError(IgnoreError::Yes);

    return isReady();
}

bool RenderableSpacecraftCameraPlane::deinitialize() {
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_planeShader) {
        renderEngine.removeRenderProgram(_planeShader);
        _planeShader = nullptr;
    }
    return true;
}

void RenderableSpacecraftCameraPlane::uploadImageDataToPBO(const int& image) {
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboHandles[0]); // 1 - _currentPBO
    // Orphan data and multithread
    glBufferData(GL_PIXEL_UNPACK_BUFFER, _pboSize, NULL, GL_STREAM_DRAW);
    // Map buffer to client memory
    //_pboBufferData = static_cast<float*>(glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY));
    _pboBufferData = static_cast<IMG_PRECISION*>(
          glMapBufferRange(GL_PIXEL_UNPACK_BUFFER, NULL, _pboSize,
                           GL_MAP_WRITE_BIT | GL_MAP_INVALIDATE_BUFFER_BIT));

    if (!_asyncUploadPBO) {
        std::string& currentFilename = _imageMetadataMap[_currentActiveInstrument][image].filename;
        decode(_pboBufferData, currentFilename);
        _pboIsDirty = true;
    } else {
        if (!_useBuffering) {
            std::string& currentFilename = _imageMetadataMap[_currentActiveInstrument][image].filename;
            _future = std::make_unique<std::future<void>>(
                  std::async(std::launch::async, [this, &currentFilename]() {
                      decode(_pboBufferData, currentFilename);
                  }));
            _pboIsDirty = true;
        } else {
            bool hadFinishedJobs = false;
            while (_concurrentJobManager.numFinishedJobs() > 0) {
                std::shared_ptr<BufferObject> b = _concurrentJobManager.popFinishedJob()->product();

                // Not found in set, queue finished old job
                if (_enqueuedImageIds.find(b->name) == _enqueuedImageIds.end()) continue;

                hadFinishedJobs = true;
                _enqueuedImageIds.erase(b->name);
                unsigned char* data = b->data;

                if (_verboseMode) {
                    LDEBUG("Popped image" << b->name);
                }

                auto t1 = Clock::now();
                std::memcpy(_pboBufferData, data, _imageSize * _imageSize * sizeof(unsigned char));
                auto t2 = Clock::now();

                if (_displayTimers) {
                    LDEBUG("Memcpy time "
                           << std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1)
                                    .count()
                           << " ms" << std::endl);
                }

                const auto& imageList = _imageMetadataMap[_currentActiveInstrument];
                const double& osTime = Time::ref().j2000Seconds();
                double time = osTime + (_bufferSize + 1) * (Time::ref().deltaTime() * ( static_cast<double>(_minRealTimeUpdateInterval )/ 1000.0 ) );
                const auto& low = std::lower_bound(imageList.begin(), imageList.end(), time);
                int nextImageIndex = low - imageList.begin();
                if (nextImageIndex
                    == _imageMetadataMap[_currentActiveInstrument].size()) {
                    nextImageIndex = nextImageIndex - 1;
                }

                std::string currentFilename
                      = _imageMetadataMap[_currentActiveInstrument][nextImageIndex].filename;
                auto job = std::make_shared<DecodeJob>(_imageSize, currentFilename,
                                                       _resolutionLevel, _verboseMode);
                _concurrentJobManager.enqueueJob(job);
                _enqueuedImageIds.insert(currentFilename);

                _initializePBO = false;
                _pboIsDirty = true;

                if (_verboseMode)  {
                    LDEBUG("Adding work from PBO " << currentFilename);
                }
            } if (!hadFinishedJobs) {
                if (_verboseMode) {
                    LWARNING("Nothing to update, buffer is not ready");
                }
                _pboBufferData = nullptr;
            }

        }

    }

    // Release the mapped buffer
    glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
    // Set back to normal texture data source
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
}

void RenderableSpacecraftCameraPlane::updateTextureGPU(bool asyncUpload, bool resChanged) {
    if (_future) {
        _future->wait();
    }
    _future = nullptr;
    if (_usePBO && asyncUpload) {
        if (_pboBufferData) {
            // Bind PBO to texture data source
            glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboHandles[0]);
            _texture->bind();
            // Send async to GPU by coping from PBO to texture objects
            glTexSubImage2D(_texture->type(), 0, 0, 0, _imageSize, _imageSize,
                            GLint(_texture->format()), _texture->dataType(), nullptr);
            glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

            _pboIsDirty = false;
        }
    } else {
        const std::string& currentFilename
              = _imageMetadataMap[_currentActiveInstrument][_currentActiveImage].filename;
        unsigned char* data
              = new unsigned char[_imageSize * _imageSize * sizeof(IMG_PRECISION)];
        decode(data, currentFilename);
        _texture->bind();

        if (!resChanged) {
            glTexSubImage2D(_texture->type(), 0, 0, 0, _imageSize, _imageSize,
                            GLint(_texture->format()), _texture->dataType(), data);
        } else {
            glTexImage2D(_texture->type(), 0, _texture->internalFormat(), _imageSize,
                         _imageSize, 0, GLint(_texture->format()), _texture->dataType(),
                         data);
        }
        delete[] data;
    }
}

void RenderableSpacecraftCameraPlane::decode(unsigned char* buffer,
                                             const std::string& filename)
{
    SimpleJ2kCodec j2c(_verboseMode);
    j2c.DecodeIntoBuffer(filename, buffer, _resolutionLevel);
}

void RenderableSpacecraftCameraPlane::performImageTimestep(const double& osTime) {
    const auto& imageList = _imageMetadataMap[_currentActiveInstrument];

    //TODO(mnoven): Do NOT perform this log(n) lookup every update - check if
    // still inside current interval => No need to check

    auto t1 = Clock::now();
    const auto& low = std::lower_bound(imageList.begin(), imageList.end(), osTime);
    auto t2 = Clock::now();

    if (_displayTimers) {
        LDEBUG("log(n) find metadata "
               << std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count()
               << " ms" << std::endl);
    }

    size_t currentActiveImageLast = _currentActiveImage;
    _currentActiveImage = low - imageList.begin();

    if (_currentActiveImage == _imageMetadataMap[_currentActiveInstrument].size()) {
        _currentActiveImage = _currentActiveImage - 1;
    }

    if (_pboIsDirty || !_usePBO) {
        updateTextureGPU();
    }

    // TODO(mnoven): Clean this up

    // Time to pop from buffer !!! - And refill with buffer offset
    if (currentActiveImageLast != _currentActiveImage || _initializePBO) {
        //  LDEBUG("Updating texture to " << _currentActiveImage);
        //_currentPBO = 1 - _currentPBO;
        //updateTextureGPU();
        // Refill PBO
        if (_verboseMode) {
            LDEBUG("Time to update image! ");
        }
        if (_usePBO /*&& !_initializePBO*/) {
            uploadImageDataToPBO(_currentActiveImage);
        }
        //_initializePBO = false;
    }
}

void RenderableSpacecraftCameraPlane::update(const UpdateData& data) {
    _realTime = duration_cast<milliseconds>(system_clock::now().time_since_epoch());
    _realTimeDiff = _realTime.count() - _lastUpdateRealTime.count();
    //updatePlane();
    // int clockwiseSign = (Time::ref().deltaTime() < 0) ? -1 : 1;

    // if ((clockwiseSign == -1 && _bufferingForwardInTime)
    //     || (clockwiseSign == 1 && !_bufferingForwardInTime)) {
    //     _bufferingForwardInTime = (clockwiseSign == 1);
    //      _concurrentJobManager.reset();
    //     while (_concurrentJobManager.numFinishedJobs() > 0) {
    //         _concurrentJobManager.popFinishedJob();
    //     }
    //     fillBuffer();
    // }
    //double j2000 = Time::ref().j2000Seconds();
    if (_useBuffering) {
        double dt = Time::ref().deltaTime();
        // Delta time changed, need to refill buffer
        if ((abs(_deltaTimeLast - dt)) > EPSILON) {
            _concurrentJobManager.reset();
            while (_concurrentJobManager.numFinishedJobs() > 0) {
                _concurrentJobManager.popFinishedJob();
            }
            _concurrentJobManager.reset();
            _pboIsDirty = false;
            fillBuffer(dt);
        }
        _deltaTimeLast = dt;
    }

    bool timeToUpdateTexture = _realTimeDiff > _minRealTimeUpdateInterval;

    // Update texture
    // The bool blockers might probably not be needed now
    if (timeToUpdateTexture && !_updatingCurrentLevelOfResolution
        && !_updatingCurrentActiveChannel) {
        performImageTimestep(data.time);
        _lastUpdateRealTime = _realTime;
    }

    _lut = _tfMap[_currentActiveInstrument].get();

    if (_planeShader->isDirty()) {
        _planeShader->rebuildFromFile();
    }

    if (_frustumShader->isDirty()) {
        _frustumShader->rebuildFromFile();
    }
}

void RenderableSpacecraftCameraPlane::render(const RenderData& data) {
    if (!isReady()) {
        return;
    }

    const glm::dmat4 viewMatrix = data.camera.combinedViewMatrix();
    const glm::mat4 projectionMatrix = data.camera.projectionMatrix();

    // Model transform and view transform needs to be in double precision
    const SceneGraphNode* target = OsEng.renderEngine().scene()->sceneGraphNode(_target);
    const glm::dvec3 positionWorld = data.modelTransform.translation;
    const glm::dvec3 targetPositionWorld = target->worldPosition();

    const glm::dvec3 upWorld = data.modelTransform.rotation * glm::dvec3(0.0, 0.0, 1.0);
    const glm::dvec3 nPositionWorld = glm::normalize(positionWorld);
    const glm::dvec3 nTargetWorld = glm::normalize(targetPositionWorld);

    // We don't normalize sun's position since its in the origin
    glm::dmat4 rotationTransformSpacecraft
          = glm::lookAt(nPositionWorld, glm::dvec3(target->worldPosition()), upWorld);
    glm::dmat4 rotationTransformWorld = glm::inverse(rotationTransformSpacecraft);

    // Scale vector to sun barycenter to get translation distance
    glm::dvec3 sunDir = targetPositionWorld - positionWorld;
    glm::dvec3 offset = sunDir * _move;

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), positionWorld + offset) *
        rotationTransformWorld *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))) *
        glm::dmat4(1.0);
    glm::dmat4 modelViewTransform = viewMatrix * modelTransform;

    glm::dmat4 spacecraftModelTransform =
        glm::translate(glm::dmat4(1.0), positionWorld) *
        rotationTransformWorld *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))) *
        glm::dmat4(1.0);


    // Activate shader
    _planeShader->activate();

   /// _planeShader->setUniform("planeSize", _size);
    _planeShader->setUniform("imageSize", _imageSize);
    _planeShader->setUniform("planeOpacity", _planeOpacity);
    _planeShader->setUniform("sharpenValue", _sharpenValue);
    _planeShader->setUniform("gammaValue", _gammaValue);
    _planeShader->setUniform("contrastValue", _contrastValue);
    _planeShader->setUniform("modelViewProjectionTransform",
        projectionMatrix * glm::mat4(modelViewTransform));

   // _planeShader->setUniform("magicPlaneFactor", _magicPlaneFactor);
    _planeShader->setUniform("magicPlaneOffset", _magicPlaneOffset);

    ghoul::opengl::TextureUnit imageUnit;
    imageUnit.activate();
    _texture->bind();
    _planeShader->setUniform("imageryTexture", imageUnit);

    //_tfMap[_currentActiveInstrument]->bind(); // Calls update internally
    ghoul::opengl::TextureUnit tfUnit;
    tfUnit.activate();
    if (_lut) {
        _lut->bind();
        _planeShader->setUniform("hasLut", true);
    } else {
        _planeShader->setUniform("hasLut", false);
    }
    // Must bind all sampler2D, otherwise undefined behaviour
    _planeShader->setUniform("lut", tfUnit);

    glBindVertexArray(_quad);

    glDrawArrays(GL_TRIANGLES, 0, 6);

    _planeShader->deactivate();

    _frustumShader->activate();
    _frustumShader->setUniform("modelViewProjectionTransform",
        projectionMatrix * glm::mat4(viewMatrix * spacecraftModelTransform));
    _frustumShader->setUniform("modelViewProjectionTransformPlane",
                               projectionMatrix * glm::mat4(modelViewTransform));
    glBindVertexArray(_frustum);

    if (!_disableBorder && !_disableFrustum) {
        glDrawArrays(GL_LINES, 0, 16);
    } else if (_disableBorder && !_disableFrustum) {
        glDrawArrays(GL_LINES, 0, 8);
    } else if (_disableFrustum && !_disableBorder) {
        glDrawArrays(GL_LINES, 8, 16);
    }

    _frustumShader->deactivate();

    _planePosSpacecraftRefFrame = glm::dvec3(rotationTransformSpacecraft * glm::dvec4(positionWorld + offset, 1.0));
    _sunToSpacecraftTransform = rotationTransformSpacecraft * glm::dmat4(target->rotationMatrix());
}

} // namespace openspace
