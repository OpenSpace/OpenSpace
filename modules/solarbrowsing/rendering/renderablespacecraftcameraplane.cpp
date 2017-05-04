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
    //static const int _minRealTimeUpdateInterval = 50;
    static const int _minOpenSpaceTimeUpdateInterval = 1; // Should probably be set to real update value of data later
    static const float FULL_PLANE_SIZE = (1391600000.f * 0.5f) / 0.785f; // Half sun radius divided by magic factor
}

namespace openspace {

RenderableSpacecraftCameraPlane::RenderableSpacecraftCameraPlane(const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)
    , _currentActiveChannel("activeChannel", "Active Channel", 0, 0, 9)
    , _moveFactor("movefactor", "Move Factor" , 0.5, 0.0, 1.0)
    , _target("target", "Target", "Sun")
    , _usePBO("usePBO", "Use PBO", true)
    , _resolutionLevel("resolutionlevel", "Level of detail", 3, 0, 5)
    , _frustum(0)
    , _frustumPositionBuffer(0)
    , _asyncUploadPBO("asyncUploadPBO", "Upload to PBO Async", true)
    , _minRealTimeUpdateInterval("minRealTimeUpdateInterval", "Min Update Interval", 75, 0, 300)
    , _sphere(nullptr)
{
    std::string target;
    if (dictionary.getValue("Target", target)) {
        _target = target;
    }

    // // TODO(mnoven): Lua
    std::vector<std::string> paths = {"/home/noven/workspace/OpenSpace/data/solarflarej2k/171/",
                                      //"/home/noven/workspace/OpenSpace/data/solarflarej2k/094/",
                                      "/home/noven/workspace/OpenSpace/data/solarflarej2k/304/",
                                      "/home/noven/workspace/OpenSpace/data/solarflarej2k/193/"};

    std::vector<std::string> tfPaths = {"/home/noven/workspace/OpenSpace/data/sdotransferfunctions/0171_new.txt",
                                        //"/home/noven/workspace/OpenSpace/data/sdotransferfunctions/0094_new.txt",
                                        "/home/noven/workspace/OpenSpace/data/sdotransferfunctions/0304_new.txt",
                                        "/home/noven/workspace/OpenSpace/data/sdotransferfunctions/0193_new.txt"};

    _type = "SDO";
   // int imageSize;
    const int numChannels = paths.size();
    for (int i = 0; i < numChannels; i++) {
        //_imageData.push_back(SpacecraftImageryManager::ref().loadImageData(paths[i], imageSize));
        //SpacecraftImageryManager::ref().scaleImageData(_imageData[i], _type, i);
        _imageMetadata.push_back(SpacecraftImageryManager::ref().loadImageMetadata(paths[i]));
        _transferFunctions.push_back(std::make_unique<TransferFunction>(tfPaths[i]));
    }
    //const std::string jpath = "/home/noven/workspace/OpenSpace/data/realj2kdata/094";
    //SpacecraftImageryManager::ref().ConvertTileJ2kImages(jpath, 1024, 1024);

    _currentActiveChannel.setMaxValue(_imageMetadata.size() - 1);

    // Have to figure out the times of the whole interval first
    ImageMetadata& start = _imageMetadata[_currentActiveChannel][0];
    ImageMetadata& end = _imageMetadata[_currentActiveChannel][ _imageMetadata[_currentActiveChannel].size() - 1];

    _startTimeSequence = start.timeObserved;
    _endTimeSequence = end.timeObserved;

    Time::ref().setTime(_startTimeSequence - 10);

     pboSize = (4096 * 4096 * sizeof(IMG_PRECISION)) / (pow(4, _resolutionLevel));
    _imageSize = 4096 / (pow(2, _resolutionLevel));

    // Generate PBO
    glGenBuffers(2, pboHandles);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboHandles[0]);
    glBufferData(GL_PIXEL_UNPACK_BUFFER, pboSize, 0, GL_STREAM_DRAW);
    // glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboHandles[1]);
    // glBufferData(GL_PIXEL_UNPACK_BUFFER, pboSize, 0, GL_STREAM_DRAW);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

    // TODO(mnoven): Faster to send GL_RGBA32F as internal format - GPU Pads anyways?
    _texture =  std::make_unique<Texture>(
                    nullptr, // Update pixel data later, is this really safe?
                    glm::size3_t(_imageSize, _imageSize, 1),
                    ghoul::opengl::Texture::Red, // Format of the pixeldata
                    GL_R8, // INTERNAL format. More preferable to give explicit precision here, otherwise up to the driver to decide
                    GL_UNSIGNED_BYTE, // Type of data
                    Texture::FilterMode::Nearest,
                    Texture::WrappingMode::ClampToEdge
                );

    _initializePBO = true;
    _future = nullptr;
    _texture->setDataOwnership(ghoul::Boolean::No);
    _texture->uploadTexture();

    _currentActiveImage = 0;
    assert(_currentActiveChannel < numChannels);
    _currentPBO = 0;

    // Initialize time
    _openSpaceTime = Time::ref().j2000Seconds();
    _lastUpdateOpenSpaceTime = 0.0;
    _realTime = duration_cast<milliseconds>(system_clock::now().time_since_epoch());
    _lastUpdateRealTime = _realTime;

    _currentActiveChannel.onChange([this]() {
        const double osTime = Time::ref().j2000Seconds();
        const auto& imageList = _imageMetadata[_currentActiveChannel];
        const auto& low = std::lower_bound(imageList.begin(), imageList.end(), osTime);
        _currentActiveImage = low - imageList.begin();
        if (_currentActiveImage == _imageMetadata[_currentActiveChannel].size()) {
            _currentActiveImage = _currentActiveImage - 1;
        }

          if (_future) {
            _future->wait();
        }
        _future = nullptr;


        LDEBUG("Updating currentactivechannel " << _currentActiveChannel );
        _updatingCurrentActiveChannel = true;

        _texture->bind();
        std::string& currentFilename
              = _imageMetadata[_currentActiveChannel][_currentActiveImage].filename;

        //std::unique_ptr<unsigned char*> data = std::make_unique<unsigned char*>(_imageSize * _imageSize * sizeof(IMG_PRECISION));
        SimpleJ2kCodec j2c;
        j2c.CreateInfileStream(currentFilename);
        j2c.SetupDecoder(_resolutionLevel);
        unsigned char* data = new unsigned char[_imageSize * _imageSize * sizeof(IMG_PRECISION)];
        j2c.DecodeIntoBuffer(data, 0);
        glTexSubImage2D(
            _texture->type(),
            0,
            0,
            0,
            _imageSize,
            _imageSize,
            GLint(_texture->format()),
            _texture->dataType(),
           data
        );
        delete[] data;
    });

    _resolutionLevel.onChange([this]() {
        LDEBUG("Updating level of resolution");
        _updatingCurrentLevelOfResolution = true;
        pboSize = (4096 * 4096 * sizeof(IMG_PRECISION)) / (pow(4, _resolutionLevel));
        _imageSize = 4096 / (pow(2, _resolutionLevel));

        if (_future) {
            _future->wait();
        }
        _future = nullptr;


        _texture->bind();
        std::string& currentFilename
              = _imageMetadata[_currentActiveChannel][_currentActiveImage].filename;

        unsigned char* data
              = new unsigned char[_imageSize * _imageSize * sizeof(IMG_PRECISION)];
        SimpleJ2kCodec j2c;
        j2c.CreateInfileStream(currentFilename);
        j2c.SetupDecoder(_resolutionLevel);
        j2c.DecodeIntoBuffer(data, 0);

        glTexImage2D(
            _texture->type(),
            0,
            _texture->internalFormat(),
            _imageSize,
            _imageSize,
            0,
            GLint(_texture->format()),
            _texture->dataType(),
            data
        );
        delete[] data;
    });

    _moveFactor.onChange([this]() {
        _size.setValue(glm::vec2(_moveFactor.value() * FULL_PLANE_SIZE, 0.f));
        createPlane();
        createFrustum();
    });

    // Initialize PBO
    if (_usePBO) {
        LDEBUG("Initializing PBO with image " << _currentActiveImage);
        uploadImageDataToPBO(_currentActiveImage); // Begin fill PBO 1
      //  updateTextureGPU();
    }

    performImageTimestep();
    addProperty(_resolutionLevel);
    addProperty(_minRealTimeUpdateInterval);
    addProperty(_asyncUploadPBO);
    addProperty(_usePBO);
    addProperty(_currentActiveChannel);
    addProperty(_target);
    addProperty(_moveFactor);
}

bool RenderableSpacecraftCameraPlane::isReady() const {
    return _shader && _texture && _sphere;
}

void RenderableSpacecraftCameraPlane::createFrustum() {
    const GLfloat size = _size.value()[0];
    const GLfloat w = _size.value()[1];

    // Vertex orders x, y, z, w
    // Where w indicates if vertex should be drawn in spacecraft
    // or planes coordinate system
    const GLfloat vertex_data[] = {
        0.f, 0.f, 0.f, 0.0,
        size, size, 0.f , 1.0,
        0.f, 0.f, 0.f, 0.0,
        -size, -size, 0.f , 1.0,
        0.f, 0.f, 0.f, 0.0,
        size, -size, 0.f , 1.0,
        0.f, 0.f, 0.f, 0.0,
        -size, size, 0.f , 1.0,
        // Borders
        // Left
        -size, -size, 0.f, 1.0,
        -size, size, 0.f, 1.0,
        // Top
        -size, size, 0.f, 1.0,
        size, size, 0.f, 1.0,
        // Right
        size, size, 0.f, 1.0,
        size, -size, 0.f, 1.0,
        // Bottom
        size, -size, 0.f, 1.0,
        -size, -size, 0.f, 1.0,
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
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer);

    _size.setValue(glm::vec2(FULL_PLANE_SIZE, 0.f));
    createPlane();
    createFrustum();

    if (!_shader) {
        RenderEngine& renderEngine = OsEng.renderEngine();
        _shader = renderEngine.buildRenderProgram("SpacecraftImagePlaneProgram",
            "${MODULE_SOLARBROWSING}/shaders/spacecraftimageplane_vs.glsl",
            "${MODULE_SOLARBROWSING}/shaders/spacecraftimageplane_fs.glsl"
            );
        if (!_shader) {
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

    if (!_sphereShader) {
        RenderEngine& renderEngine = OsEng.renderEngine();
        _sphereShader = renderEngine.buildRenderProgram("SpacecraftImagePlaneProgram",
            "${MODULE_SOLARBROWSING}/shaders/spacecraftimagesphere_vs.glsl",
            "${MODULE_SOLARBROWSING}/shaders/spacecraftimagesphere_fs.glsl"
            );
        if (!_sphereShader) {
            return false;
        }
    }

     PowerScaledScalar planetSize(glm::vec2(4.8f, 9.f));
    _sphere = std::make_unique<PowerScaledSphere>(PowerScaledSphere(planetSize, 100));

 //   _sphere->initialize();

    // using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    // _shader->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    //_shader->setIgnoreUniformLocationError(IgnoreError::Yes);

    return isReady();
}

bool RenderableSpacecraftCameraPlane::deinitialize() {
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }
    return true;
}

void RenderableSpacecraftCameraPlane::uploadImageDataToPBO(const int& image) {
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboHandles[0]); // 1 - _currentPBO
    // Orphan data and multithread
    glBufferData(GL_PIXEL_UNPACK_BUFFER, pboSize, NULL, GL_STREAM_DRAW);
    // Map buffer to client memory
    //_pboBufferData = static_cast<float*>(glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY));
    _pboBufferData = static_cast<IMG_PRECISION*>(glMapBufferRange(GL_PIXEL_UNPACK_BUFFER, NULL, pboSize, GL_MAP_WRITE_BIT|GL_MAP_INVALIDATE_BUFFER_BIT));
    std::string& currentFilename = _imageMetadata[_currentActiveChannel][image].filename;

   // LDEBUG("Updating PBO data " << _currentActiveChannel );

    if (!_asyncUploadPBO) {
        SimpleJ2kCodec j2c;
        j2c.CreateInfileStream(currentFilename);
        j2c.SetupDecoder(_resolutionLevel);
        j2c.DecodeIntoBuffer(_pboBufferData, 8);
        //j2c.SetResolutionFactor(_resolutionLevel);
        //j2c.DecodeTileIntoBuffer(/*tileid=*/1, _pboBufferData, /*numthreads*/8);
    } else {
        // if (_future) {
        //     _future->wait();
        // }
        // _future = nullptr;

        _future = std::make_unique<std::future<void>>(std::async(std::launch::async,
            [this, &currentFilename]() {
                SimpleJ2kCodec j2c;
                j2c.CreateInfileStream(currentFilename);
                j2c.SetupDecoder(_resolutionLevel);
                j2c.DecodeIntoBuffer(_pboBufferData, 16); // TODO(mnoven): future crashes sometimes if this is multi threaded
                //j2c.SetResolutionFactor(_resolutionLevel);
               // j2c.DecodeTileIntoBuffer(/*tileid=*/1, _pboBufferData, /*numthreads*/8);
            }));
    }

    // Release the mapped buffer
    glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
    // Set back to normal texture data source
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
}

void RenderableSpacecraftCameraPlane::updateTextureGPU() {
   // LDEBUG("Uploading to GPU... " << _currentActiveChannel );
    if (_usePBO) {
        //auto t1 = Clock::now();
       // Wait for texture data from previous frame
        if (_future) {
            _future->wait();
        }
      //  auto t2 = Clock::now();
        // std::cout << "Waiting time for promise: "
        //       << std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count()
        //       << " ms" << std::endl;

        _future = nullptr;
        // Bind PBO to texture data source
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pboHandles[0]);
         const glm::uvec3& dimensions = _texture->dimensions();
        _texture->bind();
        // Send async to GPU by coping from PBO to texture objects
        glTexSubImage2D(
            _texture->type(),
            0,
            0,
            0,
            _imageSize,
            _imageSize,
            GLint(_texture->format()),
            _texture->dataType(),
            nullptr
        );
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    } else {
        std::string& currentFilename
              = _imageMetadata[_currentActiveChannel][_currentActiveImage].filename;

        unsigned char* data = new unsigned char[_imageSize * _imageSize * sizeof(IMG_PRECISION)];
        SimpleJ2kCodec j2c;
        j2c.CreateInfileStream(currentFilename);
        j2c.SetupDecoder(_resolutionLevel);
        j2c.DecodeIntoBuffer(data, 0);

        //j2c.SetResolutionFactor(_resolutionLevel);
        //j2c.DecodeTileIntoBuffer(/*tileid=*/1, data, /*numthreads*/8);

        _texture->bind();
        const glm::uvec3& dimensions = _texture->dimensions();
        glTexSubImage2D(
            _texture->type(),
            0,
            0,
            0,
            _imageSize,
            _imageSize,
            GLint(_texture->format()),
            _texture->dataType(),
           data
        );
        delete[] data;
    }
}

void RenderableSpacecraftCameraPlane::performImageTimestep() {
    const double osTime = Time::ref().j2000Seconds();
    const auto& imageList = _imageMetadata[_currentActiveChannel];

    //TODO(mnoven): Do NOT perform this log(n) lookup every frame - check if
    // still inside current interval => No need to check
    const auto& low = std::lower_bound(imageList.begin(), imageList.end(), osTime);

    int currentActiveImageLast = _currentActiveImage;
    _currentActiveImage = low - imageList.begin();

    if (_currentActiveImage == _imageMetadata[_currentActiveChannel].size()) {
        _currentActiveImage = _currentActiveImage - 1;
    }
    // TODO(mnoven): Clean this up
    if (currentActiveImageLast != _currentActiveImage || _initializePBO
        || _updatingCurrentActiveChannel || _updatingCurrentLevelOfResolution) {
        //  LDEBUG("Updating texture to " << _currentActiveImage);
        //_currentPBO = 1 - _currentPBO;

        // Instant update on current active channel, no need to update from old PBO
        if (!_updatingCurrentActiveChannel && !_updatingCurrentLevelOfResolution) {
            updateTextureGPU();
        } else if (_updatingCurrentActiveChannel) {
            _updatingCurrentActiveChannel = false;
        } else if (_updatingCurrentLevelOfResolution) {
            _updatingCurrentLevelOfResolution = false;
        }

        // Refill PBO
        if (_usePBO /*&& !_initializePBO*/) {
            uploadImageDataToPBO(_currentActiveImage);
        }
        _initializePBO = false;
    }
}

void RenderableSpacecraftCameraPlane::update(const UpdateData& data) {
    _realTime = duration_cast<milliseconds>(system_clock::now().time_since_epoch());
    float realTimeDiff = _realTime.count() - _lastUpdateRealTime.count();

    bool timeToUpdateTexture = realTimeDiff > _minRealTimeUpdateInterval;

    // Future ready, push to texture vector
    // if (_imageData.valid() && DownloadManager::futureReady(_imageData)) {
    //     std::string textureResource = _imageData.get().buffer;
    //     _texture = FitsFileReader::loadTextureFromMemory(textureResource);
    //    // _texture->uploadTexture();
    // }

    // Update texture
    if (timeToUpdateTexture) {
        performImageTimestep();
        _lastUpdateRealTime = _realTime;
        _lastUpdateOpenSpaceTime =_openSpaceTime;
    }

    if (_shader->isDirty())
        _shader->rebuildFromFile();

    if (_sphereShader->isDirty())
        _sphereShader->rebuildFromFile();

    if (_frustumShader->isDirty())
        _sphereShader->rebuildFromFile();

    if (_planeIsDirty)
        createPlane();
}

void RenderableSpacecraftCameraPlane::render(const RenderData& data) {
    if (!isReady()) {
        return;
    }

    glm::mat4 scaleTransform = glm::mat4(1.0);

    // Activate shader
    _shader->activate();

    SceneGraphNode* parent = OsEng.renderEngine().scene()->sceneGraphNode(_nodeName)->parent();
    SceneGraphNode* node = OsEng.renderEngine().scene()->sceneGraphNode(_nodeName);

    SceneGraphNode* sun = OsEng.renderEngine().scene()->sceneGraphNode("Sun");
    glm::dvec3 sunPos = glm::normalize(OsEng.renderEngine().scene()->sceneGraphNode("Sun")->worldRotationMatrix() * glm::dvec3(0.0, 0.0, 1.0));

    // Model transform and view transform needs to be in double precision
    // Sun's barycenter
    SceneGraphNode* p = OsEng.renderEngine().scene()->sceneGraphNode(_target);
    glm::dmat4 rotationTransform = glm::lookAt(glm::normalize(data.modelTransform.translation),
                                         glm::dvec3(p->worldPosition()), data.modelTransform.rotation * glm::dvec3(0.0, 0.0, 1.0));
    rotationTransform = glm::dmat4(glm::inverse(rotationTransform));

    // Scale vector to sun barycenter to get translation distance
    glm::dvec3 translationTransform = (p->worldPosition() - data.modelTransform.translation) * _moveFactor.value();

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation + translationTransform) *
        rotationTransform *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))) *
        glm::dmat4(scaleTransform);
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    glm::dmat4 spacecraftModelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        rotationTransform *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))) *
        glm::dmat4(scaleTransform);

    _shader->setUniform("modelViewProjectionTransform",
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform));

    ghoul::opengl::TextureUnit imageUnit;
    ghoul::opengl::TextureUnit tfUnit;

    imageUnit.activate();
    _texture->bind();
    _shader->setUniform("texture1", imageUnit);

    tfUnit.activate();
    _transferFunctions[_currentActiveChannel]->bind(); // Calls update internally

    _shader->setUniform("texture2", tfUnit);
    //_shader->setUniform("currentActiveChannel", _currentActiveChannel);
    //_shader->setUniform("minIntensity", _imageData[_currentActiveChannel][_currentActiveImage].metaData.min);
    //_shader->setUniform("maxIntensity", _imageData[_currentActiveChannel][_currentActiveImage].metaData.max);
    //_shader->setUniform("expTime", _imageData[_currentActiveChannel][_currentActiveImage].metaData.expTime);

    bool usingFramebufferRenderer =
        OsEng.renderEngine().rendererImplementation() == RenderEngine::RendererImplementation::Framebuffer;

    bool usingABufferRenderer =
        OsEng.renderEngine().rendererImplementation() == RenderEngine::RendererImplementation::ABuffer;

    if (usingABufferRenderer) {
        _shader->setUniform("additiveBlending", _blendMode == BlendMode::Additive);
    }

    bool additiveBlending = _blendMode == BlendMode::Additive && usingFramebufferRenderer;
    if (additiveBlending) {
        glDepthMask(false);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    }

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    if (additiveBlending) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glDepthMask(true);
    }
    _shader->deactivate();

    _frustumShader->activate();

    _frustumShader->setUniform("modelViewProjectionTransform",
        data.camera.projectionMatrix() * glm::mat4(data.camera.combinedViewMatrix() * spacecraftModelTransform));
    _frustumShader->setUniform("modelViewProjectionTransformPlane",data.camera.projectionMatrix() * glm::mat4(modelViewTransform));

    glBindVertexArray(_frustum);
    glDrawArrays(GL_LINES, 0, 16);
    _frustumShader->deactivate();


    // _sphereShader->activate();
    // modelTransform =
    //     glm::translate(glm::dmat4(1.0), sun->worldPosition()) * // Translation
    //     glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
    //     glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));
    // modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    // _sphereShader->setUniform(
    //     "modelViewProjectionTransform",
    //     data.camera.projectionMatrix() * glm::mat4(modelViewTransform)
    // );
    // _sphere->render();
    // _sphereShader->deactivate();
}

} // namespace openspace
