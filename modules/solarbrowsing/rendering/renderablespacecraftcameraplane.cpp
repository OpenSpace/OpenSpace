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
#include <math.h>

using namespace ghoul::opengl;
using namespace std::chrono;

namespace {
    static const std::string _loggerCat = "RenderableSpacecraftCameraPlane";
    static const int _minRealTimeUpdateInterval = 10;
    static const int _minOpenSpaceTimeUpdateInterval = 1; // Should probably be set to real update value of data later
    //static const std::string _dummyImageUrl = "https://sdo.gsfc.nasa.gov/assets/img/swpc/fitsfiles/0094/AIAsynoptic_20170320_185420_0094.fits";
    std::vector<std::vector<std::vector<int>>> _map;
}

namespace openspace {

RenderableSpacecraftCameraPlane::RenderableSpacecraftCameraPlane(const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)
    , _moveFactor("movefactor", "Move Factor" , 0.5, 0.0, 1.0)
    , _target("target", "Target", "Sun")
    , _currentActiveChannel("activeChannel", "Active Channel", 3, 0, 9)
{
    std::string target;
    if (dictionary.getValue("Target", target)) {
        _target = target;
    }


    // TODO(mnoven): Lua
    // std::vector<std::string> paths = {"/home/noven/workspace/OpenSpace/data/realfitsdata/171", // 0
    //                                   "/home/noven/workspace/OpenSpace/data/realfitsdata/171", // 1
    //                                   "/home/noven/workspace/OpenSpace/data/realfitsdata/171", // 2
    //                                   "/home/noven/workspace/OpenSpace/data/realfitsdata/094", // 3
    //                                   "/home/noven/workspace/OpenSpace/data/realfitsdata/131", // 4
    //                                   "/home/noven/workspace/OpenSpace/data/realfitsdata/171", // 5
    //                                   "/home/noven/workspace/OpenSpace/data/realfitsdata/193", // 6
    //                                   "/home/noven/workspace/OpenSpace/data/realfitsdata/211", // 7
    //                                   "/home/noven/workspace/OpenSpace/data/realfitsdata/304", // 8
    //                                   "/home/noven/workspace/OpenSpace/data/realfitsdata/335"};// 9

    std::vector<std::string> paths =   {"/home/noven/workspace/OpenSpace/data/smallfitsseq/sdoseq0171", // 0
                                      "/home/noven/workspace/OpenSpace/data/smallfitsseq/sdoseq0171", // 1
                                      "/home/noven/workspace/OpenSpace/data/smallfitsseq/sdoseq0171", // 2
                                      "/home/noven/workspace/OpenSpace/data/smallfitsseq/sdoseq0094", // 3 // OK
                                      "/home/noven/workspace/OpenSpace/data/smallfitsseq/sdoseq0171", // 4
                                      "/home/noven/workspace/OpenSpace/data/smallfitsseq/sdoseq0171", // 5 // OK
                                      "/home/noven/workspace/OpenSpace/data/smallfitsseq/sdoseq0193", // 6 // OK
                                      "/home/noven/workspace/OpenSpace/data/smallfitsseq/sdoseq0211", // 7 // OK
                                      "/home/noven/workspace/OpenSpace/data/smallfitsseq/sdoseq0304", // 8 // OK
                                      "/home/noven/workspace/OpenSpace/data/smallfitsseq/sdoseq0171"};// 9


    std::vector<std::string> tfPaths = {"/home/noven/workspace/OpenSpace/data/sdotransferfunctions/custom.txt",   // 0
                                        "/home/noven/workspace/OpenSpace/data/sdotransferfunctions/custom.txt",   // 1
                                        "/home/noven/workspace/OpenSpace/data/sdotransferfunctions/custom.txt",   // 2
                                        "/home/noven/workspace/OpenSpace/data/sdotransferfunctions/0094_new.txt", // 3
                                        "/home/noven/workspace/OpenSpace/data/sdotransferfunctions/0131_new.txt", // 4
                                        "/home/noven/workspace/OpenSpace/data/sdotransferfunctions/0171_new.txt", // 5
                                        "/home/noven/workspace/OpenSpace/data/sdotransferfunctions/0193_new.txt", // 6
                                        "/home/noven/workspace/OpenSpace/data/sdotransferfunctions/0211_new.txt", // 7
                                        "/home/noven/workspace/OpenSpace/data/sdotransferfunctions/0304_new.txt", // 8
                                        "/home/noven/workspace/OpenSpace/data/sdotransferfunctions/0335_new.txt"};// 9

    _type = "SDO";
    int imageSize;
    const int numChannels = 10;
    _imageData.reserve(numChannels);
    for (int i = 0; i < numChannels; i++) {
        _imageData.push_back(SpacecraftImageryManager::ref().loadImageData(paths[i], imageSize));
        SpacecraftImageryManager::ref().scaleImageData(_imageData[i], _type, i);
        _transferFunctions.push_back(std::make_unique<TransferFunction>(tfPaths[i]));
    }

    _texture =  std::make_unique<Texture>(
                    glm::size3_t(imageSize, imageSize, 1),
                    ghoul::opengl::Texture::Red, // Format of the pixeldata
                    GL_R32F, // INTERNAL format. More preferable to give explicit precision here, otherwise up to the driver to decide
                    GL_FLOAT, // Type of data
                    Texture::FilterMode::Linear,
                    Texture::WrappingMode::ClampToEdge
                );

    _texture->setDataOwnership(ghoul::Boolean::No);
    _texture->uploadTexture();

    _currentActiveImage = -1;
    assert(_currentActiveChannel < numChannels);

    // Initialize time
    _openSpaceTime = Time::ref().j2000Seconds();
    _lastUpdateOpenSpaceTime = 0.0;
    _realTime = duration_cast<milliseconds>(system_clock::now().time_since_epoch());
    _lastUpdateRealTime = _realTime;

    _currentActiveChannel.onChange([this]() {
        updateTexture();
    });

    performImageTimestep();
    addProperty(_currentActiveChannel);
    addProperty(_target);
    addProperty(_moveFactor);
}

bool RenderableSpacecraftCameraPlane::isReady() const {
    return _shader && _texture;
}

bool RenderableSpacecraftCameraPlane::initialize() {
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    createPlane();

    if (!_shader) {
        RenderEngine& renderEngine = OsEng.renderEngine();
        _shader = renderEngine.buildRenderProgram("SpacecraftImagePlaneProgram",
            "${MODULE_SOLARBROWSING}/shaders/spacecraftimageplane_vs.glsl",
            "${MODULE_SOLARBROWSING}/shaders/spacecraftimageplane_fs.glsl"
            );
        if (!_shader)
            return false;
    }

    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    _shader->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _shader->setIgnoreUniformLocationError(IgnoreError::Yes);

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

void RenderableSpacecraftCameraPlane::updateTexture() {
    std::valarray<float>& contents = _imageData[_currentActiveChannel][_currentActiveImage].contents;
    _texture->setPixelData(&contents[0], ghoul::Boolean::No);

    // TODO(mnoven): This should probably be moved to Texture class
    _texture->bind();
    const glm::uvec3& dimensions = _texture->dimensions();
    glTexSubImage2D(
        _texture->type(),
        0,
        0,
        0,
        GLsizei(dimensions.x),
        GLsizei(dimensions.y),
        GLint(_texture->format()),
        _texture->dataType(),
        _texture->pixelData()
    );
}

void RenderableSpacecraftCameraPlane::performImageTimestep() {
    int clockwiseSign = (Time::ref().deltaTime()>0) ? 1 : -1;
    int newIndex = clockwiseSign + _currentActiveImage;
    if (newIndex < _imageData[_currentActiveChannel].size() && newIndex >= 0) {
        LDEBUG("Updating texture to " << newIndex);
        _currentActiveImage = newIndex;
        updateTexture();
    }
}

void RenderableSpacecraftCameraPlane::update(const UpdateData& data) {
    _openSpaceTime = Time::ref().j2000Seconds();
    _realTime = duration_cast<milliseconds>(system_clock::now().time_since_epoch());

    float realTimeDiff = _realTime.count() - _lastUpdateRealTime.count();
    float openspaceDiff = abs(_openSpaceTime-_lastUpdateOpenSpaceTime);

    bool timeToUpdateTexture = (openspaceDiff >= _minOpenSpaceTimeUpdateInterval) &&
                               (realTimeDiff > _minRealTimeUpdateInterval);

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
    glm::dvec3 translationTransform;
    // Sun's barycenter
    SceneGraphNode* p = OsEng.renderEngine().scene()->sceneGraphNode(_target);
    glm::dmat4 rotationTransform = glm::lookAt(glm::normalize(data.modelTransform.translation),
                                         glm::dvec3(p->worldPosition()), data.modelTransform.rotation * glm::dvec3(0.0, 0.0, 1.0));
    rotationTransform = glm::dmat4(glm::inverse(rotationTransform));

    // Scale vector to sun barycenter to get translation distance
    translationTransform = (p->worldPosition() - data.modelTransform.translation) * _moveFactor.value();

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation + translationTransform) *
        rotationTransform *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))) *
        glm::dmat4(scaleTransform);
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

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
}

} // namespace openspace
