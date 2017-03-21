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
#include <modules/solarbrowsing/util/spacecraftimagerymanager.h>
#include <modules/fitsfilereader/include/fitsfilereader.h>

#include <ghoul/filesystem/directory.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/util/time.h>
#include <math.h>

using namespace ghoul::opengl;

namespace {
    static const std::string _loggerCat = "RenderableSpacecraftCameraPlane";
    static const int _minRealTimeUpdateInterval = 100;
    static const int _minOpenSpaceTimeUpdateInterval = 2;
    static const std::string _dummyImageUrl = "https://sdo.gsfc.nasa.gov/assets/img/swpc/fitsfiles/0094/AIAsynoptic_20170320_185420_0094.fits";
}

namespace openspace {

RenderableSpacecraftCameraPlane::RenderableSpacecraftCameraPlane(const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)
    , _moveFactor("movefactor", "Move Factor" , 0.5, 0.0, 1.0)
    , _target("target", "Target", "Sun")
{
    std::string target;
    if (dictionary.getValue("Target", target)) {
        _target = target;
    }

    currentActiveTexture = 0;
    //downloadTextureResource();
    loadLocalTextures("/home/noven/workspace/OpenSpace/data/fitsfiles");
    updateTexture();
   // loadTexture();

    // Initialize time
    _openSpaceTime = Time::ref().j2000Seconds();
    _lastUpdateOpenSpaceTime = 0.0;
    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch());
    _lastUpdateRealTime = _realTime;

    addProperty(_target);
    addProperty(_moveFactor);
}

// void RenderableSpacecraftCameraPlane::downloadTextureResource() {
//     _imageData = SpacecraftImageryManager::ref().fetchImage(_dummyImageUrl);
// }

// TODO(mnoven): Move to manager
void RenderableSpacecraftCameraPlane::loadLocalTextures(std::string url) {
    using RawPath = ghoul::filesystem::Directory::RawPath;
    ghoul::filesystem::Directory sequenceDir(url, RawPath::Yes);
    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR("Could not load Label Directory '" << sequenceDir.path() << "'");
    }
    using Recursive = ghoul::filesystem::Directory::RawPath;
    using Sort = ghoul::filesystem::Directory::Sort;
    std::vector<std::string> sequancePaths = sequenceDir.read(Recursive::Yes, Sort:: No);

    for (auto path : sequancePaths) {
        if (size_t position = path.find_last_of(".") + 1) {
            if(position != std::string::npos) {
                ghoul::filesystem::File currentFile(path);
                std::string extension = currentFile.fileExtension();
                if(extension == "fits" || extension == "fit") {
                    std::string relativePath = FileSys.relativePath(path);
                    // We'll need to scan the header of the fits
                    // and insert in some smart data structure that handles time / mn
                    _textures.push_back(FitsFileReader::loadTexture(relativePath));
                    //_localImageData.push_back(FitsFileReader::readImageData(relativePath));
                }
            }
        }
    }
}

void RenderableSpacecraftCameraPlane::updateTexture() {
    if (currentActiveTexture + 1 < _textures.size()) {
        currentActiveTexture = currentActiveTexture + 1;
        LDEBUG("Updating texture to " << currentActiveTexture);
        _textures[currentActiveTexture]->uploadTexture();
    }
}

// void RenderableSpacecraftCameraPlane::loadTexture() {
//     std::unique_ptr<Texture> texture;
//     // Dummy start texture for program not to crash
//     std::string s = "2.fit";
//     texture = FitsFileReader::loadTexture(s);

//     if (texture) {
//         texture->uploadTexture();
//         _texture = std::move(texture);
//     }
// }

void RenderableSpacecraftCameraPlane::update(const UpdateData& data) {
    _openSpaceTime = Time::ref().j2000Seconds();
    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch());

    float realTimeDiff = _realTime.count() - _lastUpdateRealTime.count();
    float openspaceDiff = abs(_openSpaceTime-_lastUpdateOpenSpaceTime);

    bool timeToUpdateTexture = (openspaceDiff >= _minOpenSpaceTimeUpdateInterval) && 
                               (realTimeDiff > _minRealTimeUpdateInterval);
    std::chrono::milliseconds _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch());

    // Future ready, push to texture vector
    // if (_imageData.valid() && DownloadManager::futureReady(_imageData)) {
    //     std::string textureResource = _imageData.get().buffer;
    //     _texture = FitsFileReader::loadTextureFromMemory(textureResource);
    //    // _texture->uploadTexture();
    // }

    // Update texture
    if (timeToUpdateTexture) {
        updateTexture();
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

    // Model transform and view transform needs to be in double precision
    glm::dmat4 rotationTransform;
    glm::dvec3 translationTransform;
    rotationTransform = glm::inverse(glm::dmat4(data.camera.viewRotationMatrix()));
    // Sun's barycenter
    SceneGraphNode* p = OsEng.renderEngine().scene()->sceneGraphNode(_target);
    glm::dmat4 rotationTransformTangentTrajectory = glm::lookAt(glm::normalize(data.modelTransform.translation),
                                         glm::dvec3(p->worldPosition()), data.modelTransform.rotation * glm::dvec3(0.0, 0.0, 1.0));
    rotationTransform = glm::dmat4(glm::inverse(rotationTransformTangentTrajectory));

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

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _textures[currentActiveTexture]->bind();
    _shader->setUniform("texture1", unit);

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
