/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <modules/newhorizons/util/projectioncomponent.h>

#include <modules/newhorizons/util/hongkangparser.h>
#include <modules/newhorizons/util/imagesequencer.h>
#include <modules/newhorizons/util/labelparser.h>

#include <openspace/scene/scenegraphnode.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureconversion.h>
#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>

namespace {
    const std::string keyPotentialTargets = "PotentialTargets";

    const std::string keyInstrument = "Instrument.Name";
    const std::string keyInstrumentFovy = "Instrument.Fovy";
    const std::string keyInstrumentAspect = "Instrument.Aspect";
    const std::string keyInstrumentNear = "Instrument.Near";
    const std::string keyInstrumentFar = "Instrument.Far";

    const std::string keyProjObserver = "Projection.Observer";
    const std::string keyProjTarget = "Projection.Target";
    const std::string keyProjAberration = "Projection.Aberration";

    const std::string keySequenceDir = "Projection.Sequence";
    const std::string keySequenceType = "Projection.SequenceType";
    const std::string keyTranslation = "DataInputTranslation";

    const std::string sequenceTypeImage = "image-sequence";
    const std::string sequenceTypePlaybook = "playbook";
    const std::string sequenceTypeHybrid = "hybrid";

    const std::string _loggerCat = "ProjectionComponent";
}

namespace openspace {

using ghoul::Dictionary;

ProjectionComponent::ProjectionComponent()
    : _performProjection("performProjection", "Perform Projections", true)
    , _clearAllProjections("clearAllProjections", "Clear Projections", false)
    , _projectionFading("projectionFading", "Projection Fading", 1.f, 0.f, 1.f)
    , _projectionTexture(nullptr)
{}

bool ProjectionComponent::initialize() {
    bool a = generateProjectionLayerTexture();
    bool b = auxiliaryRendertarget();
    return a && b;
}

bool ProjectionComponent::deinitialize() {
    _projectionTexture = nullptr;

    glDeleteFramebuffers(1, &_fboID);

    return true;
}

bool ProjectionComponent::initializeProjectionSettings(const Dictionary& dictionary) {
    bool completeSuccess = true;
    completeSuccess &= dictionary.getValue(keyInstrument, _instrumentID);
    completeSuccess &= dictionary.getValue(keyProjObserver, _projectorID);
    completeSuccess &= dictionary.getValue(keyProjTarget, _projecteeID);
    completeSuccess &= dictionary.getValue(keyInstrumentFovy, _fovy);
    completeSuccess &= dictionary.getValue(keyInstrumentAspect, _aspectRatio);
    completeSuccess &= dictionary.getValue(keyInstrumentNear, _nearPlane);
    completeSuccess &= dictionary.getValue(keyInstrumentFar, _farPlane);
    ghoul_assert(completeSuccess, "All neccessary attributes not found in modfile");

    std::string a = "NONE";
    bool s = dictionary.getValue(keyProjAberration, a);
    _aberration = SpiceManager::AberrationCorrection(a);
    completeSuccess &= s;
    ghoul_assert(completeSuccess, "All neccessary attributes not found in modfile");


    if (dictionary.hasKeyAndValue<ghoul::Dictionary>(keyPotentialTargets)) {
        ghoul::Dictionary potentialTargets = dictionary.value<ghoul::Dictionary>(
            keyPotentialTargets
        );

        _potentialTargets.resize(potentialTargets.size());
        for (int i = 0; i < potentialTargets.size(); ++i) {
            std::string target;
            potentialTargets.getValue(std::to_string(i + 1), target);
            _potentialTargets[i] = target;
        }
    }
    return completeSuccess;
}

bool ProjectionComponent::initializeParser(const ghoul::Dictionary& dictionary) {
    bool completeSuccess = true;

    std::string name;
    dictionary.getValue(SceneGraphNode::KeyName, name);

    SequenceParser* parser;

    std::string sequenceSource;
    std::string sequenceType;
    bool foundSequence = dictionary.getValue(keySequenceDir, sequenceSource);
    if (foundSequence) {
        sequenceSource = absPath(sequenceSource);

        foundSequence = dictionary.getValue(keySequenceType, sequenceType);
        //Important: client must define translation-list in mod file IFF playbook
        if (dictionary.hasKey(keyTranslation)) {
            ghoul::Dictionary translationDictionary;
            //get translation dictionary
            dictionary.getValue(keyTranslation, translationDictionary);

            if (sequenceType == sequenceTypePlaybook) {
                parser = new HongKangParser(name,
                                            sequenceSource,
                                            _projectorID,
                                            translationDictionary,
                                            _potentialTargets);
                openspace::ImageSequencer::ref().runSequenceParser(parser);
            }
            else if (sequenceType == sequenceTypeImage) {
                parser = new LabelParser(name,
                                         sequenceSource,
                                         translationDictionary);
                openspace::ImageSequencer::ref().runSequenceParser(parser);
            }
            else if (sequenceType == sequenceTypeHybrid) {
                //first read labels
                parser = new LabelParser(name,
                                         sequenceSource,
                                         translationDictionary);
                openspace::ImageSequencer::ref().runSequenceParser(parser);

                std::string _eventFile;
                bool foundEventFile = dictionary.getValue("Projection.EventFile", _eventFile);
                if (foundEventFile) {
                    //then read playbook
                    _eventFile = absPath(_eventFile);
                    parser = new HongKangParser(name,
                                                _eventFile,
                                                _projectorID,
                                                translationDictionary,
                                                _potentialTargets);
                    openspace::ImageSequencer::ref().runSequenceParser(parser);
                }
                else {
                    LWARNING("No eventfile has been provided, please check modfiles");
                }
            }
        }
        else {
            LWARNING("No playbook translation provided, please make sure all spice calls match playbook!");
        }
    }

    return completeSuccess;
}

void ProjectionComponent::imageProjectBegin() {
    // keep handle to the current bound FBO
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &_defaultFBO);

    glGetIntegerv(GL_VIEWPORT, _viewport);
    glBindFramebuffer(GL_FRAMEBUFFER, _fboID);

    glViewport(
        0, 0,
        static_cast<GLsizei>(_projectionTexture->width()),
        static_cast<GLsizei>(_projectionTexture->height())
    );
}

void ProjectionComponent::imageProjectEnd() {
    glBindFramebuffer(GL_FRAMEBUFFER, _defaultFBO);
    glViewport(_viewport[0], _viewport[1], _viewport[2], _viewport[3]);
}

bool ProjectionComponent::auxiliaryRendertarget() {
    bool completeSuccess = true;

    GLint defaultFBO;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);

    // setup FBO
    glGenFramebuffers(1, &_fboID);
    glBindFramebuffer(GL_FRAMEBUFFER, _fboID);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        *_projectionTexture,
        0
    );
    // check FBO status
    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE)
        completeSuccess &= false;
    // switch back to window-system-provided framebuffer
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);

    return completeSuccess;
}

glm::mat4 ProjectionComponent::computeProjectorMatrix(const glm::vec3 loc, glm::dvec3 aim,
                                                      const glm::vec3 up,
                                                      const glm::dmat3& instrumentMatrix,
                                                      float fieldOfViewY,
                                                      float aspectRatio,
                                                      float nearPlane, float farPlane,
                                                      glm::vec3& boreSight)
{
    //rotate boresight into correct alignment
    boreSight = instrumentMatrix*aim;
    glm::vec3 uptmp(instrumentMatrix*glm::dvec3(up));

    // create view matrix
    glm::vec3 e3 = glm::normalize(boreSight);
    glm::vec3 e1 = glm::normalize(glm::cross(uptmp, e3));
    glm::vec3 e2 = glm::normalize(glm::cross(e3, e1));
    glm::mat4 projViewMatrix = glm::mat4(e1.x, e2.x, e3.x, 0.f,
                                         e1.y, e2.y, e3.y, 0.f,
                                         e1.z, e2.z, e3.z, 0.f,
                                         -glm::dot(e1, loc), -glm::dot(e2, loc), -glm::dot(e3, loc), 1.f);
    // create perspective projection matrix
    glm::mat4 projProjectionMatrix = glm::perspective(glm::radians(fieldOfViewY), aspectRatio, nearPlane, farPlane);
    // bias matrix
    glm::mat4 projNormalizationMatrix = glm::mat4(0.5f, 0, 0, 0,
                                                  0, 0.5f, 0, 0,
                                                  0, 0, 0.5f, 0,
                                                  0.5f, 0.5f, 0.5f, 1);
    return projNormalizationMatrix*projProjectionMatrix*projViewMatrix;
}

void ProjectionComponent::clearAllProjections() {
    // keep handle to the current bound FBO
    GLint defaultFBO;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);

    GLint m_viewport[4];
    glGetIntegerv(GL_VIEWPORT, m_viewport);
    //counter = 0;
    glBindFramebuffer(GL_FRAMEBUFFER, _fboID);

    glViewport(0, 0, static_cast<GLsizei>(_projectionTexture->width()), static_cast<GLsizei>(_projectionTexture->height()));

    glClearColor(0.f, 0.f, 0.f, 0.f);
    glClear(GL_COLOR_BUFFER_BIT);

    //bind back to default
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    glViewport(m_viewport[0], m_viewport[1],
               m_viewport[2], m_viewport[3]);

    _clearAllProjections = false;
}

std::unique_ptr<ghoul::opengl::Texture> ProjectionComponent::loadProjectionTexture(
                                                           const std::string& texturePath)
{
    using std::unique_ptr;
    using ghoul::opengl::Texture;
    using ghoul::io::TextureReader;
    unique_ptr<Texture> texture = TextureReader::ref().loadTexture(absPath(texturePath));
    if (texture) {
        ghoul::opengl::convertTextureFormat(ghoul::opengl::Texture::Format::RGB, *texture);
        texture->uploadTexture();
        // TODO: AnisotropicMipMap crashes on ATI cards ---abock
        //_textureProj->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
        texture->setFilter(Texture::FilterMode::Linear);
        texture->setWrapping(Texture::WrappingMode::ClampToBorder);
    }
    return texture;
}

bool ProjectionComponent::generateProjectionLayerTexture() {
    int maxSize = OpenGLCap.max2DTextureSize() / 2;

    LINFO(
        "Creating projection texture of size '" << maxSize << ", " << maxSize / 2 << "'"
    );
    _projectionTexture = std::make_unique<ghoul::opengl::Texture> (
        glm::uvec3(maxSize, maxSize / 2, 1),
        ghoul::opengl::Texture::Format::RGBA
        );
    if (_projectionTexture)
        _projectionTexture->uploadTexture();
    
    return _projectionTexture != nullptr;

}

} // namespace openspace
