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

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureconversion.h>
#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>

namespace openspace {

ProjectionComponent::ProjectionComponent()
    : _performProjection("performProjection", "Perform Projections", true)
    , _clearAllProjections("clearAllProjections", "Clear Projections", false)
    , _projectionFading("projectionFading", "Projection Fading", 1.f, 0.f, 1.f)
    , _projectionTexture(nullptr)
{

}

bool ProjectionComponent::auxiliaryRendertarget() {
    bool completeSuccess = true;

    GLint defaultFBO;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);

    // setup FBO
    glGenFramebuffers(1, &_fboID);
    glBindFramebuffer(GL_FRAMEBUFFER, _fboID);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, *_projectionTexture, 0);
    // check FBO status
    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE)
        completeSuccess &= false;
    // switch back to window-system-provided framebuffer
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);

    return completeSuccess;
}

glm::mat4 ProjectionComponent::computeProjectorMatrix(const glm::vec3 loc, glm::dvec3 aim, const glm::vec3 up,
                                                      const glm::dmat3& instrumentMatrix,
                                                      float fieldOfViewY,
                                                      float aspectRatio,
                                                      float nearPlane,
                                                      float farPlane,
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

std::unique_ptr<ghoul::opengl::Texture> ProjectionComponent::loadProjectionTexture(const std::string& texturePath) {
    std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(absPath(texturePath));
    if (texture) {
        ghoul::opengl::convertTextureFormat(ghoul::opengl::Texture::Format::RGB, *texture);
        texture->uploadTexture();
        // TODO: AnisotropicMipMap crashes on ATI cards ---abock
        //_textureProj->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
        texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
        texture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToBorder);
    }
    return texture;
}

void ProjectionComponent::generateProjectionLayerTexture() {
    int maxSize = OpenGLCap.max2DTextureSize() / 2;

    LINFOC(
        "ProjectionComponent",
        "Creating projection texture of size '" << maxSize << ", " << maxSize / 2 << "'"
    );
    _projectionTexture = std::make_unique<ghoul::opengl::Texture> (
        glm::uvec3(maxSize, maxSize / 2, 1),
        ghoul::opengl::Texture::Format::RGBA
        );
    _projectionTexture->uploadTexture();
}

} // namespace openspace
