/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/base/rendering/directionallightsource.h>

#include <modules/base/basemodule.h>
#include <modules/base/rendering/renderablemodel.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobjectmanager.h>
#include <limits>

namespace {

    constexpr std::string_view _loggerCat = "DirectionalLightsource";

    constexpr int DepthMapResolutionMultiplier = 4;
    constexpr double ShadowFrustumDistanceMultiplier = 500.0;

    struct [[codegen::Dictionary(DirectionalLightsource)]] Parameters {

    };
#include "directionallightsource_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DirectionalLightSource::Documentation() {
    return codegen::doc<Parameters>("base_renderable_directionallightsource");
}

DirectionalLightSource::DirectionalLightSource(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
{
    setRenderBin(Renderable::RenderBin::Background);
}

bool DirectionalLightSource::isReady() const {
    return true;
}

void DirectionalLightSource::initialize() {
    _depthMapResolution =
        global::renderEngine->renderingResolution() * DepthMapResolutionMultiplier;
}

void DirectionalLightSource::initializeGL() {
    _depthMapProgram = BaseModule::ProgramObjectManager.request(
        "ModelDepthMapProgram",
        [&]() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            std::filesystem::path vs =
                absPath("${MODULE_BASE}/shaders/model_depth_vs.glsl");
            std::filesystem::path fs =
                absPath("${MODULE_BASE}/shaders/model_depth_fs.glsl");

            std::unique_ptr<ghoul::opengl::ProgramObject> prog =
                global::renderEngine->buildRenderProgram(
                    "ModelDepthMapProgram",
                    vs,
                    fs
                );
            prog->setIgnoreAttributeLocationError(
                ghoul::opengl::ProgramObject::IgnoreError::Yes
            );
            prog->setIgnoreUniformLocationError(
                ghoul::opengl::ProgramObject::IgnoreError::Yes
            );
            return prog;
        }
    );
}

void DirectionalLightSource::deinitializeGL() {
    for (const auto& [key, tex] : _depthMaps) {
        glDeleteTextures(1, &tex);
    }

    for (const auto& [key, fbo] : _FBOs) {
        glDeleteFramebuffers(1, &fbo);
    }
}

void DirectionalLightSource::render(const RenderData& data, RendererTasks&){
    for (const auto& [key, casters] : _shadowGroups) {
        if (!_depthMaps.contains(key)) {
            GLint prevFbo;
            glGetIntegerv(GL_FRAMEBUFFER_BINDING, &prevFbo);

            GLuint tex;
            glGenTextures(1, &tex);
            glBindTexture(GL_TEXTURE_2D, tex);
            glTexImage2D(
                GL_TEXTURE_2D,
                0,
                GL_DEPTH_COMPONENT,
                _depthMapResolution.x,
                _depthMapResolution.y,
                0,
                GL_DEPTH_COMPONENT,
                GL_FLOAT,
                nullptr
            );
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
            const glm::vec4 borderColor(1.f, 1.f, 1.f, 1.f);
            glTexParameterfv(
                GL_TEXTURE_2D,
                GL_TEXTURE_BORDER_COLOR,
                glm::value_ptr(borderColor)
            );
            glBindTexture(GL_TEXTURE_2D, 0);
            _depthMaps[key] = tex;

            GLuint fbo;
            glGenFramebuffers(1, &fbo);
            glBindFramebuffer(GL_FRAMEBUFFER, fbo);
            glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, tex, 0);
            glDrawBuffer(GL_NONE);
            glReadBuffer(GL_NONE);
            glBindFramebuffer(GL_FRAMEBUFFER, 0);
            _FBOs[key] = fbo;

            glBindFramebuffer(GL_FRAMEBUFFER, prevFbo);
        }

        glm::dvec3 vmin(std::numeric_limits<double>::max()), vmax(-std::numeric_limits<double>::max());

        std::vector<RenderableModel*> torender;
        for (const std::string& identifier : casters) {
            SceneGraphNode* node = global::renderEngine->scene()->sceneGraphNode(identifier);
            if (node != nullptr) {
                RenderableModel* model = dynamic_cast<RenderableModel*>(node->renderable());
                if (model != nullptr && model->isEnabled() && model->isCastingShadow() && model->isReady()) {
                    const double fsz = model->shadowFrustumSize();
                    glm::dvec3 center = model->center();
                    vmin = glm::min(vmin, center - fsz / 2);
                    vmax = glm::max(vmax, center + fsz / 2);

                    torender.push_back(model);
                }
            }
        }

        double sz = glm::length(vmax - vmin);
        double d = sz * ShadowFrustumDistanceMultiplier;
        glm::dvec3 center = vmin + (vmax - vmin) * 0.5;

        SceneGraphNode* parentNode = parent();
        if (!parentNode) {
            LERROR("DirectionalLightSource must have a parent node");
            continue;
        }

        glm::dvec3 light = parentNode->modelTransform() * glm::dvec4(0.0, 0.0, 0.0, 1.0);
        glm::dvec3 light_dir = glm::normalize(center - light);
        glm::dvec3 right = glm::normalize(glm::cross(glm::dvec3(0, 1, 0), light_dir));
        glm::dvec3 eye = center - light_dir * d;
        glm::dvec3 up = glm::cross(right, light_dir);

        glm::dmat4 view = glm::lookAt(eye, center, up);
        glm::dmat4 projection = glm::ortho(
            -sz,
            sz,
            -sz,
            sz,
            d - sz,
            d + sz
        );
        glm::dmat4 vp = projection * view;

        GLint prevProg;
        glGetIntegerv(GL_CURRENT_PROGRAM, &prevProg);

        GLint prevFbo;
        glGetIntegerv(GL_FRAMEBUFFER_BINDING, &prevFbo);

        GLint prevVp[4];
        glGetIntegerv(GL_VIEWPORT, prevVp);

        glBindFramebuffer(GL_FRAMEBUFFER, _FBOs[key]);
        glViewport(0, 0, _depthMapResolution.x, _depthMapResolution.y);
        glClear(GL_DEPTH_BUFFER_BIT);

        for (const RenderableModel* model : torender) {
           model->renderForDepthMap(vp);
        }

        glBindFramebuffer(GL_FRAMEBUFFER, 0);

        _vps[key] = vp;

        glUseProgram(prevProg);
        glBindFramebuffer(GL_FRAMEBUFFER, prevFbo);
        glViewport(prevVp[0], prevVp[1], prevVp[2], prevVp[3]);
    }
}

void DirectionalLightSource::registerShadowCaster(const std::string& shadowgroup, const std::string& identifier) {
    _shadowGroups[shadowgroup].push_back(identifier);
}

const GLuint& DirectionalLightSource::depthMap(const std::string& shadowgroup) const {
    return _depthMaps.at(shadowgroup);
}

glm::dmat4 DirectionalLightSource::viewProjectionMatrix(const std::string& shadowgroup) const {
    return _vps.at(shadowgroup);
}

} // namespace openspace
