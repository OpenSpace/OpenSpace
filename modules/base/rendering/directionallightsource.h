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

#ifndef __OPENSPACE_MODULE_BASE___DIRECTIONALLIGHTSOURCE___H__
#define __OPENSPACE_MODULE_BASE___DIRECTIONALLIGHTSOURCE___H__

#include <openspace/rendering/renderable.h>
#include <openspace/scene/lightsource.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/glm.h>
#include <map>
#include <string>
#include <vector>

namespace ghoul::opengl { class ProgramObject; }
namespace openspace::documentation { struct Documentation; }

namespace openspace {

/**
 * DirectionalLightSource is a Renderable that manages shadow mapping for directional
 * light sources. It creates depth maps from the light's perspective and manages shadow
 * groups to optimize rendering when multiple objects cast shadows.
 *
 * Shadow groups allow models near each other to share a shadow map, reducing memory
 * usage and improving performance by grouping shadow casters that should render into
 * the same depth buffer.
 */
class DirectionalLightSource : public Renderable {
public:
    struct DepthMapData {
        glm::dmat4 viewProjection;
        GLuint depthMap;
    };

    explicit DirectionalLightSource(const ghoul::Dictionary& dictionary);
    ~DirectionalLightSource() override = default;

    bool isReady() const override;

    virtual void initialize() override;

    virtual void initializeGL() override;

    virtual void deinitializeGL() override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;

    void registerShadowCaster(const std::string& shadowgroup, const std::string& identifier);

    const GLuint& depthMap(const std::string& shadowgroup) const;

    glm::dmat4 viewProjectionMatrix(const std::string& shadowgroup) const;

    static documentation::Documentation Documentation();

private:
    glm::ivec2 _depthMapResolution;
    std::map<std::string, std::vector<std::string>> _shadowGroups;
    std::map<std::string, GLuint> _depthMaps;
    std::map<std::string, GLuint> _fbos;
    std::map<std::string, glm::dmat4> _viewports;
    ghoul::opengl::ProgramObject* _depthMapProgram = nullptr;
};

}// namespace openspace

#endif // __OPENSPACE_MODULE_BASE___DIRECTIONALLIGHTSOURCE___H__
