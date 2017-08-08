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

#ifndef __OPENSPACE_MODULE_NEWHORIZONS___RENDERABLEPLANETPROJECTION___H__
#define __OPENSPACE_MODULE_NEWHORIZONS___RENDERABLEPLANETPROJECTION___H__

#include <openspace/rendering/renderable.h>

#include <modules/newhorizons/util/projectioncomponent.h>

#include <openspace/properties/stringproperty.h>

namespace openspace {

namespace documentation { struct Documentation; }

struct Image;

namespace planetgeometry { class PlanetGeometry; }

class RenderablePlanetProjection : public Renderable {
public:
    RenderablePlanetProjection(const ghoul::Dictionary& dictionary);
    ~RenderablePlanetProjection();

    void initialize() override;
    void deinitialize() override;
    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;
    ghoul::opengl::Texture& baseTexture() const;

    static documentation::Documentation Documentation();

protected:
    bool loadTextures();
    void attitudeParameters(double time);


private:
    void imageProjectGPU(std::shared_ptr<ghoul::opengl::Texture> projectionTexture);

    ProjectionComponent _projectionComponent;

    properties::StringProperty _colorTexturePath;
    properties::StringProperty _heightMapTexturePath;

    std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;
    std::unique_ptr<ghoul::opengl::ProgramObject> _fboProgramObject;

    std::unique_ptr<ghoul::opengl::Texture> _baseTexture;
    std::unique_ptr<ghoul::opengl::Texture> _heightMapTexture;

    properties::FloatProperty _heightExaggeration;

    std::unique_ptr<planetgeometry::PlanetGeometry> _geometry;
    
    glm::vec2 _camScaling;
    glm::vec3 _up;
    glm::mat4 _transform;
    glm::mat4 _projectorMatrix;

    glm::dmat3 _stateMatrix;
    glm::dmat3 _instrumentMatrix;
    glm::vec3 _boresight;

    double _time;

    std::vector<Image> _imageTimes;

    std::string _frame;

    bool _capture;

    GLuint _quad;
    GLuint _vertexPositionBuffer;
};

}  // namespace openspace

#endif // __OPENSPACE_MODULE_NEWHORIZONS___RENDERABLEPLANETPROJECTION___H__
