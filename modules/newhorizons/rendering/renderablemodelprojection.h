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

#ifndef __OPENSPACE_MODULE_NEWHORIZONS___RENDERABLEMODELPROJECTION___H__
#define __OPENSPACE_MODULE_NEWHORIZONS___RENDERABLEMODELPROJECTION___H__

#include <openspace/rendering/renderable.h>

#include <modules/newhorizons/util/projectioncomponent.h>

#include <modules/newhorizons/util/imagesequencer.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/vec3property.h>

#include <openspace/util/powerscaledcoordinate.h>

namespace ghoul {
namespace opengl {
class ProgramObject;
class Texture;
}
}

namespace openspace {
namespace documentation { struct Documentation; } 

struct RenderData;
struct UpdateData;

namespace modelgeometry {
    class ModelGeometry;
}

class RenderableModelProjection : public Renderable {
public:
    RenderableModelProjection(const ghoul::Dictionary& dictionary);
    ~RenderableModelProjection();

    bool initialize() override;
    bool deinitialize() override;

    bool isReady() const override;

    void render(const RenderData& data) override;
    virtual void update(const UpdateData& data) final override;

    ghoul::opengl::Texture& baseTexture() const;

    static documentation::Documentation Documentation();

private:
    bool loadTextures();
    void attitudeParameters(double time);
    void imageProjectGPU(std::shared_ptr<ghoul::opengl::Texture> projectionTexture);

    void project();

    ProjectionComponent _projectionComponent;

    properties::StringProperty _colorTexturePath;

    properties::Vec3Property _rotation;

    std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;
    std::unique_ptr<ghoul::opengl::ProgramObject> _fboProgramObject;
    std::unique_ptr<ghoul::opengl::ProgramObject> _depthFboProgramObject;

    std::unique_ptr<ghoul::opengl::Texture> _baseTexture;

    std::unique_ptr<modelgeometry::ModelGeometry> _geometry;

    glm::dmat3 _stateMatrix;
    glm::dmat3 _instrumentMatrix;

    // uniforms
    glm::vec2  _camScaling;
    glm::vec3  _up;
    glm::mat4  _transform;
    glm::mat4  _projectorMatrix;
    glm::vec3  _boresight;

    std::vector<Image> _imageTimes;
    double _time;

    bool _capture;
        
    psc _sunPosition;
    properties::BoolProperty _performShading;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_NEWHORIZONS___RENDERABLEMODELPROJECTION___H__
