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

#ifndef __RENDERABLEMODELPROJECTION_H__
#define __RENDERABLEMODELPROJECTION_H__

#include <openspace/rendering/renderable.h>
#include <modules/newhorizons/util/projectioncomponent.h>

#include <modules/base/rendering/modelgeometry.h>
#include <modules/newhorizons/util/imagesequencer.h>

#include <openspace/properties/numericalproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vectorproperty.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

namespace openspace {

namespace modelgeometry {
    class ModelGeometry;
}

class RenderableModelProjection : public Renderable {
public:
    RenderableModelProjection(const ghoul::Dictionary& dictionary);

    bool initialize() override;
    bool deinitialize() override;

    bool isReady() const override;

    void render(const RenderData& data) override;
    void update(const UpdateData& data) override;

    ghoul::opengl::Texture& baseTexture() const;

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

    std::string _defaultProjImage;
    std::string _source;
    std::string _destination;
    std::string _target;

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

}  // namespace openspace

#endif  // __RENDERABLEMODELPROJECTION_H__