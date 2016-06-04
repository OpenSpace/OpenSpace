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

#ifndef __RENDERABLEGALAXY_H__
#define __RENDERABLEGALAXY_H__

#include <openspace/properties/vectorproperty.h>
#include <openspace/util/boxgeometry.h>
#include <openspace/rendering/renderable.h>
#include <modules/galaxy/rendering/galaxyraycaster.h>
#include <modules/volume/rawvolume.h>

namespace openspace {

struct RenderData;
    
class RenderableGalaxy : public Renderable {
public:
    RenderableGalaxy(const ghoul::Dictionary& dictionary);
    ~RenderableGalaxy();
    
    bool initialize() override;
    bool deinitialize() override;
    bool isReady() const override;
    void render(const RenderData& data, RendererTasks& tasks) override;
    void postRender(const RenderData& data) override;
    void update(const UpdateData& data) override;

private:
    float safeLength(const glm::vec3& vector);

    glm::vec3 _volumeSize;
    glm::vec3 _pointScaling;
    properties::FloatProperty _stepSize;
    properties::FloatProperty _pointStepSize;
    properties::Vec3Property _translation;
    properties::Vec3Property _rotation;

    std::string _volumeFilename;
    glm::ivec3 _volumeDimensions;
    std::string _pointsFilename;

    std::unique_ptr<GalaxyRaycaster> _raycaster;
    std::unique_ptr<RawVolume<glm::tvec4<GLfloat>>> _volume;
    std::unique_ptr<ghoul::opengl::Texture> _texture;
    glm::mat4 _pointTransform;
    glm::vec3 _aspect;
    float _opacityCoefficient;

    std::unique_ptr<ghoul::opengl::ProgramObject> _pointsProgram;
    size_t _nPoints;
    GLuint _pointsVao;
    GLuint _positionVbo;
    GLuint _colorVbo;
};
}

#endif // __RENDERABLEGALAXY_H__
