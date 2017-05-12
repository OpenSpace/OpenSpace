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

#ifndef __OPENSPACE_MODULE_GALAXY___GALAXYRAYCASTER___H__
#define __OPENSPACE_MODULE_GALAXY___GALAXYRAYCASTER___H__

#include <memory>

#include <ghoul/glm.h>
#include <string>
#include <vector>
#include <openspace/rendering/volumeraycaster.h>
#include <openspace/util/boxgeometry.h>
#include <openspace/util/blockplaneintersectiongeometry.h>

namespace ghoul {
    namespace opengl {
        class Texture;
        class TextureUnit;
        class ProgramObject;
    }
}

namespace openspace {

struct RenderData;
class RaycastData;

class GalaxyRaycaster : public VolumeRaycaster {
public:
    GalaxyRaycaster(ghoul::opengl::Texture& texture);

    virtual ~GalaxyRaycaster();
    void initialize();
    void deinitialize();
    void renderEntryPoints(const RenderData& data, ghoul::opengl::ProgramObject& program) override;
    void renderExitPoints(const RenderData& data, ghoul::opengl::ProgramObject& program) override;
    void preRaycast(const RaycastData& data, ghoul::opengl::ProgramObject& program) override;
    void postRaycast(const RaycastData& data, ghoul::opengl::ProgramObject& program) override;
    bool cameraIsInside(const RenderData& data, glm::vec3& localPosition) override;


    std::string getBoundsVsPath() const override;
    std::string getBoundsFsPath() const override;
    std::string getRaycastPath() const override;
    std::string getHelperPath() const override;

    void setAspect(const glm::vec3& aspect);
    void setModelTransform(glm::mat4 transform);
    void setTime(double time);
    void setStepSize(float stepSize);
    void setOpacityCoefficient(float opacityCoefficient);
private:
    BoxGeometry _boundingBox;
    float _stepSize;
    glm::mat4 _modelTransform;
    glm::vec3 _aspect;
    double _time;
    float _opacityCoefficient;
    ghoul::opengl::Texture& _texture;
    std::unique_ptr<ghoul::opengl::TextureUnit> _textureUnit;

}; // GalaxyRaycaster

} // namespace openspace

#endif // __OPENSPACE_MODULE_GALAXY___GALAXYRAYCASTER___H__
