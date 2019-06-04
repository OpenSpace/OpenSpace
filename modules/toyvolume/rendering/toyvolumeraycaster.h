/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#ifndef __OPENSPACE_MODULE_TOYVOLUME___TOYVOLUMERAYCASTER___H__
#define __OPENSPACE_MODULE_TOYVOLUME___TOYVOLUMERAYCASTER___H__

#include <openspace/rendering/volumeraycaster.h>

#include <openspace/util/boxgeometry.h>
#include <ghoul/glm.h>
#include <string>

namespace ghoul::opengl {
    class Texture;
    class ProgramObject;
} // namespace ghoul::opengl

namespace openspace {

struct RenderData;
struct RaycastData;

class ToyVolumeRaycaster : public VolumeRaycaster {
public:
    ToyVolumeRaycaster(glm::vec4 color);
    virtual ~ToyVolumeRaycaster() = default;

    void initialize();
    void deinitialize();
    void renderEntryPoints(const RenderData& data,
        ghoul::opengl::ProgramObject& program) override;
    void renderExitPoints(const RenderData& data,
        ghoul::opengl::ProgramObject& program) override;
    void preRaycast(const RaycastData& data,
        ghoul::opengl::ProgramObject& program) override;
    void postRaycast(const RaycastData& data,
        ghoul::opengl::ProgramObject& program) override;
    bool isCameraInside(const RenderData& data, glm::vec3& localPosition) override;

    std::string boundsVertexShaderPath() const override;
    std::string boundsFragmentShaderPath() const override;
    std::string raycasterPath() const override;
    std::string helperPath() const override;

    void setColor(glm::vec4 color);
    void setModelTransform(glm::mat4 transform);
    void setTime(double time);
    void setStepSize(float time);

private:
    glm::dmat4 modelViewTransform(const RenderData& data);

    BoxGeometry _boundingBox;
    glm::vec4 _color;
    glm::mat4 _modelTransform;
    float _stepSize;
    double _time;
}; // ToyVolumeRaycaster

} // openspace

#endif // __OPENSPACE_MODULE_TOYVOLUME___TOYVOLUMERAYCASTER___H__
