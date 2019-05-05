/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_CORE___RENDERER___H__
#define __OPENSPACE_CORE___RENDERER___H__

#include <ghoul/glm.h>
#include <vector>

namespace ghoul { class Dictionary; }
namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

class RenderableVolume;
class Camera;
class Scene;

class Renderer {
public:
    virtual ~Renderer() = default;

    virtual void initialize() = 0;
    virtual void deinitialize() = 0;

    virtual void setResolution(glm::ivec2 res) = 0;
    virtual void setNAaSamples(int nAaSamples) = 0;
    virtual void setHDRExposure(float hdrExposure) = 0;
    virtual void setHDRBackground(float hdrBackground) = 0;
    virtual void setGamma(float gamma) = 0;

    virtual float hdrBackground() const = 0;
    virtual int nAaSamples() const = 0;
    virtual const std::vector<double>& mSSAPattern() const = 0;

    /**
    * Set raycasting uniforms on the program object, and setup raycasting.
    */
    virtual void preRaycast(ghoul::opengl::ProgramObject& /*programObject*/) {};

    /**
    * Tear down raycasting for the specified program object.
    */
    virtual void postRaycast(ghoul::opengl::ProgramObject& /*programObject*/) {};


    virtual void update() = 0;

    virtual void render(Scene* scene, Camera* camera, float blackoutFactor) = 0;
    /**
     * Update render data
     * Responsible for calling renderEngine::setRenderData
     */
    virtual void updateRendererData() = 0;
};

} // openspace

#endif // __OPENSPACE_CORE___RENDERER___H__
