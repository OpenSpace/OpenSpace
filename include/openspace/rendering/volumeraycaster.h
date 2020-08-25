/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_CORE___VOLUMERAYCASTER___H__
#define __OPENSPACE_CORE___VOLUMERAYCASTER___H__

#include <ghoul/glm.h>
#include <string>
#include <vector>

namespace ghoul::opengl {
    class Texture;
    class ProgramObject;
} // namespace ghoul::opengl

namespace openspace {

struct RenderData;
struct RaycastData;

class VolumeRaycaster {
public:
    /**
     * Destructor
     */
    virtual ~VolumeRaycaster() = default;

    /**
     * Render the volume's entry points (front face of the bounding geometry)
     */
    virtual void renderEntryPoints(const RenderData& /*data*/,
        ghoul::opengl::ProgramObject& /*program*/) = 0;

    /**
     * Render the volume's exit points (back face of the bounding geometry)
     */
    virtual void renderExitPoints(const RenderData& /*data*/,
        ghoul::opengl::ProgramObject& /*program*/) = 0;

    /**
     * Prepare the volume for the ABuffer's resolve step.
     * Make sure textures are up to date, bind them to texture units, set program uniforms
     * etc.
     */
    virtual void preRaycast(const RaycastData& data,
        ghoul::opengl::ProgramObject& program);

    /**
     * Clean up for the volume after the ABuffer's resolve step.
     * Make sure texture units are deinitialized, etc.
     */
    virtual void postRaycast(const RaycastData& data,
        ghoul::opengl::ProgramObject& program);

    /**
    * Return true if the camera is inside the volume.
    * Also set localPosition to the camera position in the volume's local coordinate
    * system.
    */
    virtual bool isCameraInside(const RenderData& data, glm::vec3& localPosition);

    /**
     * Return a path the file to use as vertex shader
     *
     * The shader preprocessor will have acceess to
     *   A #{namespace} variable (unique per helper file)
     */
    virtual std::string boundsVertexShaderPath() const = 0;

    /*
     * Return a path to a file with the functions, uniforms and fragment shader in
     * variables required to generate the fragment color and depth.
     *
     * Should define the function:
     * Fragment getFragment()
     *
     * The shader preprocessor will have acceess to
     *   A #{namespace} variable (unique per helper file)
     */
    virtual std::string boundsFragmentShaderPath() const = 0;

    /**
     * Return a path to a file with all the uniforms, functions etc
     * required to perform ray casting through this volume.
     *
     * The header should define the following two functions:
     *   vec4 sample#{id}(vec3 samplePos, vec3 dir, float occludingAlpha,
     *                    inout float maxStepSize)
     *      (return color of sample)
     *   float stepSize#{id}(vec3 samplePos, vec3 dir)
     *      (return the preferred step size at this sample position)
     *
     * The shader preprocessor will have acceess to
     *   An #{id} variable (unique per volume)
     *   A #{namespace} variable (unique per helper file)
     */
    virtual std::string raycasterPath() const = 0;

    /**
     * Return a path to a glsl file with helper functions required for the
     * transformation and raycast steps.
     * This file will be included once per shader program generated,
     * regardless of how many volumes say they require the file.
     * Ideal to avoid redefinitions of helper functions.
     *
     * The shader preprocessor will have access to the #{namespace} variable (unique per
     * helper file) which should be a prefix to all symbols defined by the helper
     */
    virtual std::string helperPath() const = 0;

    void setMaxSteps(int nsteps);

    int maxSteps() const;

    void setDownscaleRender(float value);

    float downscaleRender() const;

private:
    /**
     * Maximum number of integration steps to be executed by the volume integrator.
     */
    int _rayCastMaxSteps = 1000;

    /**
     * Enable and set the downscale rendering of the volume. Used to improve performance.
     */
    float _downscaleRenderConst = 1.0f;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___VOLUMERAYCASTER___H__
