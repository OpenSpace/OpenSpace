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

#ifndef __OPENSPACE_CORE___VOLUME___H__
#define __OPENSPACE_CORE___VOLUME___H__

#include <string>
#include <vector>

namespace ghoul::opengl {
    class Texture;
    class ProgramObject;
} // namespace ghoul::opengl

namespace openspace {

class Volume {
public:
    /**
     * Constructor
     */
    Volume() = default;

    /**
     * Destructor
     */
    virtual ~Volume() = default;

    /**
     * Render the volume's entry points (front face of the bounding geometry)
     */
    //virtual void renderEntryPoints(const RenderData& data,
    //    ghoul::opengl::ProgramObject* program) = 0;

    /**
     * Render the volume's exit points (back face of the bounding geometry)
     */
    //virtual void renderExitPoints(const RenderData& data,
    //    ghoul::opengl::ProgramObject* program) = 0;

    /**
     * Prepare the volume for the ABuffer's resolve step.
     * Make sure textures are up to date, bind them to texture units, set program uniforms
     * etc.
     */
    //virtual void preRayCast(ghoul::opengl::ProgramObject* program) {};

    /**
     * Clean up for the volume after the ABuffer's resolve step.
     * Make sure texture units are deinitialized, etc.
     */
    //virtual void postRayCast(ghoul::opengl::ProgramObject* program) {};

    /**
     * Return a path to a file with the uniforms, per-vertex variables, functions etc
     * required to transform the vertices of the bounding geometry to view space.
     * It could also set other vertex out variables for the fragment shader to access.
     *
     * The shader preprocessor will have acceess to
     *   A #{namespace} variable (unique per helper file)
     *
     * Should define the function:
     * vec4 getVertex()
     */
    //virtual std::string boundsVertexShaderPath() = 0;

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
    //virtual std::string boundsFragmentShaderPath() = 0;

    /**
     * Return a path to a file with all the uniforms, functions etc
     * required to perform ray casting through this volume.
     *
     * The header should define the following two functions:
     *   vec4 sampler#{id}(vec3 samplePos, vec3 dir, float occludingAlpha,
     *                     inout float maxStepSize)
     *      (return color of sample)
     *   float stepSize#{id}(vec3 samplePos, vec3 dir)
     *      (return the preferred step size at this sample position)
     *
     * The shader preprocessor will have acceess to
     *   An #{id} variable (unique per volume)
     *   A #{namespace} variable (unique per helper file)
     */
    //virtual std::string getRayCastPath() = 0;

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
    //virtual std::string helperPath() = 0;

};

} // namespace openspace

#endif // __OPENSPACE_CORE___VOLUME___H__
