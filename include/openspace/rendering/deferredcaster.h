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

#ifndef __OPENSPACE_CORE___DEFERREDCASTER___H
#define __OPENSPACE_CORE___DEFERREDCASTER___H

#include <string>

namespace ghoul::opengl {
    class Texture;
    class ProgramObject;
} // ghoul::opengl

namespace openspace {

struct RenderData;
struct DeferredcastData;
struct UpdateData;

class Deferredcaster {
public:
    virtual ~Deferredcaster() = default;

    virtual void preRaycast(const RenderData& /*renderData*/,
        const DeferredcastData& /*deferredData*/,
        ghoul::opengl::ProgramObject& /*program*/) {};

    virtual void postRaycast(const RenderData & /*renderData*/,
        const DeferredcastData& /*deferredData*/,
        ghoul::opengl::ProgramObject& /*program*/) {};

    virtual std::string deferredcastPath() const = 0;

    virtual std::string deferredcastVSPath() const = 0;

    virtual std::string deferredcastFSPath() const = 0;

    virtual void initializeCachedVariables(ghoul::opengl::ProgramObject&) = 0;

    virtual void update(const UpdateData&) = 0;

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
};

} // namespace openspace

#endif // __OPENSPACE_CORE___DEFERREDCASTER___H
