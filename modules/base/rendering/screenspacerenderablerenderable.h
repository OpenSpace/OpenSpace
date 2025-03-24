/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___SCREENSPACERENDERABLERENDERABLE___H__
#define __OPENSPACE_MODULE_BASE___SCREENSPACERENDERABLERENDERABLE___H__

#include <modules/base//rendering/screenspaceframebuffer.h>

#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>

namespace openspace {

class Renderable;

namespace documentation { struct Documentation; }

class ScreenSpaceRenderableRenderable : public ScreenSpaceFramebuffer {
public:
    using RenderFunction = std::function<void()>;

    explicit ScreenSpaceRenderableRenderable(const ghoul::Dictionary& dictionary);
    virtual ~ScreenSpaceRenderableRenderable() override;

    bool initializeGL() override;
    bool deinitializeGL() override;
    void update() override;

    static documentation::Documentation Documentation();

private:
    ghoul::mm_unique_ptr<Renderable> _renderable = nullptr;

    properties::Vec3Property _cameraPosition;
    properties::Vec3Property _cameraCenter;
    properties::Vec3Property _cameraUp;
    properties::FloatProperty _cameraFov;
};

} //namespace openspace

#endif // __OPENSPACE_MODULE_BASE___SCREENSPACERENDERABLERENDERABLE___H__
