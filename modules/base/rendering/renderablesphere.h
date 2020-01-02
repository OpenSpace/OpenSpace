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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLESPHERE___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLESPHERE___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <ghoul/opengl/uniformcache.h>

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

class Sphere;
struct RenderData;
struct UpdateData;

namespace documentation { struct Documentation; }

class RenderableSphere : public Renderable {
public:
    RenderableSphere(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    void loadTexture();

    properties::StringProperty _texturePath;
    properties::OptionProperty _orientation;

    properties::FloatProperty _size;
    properties::IntProperty _segments;

    properties::BoolProperty _mirrorTexture;
    properties::BoolProperty _useAdditiveBlending;
    properties::BoolProperty _disableFadeInDistance;
    properties::BoolProperty _backgroundRendering;

    properties::FloatProperty _fadeInThreshold;
    properties::FloatProperty _fadeOutThreshold;

    ghoul::opengl::ProgramObject* _shader = nullptr;
    std::unique_ptr<ghoul::opengl::Texture> _texture;

    std::unique_ptr<Sphere> _sphere;

    UniformCache(opacity, modelViewProjection, modelViewRotation, colorTexture,
        _mirrorTexture) _uniformCache;

    bool _sphereIsDirty = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLESPHERE___H__
