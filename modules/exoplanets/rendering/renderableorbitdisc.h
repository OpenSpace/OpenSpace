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

#ifndef __OPENSPACE_MODULE_EXOPLANETS___RENDERABLEORBITDISC___H__
#define __OPENSPACE_MODULE_EXOPLANETS___RENDERABLEORBITDISC___H__

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/uniformcache.h>

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
} // namespace ghoul::opengl

namespace openspace {

namespace documentation { struct Documentation; }

class RenderableOrbitDisc : public Renderable {
public:
    RenderableOrbitDisc(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    void loadTexture();
    void createPlane();

    properties::StringProperty _texturePath;
    properties::FloatProperty _size;
    properties::FloatProperty _eccentricity;
    properties::Vec2Property _offset;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shader = nullptr;
    UniformCache(modelViewProjection, offset, opacity, texture,
        eccentricity, semiMajorAxis) _uniformCache;
    std::unique_ptr<ghoul::opengl::Texture> _texture = nullptr;
    std::unique_ptr<ghoul::filesystem::File> _textureFile;

    bool _textureIsDirty = false;
    bool _planeIsDirty = false;
    GLuint _quad = 0;
    GLuint _vertexPositionBuffer = 0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_EXOPLANETS___RENDERABLEORBITDISC___H__
