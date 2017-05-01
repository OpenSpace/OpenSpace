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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLEPLANE___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLEPLANE___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/util/updatestructures.h>

namespace ghoul {
    namespace filesystem {
        class File;
    }
    namespace opengl {
        class ProgramObject;
        class Texture;
    }
}

namespace openspace {
    struct LinePoint;

class RenderablePlane : public Renderable {

    enum class Origin {
        LowerLeft, LowerRight, UpperLeft, UpperRight, Center
    };

public:
    enum class BlendMode : int {
        Normal = 0,
        Additive
    };

    RenderablePlane(const ghoul::Dictionary& dictionary);
    ~RenderablePlane();

    bool initialize() override;
    bool deinitialize() override;

    bool isReady() const override;

    void render(const RenderData& data) override;
    void update(const UpdateData& data) override;

private:
    void loadTexture();
    void createPlane();

    properties::StringProperty _texturePath;
    properties::BoolProperty _billboard;
    properties::BoolProperty _projectionListener;
    properties::FloatProperty _size;

    Origin _origin;
    std::string _nodeName;

    bool _planeIsDirty;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
    bool _textureIsDirty;
    std::unique_ptr<ghoul::opengl::Texture> _texture;
    BlendMode _blendMode;
    ghoul::filesystem::File* _textureFile;
    GLuint _quad;
    GLuint _vertexPositionBuffer;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLEPLANE___H__
