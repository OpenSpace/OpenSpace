/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___RENDERABLEPLANEPROJECTION___H__
#define __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___RENDERABLEPLANEPROJECTION___H__

#include <openspace/rendering/renderable.h>

#include <ghoul/opengl/ghoul_gl.h>
#include <filesystem>
#include <memory>

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

struct Image;
struct LinePoint;
struct RenderData;
struct UpdateData;

class RenderablePlaneProjection : public Renderable {
public:
    RenderablePlaneProjection(const ghoul::Dictionary& dictionary);
    ~RenderablePlaneProjection() override = default;

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    void loadTexture();
    void updatePlane(const Image& img, double currentTime);
    void setTarget(std::string body);

    std::filesystem::path _texturePath;

    bool _planeIsDirty = false;

    glm::dmat3 _stateMatrix = glm::dmat3(1.0);
    std::string _frame;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
    bool _textureIsDirty = false;
    std::unique_ptr<ghoul::opengl::Texture> _texture;
    std::unique_ptr<ghoul::filesystem::File> _textureFile;
    GLuint _quad = 0;
    GLuint _vertexPositionBuffer = 0;
    std::string _spacecraft;
    std::string _instrument;
    std::string _defaultTarget;

    double _previousTime = 0.0;
    struct {
        std::string body;
        std::string frame;
        std::string node;
    } _target;
    bool _hasImage = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___RENDERABLEPLANEPROJECTION___H__
