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

#ifndef __OPENSPACE_MODULE_NEWHORIZONS___RENDERABLEPLANEPROJECTION___H__
#define __OPENSPACE_MODULE_NEWHORIZONS___RENDERABLEPLANEPROJECTION___H__

#include <openspace/rendering/renderable.h>

#include <ghoul/opengl/ghoul_gl.h>

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

struct target {
    std::string body;
    std::string frame;
    std::string node;
};

class RenderablePlaneProjection : public Renderable {
public:
    RenderablePlaneProjection(const ghoul::Dictionary& dictionary);
    ~RenderablePlaneProjection();
    
    void initialize() override;
    void deinitialize() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

private:
    void loadTexture();
    void updatePlane(const Image& img, double currentTime);
    void setTarget(std::string body);

    std::string _texturePath;
        
    bool _planeIsDirty;

    glm::dmat3 _stateMatrix;
    std::string _frame;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
    bool _textureIsDirty;
    std::unique_ptr<ghoul::opengl::Texture> _texture = nullptr;
//    ghoul::opengl::Texture* _texture;
    ghoul::filesystem::File* _textureFile;
    GLuint _quad;
    GLuint _vertexPositionBuffer;
    std::string _spacecraft;
    std::string _instrument;
    std::string _defaultTarget;

    double _previousTime;
    target _target;
    std::string _name;
    bool _moving;
    bool _hasImage;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_NEWHORIZONS___RENDERABLEPLANEPROJECTION___H__
