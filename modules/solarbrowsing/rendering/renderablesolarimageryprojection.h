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

#ifndef __OPENSPACE_MODULE_SOLARBROWSING___RENDERABLESOLARIMAGERYPROJECTION___H__
#define __OPENSPACE_MODULE_SOLARBROWSING___RENDERABLESOLARIMAGERYPROJECTION___H__

#include <openspace/rendering/renderable.h>

#include <openspace/util/powerscaledsphere.h>

#include <memory>
#include <string>
#include <vector>

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

class SceneGraphNode;

class RenderableSolarImageryProjection : public Renderable {
public:
    RenderableSolarImageryProjection(const ghoul::Dictionary& dictionary);

    void initialize() override;

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shader = nullptr;
    PowerScaledSphere _sphere;

private:
    void loadTexture();

    std::vector<std::string> _dependentNodes;
    std::vector<SceneGraphNode*> _solarImageryDependencies;
    std::string _nodeName;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOLARBROWSING___RENDERABLESOLARIMAGERYPROJECTION___H__
