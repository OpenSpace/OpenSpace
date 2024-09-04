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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLEPOLYGONCLOUD___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLEPOLYGONCLOUD___H__

#include <modules/base/rendering/pointcloud/renderablepointcloud.h>

#include <ghoul/opengl/ghoul_gl.h>

namespace ghoul::opengl { class Texture; }

namespace openspace {

namespace documentation { struct Documentation; }

/**
 * A billboarded point cloud, but with dynamically created uniform polygon shapes instead
 * of a custom texture. Overwrites the sprite set in parent class, RenderablePointCloud
 */
class RenderablePolygonCloud : public RenderablePointCloud {
public:
    explicit RenderablePolygonCloud(const ghoul::Dictionary& dictionary);
    ~RenderablePolygonCloud() override = default;

    void deinitializeGL() override;

    static documentation::Documentation Documentation();

private:
    void initializeCustomTexture() override;
    void renderToTexture(GLuint textureToRenderTo, GLuint textureWidth,
        GLuint textureHeight);
    void renderPolygonGeometry(GLuint vao);

    int _nPolygonSides = 3;

    GLuint _pTexture = 0;

    GLuint _polygonVao = 0;
    GLuint _polygonVbo = 0;

    bool _textureIsInitialized = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLEPOLYGONCLOUD___H__
