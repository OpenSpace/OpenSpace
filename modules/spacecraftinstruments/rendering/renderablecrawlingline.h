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

#ifndef __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___RENDERABLECRAWLINGLINE___H__
#define __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___RENDERABLECRAWLINGLINE___H__

#include <openspace/rendering/renderable.h>

#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>

namespace openspace {

namespace documentation { struct Documentation; }

class RenderableCrawlingLine : public Renderable {
public:
    RenderableCrawlingLine(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    std::unique_ptr<ghoul::opengl::ProgramObject> _program;

    std::string _instrumentName;
    std::string _source;
    std::string _target;
    std::string _referenceFrame;

    glm::vec4 _lineColorBegin = glm::vec4(0.f);
    glm::vec4 _lineColorEnd = glm::vec4(0.f);

    int _frameCounter = 0;

    bool _drawLine = false;
    float _imageSequenceTime = -1.f;

    GLuint _vao = 0;
    GLuint _vbo = 0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___RENDERABLECRAWLINGLINE___H__
