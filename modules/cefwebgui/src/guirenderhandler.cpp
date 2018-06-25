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

#include <modules/cefwebgui/include/guirenderhandler.h>

#include <openspace/engine/openspaceengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    constexpr const char* _loggerCat = "WebGUI:RenderHandler";
} // namespace

namespace openspace {

GUIRenderHandler::GUIRenderHandler() {
    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::InitializeGL,
        [this]() {
            LDEBUG("Initializing WebGUI RenderHandler OpenGL");
            initializeGL();
        }
    );
}

void GUIRenderHandler::initializeGL() {
    LDEBUG("Initializing CEF GL environment...");
    _programObject = ghoul::opengl::ProgramObject::Build(
        "WebGUICEFProgram",
        absPath("${MODULE_CEFWEBGUI}/shaders/gui_vs.glsl"),
        absPath("${MODULE_CEFWEBGUI}/shaders/gui_fs.glsl")
    );
    float data[] = {
        -1.0f, -1.0f, -1.0f,
         1.0f,  1.0f, -1.0f,
         1.0f, -1.0f, -1.0f,
         1.0f,  1.0f,  1.0f
    };

    glGenVertexArrays(1, &_vao);
    glBindVertexArray(_vao);
    glGenBuffers(1, &_vbo);
    glGenTextures(1, &_texture);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(GL_ARRAY_BUFFER, sizeof(data), data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(GLfloat), 0);
    glBindVertexArray(0);
    LDEBUG("Initializing CEF GL environment... done!");
}

void GUIRenderHandler::deinitializeGL() {
    _programObject = nullptr;

    glDeleteVertexArrays(1, &_vao);
    glDeleteBuffers(1, &_vbo);
}

void GUIRenderHandler::draw() {
    if (_programObject->isDirty()) {
        _programObject->rebuildFromFile();
    }

    glEnable(GL_BLEND);
    glBlendEquation(GL_FUNC_ADD);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);

    _programObject->activate();

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    glBindTexture(GL_TEXTURE_2D, _texture);
    _programObject->setUniform("tex", unit);

    glBindVertexArray(_vao);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    _programObject->deactivate();

    glEnable(GL_CULL_FACE);
}

void GUIRenderHandler::render() {}

} // namespace openspace
