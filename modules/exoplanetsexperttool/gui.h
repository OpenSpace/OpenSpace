/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___GUI___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___GUI___H__

#include <openspace/properties/propertyowner.h>

#include <modules/exoplanetsexperttool/dataviewer.h>
#include <openspace/glm.h>
#include <openspace/opengl/uniformcache.h>
#include <openspace/util/keys.h>
#include <openspace/util/mouse.h>

struct ImGuiContext;
struct ImPlotContext;

namespace openspace::opengl {
    class ProgramObject;
    class Texture;
} // namespace openspace::opengl

namespace openspace::exoplanets::gui {

class Gui : public PropertyOwner {
public:
    Gui(std::string identifier, std::string guiName = "");
    ~Gui();

    void initialize();
    void deinitialize();

    void initializeGL();
    void deinitializeGL();

    void initializeDataset();
    void hideSelectionVolume();

    bool mouseButtonCallback(MouseButton button, MouseAction action);
    bool mouseWheelCallback(double position);
    bool keyCallback(Key key, KeyModifier modifier, KeyAction action);
    bool charCallback(unsigned int character, KeyModifier modifier);

    void startFrame(float deltaTime, const glm::vec2& windowSize,
        const glm::vec2& dpiScaling, const glm::vec2& mousePos,
        uint32_t mouseButtonsPressed);
    void endFrame();

    void render();

private:
    struct Context {
        ImGuiContext* imgui;
        ImPlotContext* implot;
    };

    Context createContext();
    void destroyContext(const Context& ctx);
    void setCurrectContext(const Context& ctx);

    GLuint vao = 0;
    GLuint vbo = 0;
    GLuint vboElements = 0;
    std::unique_ptr<opengl::ProgramObject> _program;
    UniformCache(tex, ortho) _uniformCache;
    std::unique_ptr<opengl::Texture> _fontTexture;

    std::vector<Context> _contexts;

    DataViewer _dataViewer;
};

} // namespace openspace::exoplanets::gui

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___GUI___H__
