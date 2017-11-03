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

#include <openspace/rendering/loadingscreen.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>

#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#include <random>

namespace {
    const glm::vec2 LogoCenter = { 0.f, 0.4f };
    const glm::vec2 LogoSize = { 0.4f, 0.4 };

    const float LoadingTextPosition = 0.275f;
    const float StatusMessageOffset = 0.05f;

    const int MaximumMessageQueue = 6;

    const std::chrono::milliseconds RefreshRate(16);

    const float MinimumAlpha = 0.2f;
    const float MaximumAlpha = 1.f;

} // namespace

namespace openspace {

LoadingScreen::LoadingScreen() {
    const glm::vec2 dpiScaling = OsEng.windowWrapper().dpiScaling();
    const glm::ivec2 res =
        glm::vec2(OsEng.windowWrapper().currentWindowResolution()) / dpiScaling;

    _program = ghoul::opengl::ProgramObject::Build(
        "Loading Screen",
        "${SHADERS}/loadingscreen.vert",
        "${SHADERS}/loadingscreen.frag"
    );

    _loadingFont = OsEng.fontManager().font(
        "Loading",
        25,
        ghoul::fontrendering::FontManager::Outline::No,
        ghoul::fontrendering::FontManager::LoadGlyphs::No
    );


    _messageFont = OsEng.fontManager().font(
        "Loading",
        13,
        ghoul::fontrendering::FontManager::Outline::No,
        ghoul::fontrendering::FontManager::LoadGlyphs::No
    );

    {
        // Logo stuff
        _logoTexture = ghoul::io::TextureReader::ref().loadTexture(
            absPath("${OPENSPACE_DATA}/openspace-logo.png")
        );
        _logoTexture->uploadTexture();

        float screenAspectRatio = static_cast<float>(res.x) / static_cast<float>(res.y);

        float textureAspectRatio = static_cast<float>(_logoTexture->dimensions().x) /
            static_cast<float>(_logoTexture->dimensions().y);

        glm::vec2 size = {
            LogoSize.x,
            LogoSize.y * textureAspectRatio * screenAspectRatio 
        };

        glm::vec2 ll = { LogoCenter.x - size.x,  LogoCenter.y - size.y };
        glm::vec2 ur = { LogoCenter.x + size.x,  LogoCenter.y + size.y };

        GLfloat data[] = {
            ll.x, ll.y, 0.f, 0.f,
            ur.x, ur.y, 1.f, 1.f,
            ll.x, ur.y, 0.f, 1.f,
            ll.x, ll.y, 0.f, 0.f,
            ur.x, ll.y, 1.f, 0.f,
            ur.x, ur.y, 1.f, 1.f
        };

        glGenVertexArrays(1, &_logo.vao);
        glBindVertexArray(_logo.vao);
        glGenBuffers(1, &_logo.vbo);
        glBindBuffer(GL_ARRAY_BUFFER, _logo.vbo);
        glBufferData(GL_ARRAY_BUFFER, sizeof(data), data, GL_STATIC_DRAW);

        glEnableVertexAttribArray(0);
        glVertexAttribPointer(
            0,
            2,
            GL_FLOAT,
            GL_FALSE,
            4 * sizeof(GLfloat),
            nullptr
        );

        glEnableVertexAttribArray(1);
        glVertexAttribPointer(
            1,
            2,
            GL_FLOAT,
            GL_FALSE,
            4 * sizeof(GLfloat),
            reinterpret_cast<void*>(2 * sizeof(GLfloat))
        );

        glBindVertexArray(0);
    }
}

LoadingScreen::~LoadingScreen() {
    _logoTexture = nullptr;
}

void LoadingScreen::render() {
    const glm::vec2 dpiScaling = OsEng.windowWrapper().dpiScaling();
    const glm::ivec2 res =
        glm::vec2(OsEng.windowWrapper().currentWindowResolution()) / dpiScaling;

    float screenAspectRatio = static_cast<float>(res.x) / static_cast<float>(res.y);

    float textureAspectRatio = static_cast<float>(_logoTexture->dimensions().x) /
        static_cast<float>(_logoTexture->dimensions().y);

    glm::vec2 size = {
        LogoSize.x,
        LogoSize.y * textureAspectRatio * screenAspectRatio
    };

    glm::vec2 ll = { LogoCenter.x - size.x,  LogoCenter.y - size.y };
    glm::vec2 ur = { LogoCenter.x + size.x,  LogoCenter.y + size.y };

    GLfloat data[] = {
        ll.x, ll.y, 0.f, 0.f,
        ur.x, ur.y, 1.f, 1.f,
        ll.x, ur.y, 0.f, 1.f,
        ll.x, ll.y, 0.f, 0.f,
        ur.x, ll.y, 1.f, 0.f,
        ur.x, ur.y, 1.f, 1.f
    };

    glBindVertexArray(_logo.vao);
    glBindBuffer(GL_ARRAY_BUFFER, _logo.vbo);
    glBufferData(GL_ARRAY_BUFFER, sizeof(data), data, GL_STATIC_DRAW);


    // Clear background
    glClearColor(0.f, 0.f, 0.f, 1.f);
    glClear(ClearBufferMask::GL_COLOR_BUFFER_BIT);

    glDisable(GL_CULL_FACE);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_DEPTH_TEST);

    // Render logo
    _program->activate();

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _logoTexture->bind();

    _program->setUniform(
        "logoTexture",
        unit
    );

    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    _program->deactivate();

    // "Loading" text
    using FR = ghoul::fontrendering::FontRenderer;
    FR& renderer = FR::defaultRenderer();

    // We use "Loading" to center the text, but render "Loading..." to make it look more
    // pleasing
    FR::BoundingBoxInformation bbox = renderer.boundingBox(
        *_loadingFont,
        "%s",
        "Loading."
    );

    renderer.render(
        *_loadingFont,
        glm::vec2(res.x / 2.f - bbox.boundingBox.x / 2.f, res.y * LoadingTextPosition),
        glm::vec4(1.f, 1.f, 1.f, 1.f),
        "%s",
        "Loading..."
    );


    {
        std::lock_guard<std::mutex> guard(_messageQueueMutex);

        for (int i = 0; i < _messageQueue.size(); ++i) {
            const std::string& message = _messageQueue[i];

            FR::BoundingBoxInformation bboxMessage = renderer.boundingBox(
                *_messageFont,
                "%s",
                message.c_str()
            );

            renderer.render(
                *_messageFont,
                glm::vec2(
                    res.x / 2.f - bboxMessage.boundingBox.x / 2.f,
                    res.y * StatusMessageOffset + i * bboxMessage.boundingBox.y
                ),
                glm::vec4(
                    1.f, 1.f, 1.f,
                    glm::mix(
                        MaximumAlpha,
                        MinimumAlpha,
                        static_cast<float>(i) / static_cast<float>(MaximumMessageQueue - 1)
                    )
                ),
                "%s",
                message.c_str()
            );
        }
    }

    glEnable(GL_CULL_FACE);
    glEnable(GL_DEPTH_TEST);

    std::this_thread::sleep_for(RefreshRate);
    OsEng.windowWrapper().swapBuffer();
}

void LoadingScreen::queueMessage(std::string message) {
    std::lock_guard<std::mutex> guard(_messageQueueMutex);
    _messageQueue.insert(_messageQueue.begin(), std::move(message));

    // We add one message at a time, so we can also delete one at a time
    if (_messageQueue.size() > MaximumMessageQueue) {
        _messageQueue.pop_back();
    }
}

} // namespace openspace
