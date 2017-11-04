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
    const float StatusMessageOffset = 0.225f;

    const int MaximumMessageQueue = 6;

    const std::chrono::milliseconds RefreshRate(16);

    const float MinimumAlpha = 0.2f;
    const float MaximumAlpha = 1.f;

} // namespace

namespace openspace {

LoadingScreen::LoadingScreen() 
    : _randomEngine(_randomDevice())
{
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
        22,
        ghoul::fontrendering::FontManager::Outline::No,
        ghoul::fontrendering::FontManager::LoadGlyphs::No
    );

    _itemFont = OsEng.fontManager().font(
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

    glm::vec2 logoLl = { LogoCenter.x - size.x,  LogoCenter.y - size.y };
    glm::vec2 logoUr = { LogoCenter.x + size.x,  LogoCenter.y + size.y };

    GLfloat data[] = {
        logoLl.x, logoLl.y, 0.f, 0.f,
        logoUr.x, logoUr.y, 1.f, 1.f,
        logoLl.x, logoUr.y, 0.f, 1.f,
        logoLl.x, logoLl.y, 0.f, 0.f,
        logoUr.x, logoLl.y, 1.f, 0.f,
        logoUr.x, logoUr.y, 1.f, 1.f
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

    glm::vec2 loadingLl = glm::vec2(
        res.x / 2.f - bbox.boundingBox.x / 2.f,
        res.y * LoadingTextPosition
    );
    glm::vec2 loadingUr = loadingLl + bbox.boundingBox;

    renderer.render(
        *_loadingFont,
        loadingLl,
        glm::vec4(1.f, 1.f, 1.f, 1.f),
        "%s",
        "Loading..."
    );

    glm::vec2 messageLl;
    glm::vec2 messageUr;
    {
        std::lock_guard<std::mutex> guard(_messageMutex);

        FR::BoundingBoxInformation bboxMessage = renderer.boundingBox(
            *_messageFont,
            "%s",
            _message.c_str()
        );

        messageLl = glm::vec2(
            res.x / 2.f - bboxMessage.boundingBox.x / 2.f,
            res.y * StatusMessageOffset
        );
        messageUr = messageLl + bboxMessage.boundingBox;


        renderer.render(
            *_messageFont,
            messageLl,
            glm::vec4(1.f, 1.f, 1.f, 1.f),
            "%s",
            _message.c_str()
        );
    }

    {
        std::lock_guard<std::mutex> guard(_itemsMutex);

        for (Item& item : _items) {
            if (!item.hasLocation) {
                // Compute a new location
                
                FR::BoundingBoxInformation b = renderer.boundingBox(
                    *_itemFont,
                    "%s",
                    item.name.c_str()
                );

                // The maximum count is in here since we can't control the amount of
                // screen estate and the number of nodes.  Rather than looping forever
                // we make use with an overlap in the worst (=10) case
                int MaxCounts = 30;
                bool foundSpace = false;

                glm::vec2 ll;
                glm::vec2 ur;
                for (int i = 0; i < MaxCounts && !foundSpace; ++i) {
                    std::uniform_int_distribution<int> distX(
                        15,
                        res.x - b.boundingBox.x - 15
                    );
                    std::uniform_int_distribution<int> distY(
                        15,
                        res.y - b.boundingBox.y - 15
                    );

                    ll = { distX(_randomEngine), distY(_randomEngine) };
                    ur = ll + b.boundingBox;

                    // Test against logo and text
                    bool logoOverlap = !(
                        (logoUr.x + 1.f) / 2.f * res.x < ll.x ||
                        (logoLl.x + 1.f) / 2.f * res.x > ur.x ||
                        (logoUr.y + 1.f) / 2.f * res.y < ll.y ||
                        (logoLl.y + 1.f) / 2.f * res.y > ur.y
                    );

                    bool loadingOverlap = !(
                        loadingUr.x < ll.x ||
                        loadingLl.x > ur.x ||
                        loadingUr.y < ll.y ||
                        loadingLl.y > ur.y
                    );

                    bool messageOverlap = !(
                        messageUr.x < ll.x ||
                        messageLl.x > ur.x ||
                        messageUr.y < ll.y ||
                        messageLl.y > ur.y
                    );


                    if (logoOverlap || loadingOverlap || messageOverlap) {
                        continue;
                    }

                    
                    // Test against all other boxes
                    bool overlap = false;
                    for (const Item& j : _items) {
                        overlap |= !(j.ur.x < ll.x || j.ll.x > ur.x || j.ur.y < ll.y || j.ll.y > ur.y);

                        if (overlap) {
                            break;
                        }
                    }

                    if (!overlap) {
                        break;
                    }
                }

                item.ll = ll;
                item.ur = ur;

                item.hasLocation = true;
            }

            glm::vec4 color = [status = item.status]() {
                switch (status) {
                    case ItemStatus::Started:
                        return glm::vec4(0.5f, 0.5f, 0.5f, 1.f);
                    case ItemStatus::Initializing:
                        return glm::vec4(0.7f, 0.7f, 0.f, 1.f);
                    case ItemStatus::Finished:
                        return glm::vec4(1.f, 1.f, 1.f, 1.f);
                }
            }();
            

            renderer.render(
                *_itemFont,
                item.ll,
                color,
                "%s",
                item.name.c_str()
            );
        }

    }

    glEnable(GL_CULL_FACE);
    glEnable(GL_DEPTH_TEST);

    std::this_thread::sleep_for(RefreshRate);
    OsEng.windowWrapper().swapBuffer();
}

void LoadingScreen::postMessage(std::string message) {
    std::lock_guard<std::mutex> guard(_messageMutex);
    _message = std::move(message);
}

void LoadingScreen::updateItem(const std::string& itemName, ItemStatus newStatus) {
    std::lock_guard<std::mutex> guard(_itemsMutex);

    auto it = std::find_if(
        _items.begin(),
        _items.end(),
        [&itemName](const Item& i) {
            return i.name == itemName;
        }
    );
    if (it != _items.end()) {
        it->status = newStatus;
        if (newStatus == ItemStatus::Finished) {
            it->finishedTime = std::chrono::system_clock::now();
        }
    }
    else {
        ghoul_assert(
            newStatus == ItemStatus::Started,
            "Item '" + itemName + "' did not exist but first message was not Started"
        );
        // We are not computing the location in here since doing it this way might stall
        // the main thread while trying to find a position for the new item
        _items.push_back({
            itemName,
            ItemStatus::Started,
            false,
            {},
            {},
            std::chrono::system_clock::from_time_t(0)
        });
    }
}

} // namespace openspace
