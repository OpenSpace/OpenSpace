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
#include <thread>

namespace {
    const float LoadingFontSize = 25.f;
    const float MessageFontSize = 22.f;
    const float ItemFontSize = 10.f;

    const glm::vec2 LogoCenter = { 0.f, 0.525f };  // in NDC
    const glm::vec2 LogoSize = { 0.275f, 0.275 };  // in NDC

    const glm::vec2 ProgressbarCenter = { 0.f, -0.75f };  // in NDC
    const glm::vec2 ProgressbarSize = { 0.7f, 0.0075f };  // in NDC
    const float ProgressbarLineWidth = 0.0025f;  // in NDC

    const glm::vec4 ProgressbarOutlineColor = glm::vec4(0.9f, 0.9f, 0.9f, 1.f);

    const glm::vec4 PhaseColorConstruction = glm::vec4(0.7f, 0.7f, 0.f, 1.f);
    const glm::vec4 PhaseColorSynchronization = glm::vec4(0.9f, 0.9f, 0.9f, 1.f);
    const glm::vec4 PhaseColorInitialization = glm::vec4(0.1f, 0.75f, 0.1f, 1.f);

    const glm::vec4 ItemStatusColorStarted = glm::vec4(0.5f, 0.5f, 0.5f, 1.f);
    const glm::vec4 ItemStatusColorInitializing = glm::vec4(0.7f, 0.7f, 0.f, 1.f);
    const glm::vec4 ItemStatusColorFinished = glm::vec4(0.1f, 0.75f, 0.1f, 1.f);
    const glm::vec4 ItemStatusColorFailed = glm::vec4(0.8f, 0.1f, 0.1f, 1.f);

    const float ItemStandoffDistance = 5.f; // in pixels

    const float LoadingTextPosition = 0.275f;  // in NDC
    const float StatusMessageOffset = 0.225f;  // in NDC

    const int MaxNumberLocationSamples = 1000;

    const std::chrono::milliseconds TTL(5000);

    const std::chrono::milliseconds RefreshRate(16);

    bool rectOverlaps(glm::vec2 lhsLl, glm::vec2 lhsUr, glm::vec2 rhsLl, glm::vec2 rhsUr)
    {
        lhsLl -= glm::vec2(ItemStandoffDistance / 2.f);
        lhsUr += glm::vec2(ItemStandoffDistance / 2.f);

        rhsLl -= glm::vec2(ItemStandoffDistance / 2.f);
        rhsUr += glm::vec2(ItemStandoffDistance / 2.f);

        return !(
            lhsUr.x < rhsLl.x ||
            lhsLl.x > rhsUr.x ||
            lhsUr.y < rhsLl.y ||
            lhsLl.y > rhsUr.y
        );
    }

    glm::vec2 ndcToScreen(glm::vec2 ndc, glm::ivec2 res) {
        ndc.x = (ndc.x + 1.f) / 2.f * res.x;
        ndc.y = (ndc.y + 1.f) / 2.f * res.y;
        return ndc;
    }
} // namespace

namespace openspace {

LoadingScreen::LoadingScreen(ShowMessage showMessage, ShowNodeNames showNodeNames,
                             ShowProgressbar showProgressbar)
    : _showMessage(showMessage)
    , _showNodeNames(showNodeNames)
    , _showProgressbar(showProgressbar)
    , _iProgress(0)
    , _nItems(0)
    , _loadingFont(nullptr)
    , _messageFont(nullptr)
    , _itemFont(nullptr)
    , _logo{ 0, 0 }
    , _progressbar{ 0, 0, 0, 0 }
    , _hasCatastrophicErrorOccurred(false)
    , _randomEngine(_randomDevice())
{
    _program = ghoul::opengl::ProgramObject::Build(
        "Loading Screen",
        absPath("${SHADERS}/loadingscreen.vert"),
        absPath("${SHADERS}/loadingscreen.frag")
    );

    _uniformCache.logoTexture = _program->uniformLocation("logoTexture");
    _uniformCache.useTexture = _program->uniformLocation("useTexture");
    _uniformCache.color = _program->uniformLocation("color");

    _renderer = ghoul::fontrendering::FontRenderer::createDefault();

    _loadingFont = OsEng.fontManager().font(
        "Loading",
        LoadingFontSize,
        ghoul::fontrendering::FontManager::Outline::No,
        ghoul::fontrendering::FontManager::LoadGlyphs::No
    );

    if (_showMessage) {
        _messageFont = OsEng.fontManager().font(
            "Loading",
            MessageFontSize,
            ghoul::fontrendering::FontManager::Outline::No,
            ghoul::fontrendering::FontManager::LoadGlyphs::No
        );
    }

    if (_showNodeNames) {
        _itemFont = OsEng.fontManager().font(
            "Loading",
            ItemFontSize,
            ghoul::fontrendering::FontManager::Outline::No,
            ghoul::fontrendering::FontManager::LoadGlyphs::No
        );
    }

    {
        // Logo stuff
        _logoTexture = ghoul::io::TextureReader::ref().loadTexture(
            absPath("${DATA}/openspace-logo.png")
        );
        _logoTexture->uploadTexture();

        glGenVertexArrays(1, &_logo.vao);
        glBindVertexArray(_logo.vao);
        glGenBuffers(1, &_logo.vbo);
        glBindBuffer(GL_ARRAY_BUFFER, _logo.vbo);

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

    if (_showProgressbar) {
        // Progress bar stuff
        glGenVertexArrays(1, &_progressbar.vaoFill);
        glBindVertexArray(_progressbar.vaoFill);
        glGenBuffers(1, & _progressbar.vboFill);
        glBindBuffer(GL_ARRAY_BUFFER, _progressbar.vboFill);

        glEnableVertexAttribArray(0);
        glVertexAttribPointer(
            0,
            2,
            GL_FLOAT,
            GL_FALSE,
            2 * sizeof(GLfloat),
            nullptr
        );

        glGenVertexArrays(1, &_progressbar.vaoBox);
        glBindVertexArray(_progressbar.vaoBox);
        glGenBuffers(1, & _progressbar.vboBox);
        glBindBuffer(GL_ARRAY_BUFFER, _progressbar.vboBox);

        glEnableVertexAttribArray(0);
        glVertexAttribPointer(
            0,
            2,
            GL_FLOAT,
            GL_FALSE,
            2 * sizeof(GLfloat),
            nullptr
        );

        glBindVertexArray(0);
    }
}

LoadingScreen::~LoadingScreen() {
    _logoTexture = nullptr;

    _renderer = nullptr;

    _loadingFont = nullptr;
    _messageFont = nullptr;
    _itemFont = nullptr;

    glDeleteVertexArrays(1, &_logo.vao);
    glDeleteBuffers(1, &_logo.vbo);

    glDeleteVertexArrays(1, &_progressbar.vaoFill);
    glDeleteBuffers(1, &_progressbar.vboFill);

    glDeleteVertexArrays(1, &_progressbar.vaoBox);
    glDeleteBuffers(1, &_progressbar.vboBox);
}

void LoadingScreen::render() {
    using FR = ghoul::fontrendering::FontRenderer;
    // We have to recalculate the positions here because we will not be informed about a
    // window size change

    const glm::vec2 dpiScaling = OsEng.windowWrapper().dpiScaling();
    const glm::ivec2 res =
        glm::vec2(OsEng.windowWrapper().currentDrawBufferResolution()) / dpiScaling;

    float screenAspectRatio = static_cast<float>(res.x) / static_cast<float>(res.y);

    float textureAspectRatio = static_cast<float>(_logoTexture->dimensions().x) /
        static_cast<float>(_logoTexture->dimensions().y);

    _renderer->setFramebufferSize(res);

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

    //
    // Clear background
    //
    glClearColor(0.f, 0.f, 0.f, 1.f);
    glClear(ClearBufferMask::GL_COLOR_BUFFER_BIT);

    glDisable(GL_CULL_FACE);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_DEPTH_TEST);

    //
    // Render logo
    //
    _program->activate();

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _logoTexture->bind();

    _program->setUniform(_uniformCache.logoTexture, unit);
    _program->setUniform(_uniformCache.useTexture, true);

    glDrawArrays(GL_TRIANGLES, 0, 6);

    //
    // Render progress bar
    //
    glm::vec2 progressbarSize = {
        ProgressbarSize.x,
        ProgressbarSize.y * screenAspectRatio
    };

    glm::vec2 progressbarLl = {
        ProgressbarCenter.x - progressbarSize.x,
        ProgressbarCenter.y - progressbarSize.y
    };
    glm::vec2 progressbarUr = {
        ProgressbarCenter.x + progressbarSize.x ,
        ProgressbarCenter.y + progressbarSize.y
    };

    if (_showProgressbar) {
        glBindVertexArray(_progressbar.vaoFill);

        // Depending on the progress, we only want to draw the progress bar to a mixture
        // of the lowerleft and upper right extent

        float progress = _nItems != 0 ?
            static_cast<float>(_iProgress) / static_cast<float>(_nItems) :
            0.f;

        glm::vec2 ur = progressbarUr;
        ur.x = glm::mix(progressbarLl.x, progressbarUr.x, progress);

        GLfloat dataFill[] = {
            progressbarLl.x, progressbarLl.y,
                       ur.x,            ur.y,
            progressbarLl.x,            ur.y,
            progressbarLl.x, progressbarLl.y,
                       ur.x, progressbarLl.y,
                       ur.x,            ur.y,
        };

        glBindBuffer(GL_ARRAY_BUFFER, _progressbar.vboFill);
        glBufferData(GL_ARRAY_BUFFER, sizeof(dataFill), dataFill, GL_STATIC_DRAW);

        _program->setUniform(_uniformCache.useTexture, false);
        switch (_phase) {
            case Phase::Construction:
                _program->setUniform(_uniformCache.color, PhaseColorConstruction);
                break;
            case Phase::Synchronization:
                _program->setUniform(_uniformCache.color, PhaseColorSynchronization);
                break;
            case Phase::Initialization:
                _program->setUniform(_uniformCache.color, PhaseColorInitialization);
                break;
        }
        glDrawArrays(GL_TRIANGLES, 0, 6);

        glBindVertexArray(_progressbar.vaoBox);
        float w = ProgressbarLineWidth / screenAspectRatio;
        float h = ProgressbarLineWidth;
        GLfloat dataBox[] = {
            // In order to avoid the deprecated glLineWidth, we split the lines into
            // separate triangles instead

            // Left side
            progressbarLl.x - w , progressbarLl.y - h,
            progressbarLl.x + w,  progressbarUr.y + h,
            progressbarLl.x - w, progressbarUr.y + h,

            progressbarLl.x - w , progressbarLl.y - h,
            progressbarLl.x + w , progressbarLl.y - h,
            progressbarLl.x + w,  progressbarUr.y + h,

            // Top side
            progressbarLl.x - w, progressbarUr.y - h,
            progressbarUr.x + w, progressbarUr.y + h,
            progressbarLl.x - w, progressbarUr.y + h,

            progressbarLl.x - w, progressbarUr.y - h,
            progressbarUr.x + w, progressbarUr.y - h,
            progressbarUr.x + w, progressbarUr.y + h,

            // Right side
            progressbarUr.x - w, progressbarLl.y - h,
            progressbarUr.x + w, progressbarUr.y + h,
            progressbarUr.x - w, progressbarUr.y - h,

            progressbarUr.x - w, progressbarLl.y - h,
            progressbarUr.x + w, progressbarLl.y - h,
            progressbarUr.x + w, progressbarUr.y + h,

            // Bottom side
            progressbarLl.x - w, progressbarLl.y - h,
            progressbarUr.x + w, progressbarLl.y + h,
            progressbarLl.x - w, progressbarLl.y + h,

            progressbarLl.x - w, progressbarLl.y - h,
            progressbarUr.x + w, progressbarLl.y - h,
            progressbarUr.x + w, progressbarLl.y + h,
        };

        glBindBuffer(GL_ARRAY_BUFFER, _progressbar.vboBox);
        glBufferData(GL_ARRAY_BUFFER, sizeof(dataBox), dataBox, GL_STATIC_DRAW);

        _program->setUniform(_uniformCache.useTexture, false);
        _program->setUniform(_uniformCache.color, ProgressbarOutlineColor);
        glDrawArrays(GL_TRIANGLES, 0, 24);
    }

    glBindVertexArray(0);

    _program->deactivate();

    //
    // "Loading" text
    //


    std::string headline =
        _hasCatastrophicErrorOccurred ?
        "Failure":
        "Loading...";
    // We use "Loading" to center the text, but render "Loading..." to make it look more
    // pleasing
    FR::BoundingBoxInformation bbox = _renderer->boundingBox(
        *_loadingFont,
        "%s",
        headline.substr(0, headline.size() - 2).c_str()
    );

    glm::vec2 loadingLl = glm::vec2(
        res.x / 2.f - bbox.boundingBox.x / 2.f,
        res.y * LoadingTextPosition
    );
    glm::vec2 loadingUr = loadingLl + bbox.boundingBox;

    _renderer->render(
        *_loadingFont,
        loadingLl,
        glm::vec4(1.f, 1.f, 1.f, 1.f),
        "%s",
        headline.c_str()
    );

    glm::vec2 messageLl;
    glm::vec2 messageUr;
    if (_showMessage) {
        std::lock_guard<std::mutex> guard(_messageMutex);

        FR::BoundingBoxInformation bboxMessage = _renderer->boundingBox(
            *_messageFont,
            "%s",
            _message.c_str()
        );

        messageLl = glm::vec2(
            res.x / 2.f - bboxMessage.boundingBox.x / 2.f,
            res.y * StatusMessageOffset
        );
        messageUr = messageLl + bboxMessage.boundingBox;


        _renderer->render(
            *_messageFont,
            messageLl,
            glm::vec4(1.f, 1.f, 1.f, 1.f),
            "%s",
            _message.c_str()
        );
    }

    if (_showNodeNames) {
        std::lock_guard<std::mutex> guard(_itemsMutex);

        std::chrono::system_clock::time_point now = std::chrono::system_clock::now();

        for (Item& item : _items) {
            if (!item.hasLocation) {
                // Compute a new location

                FR::BoundingBoxInformation b = _renderer->boundingBox(
                    *_itemFont,
                    "%s",
                    (item.name + " 100%").c_str()
                );

                // The maximum count is in here since we can't control the amount of
                // screen estate and the number of nodes.  Rather than looping forever
                // we make use with an overlap in the worst case
                bool foundSpace = false;

                glm::vec2 ll;
                glm::vec2 ur;
                int i = 0;
                for ( /* i */; i < MaxNumberLocationSamples && !foundSpace; ++i) {
                    std::uniform_int_distribution<int> distX(
                        15,
                        static_cast<int>(res.x - b.boundingBox.x - 15)
                    );
                    std::uniform_int_distribution<int> distY(
                        15,
                        static_cast<int>(res.y - b.boundingBox.y - 15)
                    );

                    ll = { distX(_randomEngine), distY(_randomEngine) };
                    ur = ll + b.boundingBox;

                    // Test against logo and text
                    bool logoOverlap = rectOverlaps(
                        ndcToScreen(logoLl, res), ndcToScreen(logoUr, res),
                        ll, ur
                    );

                    bool loadingOverlap = rectOverlaps(
                        loadingLl, loadingUr,
                        ll, ur
                    );

                    bool messageOverlap = _showMessage ?
                        rectOverlaps(messageLl, messageUr, ll, ur) :
                        false;

                    bool barOverlap = _showProgressbar ?
                        rectOverlaps(
                            ndcToScreen(progressbarLl, res),
                            ndcToScreen(progressbarUr, res),
                            ll,
                            ur
                        ) :
                        false;

                    if (logoOverlap || loadingOverlap || messageOverlap || barOverlap) {
                        // We never want to have an overlap with these, so this try didn't
                        // count against the maximum, thus ensuring that (if there has to
                        // be an overlap, it's over other text that might disappear before
                        // this one)
                        --i;
                        continue;
                    }

                    // Test against all other boxes
                    bool overlap = false;
                    for (const Item& j : _items) {
                        overlap |= rectOverlaps(j.ll, j.ur, ll, ur);

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

#ifdef LOADINGSCREEN_DEBUGGING
                item.exhaustedSearch = (i == MaxNumberLocationSamples);
#endif // LOADINGSCREEN_DEBUGGING

            }

            glm::vec4 color = [status = item.status]() {
                switch (status) {
                    case ItemStatus::Started:
                        return ItemStatusColorStarted;
                    case ItemStatus::Initializing:
                        return ItemStatusColorInitializing;
                    case ItemStatus::Finished:
                        return ItemStatusColorFinished;
                    case ItemStatus::Failed:
                        return ItemStatusColorFailed;
                    default:
                        return glm::vec4(1.f);
                }
            }();

            if (item.status == ItemStatus::Finished) {
                auto t = std::chrono::duration_cast<std::chrono::milliseconds>(
                    now - item.finishedTime
                );

                color.a = 1.f - static_cast<float>(t.count()) /
                                static_cast<float>(TTL.count());
            }

#ifdef LOADINGSCREEN_DEBUGGING
            if (item.exhaustedSearch) {
                color = glm::vec4(0.f, 1.f, 1.f, 1.f);
            }
#endif // LOADINGSCREEN_DEBUGGING

            std::string text = item.name;
            if (item.status == ItemStatus::Started && item.progress > 0) {
                text += " " +
                    std::to_string(static_cast<int>(std::round(item.progress * 100))) +
                    "%";
            }

            _renderer->render(
                *_itemFont,
                item.ll,
                color,
                "%s",
                text.c_str()
            );
        }

        _items.erase(
            std::remove_if(
                _items.begin(),
                _items.end(),
                [now](const Item& i) {
                    if (i.status == ItemStatus::Finished) {
                        return i.finishedTime > now + TTL;
                    }
                    else {
                        return false;
                    }
                }
            ),
            _items.end()
        );

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

void LoadingScreen::setCatastrophicError(CatastrophicError catastrophicError) {
    _hasCatastrophicErrorOccurred = catastrophicError;
}

void LoadingScreen::finalize() {
    _items.erase(
        std::remove_if(
            _items.begin(),
            _items.end(),
            [](const Item& i) {
                return i.status != ItemStatus::Failed;
            }
        ),
        _items.end()
    );

    render();
}


void LoadingScreen::setItemNumber(int nItems) {
    _nItems = nItems;
}

int LoadingScreen::itemNumber() {
    return _nItems;
}

void LoadingScreen::tickItem() {
    ++_iProgress;
}

void LoadingScreen::setPhase(Phase phase) {
    _phase = phase;
    _iProgress = 0;
}

void LoadingScreen::updateItem(const std::string& itemIdentifier,
                               const std::string& itemName, ItemStatus newStatus,
                               float newProgress)
{
    if (!_showNodeNames) {
        // If we don't want to show the node names, we can disable the updating which
        // also would create any of the text information
        return;
    }
    std::lock_guard<std::mutex> guard(_itemsMutex);

    auto it = std::find_if(
        _items.begin(),
        _items.end(),
        [&itemIdentifier](const Item& i) {
            return i.identifier == itemIdentifier;
        }
    );
    if (it != _items.end()) {
        it->status = newStatus;
        it->progress = newProgress;
        if (newStatus == ItemStatus::Finished) {
            it->finishedTime = std::chrono::system_clock::now();
        }
    }
    else {
        ghoul_assert(
            newStatus == ItemStatus::Started,
            fmt::format(
                "Item '{}' did not exist and first message was not 'Started'",
                itemIdentifier
            )
        );
        // We are not computing the location in here since doing it this way might stall
        // the main thread while trying to find a position for the new item
        _items.push_back({
            itemIdentifier,
            itemName,
            ItemStatus::Started,
            newProgress,
            false,
#ifdef LOADINGSCREEN_DEBUGGING
            false,
#endif // LOADINGSCREEN_DEBUGGING
            {},
            {},
            std::chrono::system_clock::from_time_t(0)
        });
    }
}

} // namespace openspace
