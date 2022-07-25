/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/helper.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/stringconversion.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <random>
#include <thread>

namespace {
    constexpr float LoadingFontSize = 25.f;
    constexpr float MessageFontSize = 22.f;
    constexpr float ItemFontSize = 10.f;

    constexpr glm::vec2 LogoCenter = glm::vec2(0.f, 0.525f);  // in NDC
    constexpr glm::vec2 LogoSize = glm::vec2(0.275f, 0.275);  // in NDC

    constexpr glm::vec2 ProgressbarCenter = glm::vec2(0.f, -0.75f);  // in NDC
    constexpr glm::vec2 ProgressbarSize = glm::vec2(0.7f, 0.0075f);  // in NDC
    constexpr float ProgressbarLineWidth = 0.0025f;  // in NDC

    constexpr glm::vec4 ProgressbarOutlineColor = glm::vec4(0.9f, 0.9f, 0.9f, 1.f);

    constexpr glm::vec4 PhaseColorConstruction = glm::vec4(0.7f, 0.7f, 0.f, 1.f);
    constexpr glm::vec4 PhaseColorSynchronization = glm::vec4(0.9f, 0.9f, 0.9f, 1.f);
    constexpr glm::vec4 PhaseColorInitialization = glm::vec4(0.1f, 0.75f, 0.1f, 1.f);

    constexpr glm::vec4 ItemStatusColorStarted = glm::vec4(0.5f, 0.5f, 0.5f, 1.f);
    constexpr glm::vec4 ItemStatusColorInitializing = glm::vec4(0.7f, 0.7f, 0.f, 1.f);
    constexpr glm::vec4 ItemStatusColorFinished = glm::vec4(0.1f, 0.75f, 0.1f, 1.f);
    constexpr glm::vec4 ItemStatusColorFailed = glm::vec4(0.8f, 0.1f, 0.1f, 1.f);

    constexpr float ItemStandoffDistance = 5.f; // in pixels

    constexpr float LoadingTextPosition = 0.275f;  // in NDC
    constexpr float StatusMessageOffset = 0.225f;  // in NDC

    constexpr int MaxNumberLocationSamples = 1000;

    constexpr std::chrono::milliseconds TTL(5000);

    constexpr std::chrono::milliseconds RefreshRate(16);

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
    , _randomEngine(_randomDevice())
{
    _loadingFont = global::fontManager->font(
        "Loading",
        LoadingFontSize,
        ghoul::fontrendering::FontManager::Outline::No,
        ghoul::fontrendering::FontManager::LoadGlyphs::No
    );

    if (_showMessage) {
        _messageFont = global::fontManager->font(
            "Loading",
            MessageFontSize,
            ghoul::fontrendering::FontManager::Outline::No,
            ghoul::fontrendering::FontManager::LoadGlyphs::No
        );
    }

    if (_showNodeNames) {
        _itemFont = global::fontManager->font(
            "Loading",
            ItemFontSize,
            ghoul::fontrendering::FontManager::Outline::No,
            ghoul::fontrendering::FontManager::LoadGlyphs::No
        );
    }

    {
        // Logo stuff
        _logoTexture = ghoul::io::TextureReader::ref().loadTexture(
            absPath("${DATA}/openspace-logo.png").string(),
            2
        );
        _logoTexture->uploadTexture();
    }
}

LoadingScreen::~LoadingScreen() {
    _logoTexture = nullptr;

    _loadingFont = nullptr;
    _messageFont = nullptr;
    _itemFont = nullptr;
}

void LoadingScreen::render() {
    ZoneScoped
    FrameMarkStart("Loading")

    if (_phase == Phase::PreStart) {
        return;
    }

    // We have to recalculate the positions here because we will not be informed about a
    // window size change

    const glm::vec2 dpiScaling = global::windowDelegate->dpiScaling();
    const glm::ivec2 res =
        glm::vec2(global::windowDelegate->currentSubwindowSize()) * dpiScaling;

    float screenAspectRatio = static_cast<float>(res.x) / static_cast<float>(res.y);

    float textureAspectRatio = static_cast<float>(_logoTexture->dimensions().x) /
        static_cast<float>(_logoTexture->dimensions().y);

    ghoul::fontrendering::FontRenderer::defaultRenderer().setFramebufferSize(res);

    const glm::vec2 size = {
        LogoSize.x,
        LogoSize.y * textureAspectRatio * screenAspectRatio
    };

    //
    // Clear background
    //
    glClearColor(0.f, 0.f, 0.f, 1.f);
    glClear(ClearBufferMask::GL_COLOR_BUFFER_BIT);
    glViewport(0, 0, res.x, res.y);
    glDisable(GL_CULL_FACE);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_DEPTH_TEST);

    //
    // Render logo
    //
    rendering::helper::renderBox(
        glm::vec2(1.f) - ((LogoCenter + glm::vec2(1.f)) / 2.f),
        size,
        glm::vec4(1.f),
        *_logoTexture,
        rendering::helper::Anchor::Center
    );

    //
    // Render progress bar
    //
    const glm::vec2 progressbarSize = {
        ProgressbarSize.x,
        ProgressbarSize.y * screenAspectRatio
    };

    if (_showProgressbar) {
        const float progress = _nItems != 0 ?
            static_cast<float>(_iProgress) / static_cast<float>(_nItems) :
            0.f;

        const float w = ProgressbarLineWidth / screenAspectRatio;
        const float h = ProgressbarLineWidth;
        rendering::helper::renderBox(
            glm::vec2(1.f) - ((ProgressbarCenter + glm::vec2(1.f)) / 2.f),
            progressbarSize + glm::vec2(2 * w, 2 * h),
            ProgressbarOutlineColor,
            rendering::helper::Anchor::Center
        );

        rendering::helper::renderBox(
            glm::vec2(1.f) - ((ProgressbarCenter + glm::vec2(1.f)) / 2.f),
            progressbarSize,
            glm::vec4(0.f, 0.f, 0.f, 1.f),
            rendering::helper::Anchor::Center
        );

        glm::vec4 color = glm::vec4(0.f);
        switch (_phase) {
            case Phase::PreStart:
                break;
            case Phase::Construction:
                color = PhaseColorConstruction;
                break;
            case Phase::Synchronization:
                color = PhaseColorSynchronization;
                break;
            case Phase::Initialization:
                color = PhaseColorInitialization;
                break;
        }

        glm::vec2 p = glm::vec2(1.f) - ((ProgressbarCenter + glm::vec2(1.f)) / 2.f);
        rendering::helper::renderBox(
            p - progressbarSize / 2.f,
            progressbarSize * glm::vec2(progress, 1.f),
            color,
            rendering::helper::Anchor::NW
        );
    }

    //
    // "Loading" text
    //
    using FR = ghoul::fontrendering::FontRenderer;
    const FR& renderer = FR::defaultRenderer();

    const std::string headline =
        _hasCatastrophicErrorOccurred ?
        "Failure":
        "Loading...";
    // We use "Loading" to center the text, but render "Loading..." to make it look more
    // pleasing
    const glm::vec2 bbox = _loadingFont->boundingBox(
        headline.substr(0, headline.size() - 2)
    );

    const glm::vec2 loadingLl = glm::vec2(
        res.x / 2.f - bbox.x / 2.f,
        res.y * LoadingTextPosition
    );
    const glm::vec2 loadingUr = loadingLl + bbox;

    renderer.render(*_loadingFont, loadingLl, headline);

    glm::vec2 messageLl = glm::vec2(0.f);
    glm::vec2 messageUr = glm::vec2(0.f);
    if (_showMessage) {
        std::lock_guard guard(_messageMutex);

        const glm::vec2 bboxMessage = _messageFont->boundingBox(_message);

        messageLl = glm::vec2(
            res.x / 2.f - bboxMessage.x / 2.f,
            res.y * StatusMessageOffset
        );
        messageUr = messageLl + bboxMessage;


        renderer.render(*_messageFont, messageLl, _message);
    }

    if (_showNodeNames) {
        std::lock_guard guard(_itemsMutex);

        std::chrono::system_clock::time_point now = std::chrono::system_clock::now();

        const glm::vec2 logoLl = { LogoCenter.x - size.x,  LogoCenter.y - size.y };
        const glm::vec2 logoUr = { LogoCenter.x + size.x,  LogoCenter.y + size.y };

        const glm::vec2 progressbarLl = {
            ProgressbarCenter.x - progressbarSize.x,
            ProgressbarCenter.y - progressbarSize.y
        };
        const glm::vec2 progressbarUr = {
            ProgressbarCenter.x + progressbarSize.x ,
            ProgressbarCenter.y + progressbarSize.y
        };

        for (Item& item : _items) {
            if (!item.hasLocation) {
                // Compute a new location

                const glm::vec2 b = _itemFont->boundingBox(
                    (item.name + " 100%\n99999999/99999999")
                );

                glm::vec2 ll = glm::vec2(0.f);
                glm::vec2 ur = glm::vec2(0.f);
                int i = 0;
                for (; i < MaxNumberLocationSamples; ++i) {
                    std::uniform_int_distribution<int> distX(
                        15,
                        static_cast<int>(res.x - b.x - 15)
                    );
                    std::uniform_int_distribution<int> distY(
                        15,
                        static_cast<int>(res.y - b.y - 15)
                    );

                    ll = { distX(_randomEngine), distY(_randomEngine) };
                    ur = ll + b;

                    // Test against logo and text
                    const bool logoOverlap = rectOverlaps(
                        ndcToScreen(logoLl, res), ndcToScreen(logoUr, res),
                        ll, ur
                    );

                    const bool loadingOverlap = rectOverlaps(
                        loadingLl, loadingUr,
                        ll, ur
                    );

                    const bool messageOverlap = _showMessage ?
                        rectOverlaps(messageLl, messageUr, ll, ur) :
                        false;

                    const bool barOverlap = _showProgressbar ?
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
                const auto t = std::chrono::duration_cast<std::chrono::milliseconds>(
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
            if (item.status == ItemStatus::Started && item.progress.progress > 0) {
                ProgressInfo& info = item.progress;
                bool hasSecondLine = (info.totalSize != -1 && info.currentSize != -1);

                int p = static_cast<int>(std::round(info.progress * 100));
                if (hasSecondLine) {
                    if (info.totalSize < 1024 * 1024) { // 1MB
                        text = fmt::format(
                            "{} ({}%)\n{}/{} {}",
                            text, p, info.currentSize, info.totalSize, "bytes"
                        );
                    }
                    else {
                        float curr = info.currentSize / (1024.f * 1024.f);
                        float total = info.totalSize / (1024.f * 1024.f);

                        text = fmt::format(
                            "{} ({}%)\n{:.3f}/{:.3f} {}",
                            text, p, curr, total, "MB"
                        );
                    }
                }
                else {
                    text = fmt::format("{} ({}%)", text, p);
                }
            }

            renderer.render(*_itemFont, item.ll, text, color);
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
    global::windowDelegate->swapBuffer();
    FrameMarkEnd("Loading")
}

void LoadingScreen::postMessage(std::string message) {
    std::lock_guard guard(_messageMutex);
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
                               ProgressInfo progressInfo)
{
    if (!_showNodeNames) {
        // If we don't want to show the node names, we can disable the updating which
        // also would create any of the text information
        return;
    }
    std::lock_guard guard(_itemsMutex);

    auto it = std::find_if(
        _items.begin(),
        _items.end(),
        [&itemIdentifier](const Item& i) {
            return i.identifier == itemIdentifier;
        }
    );
    if (it != _items.end()) {
        it->status = newStatus;
        it->progress = std::move(progressInfo);
        if (newStatus == ItemStatus::Finished) {
            it->finishedTime = std::chrono::system_clock::now();
        }
    }
    else {
        // We are not computing the location in here since doing it this way might stall
        // the main thread while trying to find a position for the new item
        Item item = {
            itemIdentifier,
            itemName,
            ItemStatus::Started,
            std::move(progressInfo),
            false,
#ifdef LOADINGSCREEN_DEBUGGING
            false,
#endif // LOADINGSCREEN_DEBUGGING
            {},
            {},
            std::chrono::system_clock::from_time_t(0)
        };

        if (newStatus == ItemStatus::Finished) {
            // This is only going to be triggered if an item finishes so quickly that
            // there was not even time to create the item between starting and finishing
            item.finishedTime = std::chrono::system_clock::now();
        }

        _items.push_back(std::move(item));
    }
}

} // namespace openspace
