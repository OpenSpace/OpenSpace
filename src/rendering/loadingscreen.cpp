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

#include <openspace/rendering/loadingscreen.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/helper.h>
#include <openspace/scene/asset.h>
#include <openspace/scene/assetmanager.h>
#include <openspace/scene/scene.h>
#include <openspace/util/resourcesynchronization.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/format.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/stringconversion.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <random>
#include <sstream>
#include <thread>
#include <unordered_set>


namespace {
    constexpr float LoadingFontSize = 25.f;
    constexpr float MessageFontSize = 22.f;
    constexpr float ItemFontSize = 10.f;
    constexpr float LogFontSize = 10.f;

    constexpr glm::vec2 LogoCenter = glm::vec2(0.f, 0.525f);  // in NDC
    constexpr glm::vec2 LogoSize = glm::vec2(0.275f, 0.275);  // in NDC

    constexpr glm::vec4 ItemStatusColorStarted = glm::vec4(0.5f, 0.5f, 0.5f, 1.f);
    constexpr glm::vec4 ItemStatusColorInitializing = glm::vec4(0.7f, 0.7f, 0.f, 1.f);
    constexpr glm::vec4 ItemStatusColorFinished = glm::vec4(0.1f, 0.75f, 0.1f, 1.f);
    constexpr glm::vec4 ItemStatusColorFailed = glm::vec4(0.8f, 0.1f, 0.1f, 1.f);

    constexpr float ItemStandoffDistance = 5.f; // in pixels

    constexpr float LoadingTextPosition = 0.275f;  // in NDC
    constexpr float StatusMessageOffset = 0.225f;  // in NDC
    constexpr float LogBackgroundPosition = 0.125f; // in NDC

    constexpr int MaxNumberLocationSamples = 1000;

    constexpr std::chrono::milliseconds TTL(5000);

    constexpr std::chrono::milliseconds RefreshRate(16);

    bool rectOverlaps(glm::vec2 lhsLl, glm::vec2 lhsUr, glm::vec2 rhsLl, glm::vec2 rhsUr)
    {
        lhsLl -= glm::vec2(ItemStandoffDistance / 2.f);
        lhsUr += glm::vec2(ItemStandoffDistance / 2.f);

        rhsLl -= glm::vec2(ItemStandoffDistance / 2.f);
        rhsUr += glm::vec2(ItemStandoffDistance / 2.f);

        return lhsUr.x >= rhsLl.x && lhsLl.x <= rhsUr.x &&
               lhsUr.y >= rhsLl.y && lhsLl.y <= rhsUr.y;
    }

    glm::vec2 ndcToScreen(glm::vec2 ndc, const glm::ivec2& res) {
        ndc.x = (ndc.x + 1.f) / 2.f * res.x;
        ndc.y = (ndc.y + 1.f) / 2.f * res.y;
        return ndc;
    }
} // namespace

namespace openspace {

LoadingScreen::LoadingScreen(ShowMessage showMessage, ShowNodeNames showNodeNames,
                             ShowLogMessages showLogMessages)
    : _showMessage(showMessage)
    , _showNodeNames(showNodeNames)
    , _showLog(showLogMessages)
    , _randomEngine(_randomDevice())
{
    constexpr std::chrono::seconds ScreenLogTimeToLive(20);
    std::unique_ptr<ScreenLog> log = std::make_unique<ScreenLog>(
        ScreenLogTimeToLive,
        ScreenLog::LogLevel::Warning
    );
    _log = log.get();
    ghoul::logging::LogManager::ref().addLog(std::move(log));

    const float fontScaling = global::windowDelegate->osDpiScaling();

    _loadingFont = global::fontManager->font(
        "Loading",
        LoadingFontSize * fontScaling,
        ghoul::fontrendering::FontManager::Outline::No,
        ghoul::fontrendering::FontManager::LoadGlyphs::No
    );

    if (_showMessage) {
        _messageFont = global::fontManager->font(
            "Loading",
            MessageFontSize * fontScaling,
            ghoul::fontrendering::FontManager::Outline::No,
            ghoul::fontrendering::FontManager::LoadGlyphs::No
        );
    }

    if (_showNodeNames) {
        _itemFont = global::fontManager->font(
            "Loading",
            ItemFontSize * fontScaling,
            ghoul::fontrendering::FontManager::Outline::No,
            ghoul::fontrendering::FontManager::LoadGlyphs::No
        );
    }

    if (_showLog) {
        _logFont = global::fontManager->font(
            "Loading",
            LogFontSize * fontScaling,
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
    ghoul::logging::LogManager::ref().removeLog(_log);
    _log = nullptr;
}

void LoadingScreen::abort() {
    _shouldAbortLoading = true;
}

void LoadingScreen::exec(AssetManager& manager, Scene& scene) {
    setPhase(LoadingScreen::Phase::Construction);
    postMessage("Loading assets");

    std::unordered_set<const ResourceSynchronization*> finishedSynchronizations;
    while (true) {
        render();
        manager.update();

        std::vector<const Asset*> allAssets = manager.allAssets();

        std::vector<const ResourceSynchronization*> allSyncs =
            manager.allSynchronizations();

        // Filter already synchronized assets so we don't check them anymore
        auto syncIt = std::remove_if(
            allSyncs.begin(),
            allSyncs.end(),
            [&finishedSynchronizations](const ResourceSynchronization* sync) {
                return finishedSynchronizations.contains(sync);
            }
        );
        allSyncs.erase(syncIt, allSyncs.end());

        auto it = allSyncs.begin();
        while (it != allSyncs.end()) {
            ZoneScopedN("Update resource synchronization");

            if ((*it)->isSyncing()) {
                LoadingScreen::ProgressInfo progressInfo;

                progressInfo.progress = [](const ResourceSynchronization* sync) {
                    if (!sync->nTotalBytesIsKnown()) {
                        return 0.f;
                    }
                    if (sync->nTotalBytes() == 0) {
                        return 1.f;
                    }
                    return
                        static_cast<float>(sync->nSynchronizedBytes()) /
                        static_cast<float>(sync->nTotalBytes());
                    }(*it);

                    progressInfo.currentSize = (*it)->nSynchronizedBytes();
                    if ((*it)->nTotalBytesIsKnown()) {
                        progressInfo.totalSize = (*it)->nTotalBytes();
                    }

                    updateItem(
                        (*it)->identifier(),
                        (*it)->name(),
                        LoadingScreen::ItemStatus::Started,
                        progressInfo
                    );
                    it++;
            }
            else if ((*it)->isRejected()) {
                updateItem(
                    (*it)->identifier(),
                    (*it)->name(),
                    LoadingScreen::ItemStatus::Failed,
                    LoadingScreen::ProgressInfo()
                );
                it++;
            }
            else {
                LoadingScreen::ProgressInfo progressInfo;
                progressInfo.progress = 1.f;

                updateItem(
                    (*it)->identifier(),
                    (*it)->name(),
                    LoadingScreen::ItemStatus::Finished,
                    progressInfo
                );
                finishedSynchronizations.insert(*it);
                it = allSyncs.erase(it);
            }
        }

        if (_shouldAbortLoading) {
            global::windowDelegate->terminate();
            return;
        }

        const bool finishedLoading = std::all_of(
            allAssets.begin(),
            allAssets.end(),
            [](const Asset* asset) { return asset->isInitialized() || asset->isFailed(); }
        );

        if (finishedLoading) {
            break;
        }
    } // while(true)

    setPhase(LoadingScreen::Phase::Initialization);

    postMessage("Initializing scene");
    while (scene.isInitializing()) {
        render();
    }

    postMessage("Initializing OpenGL");
    finalize();
}

void LoadingScreen::render() {
    ZoneScoped;
    FrameMarkStart("Loading");

    if (_phase == Phase::PreStart) {
        return;
    }

    // We have to recalculate the positions here because we will not be informed about a
    // window size change

    const glm::vec2 dpiScaling = global::windowDelegate->dpiScaling();
    const glm::ivec2 res =
        glm::vec2(global::windowDelegate->firstWindowResolution()) * dpiScaling;

    const float screenAspectRatio = static_cast<float>(res.x) / static_cast<float>(res.y);

    const float textureAspectRatio = static_cast<float>(_logoTexture->dimensions().x) /
        static_cast<float>(_logoTexture->dimensions().y);

    ghoul::fontrendering::FontRenderer::defaultRenderer().setFramebufferSize(res);

    const glm::vec2 size = glm::vec2(
        LogoSize.x,
        LogoSize.y * textureAspectRatio * screenAspectRatio
    );

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
        const std::lock_guard guard(_messageMutex);

        const glm::vec2 bboxMessage = _messageFont->boundingBox(_message);

        messageLl = glm::vec2(
            res.x / 2.f - bboxMessage.x / 2.f,
            res.y * StatusMessageOffset
        );
        messageUr = messageLl + bboxMessage;


        renderer.render(*_messageFont, messageLl, _message);
    }

    const glm::vec2 logLl = glm::vec2(0.f, 0.f);
    const glm::vec2 logUr = glm::vec2(res.x, res.y * (LogBackgroundPosition + 0.015));

    // Font rendering enables depth testing so we disable again to render the log box
    glDisable(GL_DEPTH_TEST);
    if (_showLog) {
        constexpr glm::vec4 DarkGray = glm::vec4(glm::vec3(0.04f), 1.f);
        rendering::helper::renderBox(
            glm::vec2(0.f, 1.f),
            glm::vec2(1.f, LogBackgroundPosition),
            DarkGray,
            rendering::helper::Anchor::SW
        );
    }

    if (_showNodeNames) {
        const std::lock_guard guard(_itemsMutex);

        const auto now = std::chrono::system_clock::now();

        const glm::vec2 logoLl = glm::vec2(LogoCenter.x - size.x,  LogoCenter.y - size.y);
        const glm::vec2 logoUr = glm::vec2(LogoCenter.x + size.x,  LogoCenter.y + size.y);

        for (Item& item : _items) {
            if (!item.hasLocation) {
                // Compute a new location

                const glm::vec2 b = _itemFont->boundingBox(
                    (item.name + " 100%\n99999999/99999999")
                );

                glm::vec2 ll = glm::vec2(0.f);
                glm::vec2 ur = glm::vec2(0.f);
                int i = 0;
                for (; i < MaxNumberLocationSamples; i++) {
                    std::uniform_int_distribution<int> distX(
                        15,
                        static_cast<int>(res.x - b.x - 15)
                    );
                    std::uniform_int_distribution<int> distY(
                        15,
                        static_cast<int>(res.y - b.y - 15)
                    );

                    ll = glm::vec2(distX(_randomEngine), distY(_randomEngine));
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

                    const bool logOverlap = _showLog ?
                        rectOverlaps(logLl, logUr,ll, ur) :
                        false;

                    if (logoOverlap || loadingOverlap || messageOverlap || logOverlap) {
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
            if (item.status == ItemStatus::Started && item.progress.currentSize > 0) {
                ProgressInfo& info = item.progress;
                const bool isTotalSizeKnown = info.totalSize != -1;

                int p = static_cast<int>(std::round(info.progress * 100));
                if (isTotalSizeKnown) {
                    if (info.totalSize < 1024 * 1024) { // 1MB
                        text = std::format(
                            "{} ({}%)\n{}/{} {}",
                            text, p, info.currentSize, info.totalSize, "bytes"
                        );
                    }
                    else {
                        float curr = info.currentSize / (1024.f * 1024.f);
                        float total = info.totalSize / (1024.f * 1024.f);

                        text = std::format(
                            "{} ({}%)\n{:.3f}/{:.3f} {}",
                            text, p, curr, total, "MB"
                        );
                    }
                }
                else {
                    // We don't know the total size but we have started downloading data
                    if (info.currentSize < 1024 * 1024) {
                        text = std::format("{}\n{} {}", text, info.currentSize, "bytes");
                    }
                    else {
                        float curr = info.currentSize / (1024.f * 1024.f);
                        text = std::format("{}\n{:.3f} {}", text, curr, "MB");
                    }
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
                        return (i.finishedTime + TTL) < now;
                    }
                    else {
                        return false;
                    }
                }
            ),
            _items.end()
        );

    }

    // Render log messages last to make them slightly more visible if a download item
    // is slightly overlapping
    if (_showLog) {
        renderLogMessages();
    }
    glEnable(GL_CULL_FACE);
    glEnable(GL_DEPTH_TEST);

    std::this_thread::sleep_for(RefreshRate);
    global::windowDelegate->swapBuffer();
    FrameMarkEnd("Loading");
}

void LoadingScreen::renderLogMessages() const {
    ZoneScoped;

    constexpr size_t MaxNumberMessages = 6;
    constexpr int MessageLength = 209;

    using FR = ghoul::fontrendering::FontRenderer;
    const FR& renderer = FR::defaultRenderer();

    const std::vector<ScreenLog::LogEntry>& entries = _log->entries();

    size_t nRows = 0;
    const size_t j = std::min(MaxNumberMessages, entries.size());
    for (size_t i = 1; i <= j; i++) {
        ZoneScopedN("Entry");

        // Show only the j:th first log entries
        const ScreenLog::LogEntry& it = entries[j - i];

        std::ostringstream result;
        // Split really long messages into multiple lines for better readability
        if (it.message.size() > MessageLength) {
            std::istringstream is(it.message);

            int charactersSinceNewLine = 0;
            std::string word;
            while (is >> word) {
                charactersSinceNewLine += static_cast<int>(word.size());
                // Insert a new line when we exceede messageLength
                if (charactersSinceNewLine > MessageLength) {
                    result << '\n';
                    charactersSinceNewLine = static_cast<int>(word.size());
                    ++nRows;
                }
                result << word << ' ';
                ++charactersSinceNewLine;
            }
        }

        renderer.render(
            *_logFont,
            glm::vec2(
                10,
                10 + _logFont->pointSize() * nRows * 2
            ),
            it.message.size() < MessageLength ? it.message : result.str(),
            ghoul::toColor(it.level)
        );
        ++nRows;
    }

    const glm::vec2 dpiScaling = global::windowDelegate->dpiScaling();
    const glm::ivec2 res =
        glm::vec2(global::windowDelegate->firstWindowResolution()) * dpiScaling;

    // Render # of warnings and error messages
    std::map<ghoul::logging::LogLevel, size_t> numberOfErrorsPerLevel;
    for (const auto& entry : _log->entries()) {
        numberOfErrorsPerLevel[entry.level]++;
    }
    size_t row = 0;
    for (auto& [level, amount] : numberOfErrorsPerLevel) {
        const std::string text = std::format("{}: {}", ghoul::to_string(level), amount);
        const glm::vec2 bbox = _logFont->boundingBox(text);
        renderer.render(
            *_logFont,
            glm::vec2(
                res.x - bbox.x - 10,
                10 + _logFont->pointSize() * row * 2
            ),
            text,
            ghoul::toColor(level)
        );
        ++row;
    }
}

void LoadingScreen::postMessage(std::string message) {
    const std::lock_guard guard(_messageMutex);
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
    _log->removeExpiredEntries();
    _showLog = _showLog && !_log->entries().empty();
    render();
}

void LoadingScreen::setPhase(Phase phase) {
    _phase = phase;
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
    const std::lock_guard guard(_itemsMutex);

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
            .identifier = itemIdentifier,
            .name = itemName,
            .status = newStatus,
            .progress = std::move(progressInfo),
            .hasLocation = false,
            .finishedTime = std::chrono::system_clock::from_time_t(0)
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
