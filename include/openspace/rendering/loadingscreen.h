/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_CORE___LOADINGSCREEN___H__
#define __OPENSPACE_CORE___LOADINGSCREEN___H__

#include <openspace/util/screenlog.h>
#include <ghoul/glm.h>
#include <ghoul/misc/boolean.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <atomic>
#include <memory>
#include <mutex>
#include <random>

// #define LOADINGSCREEN_DEBUGGING

namespace ghoul::fontrendering { class Font; }

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

class AssetManager;
class Scene;

class LoadingScreen {
public:
    BooleanType(ShowMessage);
    BooleanType(ShowNodeNames);
    BooleanType(ShowLogMessages);
    BooleanType(CatastrophicError);

    LoadingScreen(ShowMessage showMessage, ShowNodeNames showNodeNames,
        ShowLogMessages showLogMessages);
    ~LoadingScreen();

    void abort();
    void exec(AssetManager& manager, Scene& scene);

    void render();

    struct ProgressInfo {
        float progress = 0.f;

        int64_t currentSize = -1;
        int64_t totalSize = -1;
    };

    enum class ItemStatus {
        Started,
        Initializing,
        Finished,
        Failed
    };

    void updateItem(const std::string& itemIdentifier, const std::string& itemName,
        ItemStatus newStatus, ProgressInfo progressInfo);

private:
    enum class Phase {
        PreStart,
        Construction,
        Synchronization,
        Initialization
    };

    void postMessage(std::string message);
    void setCatastrophicError(CatastrophicError catastrophicError);

    void finalize();
    void setPhase(Phase phase);

    void renderLogMessages() const;

    bool _showMessage = true;
    bool _showNodeNames = true;
    bool _showLog = true;

    Phase _phase = Phase::PreStart;

    std::unique_ptr<ghoul::opengl::Texture> _logoTexture;

    std::shared_ptr<ghoul::fontrendering::Font> _loadingFont;
    std::shared_ptr<ghoul::fontrendering::Font> _messageFont;
    std::shared_ptr<ghoul::fontrendering::Font> _itemFont;
    std::shared_ptr<ghoul::fontrendering::Font> _logFont;

    bool _hasCatastrophicErrorOccurred = false;
    std::string _message;
    std::mutex _messageMutex;

    struct Item {
        std::string identifier;
        std::string name;
        ItemStatus status;

        ProgressInfo progress;

        bool hasLocation;
        glm::vec2 ll = glm::vec2(0.f);
        glm::vec2 ur = glm::vec2(0.f);

        std::chrono::system_clock::time_point finishedTime;

#ifdef LOADINGSCREEN_DEBUGGING
        bool exhaustedSearch = false;
#endif // LOADINGSCREEN_DEBUGGING
    };
    std::vector<Item> _items;
    std::mutex _itemsMutex;

    bool _shouldAbortLoading = false;

    std::random_device _randomDevice;
    std::default_random_engine _randomEngine;

    // Non owning but we remove the log from LogManager on destruction
    ScreenLog* _log = nullptr;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___LOADINGSCREEN___H__
