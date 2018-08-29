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

#ifndef __OPENSPACE_CORE___LOADINGSCREEN___H__
#define __OPENSPACE_CORE___LOADINGSCREEN___H__

#include <ghoul/glm.h>
#include <ghoul/misc/boolean.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
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

class LoadingScreen {
public:
    BooleanType(ShowMessage);
    BooleanType(ShowNodeNames);
    BooleanType(ShowProgressbar);
    BooleanType(CatastrophicError);

    LoadingScreen(ShowMessage showMessage, ShowNodeNames showNodeNames,
        ShowProgressbar showProgressbar);
    ~LoadingScreen();

    void render();

    void postMessage(std::string message);
    void setCatastrophicError(CatastrophicError catastrophicError);

    void finalize();

    void setItemNumber(int nItems);
    int itemNumber();
    void tickItem();

    enum class Phase {
        Construction,
        Synchronization,
        Initialization
    };
    void setPhase(Phase phase);


    enum class ItemStatus {
        Started,
        Initializing,
        Finished,
        Failed
    };

    struct ProgressInfo {
        float progress = 0.f;

        int currentSize = -1;
        int totalSize = -1;
    };

    void updateItem(const std::string& itemIdentifier, const std::string& itemName,
        ItemStatus newStatus, ProgressInfo progressInfo);

private:
    bool _showMessage;
    bool _showNodeNames;
    bool _showProgressbar;

    Phase _phase;
    int _iProgress = 0;
    int _nItems = 0;

    std::unique_ptr<ghoul::opengl::ProgramObject> _program;
    UniformCache(logoTexture, useTexture, color) _uniformCache;

    std::unique_ptr<ghoul::opengl::Texture> _logoTexture;

    std::shared_ptr<ghoul::fontrendering::Font> _loadingFont;
    std::shared_ptr<ghoul::fontrendering::Font> _messageFont;
    std::shared_ptr<ghoul::fontrendering::Font> _itemFont;

    struct {
        GLuint vao = 0;
        GLuint vbo = 0;
    } _logo;

    struct {
        GLuint vaoFill = 0;
        GLuint vboFill = 0;

        GLuint vaoBox = 0;
        GLuint vboBox = 0;
    } _progressbar;

    bool _hasCatastrophicErrorOccurred = false;
    std::string _message;
    std::mutex _messageMutex;

    struct Item {
        std::string identifier;
        std::string name;
        ItemStatus status;

        ProgressInfo progress;

        bool hasLocation;
        glm::vec2 ll;
        glm::vec2 ur;

        std::chrono::system_clock::time_point finishedTime;
    };
    std::vector<Item> _items;
    std::mutex _itemsMutex;

    std::random_device _randomDevice;
    std::default_random_engine _randomEngine;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___LOADINGSCREEN___H__
