/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_CORE___OPENSPACEENGINE___H__
#define __OPENSPACE_CORE___OPENSPACEENGINE___H__

#include <openspace/properties/stringproperty.h>
#include <openspace/scene/profile.h>
#include <openspace/util/keys.h>
#include <openspace/util/mouse.h>
#include <openspace/util/touch.h>
#include <openspace/util/versionchecker.h>
#include <ghoul/glm.h>
#include <future>
#include <memory>
#include <string>
#include <vector>

namespace openspace {

class AssetManager;
class LoadingScreen;
class Scene;

namespace scripting { struct LuaLibrary; }

  // Structure that is responsible for the delayed shutdown of the application
struct ShutdownInformation {
    // Whether the application is currently in shutdown mode (i.e. counting down the
    // timer and closing it at '0'
    bool inShutdown = false;
    // Total amount of time the application will wait before actually shutting down
    float waitTime = 0.f;
    // Current state of the countdown; if it reaches '0', the application will
    // close
    float timer = 0.f;
};

struct CommandlineArguments {
    std::string configurationName;
    std::string configurationOverride;
};

class OpenSpaceEngine {
public:
    OpenSpaceEngine();
    ~OpenSpaceEngine();

    void registerPathTokens();
    void initialize();
    void initializeGL();
    void deinitialize();
    void deinitializeGL();
    void preSynchronization();
    void postSynchronizationPreDraw();
    void render(const glm::mat4& sceneMatrix, const glm::mat4& viewMatrix,
        const glm::mat4& projectionMatrix);
    void drawOverlays();
    void postDraw();
    void resetPropertyChangeFlags();
    void keyboardCallback(Key key, KeyModifier mod, KeyAction action);
    void charCallback(unsigned int codepoint, KeyModifier modifier);
    void mouseButtonCallback(MouseButton button, MouseAction action, KeyModifier mods);
    void mousePositionCallback(double x, double y);
    void mouseScrollWheelCallback(double posX, double posY);
    void touchDetectionCallback(TouchInput input);
    void touchUpdateCallback(TouchInput input);
    void touchExitCallback(TouchInput input);
    void handleDragDrop(const std::string& file);
    std::vector<std::byte> encode();
    void decode(std::vector<std::byte> data);

    void scheduleLoadSingleAsset(std::string assetPath);
    void toggleShutdownMode();

    // Guaranteed to return a valid pointer
    AssetManager& assetManager();
    LoadingScreen* loadingScreen();

    void writeSceneDocumentation();
    void writeStaticDocumentation();
    void createUserDirectoriesIfNecessary();

    /**
     * Returns the Lua library that contains all Lua functions available to affect the
     * application.
     */
    static scripting::LuaLibrary luaLibrary();

private:
    void loadAsset(const std::string& assetName);
    void loadFonts();

    void runGlobalCustomizationScripts();
    void configureLogging();
    std::string generateFilePath(std::string openspaceRelativePath);
    void resetPropertyChangeFlagsOfSubowners(openspace::properties::PropertyOwner* po);

    std::unique_ptr<Scene> _scene;
    std::unique_ptr<AssetManager> _assetManager;
    bool _shouldAbortLoading = false;
    std::unique_ptr<LoadingScreen> _loadingScreen;
    std::unique_ptr<VersionChecker> _versionChecker;

    bool _hasScheduledAssetLoading = false;
    std::string _scheduledAssetPathToLoad;

    glm::vec2 _mousePosition = glm::vec2(0.f);

    //grabs json from each module to pass to the documentation engine.
    std::string _documentationJson;

    std::future<void> _writeDocumentationTask;

    ShutdownInformation _shutdown;

    // The first frame might take some more time in the update loop, so we need to know to
    // disable the synchronization; otherwise a hardware sync will kill us after 1 minute
    bool _isRenderingFirstFrame = true;
};

/**
 * Sets the camera position using the time contents of a profile. The function will
 * set an absolute position or a go-to-geolocation command using the globebrowsing
 * module.
 * \param p The Profile to be read.
 */
void setCameraFromProfile(const Profile& p);

/**
 * Reads a list of modules from a profile, and executes scripts based on whether or
 * not the corresponding module is loaded.
 *
 * \param p The Profile to be read.
 */
void setModulesFromProfile(const Profile& p);

/**
 * Registers actions from the contents of a profile.
 *
 * \param p The Profile to be read.
 */
void setActionsFromProfile(const Profile& p);

/**
 * Registers keybindings from the contents of a profile.
 *
 * \param p The Profile to be read.
 */
void setKeybindingsFromProfile(const Profile& p);

/**
 * Reads list of nodes from profile to be marked as interesting nodes.
 * If any nodes are listed, a script to mark these will be queued with the
 * script engine.
 *
 * \param p The Profile to be read.
 */
void setMarkInterestingNodesFromProfile(const Profile& p);

/**
 * Reads list of "additional scripts" that are added to the profile to be run
 * at the end of the initialization. Any openspace lua commands are allowed,
 * and will be added to the script queue.
 *
 * \param p The Profile to be read.
 */
void setAdditionalScriptsFromProfile(const Profile& p);

} // namespace openspace

// Lua functions - exposed for testing
namespace openspace::luascriptfunctions {

int createSingleColorImage(lua_State* L);

} // openspace::luascriptfunctions

#endif // __OPENSPACE_CORE___OPENSPACEENGINE___H__
