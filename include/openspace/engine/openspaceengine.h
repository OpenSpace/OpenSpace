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

#ifndef __OPENSPACE_CORE___OPENSPACEENGINE___H__
#define __OPENSPACE_CORE___OPENSPACEENGINE___H__

#include <openspace/engine/globalscallbacks.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/property.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
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

/**
 * Structure that is responsible for the delayed shutdown of the application.
 */
struct ShutdownInformation {
    /// Whether the application is currently in shutdown mode (i.e. counting down the
    /// timer and closing it at '0'
    bool inShutdown = false;
    /// Total amount of time the application will wait before actually shutting down
    float waitTime = 0.f;
    /// Current state of the countdown; if it reaches '0', the application will
    /// close
    float timer = 0.f;
};

struct CommandlineArguments {
    std::optional<std::string> configuration;
    std::optional<std::string> windowConfig;
    std::optional<std::string> profile;
    std::optional<std::string> propertyVisibility;
    std::optional<bool> bypassLauncher;
};

class OpenSpaceEngine : public properties::PropertyOwner {
public:
    /**
     * A mode that specifies which part of the system is currently in control. The mode
     * can be used to limit certain features, like setting time, navigation or triggering
     * scripts.
     */
    enum class Mode {
        UserControl = 0,
        SessionRecordingPlayback,
        CameraPath
    };

    OpenSpaceEngine();
    ~OpenSpaceEngine() override;

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
    void keyboardCallback(Key key, KeyModifier mod, KeyAction action,
        IsGuiWindow isGuiWindow);
    void charCallback(unsigned int codepoint, KeyModifier modifier,
        IsGuiWindow isGuiWindow);
    void mouseButtonCallback(MouseButton button, MouseAction action,
        KeyModifier mods, IsGuiWindow isGuiWindow);
    void mousePositionCallback(double x, double y, IsGuiWindow isGuiWindow);
    void mouseScrollWheelCallback(double posX, double posY, IsGuiWindow isGuiWindow);
    void touchDetectionCallback(TouchInput input);
    void touchUpdateCallback(TouchInput input);
    void touchExitCallback(TouchInput input);
    void handleDragDrop(std::filesystem::path file);
    std::vector<std::byte> encode();
    void decode(std::vector<std::byte> data);

    properties::Property::Visibility visibility() const;
    void toggleShutdownMode();

    Mode currentMode() const;
    bool setMode(Mode newMode);
    void resetMode();

    using CallbackHandle = int;
    using ModeChangeCallback = std::function<void()>;

    CallbackHandle addModeChangeCallback(ModeChangeCallback cb);
    void removeModeChangeCallback(CallbackHandle handle);

    // Guaranteed to return a valid pointer
    AssetManager& assetManager();
    LoadingScreen* loadingScreen();

    void createUserDirectoriesIfNecessary();

    /**
     * Returns the Lua library that contains all Lua functions available to affect the
     * application.
     */
    static scripting::LuaLibrary luaLibrary();

private:
    void loadAssets();
    void loadFonts();

    void runGlobalCustomizationScripts();

    properties::BoolProperty _printEvents;
    properties::OptionProperty _visibility;
    properties::FloatProperty _fadeOnEnableDuration;
    properties::BoolProperty _disableAllMouseInputs;

    std::unique_ptr<Scene> _scene;
    std::unique_ptr<AssetManager> _assetManager;
    std::unique_ptr<LoadingScreen> _loadingScreen;
    std::unique_ptr<VersionChecker> _versionChecker;

    glm::vec2 _mousePosition = glm::vec2(0.f);

    std::future<void> _writeDocumentationTask;

    ShutdownInformation _shutdown;

    // The first frame might take some more time in the update loop, so we need to know to
    // disable the synchronization; otherwise a hardware sync will kill us after 1 minute
    bool _isRenderingFirstFrame = true;

    Mode _currentMode = Mode::UserControl;
    Mode _modeLastFrame = Mode::UserControl;

    int _nextCallbackHandle = 0;
    std::vector<std::pair<CallbackHandle, ModeChangeCallback>> _modeChangeCallbacks;
};

/**
 * Sets the camera position using the time contents of a profile. The function will
 * set an absolute position or a go-to-geolocation command using the globebrowsing
 * module.
 *
 * \param p The Profile to be read
 */
void setCameraFromProfile(const Profile& p);

/**
 * Reads a list of modules from a profile, and executes scripts based on whether or
 * not the corresponding module is loaded.
 *
 * \param p The Profile to be read
 */
void setModulesFromProfile(const Profile& p);

/**
 * Registers actions from the contents of a profile.
 *
 * \param p The Profile to be read
 */
void setActionsFromProfile(const Profile& p);

/**
 * Registers keybindings from the contents of a profile.
 *
 * \param p The Profile to be read
 */
void setKeybindingsFromProfile(const Profile& p);

/**
 * Reads list of nodes from profile to be marked as interesting nodes.
 * If any nodes are listed, a script to mark these will be queued with the
 * script engine.
 *
 * \param p The Profile to be read
 */
void setMarkInterestingNodesFromProfile(const Profile& p);

/**
 * Reads list of "additional scripts" that are added to the profile to be run
 * at the end of the initialization. Any openspace lua commands are allowed,
 * and will be added to the script queue.
 *
 * \param p The Profile to be read
 */
void setAdditionalScriptsFromProfile(const Profile& p);

} // namespace openspace

std::filesystem::path createSingleColorImage(std::string name, glm::dvec3 color);

#endif // __OPENSPACE_CORE___OPENSPACEENGINE___H__
