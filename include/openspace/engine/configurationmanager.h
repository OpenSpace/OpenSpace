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

#ifndef __OPENSPACE_CORE___CONFIGURATIONMANAGER___H__
#define __OPENSPACE_CORE___CONFIGURATIONMANAGER___H__

#include <ghoul/misc/dictionary.h>

namespace openspace {

namespace documentation {  struct Documentation; }

/**
 * The ConfigurationManager takes care of loading the major configuration file
 * <code>openspace.cfg</code> and making it available to the rest of the application. The
 * exposed keys in the ghoul::Dictionary are declared in this class as static constants.
 * The findConfiguration method walks the filesystem from a provided starting point until
 * it found the requested file or throws a ghoul::RuntimeError if it could not find the
 * file. The loadFromFile method then loads the file into a ghoul::Dictionary format.
 */
class ConfigurationManager : public ghoul::Dictionary {
public:
    /// The key that stores the subdirectory containing all predefined path tokens
    static constexpr const char* KeyPaths = "Paths";

    /// The key that stores the location to the cache directory used to store all the
    /// permanent and non-permanent cached files
    static constexpr const char* KeyCachePath = "Paths.CACHE";

    /// The key that stores the main directory for fonts
    static constexpr const char* KeyFonts = "Fonts";

    /// The key that stores the location of the SGCT configuration file that is used on
    /// application launch
    static constexpr const char* KeyConfigSgct = "SGCTConfig";

    /// The key that defines a list of scripts for global customization that get executed
    /// regardless of which scene is loaded
    static constexpr const char* KeyGlobalCustomizationScripts =
                                                             "GlobalCustomizationScripts";

    /// The part of the key that defines the type
    // static constexpr const char* PartType = "Type";
    /// The part of the key that defines the file
    // static constexpr const char* PartFile = "File";

    /// The key that stores the Lua documentation
    static constexpr const char* KeyLuaDocumentation = "LuaDocumentation";

    /// The key that stores the scripting log
    static constexpr const char* KeyScriptLog = "ScriptLog";

    /// The key that stores the Scene Property documentation
    static constexpr const char* KeyScenePropertyDocumentation =
                                                             "ScenePropertyDocumentation";

    /// The key that stores the Root Property documentation
    static constexpr const char* KeyPropertyDocumentation = "PropertyDocumentation";

    /// The key that stores the keyboard bindings that should be stored
    static constexpr const char* KeyKeyboardShortcuts = "KeyboardShortcuts";

    /// The key that stores the main documentation
    static constexpr const char* KeyDocumentation = "Documentation";

    /// The key that stores the factory documentation values
    static constexpr const char* KeyFactoryDocumentation = "FactoryDocumentation";
    /// The key that decides whether or not we should require incoming web socket connections
    /// to authorize or not
    static constexpr const char* KeyRequireSocketAuthentication = "RequireSocketAuthentication";
    
    /// The key that stores the location of the tasks files
    static constexpr const char* KeyConfigTasksRoot = "TasksRoot";

    /// The key that stores the location of the asset file that is initially loaded
    static constexpr const char* KeyConfigAsset = "Asset";

    /// The key that stores the scene license documentation values
    static constexpr const char* KeySceneLicenseDocumentation = "LicenseDocumentation";

    /// The key that stores the settings for determining log-related settings
    static constexpr const char* KeyLogging = "Logging";

    /// The key that stores the directory for Logging
    static constexpr const char* LoggingDirectory = "Logging.LogDir";
    static constexpr const char* PartLogDir = "LogDir";

    /// The key that stores the desired LogLevel for the whole application
    /// \sa ghoul::logging::LogManager
    static constexpr const char* KeyLoggingLogLevel = "Logging.LogLevel";
    static constexpr const char* PartLogLevel = "LogLevel";

    /// The key that stores whether the log should be immediately flushed after a n
    /// \sa ghoul::logging::LogManager
    static constexpr const char* KeyLoggingImmediateFlush = "Logging.ImmediateFlush";
    static constexpr const char* PartImmediateFlush = "ImmediateFlush";

    /// The key for prefixing PerformanceMeasurement logfiles
    static constexpr const char* LoggingPerformancePrefix = "Logging.PerformancePrefix";
    static constexpr const char* PartLogPerformancePrefix = "PerformancePrefix";
    /// The key that stores a subdirectory with a description for additional
    /// ghoul::logging::Log%s to be created
    /// \sa LogFactory
    static constexpr const char* KeyLoggingLogs = "Logging.Logs";
    static constexpr const char* PartLogs = "Logs";

    /// The key that stores whether a log should be appended to or should be overwritten
    static constexpr const char* PartAppend = "Append";

    /// The key that stores the verbosity (None, Minimal, Default, Full) of the system
    /// capabilities components
    static constexpr const char* PartCapabilitiesVerbosity = "CapabilitiesVerbosity";

    /// The key that stores the settings for determining Launcher-related settings
    static constexpr const char* KeyLauncher = "Launcher";

    /// The full key that stores the verbosity of the system capabilities component
    static constexpr const char* KeyCapabilitiesVerbosity =
                                                          "Logging.CapabilitiesVerbosity";

    /// The key that stores the time (in seconds) that the application will wait before
    /// shutting down after the shutdown call is made
    static constexpr const char* KeyShutdownCountdown = "ShutdownCountdown";

    /// The key that stores whether the onscreen text should be scaled to the window size
    /// or the window resolution
    static constexpr const char* KeyOnScreenTextScaling = "OnScreenTextScaling";

    /// The key that stores whether the master node should perform rendering just function
    /// as a pure manager
    static constexpr const char* KeyDisableMasterRendering = "DisableRenderingOnMaster";

    /// The key that stores whether the master node should apply the scene transformation
    static constexpr const char* KeyDisableSceneOnMaster = "DisableSceneOnMaster";

    /// The key that stores the switch for enabling/disabling the rendering on a master
    /// computer
    static constexpr const char* KeyRenderingMethod = "RenderingMethod";

    /// The key that determines whether a new cache folder is used for each scene file
    static constexpr const char* KeyPerSceneCache = "PerSceneCache";

    /// The key that stores the http proxy settings for the downloadmanager
    static constexpr const char* KeyHttpProxy = "HttpProxy";

    /// The key that stores the address of the http proxy
    static constexpr const char* PartHttpProxyAddress = "Address";

    /// The key that stores the port of the http proxy
    static constexpr const char* PartHttpProxyPort = "Port";

    /// The key that stores the authentication method of the http proxy
    static constexpr const char* PartHttpProxyAuthentication = "Authentication";

    /// Key that stores the username to use for authentication to access the http proxy
    static constexpr const char* PartHttpProxyUser = "User";

    /// Key that stores the password to use for authentication to access the http proxy
    static constexpr const char* PartHttpProxyPassword = "Password";

    /// The key that stores the dictionary containing information about debug contexts
    static constexpr const char* KeyOpenGLDebugContext = "OpenGLDebugContext";

    /// The part of the key storing whether an OpenGL Debug context should be created
    static constexpr const char* PartActivate = "Activate";

    /// The part of the key storing whether the debug callbacks are performed synchronous
    static constexpr const char* PartSynchronous = "Synchronous";

    /// The part of the key storing a list of identifiers that should be filtered out
    static constexpr const char* PartFilterIdentifier = "FilterIdentifier";

    /// The part of the key that stores the source of the ignored identifier
    static constexpr const char* PartFilterIdentifierSource = "Source";

    /// The part of the key that stores the type of the ignored identifier
    static constexpr const char* PartFilterIdentifierType = "Type";

    /// The part of the key that stores the identifier of the ignored identifier
    static constexpr const char* PartFilterIdentifierIdentifier = "Identifier";

    /// The part of the key storing a list of severities that should be filtered out
    static constexpr const char* PartFilterSeverity = "PartFilterSeverity";

    /// The part of the key storing whether the OpenGL state should be checked each call
    static constexpr const char* KeyCheckOpenGLState = "CheckOpenGLState";

    /// The part of the key storing whether the OpenGL state should be checked each call
    static constexpr const char* KeyServerPasskey = "ServerPasskey";

    /// Whitelist of client addresses that won't need autorization
    static constexpr const char* KeyServerClientAddressWhitelist =
                                                                 "ClientAddressWhitelist";

    static constexpr const char* KeyLogEachOpenGLCall = "LogEachOpenGLCall";

    /// This key determines whether the scene graph nodes should initialized multithreaded
    static constexpr const char* KeyUseMultithreadedInitialization =
                                                         "UseMultithreadedInitialization";

    /// The key under which all of the loading settings are grouped
    static constexpr const char* KeyLoadingScreen = "LoadingScreen";

    /// The part of the key storing whether the loading screen should display the message
    /// about current status
    static constexpr const char* KeyLoadingScreenShowMessage =
                                                              "LoadingScreen.ShowMessage";
    static constexpr const char* PartShowMessage = "ShowMessage";

    /// The part of the key storing whether the loading screen should display node names
    static constexpr const char* KeyLoadingScreenShowNodeNames =
                                                            "LoadingScreen.ShowNodeNames";
    static constexpr const char* PartShowNodeNames = "ShowNodeNames";

    /// The part of the key storing whether the loading screen should contain a progress
    /// bar
    static constexpr const char* KeyLoadingScreenShowProgressbar =
                                                          "LoadingScreen.ShowProgressbar";
    static constexpr const char* PartShowProgressbar = "ShowProgressbar";

    /// The key used to specify module specific configurations
    static constexpr const char* KeyModuleConfigurations = "ModuleConfigurations";

    /// The key used to specify whether screenshots should contain the current date
    static constexpr const char* KeyScreenshotUseDate = "ScreenshotUseDate";

    static constexpr const char* KeyWebHelperLocation = "WebHelperLocation";

    static constexpr const char* KeyCefWebGuiUrl = "CefWebGuiUrl";

    /**
     * Iteratively walks the directory structure starting with \p filename to find the
     * base name compoenent of \p filename. The directory structure is searched by first
     * searching the current directory and then moving iteratively to the the parent
     * directory.
     * \param filename The fully qualified filename to be searched for
     * \return The path to the file that was found with the same base name as \p filename
     * but higher up in the file structure.
     * \throw ghoul::RuntimeError If the configuration could not be found
     */
    static std::string findConfiguration(const std::string& filename);

    /**
     * Load the provided configuration file (\p filename) into this Dictionary. All paths
     * that are specified in the configuration file will automatically be registered in
     * the ghoul::filesystem::FileSystem.
     * \param filename The filename to be loaded
     * \throw SpecificationError If the configuration file was not complete (i.e., did
     * not specify the necessary keys KeyPaths, KeyPaths.KeyCache, KeyFonts, and
     * KeyConfigSgct)
     * \throw ghoul::lua::LuaRuntimeException If there was Lua-based error loading the
     * configuration file
     * \pre \p filename must not be empty
     * \pre \p filename must exist
     */
    void loadFromFile(const std::string& filename);


    static documentation::Documentation Documentation();
};

} // namespace openspace

#endif // __OPENSPACE_CORE___CONFIGURATIONMANAGER___H__
