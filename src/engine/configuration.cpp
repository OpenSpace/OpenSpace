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

#include <openspace/engine/configuration.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/settings.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/assert.h>
#include <json/json.hpp>
#include <optional>

namespace {
    // We can't use ${SCRIPTS} here as that hasn't been defined by this point
    constexpr std::string_view InitialConfigHelper =
        "${BASE}/scripts/configuration_helper.lua";

    struct [[codegen::Dictionary(Configuration)]] Parameters {
        // The SGCT configuration file that determines the window and view frustum
        // settings that are being used when OpenSpace is started
        std::optional<std::string> windowConfiguration [[codegen::key("SGCTConfig")]];

        // The scene description that is used to populate the application after startup.
        // The scene determines which objects are loaded, the startup time and other
        // scene-specific settings. More information is provided in the Scene
        // documentation. If the 'Asset' and the 'Profile' values are specified, the asset
        // is silently ignored
        std::optional<std::string> asset;

        // The profile that should be loaded at the startup. The profile determines which
        // assets are loaded, the startup time, keyboard shortcuts, and other settings.
        std::optional<std::string> profile;

        // This value names a list of scripts that get executed after initialization of
        // any scene. These scripts can be used for user-specific customization, such as a
        // global rebinding of keys from the default
        std::optional<std::vector<std::string>> globalCustomizationScripts;

        enum class [[codegen::map(openspace::properties::Property::Visibility)]]
        Visibility
        {
            NoviceUser = 0,
            User,
            AdvancedUser,
            Developer
        };
        // Determines the property visibility level that is selected when starting up
        // OpenSpace. If it is not provided, it defaults to 'User'
        std::optional<Visibility> propertyVisibility;

        // A list of paths that are automatically registered with the file system. If a
        // key X is used in the table, it is then useable by referencing ${X} in all other
        // configuration files or scripts
        std::map<std::string, std::string> paths;

        // A list of all fonts that will automatically be loaded on startup. Each
        // key-value pair contained in the table will become the name and the file for a
        // font
        std::optional<std::map<std::string, std::string>> fonts;

        struct FontSizes {
            // The font size (in pt) used for printing optional information about the
            // currently rendered frame
            float frameInfo;
            // The font size (in pt) used for rendering the shutdown text
            float shutdown;
            // The font size (in pt) used for rendering the screen log
            float log;
            // The font size (in pt) used for printing the camera friction state
            float cameraInfo;
            // The font size (in pt) used for printing the version information
            float versionInfo;
        };
        // Information about the hardcoded fontsizes used by the rendering engine itself
        FontSizes fontSize;

        struct Logging {
            // The severity of log messages that will be displayed. Only messages of the
            // selected level or higher will be displayed. All levels below will be
            // silently discarded. The order of severities is:
            // Debug < Info < Warning < Error < Fatal < None.
            std::optional<std::string> logLevel [[codegen::inlist("Trace", "Debug",
                "Info", "Warning", "Error", "Fatal", "None")]];

            // Determines whether error messages will be displayed immediately or if it is
            // acceptable to have a short delay, but being more performant. If the delay
            // is allowed ('true'), messages might get lost if the application crashes
            // shortly after a message was logged
            std::optional<bool> immediateFlush;

            // Per default, log messages are written to the console, the onscreen text,
            // and (if available) the Visual Studio output window. This table can define
            // other logging methods that will be used additionally
            std::optional<std::vector<ghoul::Dictionary>> logs
                [[codegen::reference("core_logfactory")]];

            // At startup, a list of system capabilities is created and logged. This value
            // determines how verbose this listing should be
            std::optional<std::string> capabilitiesVerbosity [[codegen::inlist("None",
                "Minimal", "Default", "Full"
            )]];
        };
        // Configurations for the logging of messages that are generated throughout the
        // code and are useful for debugging potential errors or other information
        std::optional<Logging> logging;

        // The file that will be created on startup containing the log of all Lua scripts
        // that are executed in the last session. Any existing file (including the results
        // from previous runs) will be silently overwritten
        std::optional<std::string> scriptLog;

        // If this value is specified, this many number of script log files are being
        // retained before overwriting any
        std::optional<int> scriptLogRotation;

        struct Documentation {
            // The path where the documentation files will be stored
            std::optional<std::string> path;
        };
        // Right now only contains the path where the documentation is written to
        std::optional<Documentation> documentation;

        // The countdown that the application will wait between pressing ESC and actually
        // shutting down. If ESC is pressed again in this time, the shutdown is aborted
        std::optional<float> shutdownCountdown [[codegen::greater(0.0)]];

        // If this is set to 'true', the name of the profile will be appended to the cache
        // directory, thus not reusing the same directory. This is useful in cases where
        // the same instance of OpenSpace is run with multiple profiles, but the caches
        // should be retained. This value defaults to 'false'
        std::optional<bool> perProfileCache;

        // The method for scaling the onscreen text in the window. As the resolution of
        // the rendering can be different from the size of the window, the onscreen text
        // can either be scaled according to the window size ('window'), or the rendering
        // resolution ('framebuffer'). This value defaults to 'window'
        std::optional<std::string> onScreenTextScaling [[codegen::inlist("window",
            "framebuffer")]];

        // Toggles whether the master in a multi-application setup should be rendering or
        // just managing the state of the network. This is desired in cases where the
        // master computer does not have the resources to render a scene
        std::optional<bool> disableRenderingOnMaster;

        // Applies a global view rotation. Use this to rotate the position of the focus
        // node away from the default location on the screen. This setting persists even
        // when a new focus node is selected. Defined using roll, pitch, yaw in radians
        std::optional<glm::vec3> globalRotation;

        // Applies a view rotation for only the master node, defined using roll, pitch yaw
        // in radians. This can be used to compensate the master view direction for tilted
        // display systems in clustered immersive environments
        std::optional<glm::vec3> masterRotation;

        // Applies a global rotation for all screenspace renderables. Defined using roll,
        // pitch, yaw in radians
        std::optional<glm::vec3> screenSpaceRotation;

        // If this value is set to 'true' the ingame console is disabled, locking the
        // system down against random access
        std::optional<bool> disableInGameConsole;

        // Toggles whether screenshots generated by OpenSpace contain the date when the
        // concrete OpenSpace instance was started. This value is enabled by default, but
        // it is advised to disable this value if rendering sessions of individual frames
        // pass beyond local midnight
        std::optional<bool> screenshotUseDate;

        struct HttpProxy {
            // Determines whether the proxy is being used
            std::optional<bool> activate;

            // The address of the http proxy
            std::string address;

            // The port of the http proxy
            int port [[codegen::inrange(0, 65536)]];

            // The authentication method of the http proxy
            std::optional<std::string> authentication [[codegen::inlist("basic", "ntlm",
                "digest", "any")]];

            // The user of the http proxy
            std::optional<std::string> user;

            // The password of the http proxy
            std::optional<std::string> password;
        };
        // This defines the use for a proxy when fetching data over http. No proxy will be
        // used if this is left out
        std::optional<HttpProxy> httpProxy;

        struct OpenGLDebugContext {
            // Determines whether the OpenGL context should be a debug context
            bool activate;

            // If this is set to 'true', everytime an OpenGL error is logged, the full
            // stacktrace leading to the error is printed as well, making debugging under
            // production situations much easier
            std::optional<bool> printStacktrace;

            // Determines whether the OpenGL debug callbacks are performed synchronously.
            // If set to 'true' the callbacks are in the same thread as the context and in
            // the scope of the OpenGL function that triggered the message. The default
            // value is 'true'
            std::optional<bool> synchronous;

            // Individual OpenGL debug message identifiers
            struct Filter {
                // The identifier that is to be filtered
                int identifier;

                // The source of the identifier to be filtered
                std::string source [[codegen::inlist("API", "Window System",
                    "Shader Compiler", "Third Party", "Application", "Other",
                    "Don't care")]];

                // The type of the identifier to be filtered
                std::string type [[codegen::inlist("Error", "Deprecated", "Undefined",
                    "Portability", "Performance", "Marker", "Push group", "Pop group",
                    "Other", "Don't care")]];
            };
            // A list of OpenGL debug messages identifiers that are filtered
            std::optional<std::vector<Filter>> filterIdentifier;

            // Determines the settings for the creation of an OpenGL debug context
            std::optional<std::vector<std::string>> filterSeverity [[codegen::inlist(
                "High", "Medium", "Low", "Notification"
            )]];
        };
        // Determines the settings for the creation of an OpenGL debug context
        std::optional<OpenGLDebugContext> openGLDebugContext;

        // Determines whether the OpenGL state is checked after each OpenGL function call.
        // This will dramatically slow down the rendering, but will make finding OpenGL
        // errors easier. This defaults to 'false'
        std::optional<bool> checkOpenGLState;

        // Determines whether each OpenGL call that happens should be logged using the
        // 'TRACE' loglevel. This will bring the rendering to a crawl but provides useful
        // debugging features for the order in which OpenGL calls occur. This defaults to
        // 'false'
        std::optional<bool> logEachOpenGLCall;

        // Determines whether events are printed as debug messages to the console each
        // frame. If this value is set it determines the default value of the property of
        // the OpenSpaceEngine with the same name
        std::optional<bool> printEvents;

        /// Determines which key opens the in-game console. The value passed in must be a
        /// valid key (see keys.h for a list)
        std::optional<std::string> consoleKey;

        // This value determines whether the initialization of the scene graph should
        // occur multithreaded, that is, whether multiple scene graph nodes should
        // initialize in parallel. The only use for this value is to disable it for
        // debugging support
        std::optional<bool> useMultithreadedInitialization;

        // If this value is set to 'true', the launcher will not be shown and OpenSpace
        // will start with the provided configuration options directly. Useful in
        // multiprojector setups where a launcher window would be undesired
        std::optional<bool> bypassLauncher;

        // Set which layer server should be preferd to be used, the options are
        // \"All\", \"NewYork\", \"Sweden\", \"Utah\" and \"None\".
        std::optional<std::string> layerServer [[codegen::inlist("All", "NewYork",
            "Sweden", "Utah", "None")]];

        // The URL that is pinged to check which version of OpenSpace is the most current
        // if you don't want this request to happen, this value should not be set at all
        std::optional<std::string> versionCheckUrl;

        struct LoadingScreen {
            // If this value is set to 'true', the loading screen will display a message
            // information about the current phase the loading is in
            std::optional<bool> showMessage;

            // If this value is set to 'true', the loading screen will display a list of
            // all of the nodes with their respective status (created, loaded,
            // initialized)
            std::optional<bool> showNodeNames;

            // If this value is set to 'true', the loading screen will display a list of
            // warning and error messages
            std::optional<bool> showLogMessages;
        };
        // Values in this table describe the behavior of the loading screen that is
        // displayed while the scene graph is created and initialized
        std::optional<LoadingScreen> loadingScreen;

        // Configurations for each module
        std::optional<std::map<std::string, ghoul::Dictionary>> moduleConfigurations;
    };
#include "configuration_codegen.cpp"
} // namespace

namespace openspace {

ghoul::Dictionary Configuration::createDictionary() {
    ghoul::Dictionary res;

    res.setValue("WindowConfiguration", windowConfiguration);
    res.setValue("Asset", asset);
    res.setValue("Profile", profile);
    res.setValue("PropertyVisibility", static_cast<int>(propertyVisibility));

    ghoul::Dictionary globalCustomizationScriptsDict;
    for (size_t i = 0; i < globalCustomizationScripts.size(); i++) {
        globalCustomizationScriptsDict.setValue(
            std::to_string(i),
            globalCustomizationScripts[i]
        );
    }
    res.setValue("GlobalCustomizationScripts", globalCustomizationScriptsDict);

    ghoul::Dictionary fontsDict;
    for (const auto& [name, path] : fonts) {
        fontsDict.setValue(name, path);
    }
    res.setValue("Fonts", fontsDict);

    {
        ghoul::Dictionary fontSizeDict;
        fontSizeDict.setValue("FrameInfo", static_cast<double>(fontSize.frameInfo));
        fontSizeDict.setValue("Shutdown", static_cast<double>(fontSize.shutdown));
        fontSizeDict.setValue("Log", static_cast<double>(fontSize.log));
        fontSizeDict.setValue("CameraInfo", static_cast<double>(fontSize.cameraInfo));
        fontSizeDict.setValue("VersionInfo", static_cast<double>(fontSize.versionInfo));

        res.setValue("FontSize", fontSizeDict);
    }

    {
        ghoul::Dictionary loggingDict;
        loggingDict.setValue("Level", logging.level);
        loggingDict.setValue("ForceImmediateFlush", logging.forceImmediateFlush);
        loggingDict.setValue("CapabilitiesVerbosity", logging.capabilitiesVerbosity);

        ghoul::Dictionary logsDict;
        for (size_t i = 0; i < logging.logs.size(); i++) {
            logsDict.setValue(std::to_string(i), logging.logs[i]);
        }
        loggingDict.setValue("Logs", logsDict);

        res.setValue("Logging", loggingDict);
    }

    res.setValue("ScriptLog", scriptLog);
    res.setValue("ScriptLogRotation", scriptLogRotation);

    {
        ghoul::Dictionary documentationDict;
        documentationDict.setValue("Path", documentation.path);

        res.setValue("Documentation", documentationDict);
    }

    res.setValue("VersionCheckUrl", versionCheckUrl);
    res.setValue("UseMultithreadedInitialization", useMultithreadedInitialization);

    {
        ghoul::Dictionary loadingScreenDict;
        loadingScreenDict.setValue("IsShowingMessages", loadingScreen.isShowingMessages);
        loadingScreenDict.setValue(
            "IsShowingNodeNames",
            loadingScreen.isShowingNodeNames
        );
        loadingScreenDict.setValue(
            "IsShowingLogMessages",
            loadingScreen.isShowingLogMessages
        );

        res.setValue("LoadingScreen", loadingScreenDict);
    }

    res.setValue("IsCheckingOpenGLState", isCheckingOpenGLState);
    res.setValue("IsLoggingOpenGLCalls", isLoggingOpenGLCalls);
    res.setValue("IsPrintingEvents", isPrintingEvents);
    res.setValue("ConsoleKey", ghoul::to_string(consoleKey));
    res.setValue("ShutdownCountdown", static_cast<double>(shutdownCountdown));
    res.setValue("shouldUseScreenshotDate", shouldUseScreenshotDate);
    res.setValue("OnScreenTextScaling", onScreenTextScaling);
    res.setValue("UsePerProfileCache", usePerProfileCache);
    res.setValue("IsRenderingOnMasterDisabled", isRenderingOnMasterDisabled);
    res.setValue("GlobalRotation", static_cast<glm::dvec3>(globalRotation));
    res.setValue("ScreenSpaceRotation", static_cast<glm::dvec3>(screenSpaceRotation));
    res.setValue("MasterRotation", static_cast<glm::dvec3>(masterRotation));
    res.setValue("IsConsoleDisabled", isConsoleDisabled);
    res.setValue("BypassLauncher", bypassLauncher);
    res.setValue("LayerServer", layerServerToString(layerServer));

    ghoul::Dictionary moduleConfigurationsDict;
    for (const auto& [key, value] : moduleConfigurations) {
        moduleConfigurationsDict.setValue(key, value);
    }
    res.setValue("ModuleConfigurations", moduleConfigurationsDict);

    {
        ghoul::Dictionary openGLDebugContextDict;
        openGLDebugContextDict.setValue("IsActive", openGLDebugContext.isActive);
        openGLDebugContextDict.setValue(
            "PrintStacktrace",
            openGLDebugContext.printStacktrace
        );
        openGLDebugContextDict.setValue(
            "IsSynchronous",
            openGLDebugContext.isSynchronous
        );

        ghoul::Dictionary identifierFiltersDict;
        for (size_t i = 0; i < openGLDebugContext.severityFilters.size(); i++) {
            {
                ghoul::Dictionary identifierFilterDict;
                identifierFilterDict.setValue(
                    "Type",
                    openGLDebugContext.identifierFilters[i].type
                );
                identifierFilterDict.setValue(
                    "Source",
                    openGLDebugContext.identifierFilters[i].source
                );
                identifierFilterDict.setValue(
                    "Identifier",
                    static_cast<int>(openGLDebugContext.identifierFilters[i].identifier)
                );

                identifierFiltersDict.setValue(std::to_string(i), identifierFilterDict);
            }
        }
        openGLDebugContextDict.setValue("IdentifierFilters", identifierFiltersDict);

        ghoul::Dictionary severityFiltersDict;
        for (size_t i = 0; i < openGLDebugContext.severityFilters.size(); i++) {
            severityFiltersDict.setValue(
                std::to_string(i),
                openGLDebugContext.severityFilters[i]
            );
        }
        openGLDebugContextDict.setValue("SeverityFilters", severityFiltersDict);

        res.setValue("OpenGLDebugContext", openGLDebugContextDict);
    }

    {
        ghoul::Dictionary httpProxyDict;
        httpProxyDict.setValue("UsingHttpProxy", httpProxy.usingHttpProxy);
        httpProxyDict.setValue("Address", httpProxy.address);
        httpProxyDict.setValue("Port", static_cast<int>(httpProxy.port));
        httpProxyDict.setValue("Authentication", httpProxy.authentication);
        httpProxyDict.setValue("User", httpProxy.user);
        httpProxyDict.setValue("Password", httpProxy.password);

        res.setValue("HttpProxy", httpProxyDict);
    }

    res.setValue("SgctConfigNameInitialized", sgctConfigNameInitialized);

    return res;
}

void parseLuaState(Configuration& configuration) {
    using namespace ghoul::lua;

    // Shorten the rest of this function
    Configuration& c = configuration;
    const LuaState& s = c.state;

    // The sgctConfigNameInitialized is a bit special
    lua_getglobal(s, "sgctconfiginitializeString");
    c.sgctConfigNameInitialized = ghoul::lua::value<std::string>(
        s,
        1,
        ghoul::lua::PopValue::Yes
    );


    // The configuration file sets all values as global variables, so we need to pull them
    // into a table first so that we can pass that table to the dictionary constructor
    lua_newtable(s);

    // We go through all of the entries and lift them from global scope into the table on
    // the stack so that we can create a ghoul::Dictionary from this new table
    const documentation::Documentation doc = codegen::doc<Parameters>(
        "core_configuration"
    );
    for (const documentation::DocumentationEntry& e : doc.entries) {
        lua_pushstring(s, e.key.c_str());
        lua_getglobal(s, e.key.c_str());
        lua_settable(s, -3);
    }


    ghoul::Dictionary d;
    ghoul::lua::luaDictionaryFromState(s, d);
    lua_settop(s, 0);
    const Parameters p = codegen::bake<Parameters>(d);

    c.windowConfiguration = p.windowConfiguration.value_or(c.windowConfiguration);
    c.asset = p.asset.value_or(c.asset);
    c.profile = p.profile.value_or(c.profile);
    c.globalCustomizationScripts =
        p.globalCustomizationScripts.value_or(c.globalCustomizationScripts);

    if (p.propertyVisibility.has_value()) {
        c.propertyVisibility = codegen::map<properties::Property::Visibility>(
            *p.propertyVisibility
        );
    }
    c.pathTokens = p.paths;
    c.fonts = p.fonts.value_or(c.fonts);
    c.fontSize.frameInfo = p.fontSize.frameInfo;
    c.fontSize.shutdown = p.fontSize.shutdown;
    c.fontSize.log = p.fontSize.log;
    c.fontSize.cameraInfo = p.fontSize.cameraInfo;
    c.fontSize.versionInfo = p.fontSize.versionInfo;
    c.scriptLog = p.scriptLog.value_or(c.scriptLog);
    c.scriptLogRotation = p.scriptLogRotation.value_or(3);
    c.versionCheckUrl = p.versionCheckUrl.value_or(c.versionCheckUrl);
    c.useMultithreadedInitialization =
        p.useMultithreadedInitialization.value_or(c.useMultithreadedInitialization);
    c.isCheckingOpenGLState = p.checkOpenGLState.value_or(c.isCheckingOpenGLState);
    c.isLoggingOpenGLCalls = p.logEachOpenGLCall.value_or(c.isLoggingOpenGLCalls);
    c.isPrintingEvents = p.printEvents.value_or(c.isPrintingEvents);

    if (p.consoleKey.has_value()) {
        const KeyWithModifier km = stringToKey(*p.consoleKey);
        if (km.modifier != KeyModifier::None) {
            throw ghoul::RuntimeError(std::format(
                "Console key '{}' must be a 'bare' key and cannot contain any modifiers",
                *p.consoleKey
            ));
        }

        c.consoleKey = km.key;
    }

    c.shutdownCountdown = p.shutdownCountdown.value_or(c.shutdownCountdown);
    c.shouldUseScreenshotDate = p.screenshotUseDate.value_or(c.shouldUseScreenshotDate);
    c.onScreenTextScaling = p.onScreenTextScaling.value_or(c.onScreenTextScaling);
    c.usePerProfileCache = p.perProfileCache.value_or(c.usePerProfileCache);
    c.isRenderingOnMasterDisabled =
        p.disableRenderingOnMaster.value_or(c.isRenderingOnMasterDisabled);
    c.globalRotation = p.globalRotation.value_or(c.globalRotation);
    c.masterRotation = p.masterRotation.value_or(c.masterRotation);
    c.screenSpaceRotation = p.screenSpaceRotation.value_or(c.screenSpaceRotation);
    c.isConsoleDisabled = p.disableInGameConsole.value_or(c.isConsoleDisabled);
    if (p.logging.has_value()) {
        c.logging.level = p.logging->logLevel.value_or(c.logging.level);
        c.logging.forceImmediateFlush =
            p.logging->immediateFlush.value_or(c.logging.forceImmediateFlush);
        c.logging.logs = p.logging->logs.value_or(c.logging.logs);
        c.logging.capabilitiesVerbosity =
            p.logging->capabilitiesVerbosity.value_or(c.logging.capabilitiesVerbosity);
    }

    if (p.documentation.has_value()) {
        c.documentation.path = p.documentation->path.value_or(c.documentation.path);
    }

    if (p.loadingScreen.has_value()) {
        const Parameters::LoadingScreen& l = *p.loadingScreen;
        c.loadingScreen.isShowingMessages =
            l.showMessage.value_or(c.loadingScreen.isShowingMessages);
        c.loadingScreen.isShowingNodeNames =
            l.showNodeNames.value_or(c.loadingScreen.isShowingNodeNames);
        c.loadingScreen.isShowingLogMessages =
            l.showLogMessages.value_or(c.loadingScreen.isShowingLogMessages);
    }

    c.moduleConfigurations = p.moduleConfigurations.value_or(c.moduleConfigurations);

    if (p.openGLDebugContext.has_value()) {
        const Parameters::OpenGLDebugContext& l = *p.openGLDebugContext;
        c.openGLDebugContext.isActive = l.activate;
        c.openGLDebugContext.printStacktrace = l.printStacktrace.value_or(
            c.openGLDebugContext.printStacktrace
        );
        c.openGLDebugContext.isSynchronous = l.synchronous.value_or(
            c.openGLDebugContext.isSynchronous
        );
        if (l.filterIdentifier.has_value()) {
            for (const Parameters::OpenGLDebugContext::Filter& f : *l.filterIdentifier) {
                Configuration::OpenGLDebugContext::IdentifierFilter filter;
                filter.identifier = static_cast<unsigned int>(f.identifier);
                filter.source = f.source;
                filter.type = f.type;
                c.openGLDebugContext.identifierFilters.push_back(filter);
            }
        }
        if (l.filterSeverity.has_value()) {
            c.openGLDebugContext.severityFilters = *l.filterSeverity;
        }
    }

    if (p.httpProxy.has_value()) {
        c.httpProxy.usingHttpProxy =
            p.httpProxy->activate.value_or(c.httpProxy.usingHttpProxy);
        c.httpProxy.address = p.httpProxy->address;
        c.httpProxy.port = static_cast<unsigned int>(p.httpProxy->port);
        c.httpProxy.authentication =
            p.httpProxy->authentication.value_or(c.httpProxy.authentication);
        c.httpProxy.user = p.httpProxy->user.value_or(c.httpProxy.user);
        c.httpProxy.password = p.httpProxy->password.value_or(c.httpProxy.password);
    }

    c.bypassLauncher = p.bypassLauncher.value_or(c.bypassLauncher);

    if (p.layerServer.has_value()) {
        c.layerServer = stringToLayerServer(*p.layerServer);
    }
}

void patchConfiguration(Configuration& configuration, const Settings& settings) {
    if (settings.configuration.has_value()) {
        configuration.windowConfiguration = *settings.configuration;
        configuration.sgctConfigNameInitialized.clear();
    }
    if (settings.profile.has_value()) {
        configuration.profile = *settings.profile;
    }
    if (settings.visibility.has_value()) {
        configuration.propertyVisibility = *settings.visibility;
    }
    if (settings.bypassLauncher.has_value()) {
        configuration.bypassLauncher = *settings.bypassLauncher;
    }
    if (settings.layerServer.has_value()) {
        configuration.layerServer = *settings.layerServer;
    }
    auto it = configuration.moduleConfigurations.find("GlobeBrowsing");
    // Just in case we have a configuration file that does not specify anything
    // about the globebrowsing module
    if (it == configuration.moduleConfigurations.end()) {
        configuration.moduleConfigurations["GlobeBrowsing"] = ghoul::Dictionary();
    }
    if (settings.mrf.isEnabled.has_value()) {
        configuration.moduleConfigurations["GlobeBrowsing"].setValue(
            "MRFCacheEnabled", *settings.mrf.isEnabled
        );
    }
    if (settings.mrf.location.has_value()) {
        configuration.moduleConfigurations["GlobeBrowsing"].setValue(
            "MRFCacheLocation", *settings.mrf.location
        );
    }
}

documentation::Documentation Configuration::Documentation =
    codegen::doc<Parameters>("core_configuration");

std::filesystem::path findConfiguration(const std::string& filename) {
    std::filesystem::path directory = absPath("${BIN}");

    while (true) {
        std::filesystem::path p = directory / filename;
        if (std::filesystem::exists(p) && std::filesystem::is_regular_file(p)) {
            // We have found the configuration file and can bail out
            return p;
        }

        // Otherwise, we traverse the directory tree up
        const std::filesystem::path nextDirectory = directory.parent_path();

        if (directory == nextDirectory) {
            // We have reached the root of the file system and did not find the file
            throw ghoul::RuntimeError(
                std::format("Could not find configuration file '{}'", filename),
                "ConfigurationManager"
            );
        }
        directory = nextDirectory;
    }
}

Configuration loadConfigurationFromFile(const std::filesystem::path& configurationFile,
                                        const std::filesystem::path& settingsFile,
                                        const glm::ivec2& primaryMonitorResolution)
{
    ghoul_assert(std::filesystem::is_regular_file(configurationFile), "File must exist");

    Configuration result;

    // Injecting the resolution of the primary screen into the Lua state
    const std::string script = std::format(
        "ScreenResolution = {{ x = {}, y = {} }}",
        primaryMonitorResolution.x, primaryMonitorResolution.y
    );
    ghoul::lua::runScript(result.state, script);

    // If there is an initial config helper file, load it into the state
    if (std::filesystem::is_regular_file(absPath(InitialConfigHelper))) {
        ghoul::lua::runScriptFile(result.state, absPath(InitialConfigHelper));
    }

    // Load the configuration file into the state
    ghoul::lua::runScriptFile(result.state, configurationFile);

    // If an override file exist, we want to run it straight after
    const std::filesystem::path override = std::format("{}.override", configurationFile);
    if (std::filesystem::exists(override)) {
        ghoul::lua::runScriptFile(result.state, override);
    }

    parseLuaState(result);

    if (std::filesystem::is_regular_file(settingsFile)) {
        const Settings settings = loadSettings(settingsFile);

        patchConfiguration(result, settings);
    }

    return result;
}

openspace::Configuration::LayerServer stringToLayerServer(std::string_view server) {
    using Server = openspace::Configuration::LayerServer;
    if (server == "All") { return Server::All; }
    else if (server == "NewYork") { return Server::NewYork; }
    else if (server == "Sweden") { return Server::Sweden; }
    else if (server == "Utah") { return Server::Utah; }
    else if (server == "None") { return Server::None; }
    else { throw ghoul::MissingCaseException(); }
}

std::string layerServerToString(openspace::Configuration::LayerServer server) {
    using Server = openspace::Configuration::LayerServer;
    switch (server) {
        case Server::All: return "All";
        case Server::NewYork: return "NewYork";
        case Server::Sweden: return "Sweden";
        case Server::Utah: return "Utah";
        case Server::None: return "None";
        default: throw ghoul::MissingCaseException();
    }
}

} // namespace openspace
