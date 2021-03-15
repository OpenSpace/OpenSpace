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

#include <openspace/engine/configuration.h>

#include <openspace/documentation/documentation.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/assert.h>
#include <optional>

namespace {
    constexpr const char* BasePathToken = "${BASE}";
    // We can't use ${SCRIPTS} here as that hasn't been defined by this point
    constexpr const char* InitialConfigHelper =
                                               "${BASE}/scripts/configuration_helper.lua";

    // Variable names for the openspace.cfg file
    // These are also used in the _doc include file
    constexpr const char* KeySGCTConfig = "SGCTConfig";
    constexpr const char* KeyAsset = "Asset";
    constexpr const char* KeyProfile = "Profile";
    constexpr const char* KeyGlobalCustomizationScripts = "GlobalCustomizationScripts";
    constexpr const char* KeyPaths = "Paths";
    constexpr const char* KeyFonts = "Fonts";
    constexpr const char* KeyLogging = "Logging";
    // @TODO remove; KeyLogDir is not used
    constexpr const char* KeyLogDir = "LogDir";
    constexpr const char* KeyPerformancePrefix = "PerformancePrefix";
    constexpr const char* KeyLogLevel = "LogLevel";
    constexpr const char* KeyImmediateFlush = "ImmediateFlush";
    constexpr const char* KeyLogs = "Logs";
    constexpr const char* KeyCapabilitiesVerbosity = "CapabilitiesVerbosity";
    constexpr const char* KeyDocumentationPath = "Path";
    constexpr const char* KeyDocumentation = "Documentation";
    constexpr const char* KeyScriptLog = "ScriptLog";
    constexpr const char* KeyShutdownCountdown = "ShutdownCountdown";
    constexpr const char* KeyPerSceneCache = "PerSceneCache";
    constexpr const char* KeyOnScreenTextScaling = "OnScreenTextScaling";
    constexpr const char* KeyRenderingMethod = "RenderingMethod";
    constexpr const char* KeyDisableRenderingOnMaster = "DisableRenderingOnMaster";
    constexpr const char* KeyGlobalRotation = "GlobalRotation";
    constexpr const char* KeyScreenSpaceRotation = "ScreenSpaceRotation";
    constexpr const char* KeyMasterRotation = "MasterRotation";
    constexpr const char* KeyDisableInGameConsole = "DisableInGameConsole";
    constexpr const char* KeyScreenshotUseDate = "ScreenshotUseDate";
    constexpr const char* KeyHttpProxy = "HttpProxy";
    constexpr const char* KeyAddress = "Address";
    constexpr const char* KeyPort = "Port";
    constexpr const char* KeyAuthentication = "Authentication";
    constexpr const char* KeyUser = "User";
    constexpr const char* KeyPassword = "Password";
    constexpr const char* KeyOpenGLDebugContext = "OpenGLDebugContext";
    constexpr const char* KeyActivate = "Activate";
    constexpr const char* KeySynchronous = "Synchronous";
    constexpr const char* KeyFilterIdentifier = "FilterIdentifier";
    constexpr const char* KeyIdentifier = "Identifier";
    constexpr const char* KeySource = "Source";
    constexpr const char* KeyType = "Type";
    constexpr const char* KeyFilterSeverity = "FilterSeverity";
    constexpr const char* KeyCheckOpenGLState = "CheckOpenGLState";
    constexpr const char* KeyLogEachOpenGLCall = "LogEachOpenGLCall";
    constexpr const char* KeyVersionCheckUrl = "VersionCheckUrl";
    constexpr const char* KeyUseMultithreadedInitialization =
                                                         "UseMultithreadedInitialization";
    constexpr const char* KeyLoadingScreen = "LoadingScreen";
    constexpr const char* KeyShowMessage = "ShowMessage";
    constexpr const char* KeyShowNodeNames = "ShowNodeNames";
    constexpr const char* KeyShowProgressbar = "ShowProgressbar";
    constexpr const char* KeyModuleConfigurations = "ModuleConfigurations";

    constexpr const char* KeySgctConfigNameInitialized = "sgctconfiginitializeString";
    constexpr const char* KeyReadOnlyProfiles = "ReadOnlyProfiles";
    constexpr const char* KeyBypassLauncher = "BypassLauncher";

    template <typename T>
    void getValue(ghoul::lua::LuaState& L, const char* name, T& value) {
        using namespace openspace::configuration;

        auto it = std::find_if(
            Configuration::Documentation.entries.begin(),
            Configuration::Documentation.entries.end(),
            [name](const openspace::documentation::DocumentationEntry& e) {
            return e.key == name;
        }
        );

        bool isOptional =
            it != Configuration::Documentation.entries.end()
            ? it->optional :
            true;

        lua_getglobal(L, name);
        if (isOptional && lua_isnil(L, -1)) {
            return;
        }

        if (!isOptional && lua_isnil(L, -1)) {
            openspace::documentation::TestResult testResult = {
                false,
                { {
                    name,
                    openspace::documentation::TestResult::Offense::Reason::MissingKey
                }},
                {}
            };
            throw openspace::documentation::SpecificationError(
                std::move(testResult),
                "Configuration"
            );
        }

        if constexpr (std::is_same_v<T, glm::dvec3>) {
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);
            glm::dvec3 res;
            res.x = d.value<double>("1");
            res.y = d.value<double>("2");
            res.z = d.value<double>("3");
            value = res;
        }
        // NOLINTNEXTLINE
        else if constexpr (std::is_same_v<T, std::vector<std::string>>) {
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

            std::vector<std::string> res;
            for (size_t i = 1; i <= d.size(); ++i) {
                res.push_back(d.value<std::string>(std::to_string(i)));
            }
            value = std::move(res);
        }
        // NOLINTNEXTLINE
        else if constexpr (std::is_same_v<T, std::map<std::string, std::string>>) {
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

            std::map<std::string, std::string> res;
            std::vector<std::string_view> keys = d.keys();
            for (size_t i = 0; i < d.size(); ++i) {
                std::string_view key = keys[i];
                std::string v = d.value<std::string>(key);
                res[std::string(key)] = std::move(v);
            }
            value = std::move(res);
        }
        // NOLINTNEXTLINE
        else if constexpr (std::is_same_v<T, std::map<std::string, ghoul::Dictionary>>) {
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

            std::map<std::string, ghoul::Dictionary> res;
            std::vector<std::string_view> keys = d.keys();
            for (size_t i = 0; i < d.size(); ++i) {
                std::string_view key = keys[i];
                ghoul::Dictionary v = d.value<ghoul::Dictionary>(key);
                res[std::string(key)] = std::move(v);
            }
            value = std::move(res);
        }
        // NOLINTNEXTLINE
        else if constexpr (std::is_same_v<T, Configuration::Logging>) {
            Configuration::Logging& v = static_cast<Configuration::Logging&>(value);
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

            if (d.hasValue<std::string>(KeyLogLevel)) {
                v.level = d.value<std::string>(KeyLogLevel);
            }
            if (d.hasValue<bool>(KeyImmediateFlush)) {
                v.forceImmediateFlush = d.value<bool>(KeyImmediateFlush);
            }
            if (d.hasValue<std::string>(KeyCapabilitiesVerbosity)) {
                v.capabilitiesVerbosity = d.value<std::string>(KeyCapabilitiesVerbosity);
            }

            if (d.hasValue<ghoul::Dictionary>(KeyLogs)) {
                ghoul::Dictionary l = d.value<ghoul::Dictionary>(KeyLogs);
                std::vector<ghoul::Dictionary> res;
                for (size_t i = 1; i <= l.size(); ++i) {
                    res.push_back(l.value<ghoul::Dictionary>(std::to_string(i)));
                }
                v.logs = res;
            }
        }
        // NOLINTNEXTLINE
        else if constexpr (std::is_same_v<T, Configuration::DocumentationInfo>) {
            Configuration::DocumentationInfo& v =
                static_cast<Configuration::DocumentationInfo&>(value);
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);
            if (d.hasValue<std::string>(KeyDocumentationPath)) {
                v.path = d.value<std::string>(KeyDocumentationPath);
            }
        }
        // NOLINTNEXTLINE
        else if constexpr (std::is_same_v<T, Configuration::LoadingScreen>) {
            Configuration::LoadingScreen& v =
                static_cast<Configuration::LoadingScreen&>(value);
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);
            if (d.hasValue<bool>(KeyShowMessage)) {
                v.isShowingMessages = d.value<bool>(KeyShowMessage);
            }
            if (d.hasValue<bool>(KeyShowNodeNames)) {
                v.isShowingNodeNames = d.value<bool>(KeyShowNodeNames);
            }
            if (d.hasValue<bool>(KeyShowProgressbar)) {
                v.isShowingProgressbar = d.value<bool>(KeyShowProgressbar);
            }
        }
        // NOLINTNEXTLINE
        else if constexpr (std::is_same_v<T, Configuration::OpenGLDebugContext>) {
            Configuration::OpenGLDebugContext& v =
                static_cast<Configuration::OpenGLDebugContext&>(value);
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

            if (d.hasValue<bool>(KeyActivate)) {
                v.isActive = d.value<bool>(KeyActivate);
            }
            if (d.hasValue<bool>(KeySynchronous)) {
                v.isSynchronous = d.value<bool>(KeySynchronous);
            }

            if (d.hasValue<ghoul::Dictionary>(KeyFilterIdentifier)) {
                ghoul::Dictionary f = d.value<ghoul::Dictionary>(KeyFilterIdentifier);

                std::vector<Configuration::OpenGLDebugContext::IdentifierFilter> res;
                for (size_t i = 1; i <= f.size(); ++i) {
                    Configuration::OpenGLDebugContext::IdentifierFilter filter;
                    ghoul::Dictionary fi = f.value<ghoul::Dictionary>(std::to_string(i));

                    if (fi.hasValue<double>(KeyIdentifier)) {
                        filter.identifier = static_cast<unsigned int>(
                            fi.value<double>(KeyIdentifier)
                        );
                    }
                    if (fi.hasValue<std::string>(KeySource)) {
                        filter.source = fi.value<std::string>(KeySource);
                    }
                    if (fi.hasValue<std::string>(KeyType)) {
                        filter.type = fi.value<std::string>(KeyType);
                    }

                    res.push_back(std::move(filter));
                }

                v.identifierFilters = res;
            }

            if (d.hasValue<ghoul::Dictionary>(KeyFilterSeverity)) {
                ghoul::Dictionary f = d.value<ghoul::Dictionary>(KeyFilterSeverity);

                std::vector<std::string> res;
                for (size_t i = 1; i <= f.size(); ++i) {
                    res.push_back(f.value<std::string>(std::to_string(i)));
                }
                v.severityFilters = res;
            }
        }
        // NOLINTNEXTLINE
        else if constexpr (std::is_same_v<T, Configuration::HTTPProxy>) {
            Configuration::HTTPProxy& v = static_cast<Configuration::HTTPProxy&>(value);
            ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

            if (d.hasValue<bool>(KeyActivate)) {
                v.usingHttpProxy = d.value<bool>(KeyActivate);
            }
            if (d.hasValue<std::string>(KeyAddress)) {
                v.address = d.value<std::string>(KeyAddress);
            }
            if (d.hasValue<double>(KeyPort)) {
                v.port = static_cast<unsigned int>(d.value<double>(KeyPort));
            }
            if (d.hasValue<std::string>(KeyAuthentication)) {
                v.authentication = d.value<std::string>(KeyAuthentication);
            }
            if (d.hasValue<std::string>(KeyUser)) {
                v.user = d.value<std::string>(KeyUser);
            }
            if (d.hasValue<std::string>(KeyPassword)) {
                v.password = d.value<std::string>(KeyPassword);
            }
        }
        else {
            value = ghoul::lua::value<T>(L);
        }
    }

    struct [[codegen::Dictionary(Configuration)]] Parameters {
        // The SGCT configuration file that determines the window and view frustum
        // settings that are being used when OpenSpace is started
        std::optional<std::string> sgctConfig [[codegen::key("SGCTConfig")]];

        // The SGCT configuration can be defined from an .xml file, or auto-generated
        // by an sgct.config.* lua function. If a lua function is used to generate the
        // SGCT configuration, then this key contains the name of the function, otherwise
        // is blank
        std::optional<std::string> sgctConfigString
            [[codegen::key("sgctconfiginitializeString")]];

        // The scene description that is used to populate the application after startup.
        // The scene determines which objects are loaded, the startup time and other
        // scene-specific settings. More information is provided in the Scene
        // documentation
        std::optional<std::string> asset;

        // This value names a list of scripts that get executed after initialization of
        // any scene. These scripts can be used for user-specific customization, such as a
        // global rebinding of keys from the default
        std::optional<std::vector<std::string>> globalCustomizationScripts;

        // A list of paths that are automatically registered with the file system. If a
        // key X is used in the table, it is then useable by referencing ${X} in all other 
        // configuration files or scripts
        std::map<std::string, std::string> paths;

        // A list of all fonts that will automatically be loaded on startup. Each
        // key-value pair contained in the table will become the name and the file for a
        // font
        std::optional<std::map<std::string, std::string>> fonts;

        struct Logging {
            // The directory for logs. Default value is \"${BASE}\"
            std::optional<std::string> logDir;

            // A string to prefix PerformanceMeasurement logfiles. Default value is
            // \"PM-\"
            std::optional<std::string> performancePrefix;

            // List from logmanager.cpp::levelFromString
            enum class Level {
                Trace,
                Debug,
                Info,
                Warning,
                Error,
                Fatal,
                None
            };
            // The severity of log messages that will be displayed. Only messages of the
            // selected level or higher will be displayed. All levels below will be
            // silently discarded. The order of severities is:
            // Debug < Info < Warning < Error < Fatal < None.
            std::optional<Level> logLevel;

            // Determines whether error messages will be displayed immediately or if it is
            // acceptable to have a short delay, but being more performant. If the delay
            // is allowed ('true'), messages might get lost if the application crashes
            // shortly after a message was logged
            std::optional<bool> immediateFlush;

            // Per default, log messages are written to the console, the onscreen text,
            // and (if available) the Visual Studio output window. This table can define
            // other logging methods that will be used additionally
            std::optional<std::vector<std::monostate>> logs
                [[codegen::reference("core_logfactory")]];

            // List from OpenspaceEngine::initialize
            enum class Verbosity {
                None,
                Minimal,
                Default,
                Full
            };
            // At startup, a list of system capabilities is created and logged. This value
            // determines how verbose this listing should be
            std::optional<Verbosity> capabilitiesVerbosity;
        };
        // Configurations for the logging of messages that are generated throughout the
        // code and are useful for debugging potential errors or other information
        std::optional<Logging> logging;

        // The file that will be created on startup containing the log of all Lua scripts
        // that are executed in the last session. Any existing file (including the results
        // from previous runs) will be silently overwritten
        std::optional<std::string> scriptLog;

        struct Documentation {
            // The path where the documentation files will be stored
            std::optional<std::string> path;
        };
        // Right now only contains the path where the documentation is written to
        std::optional<Documentation> documentation;

        // The countdown that the application will wait between pressing ESC and actually
        // shutting down. If ESC is pressed again in this time, the shutdown is aborted
        std::optional<double> shutdownCountdown [[codegen::greater(0.0)]];

        // If this is set to 'true', the name of the scene will be appended to the cache
        // directory, thus not reusing the same directory. This is useful in cases where
        // the same instance of OpenSpace is run with multiple scenes, but the caches
        // should be retained. This value defaults to 'false'
        std::optional<bool> perSceneCache;

        enum class Scaling {
            Window [[codegen::key("window")]],
            FrameBuffer [[codegen::key("framebuffer")]]
        };
        // The method for scaling the onscreen text in the window. As the resolution of
        // the rendering can be different from the size of the window, the onscreen text
        // can either be scaled according to the window size ('window'), or the rendering
        // resolution ('framebuffer'). This value defaults to 'window'
        std::optional<Scaling> onScreenTextScaling;

        // List from RenderEngine::setRendererFromString
        enum class RenderingMethod {
            Framebuffer,
            ABuffer
        };
        // The renderer that is use after startup. The renderer 'ABuffer' requires support
        // for at least OpenGL 4.3
        std::optional<RenderingMethod> renderingMethod;

        // Toggles whether the master in a multi-application setup should be rendering or
        // just managing the state of the network. This is desired in cases where the
        // master computer does not have the resources to render a scene
        std::optional<bool> disableRenderingOnMaster;

        // Applies a global view rotation. Use this to rotate the position of the focus
        // node away from the default location on the screen. This setting persists even
        // when a new focus node is selected. Defined using roll, pitch, yaw in radians
        std::optional<glm::dvec3> globalRotation;

        // Applies a view rotation for only the master node, defined using roll, pitch yaw
        // in radians. This can be used to compensate the master view direction for tilted
        // display systems in clustered immersive environments
        std::optional<glm::dvec3> masterRotation;

        // Applies a global rotation for all screenspace renderables. Defined using roll,
        // pitch, yaw in radians
        std::optional<glm::dvec3> screenSpaceRotation;

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
            int port;

            enum class Authentication {
                Basic [[codegen::key("basic")]],
                Ntlm [[codegen::key("ntlm")]],
                Digest [[codegen::key("digest")]],
                Any [[codegen::key("any")]]
            };
            // The authentication method of the http proxy
            std::optional<Authentication> authentication;

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

            // Determines whether the OpenGL debug callbacks are performed synchronously.
            // If set to 'true' the callbacks are in the same thread as the context and in
            // the scope of the OpenGL function that triggered the message. The default
            // value is 'true'
            std::optional<bool> synchronous;

            // Individual OpenGL debug message identifiers
            struct Filter {
                // The identifier that is to be filtered
                int identifier;

                // Taken from ghoul::debugcontext.cpp
                enum class Source {
                    API,
                    WindowSystem [[codegen::key("Window System")]],
                    ShaderCompiler [[codegen::key("Shader Compiler")]],
                    ThirdParty [[codegen::key("Third Party")]],
                    Application,
                    Other,
                    DontCare [[codegen::key("Don't care")]]
                };
                // The source of the identifier to be filtered
                Source source;

                // Taken from ghoul::debugcontext.cpp
                enum class Type {
                    Error,
                    Deprecated,
                    Undefined,
                    Portability,
                    Performance,
                    Marker,
                    PushGroup [[codegen::key("Push group")]],
                    PopGroup [[codegen::key("Pop group")]],
                    Other,
                    DontCare [[codegen::key("Don't care")]]
                };
                // The type of the identifier to be filtered
                Type type;
            };
            // A list of OpenGL debug messages identifiers that are filtered
            std::optional<std::vector<Filter>> filterIdentifier;

            // A list of severities that can are filtered out
            enum class Severity {
                High,
                Medium,
                Low,
                Notification
            };
            // Determines the settings for the creation of an OpenGL debug context
            std::optional<std::vector<Severity>> filterSeverity;
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

        // This value determines whether the initialization of the scene graph should
        // occur multithreaded, that is, whether multiple scene graph nodes should
        // initialize in parallel. The only use for this value is to disable it for
        // debugging support
        std::optional<bool> useMultithreadedInitialization;

        // If this value is set to 'true', the launcher will not be shown and OpenSpace
        // will start with the provided configuration options directly. Useful in
        // multiprojector setups where a launcher window would be undesired
        std::optional<bool> bypassLauncher;

        // The URL that is pinged to check which version of OpenSpace is the most current
        // if you don't want this request to happen, this value should not be set at all
        std::optional<std::string> versionCheckUrl [[codegen::key("VersionCheckUrl")]];

        struct LoadingScreen {
            // If this value is set to 'true', the loading screen will display a message
            // information about the current phase the loading is in
            std::optional<bool> showMessage;

            // If this value is set to 'true', the loading screen will display a list of
            // all of the nodes with their respective status (created, loaded,
            // initialized)
            std::optional<bool> showNodeNames;

            // If this value is set to 'true', the loading screen will contain a progress
            // bar that gives an estimate of the loading progression
            std::optional<bool> showProgressbar;
        };
        // Values in this table describe the behavior of the loading screen that is 
        // displayed while the scene graph is created and initialized
        std::optional<LoadingScreen> loadingScreen;

        // List of profiles that cannot be overwritten by user
        std::optional<std::vector<std::string>> readOnlyProfiles;
    };

    //// Configurations for each module
    //std::optional<std::map<std::string, ghoul::Dictionary>> moduleConfigurations;

#include "configuration_codegen.cpp"
} // namespace

#include "configuration_doc.inl"

namespace openspace::configuration {

void parseLuaState(Configuration& configuration) {
    using namespace ghoul::lua;

    // Shorten the rest of this function
    Configuration& c = configuration;
    LuaState& s = c.state;

    // The configuration file sets all values as global variables, so we need to pull them
    // into a table first so that we can pass that table to the dictionary constructor
    lua_newtable(s);

    // We go through all of the entries and lift them from global scope into the stack
    documentation::Documentation doc = codegen::doc<Parameters>();
    for (const documentation::DocumentationEntry& e : doc.entries) {
        lua_pushstring(s, e.key.c_str());
        lua_getglobal(s, e.key.c_str());
        lua_settable(s, -3);
    }
    // @TODO (abock, 2020-03-16)  We are naughty about the module configuration right now,
    // so we need to handle this separately
    lua_pushstring(s, "ModuleConfigurations");
    lua_getglobal(s, "ModuleConfigurations");
    lua_settable(s, -3);


    ghoul::Dictionary d;
    ghoul::lua::luaDictionaryFromState(s, d);
    lua_settop(s, 0);
    Parameters p = codegen::bake<Parameters>(d);


    getValue(s, KeySGCTConfig, c.windowConfiguration);
    getValue(s, KeyAsset, c.asset);
    getValue(s, KeyProfile, c.profile);
    getValue(s, KeyGlobalCustomizationScripts, c.globalCustomizationScripts);
    getValue(s, KeyPaths, c.pathTokens);
    getValue(s, KeyFonts, c.fonts);
    getValue(s, KeyScriptLog, c.scriptLog);
    getValue(s, KeyVersionCheckUrl, c.versionCheckUrl);
    getValue(s, KeyUseMultithreadedInitialization, c.useMultithreadedInitialization);
    getValue(s, KeyCheckOpenGLState, c.isCheckingOpenGLState);
    getValue(s, KeyLogEachOpenGLCall, c.isLoggingOpenGLCalls);
    getValue(s, KeyShutdownCountdown, c.shutdownCountdown);
    getValue(s, KeyScreenshotUseDate, c.shouldUseScreenshotDate);
    getValue(s, KeyOnScreenTextScaling, c.onScreenTextScaling);
    getValue(s, KeyPerSceneCache, c.usePerSceneCache);
    getValue(s, KeyDisableRenderingOnMaster, c.isRenderingOnMasterDisabled);

    getValue(s, KeyGlobalRotation, c.globalRotation);
    getValue(s, KeyScreenSpaceRotation, c.screenSpaceRotation);
    getValue(s, KeyMasterRotation, c.masterRotation);
    getValue(s, KeyDisableInGameConsole, c.isConsoleDisabled);
    getValue(s, KeyRenderingMethod, c.renderingMethod);
    getValue(s, KeyLogging, c.logging);
    getValue(s, KeyDocumentation, c.documentation);
    getValue(s, KeyLoadingScreen, c.loadingScreen);
    getValue(s, KeyModuleConfigurations, c.moduleConfigurations);
    getValue(s, KeyOpenGLDebugContext, c.openGLDebugContext);
    getValue(s, KeyHttpProxy, c.httpProxy);

    getValue(s, KeyReadOnlyProfiles, c.readOnlyProfiles);
    getValue(s, KeyBypassLauncher, c.bypassLauncher);
}

std::string findConfiguration(const std::string& filename) {
    using ghoul::filesystem::Directory;

    Directory directory = FileSys.absolutePath("${BIN}");

    while (true) {
        std::string fullPath = FileSys.pathByAppendingComponent(
            directory,
            filename
        );

        if (FileSys.fileExists(fullPath)) {
            // We have found the configuration file and can bail out
            return fullPath;
        }

        // Otherwise, we traverse the directory tree up
        Directory nextDirectory = directory.parentDirectory(
            ghoul::filesystem::Directory::AbsolutePath::Yes
        );

        if (directory.path() == nextDirectory.path()) {
            // We have reached the root of the file system and did not find the file
            throw ghoul::RuntimeError(
                "Could not find configuration file '" + filename + "'",
                "ConfigurationManager"
            );
        }
        directory = nextDirectory;
    }
}

Configuration loadConfigurationFromFile(const std::string& filename,
                                        const std::string& overrideScript)
{
    ghoul_assert(!filename.empty(), "Filename must not be empty");
    ghoul_assert(FileSys.fileExists(filename), "File must exist");

    Configuration result;

    // Register the base path as the directory where 'filename' lives
    std::string basePath = ghoul::filesystem::File(filename).directoryName();
    FileSys.registerPathToken(BasePathToken, basePath);

    // If there is an initial config helper file, load it into the state
    if (FileSys.fileExists(absPath(InitialConfigHelper))) {
        ghoul::lua::runScriptFile(result.state, absPath(InitialConfigHelper));
    }

    // Load the configuration file into the state
    ghoul::lua::runScriptFile(result.state, filename);

    if (!overrideScript.empty()) {
        LDEBUGC("Configuration", "Executing Lua script passed through the commandline:");
        LDEBUGC("Configuration", overrideScript);
        ghoul::lua::runScript(result.state, overrideScript);
    }

    parseLuaState(result);

    return result;
}

} // namespace openspace::configuration
