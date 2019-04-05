/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

namespace openspace::configuration {

using namespace documentation;
documentation::Documentation Configuration::Documentation = {
    "OpenSpace Configuration",
    "openspace_configuraion",
    {
        {
            KeySGCTConfig,
            new StringAnnotationVerifier("A valid SGCT configuration file"),
            Optional::No,
            "The SGCT configuration file that determines the window and view frustum "
            "settings that are being used when OpenSpace is started."
        },
        {
            KeyAsset,
            new StringAnnotationVerifier(
                "A valid scene file as described in the Scene documentation"
            ),
            Optional::Yes,
            "The scene description that is used to populate the application after "
            "startup. The scene determines which objects are loaded, the startup "
            "time and other scene-specific settings. More information is provided in "
            "the Scene documentation."
        },
        {
            KeyGlobalCustomizationScripts,
            new StringListVerifier,
            Optional::Yes,
            "This value names a list of scripts that get executed after initialization "
            "of any scene. These scripts can be used for user-specific customization, "
            "such as a global rebinding of keys from the default."
        },
        {
            KeyPaths,
            new StringListVerifier,
            Optional::No,
            "A list of paths that are automatically registered with the file system. "
            "If a key X is used in the table, it is then useable by referencing ${X} "
            "in all other configuration files or scripts."
        },
        {
            KeyFonts,
            new StringListVerifier("Font paths loadable by FreeType"),
            Optional::Yes,
            "A list of all fonts that will automatically be loaded on startup. Each "
            "key-value pair contained in the table will become the name and the file "
            "for a font."
        },
        {
            KeyLogging,
            new TableVerifier({
                {
                    KeyLogDir,
                    new StringVerifier,
                    Optional::Yes,
                    "The directory for logs. Default value is \"${BASE}\""
                },
                {
                    KeyPerformancePrefix,
                    new StringVerifier,
                    Optional::Yes,
                    "A string to prefix PerformanceMeasurement logfiles."
                    "Default value is \"PM-\""
                },
                {
                    KeyLogLevel,
                    new StringInListVerifier(
                        // List from logmanager.cpp::levelFromString
                        { "Trace", "Debug", "Info", "Warning", "Error", "Fatal", "None" }
                    ),
                    Optional::Yes,
                    "The severity of log messages that will be displayed. Only "
                    "messages of the selected level or higher will be displayed. All "
                    "levels below will be silently discarded. The order of "
                    "severities is: Debug < Info < Warning < Error < Fatal < None."
                },
                {
                    KeyImmediateFlush,
                    new BoolVerifier,
                    Optional::Yes,
                    "Determines whether error messages will be displayed immediately "
                    "or if it is acceptable to have a short delay, but being more "
                    "performant. If the delay is allowed ('true'), messages might "
                    "get lost if the application crashes shortly after a message was "
                    "logged."
                },
                {
                    KeyLogs,
                    new TableVerifier({
                        {
                            "*",
                            new ReferencingVerifier("core_logfactory"),
                            Optional::No,
                            "Additional log files"
                        }
                    }),
                    Optional::Yes,
                    "Per default, log messages are written to the console, the "
                    "onscreen text, and (if available) the Visual Studio output "
                    "window. This table can define other logging methods that will "
                    "be used additionally."
                },
                {
                    KeyCapabilitiesVerbosity,
                    new StringInListVerifier(
                        // List from OpenspaceEngine::initialize
                        { "None", "Minimal", "Default", "Full" }
                    ),
                    Optional::Yes,
                    "At startup, a list of system capabilities is created and logged."
                    "This value determines how verbose this listing should be."
                }
            }),
            Optional::Yes,
            "Configurations for the logging of messages that are generated "
            "throughout the code and are useful for debugging potential errors or "
            "other information."
        },
        {
            KeyScriptLog,
            new StringVerifier,
            Optional::Yes,
            "The file that will be created on startup containing the log of all Lua "
            "scripts that are executed in the last session. Any existing file (including "
            "the results from previous runs) will be silently overwritten."
        },
        {
            KeyDocumentation,
            new TableVerifier({
                {
                    KeyLuaDocumentation,
                    new StringVerifier,
                    Optional::Yes,
                    "The filename that will be created on startup containing the "
                    "documentation of available Lua functions that can be executed in "
                    "scene files or per console. Any existing file will be silently "
                    "overwritten."
                },
                {
                    KeyPropertyDocumentation,
                    new StringVerifier,
                    Optional::Yes,
                    "The file that will be created on startup containing a list of all "
                    "properties in the scene. Any existing file will be silently "
                    "overwritten."
                },
                {
                    KeyKeyboardShortcuts,
                    new StringVerifier,
                    Optional::Yes,
                    "The file that will be created on startup containing the list of all "
                    "keyboard bindings with their respective Lua scripts. For each key, "
                    "it mentions which scripts will be executed in the current session."
                },
                {
                    KeyDocumentation,
                    new StringVerifier,
                    Optional::Yes,
                    "The file that will be created on startup containing this "
                    "documentation. Any previous file in this location will be silently "
                    "overwritten."
                },
                {
                    KeyFactoryDocumentation,
                    new StringVerifier,
                    Optional::Yes,
                    "The file that will be created on startup containing the factory "
                    "documentation which shows the different types of objects that can "
                    "be created in the current application configuration. Any previous "
                    "file in this location will be silently overritten."
                },
                {
                    KeyLicenseDocumentation,
                    new StringVerifier,
                    Optional::Yes,
                    "The file that will be created on startup containing the scene "
                    "license information. Any previous file in this location will be "
                    "silently overwritten."
                },
            }),
            Optional::Yes,
            "All documentations that are generated at application startup."
        },
        {
            KeyShutdownCountdown,
            new DoubleGreaterEqualVerifier(0.0),
            Optional::Yes,
            "The countdown that the application will wait between pressing ESC and "
            "actually shutting down. If ESC is pressed again in this time, the "
            "shutdown is aborted."
        },
        {
            KeyPerSceneCache,
            new BoolVerifier,
            Optional::Yes,
            "If this is set to 'true', the name of the scene will be appended to the "
            "cache directory, thus not reusing the same directory. This is useful in "
            "cases where the same instance of OpenSpace is run with multiple scenes, but "
            "the caches should be retained. This value defaults to 'false'."
        },
        {
            KeyOnScreenTextScaling,
            new StringInListVerifier({
                // Values from RenderEngine:updateRenderer
                "window", "framebuffer"
            }),
            Optional::Yes,
            "The method for scaling the onscreen text in the window. As the resolution "
            "of the rendering can be different from the size of the window, the onscreen "
            "text can either be scaled according to the window size ('window'), or the "
            "rendering resolution ('framebuffer'). This value defaults to 'window'."
        },
        {
            KeyRenderingMethod,
            new StringInListVerifier(
                // List from RenderEngine::setRendererFromString
                { "Framebuffer", "ABuffer" }
            ),
            Optional::Yes,
            "The renderer that is use after startup. The renderer 'ABuffer' requires "
            "support for at least OpenGL 4.3"
        },
        {
            KeyDisableRenderingOnMaster,
            new BoolVerifier,
            Optional::Yes,
            "Toggles whether the master in a multi-application setup should be rendering "
            "or just managing the state of the network. This is desired in cases where "
            "the master computer does not have the resources to render a scene."
        },
        {
            KeyGlobalRotation,
            new Vector3Verifier<double>,
            Optional::Yes,
            "Applies a global view rotation. Use this to rotate the position of the "
            "focus node away from the default location on the screen. This setting "
            "persists even when a new focus node is selected. Defined using roll, pitch, "
            "yaw in radians"
        },
        {
            KeyMasterRotation,
            new Vector3Verifier<double>,
            Optional::Yes,
            "Applies a view rotation for only the master node, defined using "
            "roll, pitch yaw in radians. This can be used to compensate the master view "
            "direction for tilted display systems in clustered immersive environments."
        },
        {
            KeyScreenshotUseDate,
            new BoolVerifier,
            Optional::Yes,
            "Toggles whether screenshots generated by OpenSpace contain the date when "
            "the concrete OpenSpace instance was started. This value is enabled by "
            "default, but it is advised to disable this value if rendering sessions of "
            "individual frames pass beyond local midnight."
        },
        {
            KeyHttpProxy,
            new TableVerifier({
                {
                    KeyActivate,
                    new BoolVerifier,
                    Optional::Yes,
                    "Determines whether the proxy is being used"
                },
                {
                    KeyAddress,
                    new StringVerifier,
                    Optional::No,
                    "The address of the http proxy"
                },
                {
                    KeyPort,
                    new IntVerifier,
                    Optional::No,
                    "The port of the http proxy"
                },
                {
                    KeyAuthentication,
                    new StringInListVerifier(
                        { "basic", "ntlm", "digest", "any" }
                    ),
                    Optional::Yes,
                    "The authentication method of the http proxy"
                },
                {
                    KeyUser,
                    new StringVerifier,
                    Optional::Yes,
                    "The user of the http proxy"
                },
                {
                    KeyPassword,
                    new StringVerifier,
                    Optional::Yes,
                    "The password of the http proxy"
                }
            }),
            Optional::Yes,
            "This defines the use for a proxy when fetching data over http."
            "No proxy will be used if this is left out."
        },
        {
            KeyOpenGLDebugContext,
            new TableVerifier({
                {
                    KeyActivate,
                    new BoolVerifier,
                    Optional::No,
                    "Determines whether the OpenGL context should be a debug context"
                },
                {
                    KeySynchronous,
                    new BoolVerifier,
                    Optional::Yes,
                    "Determines whether the OpenGL debug callbacks are performed "
                    "synchronously. If set to <True> the callbacks are in the same "
                    "thread as the context and in the scope of the OpenGL function that "
                    "triggered the message. The default value is <True>."
                },
                {
                    KeyFilterIdentifier,
                    new TableVerifier({{
                        "*",
                        new TableVerifier({
                            {
                                KeyIdentifier,
                                new IntVerifier,
                                Optional::No,
                                "The identifier that is to be filtered"
                            },
                            {
                                KeySource,
                                new StringInListVerifier({
                                    // Taken from ghoul::debugcontext.cpp
                                    "API", "Window System", "Shader Compiler",
                                    "Third Party", "Application", "Other", "Don't care"
                                }),
                                Optional::No,
                                "The source of the identifier to be filtered"
                            },
                            {
                                KeyType,
                                new StringInListVerifier({
                                    // Taken from ghoul::debugcontext.cpp
                                    "Error", "Deprecated", "Undefined", "Portability",
                                    "Performance", "Marker", "Push group", "Pop group",
                                    "Other", "Don't care"
                                }),
                                Optional::No,
                                "The type of the identifier to be filtered"
                            }
                        }),
                        Optional::No,
                        "Individual OpenGL debug message identifiers"
                    }}),
                    Optional::Yes,
                    "A list of OpenGL debug messages identifiers that are filtered"
                },
                {
                    KeyFilterSeverity,
                    new TableVerifier({
                        {
                            "*",
                            new StringInListVerifier(
                                // ghoul::debugcontext.cpp
                                { "High", "Medium", "Low", "Notification" }
                            ),
                            Optional::No
                        }
                    }),
                    Optional::Yes,
                    "A list of severities that can are filtered out"
                }
            }),
            Optional::Yes,
            "Determines the settings for the creation of an OpenGL debug context.",
        },
        {
            KeyCheckOpenGLState,
            new BoolVerifier,
            Optional::Yes,
            "Determines whether the OpenGL state is checked after each OpenGL function "
            "call. This will dramatically slow down the rendering, but will make finding "
            "OpenGL errors easier. This defaults to 'false'."
        },
        {
            KeyLogEachOpenGLCall,
            new BoolVerifier,
            Optional::Yes,
            "Determines whether each OpenGL call that happens should be logged using the "
            "'TRACE' loglevel. This will bring the rendering to a crawl but provides "
            "useful debugging features for the order in which OpenGL calls occur. This "
            "defaults to 'false'."
        },
        {
            KeyUseMultithreadedInitialization,
            new BoolVerifier,
            Optional::Yes,
            "This value determines whether the initialization of the scene graph should "
            "occur multithreaded, that is, whether multiple scene graph nodes should "
            "initialize in parallel. The only use for this value is to disable it for "
            "debugging support."
        },
        {
            KeyLoadingScreen,
            new TableVerifier({
                {
                    KeyShowMessage,
                    new BoolVerifier,
                    Optional::Yes,
                    "If this value is set to 'true', the loading screen will display a "
                    "message information about the current phase the loading is in."
                },
                {
                    KeyShowNodeNames,
                    new BoolVerifier,
                    Optional::Yes,
                    "If this value is set to 'true', the loading screen will display a "
                    "list of all of the nodes with their respective status (created, "
                    "loaded, initialized)."
                },
                {
                    KeyShowProgressbar,
                    new BoolVerifier,
                    Optional::Yes,
                    "If this value is set to 'true', the loading screen will contain a "
                    "progress bar that gives an estimate of the loading progression."
                }
            }),
            Optional::Yes,
            "Values in this table describe the behavior of the loading screen that is "
            "displayed while the scene graph is created and initialized."
        },
        {
            KeyModuleConfigurations,
            new TableVerifier,
            Optional::Yes,
            "Configurations for each module"
        }
    }
};

} // namespace openspace::configuration
