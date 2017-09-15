/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

namespace openspace {

documentation::Documentation ConfigurationManager::Documentation() {
    using namespace documentation;

    return {
        "OpenSpace Configuration",
        "openspace_configuraion",
        {
        {
            ConfigurationManager::KeyConfigSgct,
            new StringAnnotationVerifier("A valid SGCT configuration file"),
            Optional::No,
            "The SGCT configuration file that determines the window and view frustum "
            "settings that are being used when OpenSpace is started."
        },
        {
            ConfigurationManager::KeyConfigAsset,
            new StringAnnotationVerifier(
                "A valid scene file as described in the Scene documentation"
            ),
            Optional::No,
            "The scene description that is used to populate the application after "
            "startup. The scene determines which objects are loaded, the startup "
            "time and other scene-specific settings. More information is provided in "
            "the Scene documentation."
        },
        {
            ConfigurationManager::KeyConfigTask,
            new StringAnnotationVerifier(
                "A valid task file as described in the Task documentation"
            ),
            Optional::Yes,
            "The root task to be performed when launching the task runner application."
        },
        {
            ConfigurationManager::KeyPaths,
            new StringListVerifier,
            Optional::No,
            "A list of paths that are automatically registered with the file system. "
            "If a key X is used in the table, it is then useable by referencing ${X} "
            "in all other configuration files or scripts."
        },
        {
            ConfigurationManager::KeyPaths + '.' + ConfigurationManager::KeyCache,
            new StringVerifier,
            Optional::No,
            "The path to be used as a cache folder. If per scene caching is enabled, the "
            "name of the scene will be appended to this folder"
        },
        {
            ConfigurationManager::KeyFonts,
            new StringListVerifier("Font paths loadable by FreeType"),
            Optional::Yes,
            "A list of all fonts that will automatically be loaded on startup. Each "
            "key-value pair contained in the table will become the name and the file "
            "for a font."
        },
        {
            ConfigurationManager::KeyLogging,
            new TableVerifier({
                {
                    ConfigurationManager::PartLogDir,
                    new StringVerifier,
                    Optional::Yes,
                    "The directory for logs. Default value is \"${BASE_PATH}\""
                },
                {
                    ConfigurationManager::PartLogPerformancePrefix,
                    new StringVerifier,
                    Optional::Yes,
                    "A string to prefix PerformanceMeasurement logfiles."
                    "Default value is \"PM-\""
                },
                {
                    ConfigurationManager::PartLogLevel,
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
                    ConfigurationManager::PartImmediateFlush,
                    new BoolVerifier,
                    Optional::Yes,
                    "Determines whether error messages will be displayed immediately "
                    "or if it is acceptable to have a short delay, but being more "
                    "performant. If the delay is allowed ('true'), messages might "
                    "get lost if the application crashes shortly after a message was "
                    "logged."
                },
                {
                    ConfigurationManager::PartLogs,
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
                    ConfigurationManager::PartCapabilitiesVerbosity,
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
            ConfigurationManager::KeyLuaDocumentation,
            new StringVerifier,
            Optional::Yes,
            "The filename that will be created on startup containing the documentation "
            "of available Lua functions that can be executed in scene files or per "
            "console. Any existing file will be silently overwritten."
        },
        {
            ConfigurationManager::KeyPropertyDocumentation,
            new StringVerifier,
            Optional::Yes,
            "The file that will be created on startup containing a list of all "
            "properties in the scene. Any existing file will be silently overwritten."
        },
        {
            ConfigurationManager::KeyScriptLog,
            new StringVerifier,
            Optional::Yes,
            "The file that will be created on startup containing the log of all Lua "
            "scripts that are executed in the last session. Any existing file (including "
            "the results from previous runs) will be silently overwritten."
        },
        {
            ConfigurationManager::KeyKeyboardShortcuts,
            new StringVerifier,
            Optional::Yes,
            "The file that will be created on startup containing the list of all "
            "keyboard bindings with their respective Lua scripts. For each key, it "
            "mentions which scripts will be executed in the current session."
        },
        {
            ConfigurationManager::KeyDocumentation,
            new StringVerifier,
            Optional::Yes,
            "The file that will be created on startup containing this documentation. Any "
            "previous file in this location will be silently overwritten."
        },
        {
            ConfigurationManager::KeyFactoryDocumentation,
            new StringVerifier,
            Optional::Yes,
            "The file that will be created on startup containing the factory "
            "documentation which shows the different types of objects that can be "
            "created in the current application configuration. Any previous file in this "
            "location will be silently overritten."
        },
        {
            ConfigurationManager::KeyLauncher,
            new TableVerifier({
                {
                    ConfigurationManager::PartLogLevel,
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
            }),
            Optional::Yes,
            "Configurations for the Launcher & syncing application."
        },
        {
            ConfigurationManager::KeyShutdownCountdown,
            new DoubleGreaterEqualVerifier(0.0),
            Optional::Yes,
            "The countdown that the application will wait between pressing ESC and "
            "actually shutting down. If ESC is pressed again in this time, the "
            "shutdown is aborted."
        },
        {
            ConfigurationManager::KeyPerSceneCache,
            new BoolVerifier,
            Optional::Yes,
            "If this is set to 'true', the name of the scene will be appended to the "
            "cache directory, thus not reusing the same directory. This is useful in "
            "cases where the same instance of OpenSpace is run with multiple scenes, but "
            "the caches should be retained. This value defaults to 'false'."
        },
        {
            ConfigurationManager::KeyOnScreenTextScaling,
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
            ConfigurationManager::KeyDownloadRequestURL,
            new OrVerifier(
                new StringVerifier,
                new StringListVerifier
            ),
            Optional::Yes,
            "The URL from which files will be downloaded by the Launcher. This can "
            "either be a single URL or a list of possible URLs from which the "
            "Launcher can then choose."
        },
        {
            ConfigurationManager::KeyRenderingMethod,
            new StringInListVerifier(
                // List from RenderEngine::setRendererFromString
                { "Framebuffer", "ABuffer" }
            ),
            Optional::Yes,
            "The renderer that is use after startup. The renderer 'ABuffer' requires "
            "support for at least OpenGL 4.3"
        },
        {
            ConfigurationManager::KeyDisableMasterRendering,
            new BoolVerifier,
            Optional::Yes,
            "Toggles whether the master in a multi-application setup should be rendering "
            "or just managing the state of the network. This is desired in cases where "
            "the master computer does not have the resources to render a scene."
        },
        {
            ConfigurationManager::KeyDisableSceneOnMaster,
            new BoolVerifier,
            Optional::Yes,
            "Toggles whether a potential scene transformation matrix, for example as "
            "specified in an SGCT configuration file, should apply to the master node. "
            "With some configurations, applying such a transformation complicates the "
            "interaction and it is thus desired to disable the transformation. The "
            "default is false."
        },
        {
            ConfigurationManager::KeyHttpProxy,
            new TableVerifier({
                {
                    ConfigurationManager::PartHttpProxyAddress,
                    new StringVerifier,
                    Optional::No,
                    "The address of the http proxy"
                },
                {
                    ConfigurationManager::PartHttpProxyPort,
                    new StringVerifier,
                    Optional::No,
                    "The port of the http proxy"
                },
                {
                    ConfigurationManager::PartHttpProxyAuthentication,
                    new StringInListVerifier(
                        { "basic", "ntlm", "digest", "any" }
                    ),
                    Optional::Yes,
                    "The authentication method of the http proxy"
                },
                {
                    ConfigurationManager::PartHttpProxyUser,
                    new StringVerifier,
                    Optional::Yes,
                    "The user of the http proxy"
                },
                {
                    ConfigurationManager::PartHttpProxyPassword,
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
            ConfigurationManager::KeyOpenGLDebugContext,
            new TableVerifier({
                {
                    ConfigurationManager::PartActivate,
                    new BoolVerifier,
                    Optional::No,
                    "Determines whether the OpenGL context should be a debug context"
                },
                {
                    ConfigurationManager::PartSynchronous,
                    new BoolVerifier,
                    Optional::Yes,
                    "Determines whether the OpenGL debug callbacks are performed "
                    "synchronously. If set to <True> the callbacks are in the same thead "
                    "as the context and in the scope of the OpenGL function that "
                    "triggered the message. The default value is <True>."
                },
                {
                    ConfigurationManager::PartFilterIdentifier,
                    new TableVerifier({{
                        "*",
                        new TableVerifier({
                            {
                                ConfigurationManager::PartFilterIdentifierIdentifier,
                                new IntVerifier,
                                Optional::No,
                                "The identifier that is to be filtered"
                            },
                            {
                                ConfigurationManager::PartFilterIdentifierSource,
                                new StringInListVerifier({
                                    // Taken from ghoul::debugcontext.cpp
                                    "API", "Window System", "Shader Compiler",
                                    "Third Party", "Application", "Other", "Don't care"
                                }),
                                Optional::No,
                                "The source of the identifier to be filtered"
                            },
                            {
                                ConfigurationManager::PartFilterIdentifierType,
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
                    ConfigurationManager::PartFilterSeverity,
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
            ConfigurationManager::KeyCheckOpenGLState,
            new BoolVerifier,
            Optional::Yes,
            "Determines whether the OpenGL state is checked after each OpenGL function "
            "call. This will dramatically slow down the rendering, but will make finding "
            "OpenGL errors easier. This defaults to 'false'."
        },
        {
            ConfigurationManager::KeyLogEachOpenGLCall,
            new BoolVerifier,
            Optional::Yes,
            "Determines whether each OpenGL call that happens should be logged using the "
            "'TRACE' loglevel. This will bring the rendering to a crawl but provides "
            "useful debugging features for the order in which OpenGL calls occur. This "
            "defaults to 'false'."
        }
        }
    };
}


} // namespace openspace
