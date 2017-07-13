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
            "The SGCT configuration file that determines the window and view frustum "
            "settings that are being used when OpenSpace is started.",
            Optional::No
        },
        {
            ConfigurationManager::KeyConfigScene,
            new StringAnnotationVerifier(
                "A valid scene file as described in the Scene documentation"
            ),
            "The scene description that is used to populate the application after "
            "startup. The scene determines which objects are loaded, the startup "
            "time and other scene-specific settings. More information is provided in "
            "the Scene documentation.",
            Optional::No
        },
        {
            ConfigurationManager::KeyConfigTask,
            new StringAnnotationVerifier(
                "A valid task file as described in the Task documentation"),
                "The root task to be performed when launching the task runner "
                "applicaition.",
                Optional::Yes
        },
        {
            ConfigurationManager::KeyPaths,
            new StringListVerifier,
            "A list of paths that are automatically registered with the file system. "
            "If a key X is used in the table, it is then useable by referencing ${X} "
            "in all other configuration files or scripts.",
            Optional::No
        },
        {
            ConfigurationManager::KeyPaths + '.' + ConfigurationManager::KeyCache,
            new StringVerifier,
            "The path to be used as a cache folder. If per scene caching is enabled, the "
            "name of the scene will be appended to this folder",
            Optional::No
        },
        {
            ConfigurationManager::KeyFonts,
            new StringListVerifier("Font paths loadable by FreeType"),
            "A list of all fonts that will automatically be loaded on startup. Each "
            "key-value pair contained in the table will become the name and the file "
            "for a font.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyLogging,
            new TableVerifier({
                {
                    ConfigurationManager::PartLogLevel,
                    new StringInListVerifier(
                        // List from logmanager.cpp::levelFromString
                        { "Trace", "Debug", "Info", "Warning", "Error", "Fatal", "None" }
                    ),
                    "The severity of log messages that will be displayed. Only "
                    "messages of the selected level or higher will be displayed. All "
                    "levels below will be silently discarded. The order of "
                    "severities is: Debug < Info < Warning < Error < Fatal < None.",
                    Optional::Yes
                },
                {
                    ConfigurationManager::PartImmediateFlush,
                    new BoolVerifier,
                    "Determines whether error messages will be displayed immediately "
                    "or if it is acceptable to have a short delay, but being more "
                    "performant. If the delay is allowed ('true'), messages might "
                    "get lost if the application crashes shortly after a message was "
                    "logged.",
                    Optional::Yes
                },
                {
                    ConfigurationManager::PartLogs,
                    new TableVerifier({
                        {
                            "*",
                            new ReferencingVerifier("core_logfactory"),
                            "Additional log files"
                        }
                    }),
                    "Per default, log messages are written to the console, the "
                    "onscreen text, and (if available) the Visual Studio output "
                    "window. This table can define other logging methods that will "
                    "be used additionally.",
                    Optional::Yes
                },
                {
                    ConfigurationManager::PartCapabilitiesVerbosity,
                    new StringInListVerifier(
                        // List from OpenspaceEngine::initialize
                        { "None", "Minimal", "Default", "Full" }
                    ),
                    "At startup, a list of system capabilities is created and logged."
                    "This value determines how verbose this listing should be.",
                    Optional::Yes
                }
            }),
            "Configurations for the logging of messages that are generated "
            "throughout the code and are useful for debugging potential errors or "
            "other information.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyLuaDocumentation,
            new StringVerifier,
            "The filename that will be created on startup containing the documentation "
            "of available Lua functions that can be executed in scene files or per "
            "console. Any existing file will be silently overwritten.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyPropertyDocumentation,
            new StringVerifier,
            "The file that will be created on startup containing a list of all "
            "properties in the scene. Any existing file will be silently overwritten.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyScriptLog,
            new StringVerifier,
            "The file that will be created on startup containing the log of all Lua "
            "scripts that are executed in the last session. Any existing file (including "
            "the results from previous runs) will be silently overwritten.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyKeyboardShortcuts,
            new StringVerifier,
            "The file that will be created on startup containing the list of all "
            "keyboard bindings with their respective Lua scripts. For each key, it "
            "mentions which scripts will be executed in the current session.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyDocumentation,
            new StringVerifier,
            "The file that will be created on startup containing this documentation. Any "
            "previous file in this location will be silently overwritten.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyFactoryDocumentation,
            new StringVerifier,
            "The file that will be created on startup containing the factory "
            "documentation which shows the different types of objects that can be "
            "created in the current application configuration. Any previous file in this "
            "location will be silently overritten.",
            Optional::Yes
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
                    "The severity of log messages that will be displayed. Only "
                    "messages of the selected level or higher will be displayed. All "
                    "levels below will be silently discarded. The order of "
                    "severities is: Debug < Info < Warning < Error < Fatal < None.",
                    Optional::Yes
                },
            }),
            "Configurations for the Launcher & syncing application.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyShutdownCountdown,
            new DoubleGreaterEqualVerifier(0.0),
            "The countdown that the application will wait between pressing ESC and "
            "actually shutting down. If ESC is pressed again in this time, the "
            "shutdown is aborted.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyPerSceneCache,
            new BoolVerifier,
            "If this is set to 'true', the name of the scene will be appended to the "
            "cache directory, thus not reusing the same directory. This is useful in "
            "cases where the same instance of OpenSpace is run with multiple scenes, but "
            "the caches should be retained. This value defaults to 'false'.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyOnScreenTextScaling,
            new StringInListVerifier({
                // Values from RenderEngine:updateRenderer
                "window", "framebuffer"
            }),
            "The method for scaling the onscreen text in the window. As the resolution "
            "of the rendering can be different from the size of the window, the onscreen "
            "text can either be scaled according to the window size ('window'), or the "
            "rendering resolution ('framebuffer'). This value defaults to 'window'.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyDownloadRequestURL,
            new OrVerifier(
                new StringVerifier,
                new StringListVerifier
            ),
            "The URL from which files will be downloaded by the Launcher. This can "
            "either be a single URL or a list of possible URLs from which the "
            "Launcher can then choose.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyRenderingMethod,
            new StringInListVerifier(
                // List from RenderEngine::setRendererFromString
                { "Framebuffer", "ABuffer" }
            ),
            "The renderer that is use after startup. The renderer 'ABuffer' requires "
            "support for at least OpenGL 4.3",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyDisableMasterRendering,
            new BoolVerifier,
            "Toggles whether the master in a multi-application setup should be rendering "
            "or just managing the state of the network. This is desired in cases where "
            "the master computer does not have the resources to render a scene.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyDisableSceneOnMaster,
            new BoolVerifier,
            "Toggles whether a potential scene transformation matrix, for example as "
            "specified in an SGCT configuration file, should apply to the master node. "
            "With some configurations, applying such a transformation complicates the "
            "interaction and it is thus desired to disable the transformation. The "
            "default is false.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyHttpProxy,
            new TableVerifier({
                {
                    ConfigurationManager::PartHttpProxyAddress,
                    new StringVerifier,
                    "The address of the http proxy"
                },
                {
                    ConfigurationManager::PartHttpProxyPort,
                    new StringVerifier,
                    "The port of the http proxy"
                },
                {
                    ConfigurationManager::PartHttpProxyAuthentication,
                    new StringInListVerifier(
                        { "basic", "ntlm", "digest", "any" }
                    ),
                    "The authentication method of the http proxy",
                    Optional::Yes
                },
                {
                    ConfigurationManager::PartHttpProxyUser,
                    new StringVerifier,
                    "The user of the http proxy",
                    Optional::Yes
                },
                {
                    ConfigurationManager::PartHttpProxyPassword,
                    new StringVerifier,
                    "The password of the http proxy",
                    Optional::Yes
                }
            }),
            "This defines the use for a proxy when fetching data over http."
            "No proxy will be used if this is left out.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyOpenGLDebugContext,
            new TableVerifier({
                {
                    ConfigurationManager::PartActivate,
                    new BoolVerifier,
                    "Determines whether the OpenGL context should be a debug context",
                    Optional::No
                },
                {
                    ConfigurationManager::PartSynchronous,
                    new BoolVerifier,
                    "Determines whether the OpenGL debug callbacks are performed "
                    "synchronously. If set to <True> the callbacks are in the same thead "
                    "as the context and in the scope of the OpenGL function that "
                    "triggered the message. The default value is <True>.",
                    Optional::Yes
                },
                {
                    ConfigurationManager::PartFilterIdentifier,
                    new TableVerifier({{
                        "*",
                        new TableVerifier({
                            {
                                ConfigurationManager::PartFilterIdentifierIdentifier,
                                new IntVerifier,
                                "The identifier that is to be filtered",
                                Optional::No
                            },
                            {
                                ConfigurationManager::PartFilterIdentifierSource,
                                new StringInListVerifier({
                                    // Taken from ghoul::debugcontext.cpp
                                    "API", "Window System", "Shader Compiler",
                                    "Third Party", "Application", "Other", "Don't care"
                                }),
                                "The source of the identifier to be filtered",
                                Optional::No
                            },
                            {
                                ConfigurationManager::PartFilterIdentifierType,
                                new StringInListVerifier({
                                    // Taken from ghoul::debugcontext.cpp
                                    "Error", "Deprecated", "Undefined", "Portability",
                                    "Performance", "Marker", "Push group", "Pop group",
                                    "Other", "Don't care"
                                }),
                                "The type of the identifier to be filtered"
                            }
                        }),
                        "Individual OpenGL debug message identifiers"
                    }}),
                    "A list of OpenGL debug messages identifiers that are filtered",
                    Optional::Yes
                },
                {
                    ConfigurationManager::PartFilterSeverity,
                    new TableVerifier({
                        {
                            "*",
                            new StringInListVerifier(
                                // ghoul::debugcontext.cpp
                                { "High", "Medium", "Low", "Notification" }
                            )
                        }
                    }),
                    "A list of severities that can are filtered out",
                    Optional::Yes
                }
            }),
            "Determines the settings for the creation of an OpenGL debug context.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyCheckOpenGLState,
            new BoolVerifier,
            "Determines whether the OpenGL state is checked after each OpenGL function "
            "call. This will dramatically slow down the rendering, but will make finding "
            "OpenGL errors easier. This defaults to 'false'.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyLogEachOpenGLCall,
            new BoolVerifier,
            "Determines whether each OpenGL call that happens should be logged using the "
            "'TRACE' loglevel. This will bring the rendering to a crawl but provides "
            "useful debugging features for the order in which OpenGL calls occur. This "
            "defaults to 'false'.",
            Optional::Yes
        }
        }
    };
};


} // namespace openspace
