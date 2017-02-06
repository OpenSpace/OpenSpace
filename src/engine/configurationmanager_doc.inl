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

#include <openspace/documentation/verifier.h>

namespace openspace {

Documentation ConfigurationManager::Documentation() {
    using namespace documentation;

    return {
        "OpenSpace Configuration",
        "openspace_configuraion",
        {
        {
            ConfigurationManager::KeyConfigSgct,
            new StringAnnotationVerifier("A valid SGCT configuration file"),
            "The SGCT configuration file that determines the window and view frustum "
            "settings that are being used when OpenSpace is started."
        },
        {
            ConfigurationManager::KeyConfigScene,
            new StringAnnotationVerifier(
                "A valid scene file as described in the Scene documentation"
            ),
            "The scene description that is used to populate the application after "
            "startup. The scene determines which objects are loaded, the startup "
            "time and other scene-specific settings. More information is provided in "
            "the Scene documentation."
        },
        {
            ConfigurationManager::KeyPaths,
            new StringListVerifier,
            "A list of paths that are automatically registered with the file system. "
            "If a key X is used in the table, it is then useable by referencing ${X} "
            "in all other configuration files or scripts.",
            Optional::Yes
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
                        { "Debug", "Info", "Warning", "Error", "Fatal", "None" }
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
                            new TableVerifier({
                                {
                                    ConfigurationManager::PartType,
                                    new StringInListVerifier({
                                    // List from logfactory.cpp::createLog
                                        "text", "html"
                                    }),
                                    "The type of the new log to be generated."
                                },
                                {
                                    ConfigurationManager::PartFile,
                                    new StringVerifier,
                                    "The filename to which the log will be written."
                                },
                                {
                                    ConfigurationManager::PartAppend,
                                    new BoolVerifier,
                                    "Determines whether the file will be cleared at "
                                    "startup or if the contents will be appended to "
                                    "previous runs.",
                                    Optional::Yes
                                }
                            }),
                            "Additional log files",
                            Optional::Yes
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
            new TableVerifier({
                {
                    ConfigurationManager::PartType,
                    new StringInListVerifier(
                        // List from ScriptEngine::writeDocumentation
                        { "text", "html" }
                    ),
                    "The type of documentation that will be written."
                },
                {
                    ConfigurationManager::PartFile,
                    new StringVerifier,
                    "The filename that will be created on startup containing the "
                    "documentation of available Lua functions. Any existing file "
                    "will be silently overwritten."
                }
            }),
            "Descriptions of whether and where to create a documentation file that "
            "describes the available Lua functions that can be executed in scene "
            "files or per console.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyPropertyDocumentation,
            new TableVerifier({
                {
                    ConfigurationManager::PartType,
                    new StringInListVerifier(
                        // List taken from Scene::writePropertyDocumentation
                        { "text", "html" }
                    ),
                    "The type of property documentation file that is created."
                },
                {
                    ConfigurationManager::PartFile,
                    new StringVerifier,
                    "The file that will be created on startup containing a list of "
                    "all properties in the scene. Any existing file will be silently "
                    "overwritten."
                }
            }),
            "Descriptions of whether and where to create a list of all properties "
            "that were created in the current scene.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyScriptLog,
            new TableVerifier({
                {
                    ConfigurationManager::PartType,
                    new StringInListVerifier(
                        // List taken from ScriptEngine::writeLog
                        { "text" }
                        ),
                        "The type of logfile that will be created."
                },
                {
                    ConfigurationManager::PartFile,
                    new StringVerifier,
                    "The file that will be created on startup containing the log of "
                    "all Lua scripts that are executed. Any existing file (including "
                    "the results from previous runs) will be silently overwritten."
                }
            }),
            "Contains a log of all Lua scripts that were executed in the last "
            "session.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyKeyboardShortcuts,
            new TableVerifier({
                {
                    ConfigurationManager::PartType,
                    new StringInListVerifier(
                        // List from InteractionHandler::writeKeyboardDocumentation
                        { "text", "html" }
                    ),
                    "The type of keyboard binding documentation that should be "
                    "written."
                },
                {
                    ConfigurationManager::PartFile,
                    new StringVerifier,
                    "The file that will be created on startup containing the list of "
                    "all keyboard bindings with their respective Lua scripts. Any "
                    "previous file in this location will be silently overritten."
                }
            }),
            "Contains the collection of all keyboard shortcuts that were collected "
            "during startup. For each key, it mentions which scripts will be "
            "executed in the current session.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyDocumentation,
            new TableVerifier({
                {
                    ConfigurationManager::PartType,
                    new StringInListVerifier(
                        // List from DocumentationEngine::writeDocumentation
                        { "text", "html" }
                    ),
                    "The type of documentation that should be written."
                },
                {
                    ConfigurationManager::PartFile,
                    new StringVerifier,
                    "The file that will be created on startup containing this "
                    "documentation. Any previous file in this location will be silently "
                    "overritten."
                }
            }),
            "This defines the location and type of this documentation file.",
            Optional::Yes
        },
        {
            ConfigurationManager::KeyFactoryDocumentation,
            new TableVerifier({
                {
                    ConfigurationManager::PartType,
                    new StringInListVerifier(
                        // List from FactoryManager::writeDocumentation
                        { "text", "html" }
                    ),
                    "The type of documentation that should be written."
                },
                {
                    ConfigurationManager::PartFile,
                    new StringVerifier,
                    "The file that will be created on startup containing the factory "
                    "documentation. Any previous file in this location will be silently "
                    "overritten."
                }
            }),
            "This defines the location and type of the factory documentation file, which "
            "shows the different types of objects that can be created in the current "
            "application configuration.",
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
            }
        }
    };
};


} // namespace openspace