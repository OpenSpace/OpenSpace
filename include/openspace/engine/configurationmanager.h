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
    static const std::string KeyPaths;
    /// The key that stores the location to the cache directory used to store all the
    /// permanent and non-permanent cached files
    static const std::string KeyCache;
    /// The key that stores the main directory for fonts
    static const std::string KeyFonts;
    /// The key that stores the location of the SGCT configuration file that is used on
    /// application launch
    static const std::string KeyConfigSgct;
    /// The part of the key that defines the type
    static const std::string PartType;
    /// The part of the key that defines the file
    static const std::string PartFile;
    /// The key that stores the Lua documentation
    static const std::string KeyLuaDocumentation;
    /// The key that stores the scripting log
    static const std::string KeyScriptLog;
    /// The key that stores the Property documentation
    static const std::string KeyPropertyDocumentation;
    /// The key that stores the keyboard bindings that should be stored
    static const std::string KeyKeyboardShortcuts;
    /// The key that stores the main documentation
    static const std::string KeyDocumentation;
    /// The key that stores the factory documentation values
    static const std::string KeyFactoryDocumentation;
    /// The key that stores the location of the scene file that is initially loaded
    static const std::string KeyConfigScene;
    /// The key that stores the location of the tasks file that is initially loaded
    static const std::string KeyConfigTask;
    /// The key that stores the subdirectory containing a list of all startup scripts to
    /// be executed on application start before the scene file is loaded
    static const std::string KeyStartupScript;
    /// The key that stores the subdirectory containing a list of all settings scripts to
    /// be executed on application start and after the scene file is loaded
    static const std::string KeySettingsScript;
    /// The key that stores the settings for determining log-related settings
    static const std::string KeyLogging;
    /// The key that stores the desired LogLevel for the whole application
    /// \sa ghoul::logging::LogManager
    static const std::string PartLogLevel;
    /// The key that stores whether the log should be immediately flushed after a n
    /// \sa ghoul::logging::LogManager
    static const std::string PartImmediateFlush;
    /// The key that stores a subdirectory with a description for additional
    /// ghoul::logging::Log%s to be created
    /// \sa LogFactory
    static const std::string PartLogs;
    /// The key that stores whether a log should be appended to or should be overwritten
    static const std::string PartAppend;
    /// The key that stores the verbosity (None, Minimal, Default, Full) of the system
    /// capabilities components
    static const std::string PartCapabilitiesVerbosity;
    /// The full key that stores the verbosity of the system capabilities component
    static const std::string KeyCapabilitiesVerbosity;
    /// The key that stores the time (in seconds) that the application will wait before
    /// shutting down after the shutdown call is made
    static const std::string KeyShutdownCountdown;
    /// The key that stores whether the onscreen text should be scaled to the window size
    /// or the window resolution
    static const std::string KeyOnScreenTextScaling;
    /// The key that stores whether the master node should perform rendering just function
    /// as a pure manager
    static const std::string KeyDisableMasterRendering;
    /// The key that stores whether the master node should apply the scene transformation
    static const std::string KeyDisableSceneOnMaster;
    /// The key that sets the request URL that is used to request additional data to be
    /// downloaded
    static const std::string KeyDownloadRequestURL;
    /// The key that stores the switch for enabling/disabling the rendering on a master
    /// computer
    static const std::string KeyRenderingMethod;
    /// The key that determines whether a new cache folder is used for each scene file
    static const std::string KeyPerSceneCache;
    /// The key that stores the http proxy settings for the downloadmanager
    static const std::string KeyHttpProxy;
    /// The key that stores the address of the http proxy
    static const std::string PartHttpProxyAddress;
    /// The key that stores the port of the http proxy
    static const std::string PartHttpProxyPort;
    /// The key that stores the authentication method of the http proxy
    static const std::string PartHttpProxyAuthentication;
    /// The key that stores the username to use for authentication to access the http proxy
    static const std::string PartHttpProxyUser;
    /// The key that stores the password to use for authentication to access the http proxy
    static const std::string PartHttpProxyPassword;


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
