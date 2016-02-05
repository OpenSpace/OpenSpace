/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __CONFIGURATIONMANAGER_H__
#define __CONFIGURATIONMANAGER_H__

#include <ghoul/misc/dictionary.h>

namespace openspace {

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
    /// The key that stores the type of Lua documentation that should be stored
    static const std::string KeyLuaDocumentationType;
    /// The key that stores the save location of the Lua documentation
    static const std::string KeyLuaDocumentationFile;
    /// The key that stores the type of Property documentation that should be stored
    static const std::string KeyPropertyDocumentationType;
    /// The key that stores the save location of the Property documentation
    static const std::string KeyPropertyDocumentationFile;
    /// The key that stores the location of the scene file that is initially loaded
    static const std::string KeyConfigScene;
    /// The key that stores the subdirectory containing a list of all startup scripts to
    /// be executed on application start before the scene file is loaded
    static const std::string KeyStartupScript;
    /// The key that stores the subdirectory containing a list of all settings scripts to
    /// be executed on application start and after the scene file is loaded
    static const std::string KeySettingsScript;
    /// The key that stores the location of the SPICE time kernel to be loaded on
    /// application start
    static const std::string KeySpiceTimeKernel;
    /// The key that stores the location of the SPICE leapsecond kernel to be loaded on
    /// application start
    static const std::string KeySpiceLeapsecondKernel;
    /// The key that stores the desired LogLevel for the whole application
    /// \sa ghoul::logging::LogManager
    static const std::string KeyLogLevel;
    /// The key that stores whether the log should be immediately flushed after a n
    /// \sa ghoul::logging::LogManager
    static const std::string KeyLogImmediateFlush;
    /// The key that stores a subdirectory with a description for additional
    /// ghoul::logging::Log%s to be created
    /// \sa LogFactory
    static const std::string KeyLogs;
    /// The key that stores the verbosity (None, Minimal, Default, Full) of the system
    /// capabilities components
    static const std::string KeyCapabilitiesVerbosity;
    /// The key that stores whether the master node should perform rendering just function
    /// as a pure manager
    static const std::string KeyDisableMasterRendering;
    /// The key that sets the request URL that is used to request additional data to be
    /// downloaded
    static const std::string KeyDownloadRequestURL;

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
     * \throw ghoul::FileNotFoundException If the \p filename did not exist
     * \throw ghoul::RuntimeError If the configuration file was not complete (i.e., did
     * not specify the necessary keys KeyPaths, KeyPaths.KeyCache, KeyFonts, and
     * KeyConfigSgct)
     * \throw ghoul::lua::LuaRuntimeException If there was Lua-based error loading the
     * configuration file
     * \pre \p filename must not be empty
     */
	void loadFromFile(const std::string& filename);

private:
    /**
     * Checks whether the loaded configuration file is complete, that is specifying the
     * necessary keys KeyPaths, KeyPaths.KeyCache, KeyFonts, and KeyConfigSgct. The method
     * will log fatal errors if a key is missing.
     * \return <code>true</code> if the configuration file was complete;
     * <code>false</code> otherwise
     */
	bool checkCompleteness() const;
};

} // namespace openspace

#endif  // __CONFIGURATIONMANAGER_H__
