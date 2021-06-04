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

#include <ghoul/filesystem/file.h>
#include <filesystem>

#include <ghoul/ext/assimp/contrib/zip/src/zip.h>

namespace openspace::luascriptfunctions {

int printInternal(ghoul::logging::LogLevel level, lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::printInternal");

    using ghoul::lua::luaTypeToString;

    const int type = lua_type(L, 1);
    switch (type) {
        case LUA_TNONE:
        case LUA_TLIGHTUSERDATA:
        case LUA_TTABLE:
        case LUA_TFUNCTION:
        case LUA_TUSERDATA:
        case LUA_TTHREAD:
            log(
                level,
                "print",
                fmt::format("Function parameter was of type '{}'", luaTypeToString(type))
            );
            break;
        case LUA_TNIL:
            break;
        case LUA_TBOOLEAN:
            log(level, "print", std::to_string(ghoul::lua::value<bool>(L, 1)));
            break;
        case LUA_TNUMBER:
            log(level, "print", std::to_string(ghoul::lua::value<double>(L, 1)));
            break;
        case LUA_TSTRING:
            log(level, "print", ghoul::lua::value<std::string>(L, 1));
            break;
    }
    lua_pop(L, 1);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
 * \ingroup LuaScripts
 * printTrace(*):
 * Logs the passed value to the installed LogManager with a LogLevel of 'Trace'.
 * For Boolean, numbers, and strings, the internal values are printed, for all other
 * types, the type is printed instead
 */
int printTrace(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Trace, L);
}

/**
 * \ingroup LuaScripts
 * printDebug(*):
 * Logs the passed value to the installed LogManager with a LogLevel of 'Debug'.
 * For Boolean, numbers, and strings, the internal values are printed, for all other
 * types, the type is printed instead
 */
int printDebug(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Debug, L);
}

/**
 * \ingroup LuaScripts
 * printInfo(*):
 * Logs the passed value to the installed LogManager with a LogLevel of 'Info'.
 * For Boolean, numbers, and strings, the internal values are printed, for all other
 * types, the type is printed instead
 */
int printInfo(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Info, L);
}

/**
 * \ingroup LuaScripts
 * printWarning(*):
 * Logs the passed value to the installed LogManager with a LogLevel of 'Warning'.
 * For Boolean, numbers, and strings, the internal values are printed, for all other
 * types, the type is printed instead
 */
int printWarning(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Warning, L);
}

/**
 * \ingroup LuaScripts
 * printError(*):
 * Logs the passed value to the installed LogManager with a LogLevel of 'Error'.
 * For Boolean, numbers, and strings, the internal values are printed, for all other
 * types, the type is printed instead
 */
int printError(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Error, L);
}

/**
 * \ingroup LuaScripts
 * printFatal(*):
 * Logs the passed value to the installed LogManager with a LogLevel of 'Fatal'.
 * For Boolean, numbers, and strings, the internal values are printed, for all other
 * types, the type is printed instead
 */
int printFatal(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Fatal, L);
}

/**
 * \ingroup LuaScripts
 * absPath(string):
 * Passes the argument to FileSystem::absolutePath, which resolves occuring path
 * tokens and returns the absolute path.
 */
int absolutePath(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::absolutePath");

    const std::string& path = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );

    ghoul::lua::push(L, absPath(path).string());

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

/**
 * \ingroup LuaScripts
 * setPathToken(string, string):
 * Registers the path token provided by the first argument to the path in the second
 * argument. If the path token already exists, it will be silently overridden.
 */
int setPathToken(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::setPathToken");

    std::string pathToken = ghoul::lua::value<std::string>(L, 1);
    std::string path = ghoul::lua::value<std::string>(L, 2);

    FileSys.registerPathToken(
        std::move(pathToken),
        std::move(path),
        ghoul::filesystem::FileSystem::Override::Yes
    );

    lua_pop(L, 2);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
 * \ingroup LuaScripts
 * fileExists(string):
 * Checks whether the provided file exists
 */
int fileExists(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::fileExists");

    const std::string& file = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );
    const bool e = std::filesystem::is_regular_file(absPath(file));

    ghoul::lua::push(L, e);

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

/**
 * \ingroup LuaScripts
 * directoryExists(string):
 * Checks whether the provided file exists
 */
int directoryExists(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::directoryExists");

    const std::string& file = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );
    const bool e = std::filesystem::is_directory(absPath(file));

    ghoul::lua::push(L, e);

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

int walkCommon(lua_State* L, std::function<bool(const std::filesystem::path&)> filter) {
    int nArguments = ghoul::lua::checkArgumentsAndThrow(L, { 1, 3 }, "lua::walkCommon");

    const std::string path = ghoul::lua::value<std::string>(L, 1);
    const bool recursive = nArguments >= 2 ? ghoul::lua::value<bool>(L, 2) : false;
    const bool sorted = nArguments == 3 ? ghoul::lua::value<bool>(L, 3) : false;

    namespace fs = std::filesystem;
    std::vector<fs::directory_entry> result;
    if (fs::is_directory(path)) {
        if (recursive) {
            for (fs::directory_entry e : fs::recursive_directory_iterator(path)) {
                if (filter(e)) {
                    result.push_back(e);
                }
            }
        }
        else {
            for (fs::directory_entry e : fs::directory_iterator(path)) {
                if (filter(e)) {
                    result.push_back(e);
                }
            }
        }

        if (sorted) {
            std::sort(result.begin(), result.end());
        }
    }

    lua_newtable(L);

    for (int i = 0; i < static_cast<int>(result.size()); ++i) {
        lua_pushstring(L, result[i].path().string().c_str());
        lua_rawseti(L, -2, i + 1);
    }
    return 1;
}

/**
 * \ingroup LuaScripts
 * walkDirectory(string, bool, bool):
 * Walks a directory and returns the contents of the directory as absolute paths. The
 * first argument is the path of the directory that should be walked, the second argument
 * determines if the walk is recursive and will continue in contained directories. The
 * default value for this parameter is "false". The third argument determines whether the
 * table that is returned is sorted. The default value for this parameter is "false".
 */
int walkDirectory(lua_State* L) {
    namespace fs = std::filesystem;
    return walkCommon(
        L,
        [](const fs::path& p) { return fs::is_directory(p) || fs::is_regular_file(p); }
    );
}

/**
 * \ingroup LuaScripts
 * walkDirectoryFiles(string, bool, bool):
 * Walks a directory and returns the files of the directory as absolute paths. The first
 * argument is the path of the directory that should be walked, the second argument
 * determines if the walk is recursive and will continue in contained directories. The
 * default value for this parameter is "false". The third argument determines whether the
 * table that is returned is sorted. The default value for this parameter is "false".
 */
int walkDirectoryFiles(lua_State* L) {
    namespace fs = std::filesystem;
    return walkCommon(L, [](const fs::path& p) { return fs::is_regular_file(p); });
}

/**
* \ingroup LuaScripts
* walkDirectory(string, bool, bool):
* Walks a directory and returns the subfolders of the directory as absolute paths. The
* first argument is the path of the directory that should be walked, the second argument
* determines if the walk is recursive and will continue in contained directories. The
* default value for this parameter is "false". The third argument determines whether the
* table that is returned is sorted. The default value for this parameter is "false".
*/
int walkDirectoryFolder(lua_State* L) {
    namespace fs = std::filesystem;
    return walkCommon(L, [](const fs::path& p) { return fs::is_directory(p); });
}

/**
 * \ingroup LuaScripts
 * directoryForPath(string):
 * This function extracts the directory part of the passed path. For example, if the
 * parameter is 'C:\\OpenSpace\\foobar\\foo.txt', this function returns
 * 'C:\\OpenSpace\\foobar'."
 */
int directoryForPath(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::directoryForPath");

    std::string file = ghoul::lua::value<std::string>(L, 1, ghoul::lua::PopValue::Yes);
    std::string path = std::filesystem::path(std::move(file)).parent_path().string();

    ghoul::lua::push(L, path);

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

/**
 * \ingroup LuaScripts
 * This function extracts the contents of a zip file. The first
 * argument is the path to the zip file. The second argument is the
 * directory where to put the extracted files. If the third argument is
 * true, the compressed file will be deleted after the decompression
 * is finished.
 */
int unzipFile(lua_State* L) {
    const int nArguments = ghoul::lua::checkArgumentsAndThrow(
        L,
        { 2, 3 },
        "lua::unzipFile"
    );

    std::filesystem::path source = absPath(
        ghoul::lua::value<std::string>(L, 1, ghoul::lua::PopValue::No)
    );
    std::filesystem::path dest = absPath(
        ghoul::lua::value<std::string>(L, 2, ghoul::lua::PopValue::No)
    );

    bool deleteSource = false;
    if (nArguments == 3) {
        deleteSource = ghoul::lua::value<bool>(L, 3, ghoul::lua::PopValue::No);
    }

    auto onExtractEntry = [](const char*, void*) { return 0; };
    int arg = 2;
    zip_extract(source.string().c_str(), dest.string().c_str(), onExtractEntry, &arg);

    if (deleteSource && std::filesystem::is_regular_file(source)) {
        std::filesystem::remove(source);
    }

    lua_settop(L, 0);
    return 0;
}

/**
 * \ingroup LuaScripts
 * saveLastChangeToProfile(string):
 * Saves the last entry from the script log to the current profile
 */
int saveLastChangeToProfile(lua_State* L) {
    std::string asset = global::configuration->asset;
    std::filesystem::path logFilePath = absPath(global::configuration->scriptLog);
    std::ifstream logfile(logFilePath);
    std::string actualLastLine;
    std::string lastLine;
    std::string line;
    //add check for log file
    if (!logfile.good()) {
        ghoul::lua::push(L, fmt::format("Could not open scriptlog {}", logFilePath));
        printInternal(ghoul::logging::LogLevel::Error, L);
    }
    while (std::getline (logfile,line)) {
        actualLastLine = lastLine;
        lastLine=line;
    }

    if (actualLastLine.find("openspace.setPropertyValue") == std::string::npos) {
        ghoul::lua::push(L, "Only property value changes can be saved.");
        printInternal(ghoul::logging::LogLevel::Error, L);
        return -1;
    }

    std::string dataString = "${ASSETS}/";
    std::filesystem::path assetPath = absPath(fmt::format(
        "{}{}.scene", dataString, asset
    ));
    std::filesystem::path tempAssetPath = absPath(fmt::format(
        "{}{}.scene.tmp", dataString, asset
    ));
    std::string strReplace = "--customizationsend";
    std::string strNew = fmt::format("{}\n{}",actualLastLine, strReplace);
    std::ifstream filein(assetPath);
    std::ofstream fileout(tempAssetPath);
    if (!filein) {
        ghoul::lua::push(L, fmt::format("Could not open profile {}", assetPath));
        printInternal(ghoul::logging::LogLevel::Error, L);
    }
    if (!fileout) {
        ghoul::lua::push(L, fmt::format("Could not open tmp profile {}", tempAssetPath));
        printInternal(ghoul::logging::LogLevel::Error, L);
    }

    bool found = false;
    while(std::getline (filein, line)) {
        if (line == strReplace) {
            line = strNew;
            found = true;
        }
        line += "\n";
        fileout << line;
    }
    filein.close();
    fileout.close();
    if (found) {
        if (std::filesystem::is_regular_file(assetPath)) {
            std::filesystem::remove(assetPath);
        }
        int success = rename(tempAssetPath.string().c_str(), assetPath.string().c_str());
        if (success != 0) {
            std::string error = fmt::format(
                "Error renaming file {} to {}", tempAssetPath, assetPath
            );
            ghoul::lua::push(L, error);
            printInternal(ghoul::logging::LogLevel::Error, L);
            return -1;
        }
        if (std::filesystem::is_regular_file(tempAssetPath)) {
            std::filesystem::remove(tempAssetPath);
        }
        return 0;
    }
    else {
        ghoul::lua::push(L, "can not save to built in profiles");
        printInternal(ghoul::logging::LogLevel::Error, L);
        return -1;
    }
}

} // namespace openspace::luascriptfunctions
