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

namespace openspace::luascriptfunctions {

int printInternal(ghoul::logging::LogLevel level, lua_State* L) {
    const int nArguments = lua_gettop(L);
    for (int i = 1; i <= nArguments; i++) {
        log(level, "print", ghoul::lua::luaValueToString(L, i));
    }
    lua_pop(L, nArguments);
    return 0;
}

int printTrace(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Trace, L);
}

int printDebug(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Debug, L);
}

int printInfo(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Info, L);
}

int printWarning(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Warning, L);
}

int printError(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Error, L);
}

int printFatal(lua_State* L) {
    return printInternal(ghoul::logging::LogLevel::Fatal, L);
}

} // namespace openspace::luascriptfunctions

namespace {

/**
 * Passes the argument to FileSystem::absolutePath, which resolves occuring path tokens
 * and returns the absolute path.
 */
[[codegen::luawrap("absPath")]] std::filesystem::path absolutePath(std::string path) {
    std::filesystem::path result = absPath(path);
    return result;
}

/**
 * Registers the path token provided by the first argument to the path in the second
 * argument. If the path token already exists, it will be silently overridden.
 */
[[codegen::luawrap]] void setPathToken(std::string pathToken, std::filesystem::path path)
{
    FileSys.registerPathToken(
        std::move(pathToken),
        std::move(path),
        ghoul::filesystem::FileSystem::Override::Yes
    );
}

// Checks whether the provided file exists.
[[codegen::luawrap]] bool fileExists(std::string file) {
    const bool e = std::filesystem::is_regular_file(absPath(std::move(file)));
    return e;
}

// Reads a file from disk and return its contents.
[[codegen::luawrap]] std::string readFile(std::filesystem::path file) {
    std::filesystem::path p = absPath(file);
    if (!std::filesystem::is_regular_file(p)) {
        throw ghoul::lua::LuaError(std::format("Could not open file '{}'", file));
    }

    std::ifstream f(p);
    std::stringstream buffer;
    buffer << f.rdbuf();
    std::string contents = buffer.str();
    return contents;
}

// Checks whether the provided file exists.
[[codegen::luawrap]] bool directoryExists(std::filesystem::path file) {
    const bool e = std::filesystem::is_directory(absPath(std::move(file)));
    return e;
}

std::vector<std::string> walkCommon(std::string path, bool recursive, bool sorted,
                                 std::function<bool(const std::filesystem::path&)> filter)
{
    namespace fs = std::filesystem;
    std::vector<std::string> result;
    if (fs::is_directory(path)) {
        if (recursive) {
            for (fs::directory_entry e : fs::recursive_directory_iterator(path)) {
                if (filter(e)) {
                    result.push_back(e.path().string());
                }
            }
        }
        else {
            for (fs::directory_entry e : fs::directory_iterator(path)) {
                if (filter(e)) {
                    result.push_back(e.path().string());
                }
            }
        }
    }
    if (sorted) {
        std::sort(result.begin(), result.end());
    }
    return result;
}

/**
 * Walks a directory and returns the contents of the directory as absolute paths. The
 * first argument is the path of the directory that should be walked, the second argument
 * determines if the walk is recursive and will continue in contained directories. The
 * default value for this parameter is "false". The third argument determines whether the
 * table that is returned is sorted. The default value for this parameter is "false".
 */
[[codegen::luawrap]] std::vector<std::string> walkDirectory(std::string path,
                                                            bool recursive = false,
                                                            bool sorted = false)
{
    namespace fs = std::filesystem;
    return walkCommon(
        path,
        recursive,
        sorted,
        [](const fs::path& p) { return fs::is_directory(p) || fs::is_regular_file(p); }
    );
}

/**
 * Walks a directory and returns the files of the directory as absolute paths. The first
 * argument is the path of the directory that should be walked, the second argument
 * determines if the walk is recursive and will continue in contained directories. The
 * default value for this parameter is "false". The third argument determines whether the
 * table that is returned is sorted. The default value for this parameter is "false".
 */
[[codegen::luawrap]] std::vector<std::string> walkDirectoryFiles(std::string path,
                                                                 bool recursive = false,
                                                                 bool sorted = false)
{
    namespace fs = std::filesystem;
    return walkCommon(
        path,
        recursive,
        sorted,
        [](const fs::path& p) { return fs::is_regular_file(p); }
    );
}

/**
 * Walks a directory and returns the subfolders of the directory as absolute paths. The
 * first argument is the path of the directory that should be walked, the second argument
 * determines if the walk is recursive and will continue in contained directories. The
 * default value for this parameter is "false". The third argument determines whether the
 * table that is returned is sorted. The default value for this parameter is "false".
 */
[[codegen::luawrap]] std::vector<std::string> walkDirectoryFolders(std::string path,
                                                                   bool recursive = false,
                                                                   bool sorted = false)
{
    namespace fs = std::filesystem;
    return walkCommon(
        path,
        recursive,
        sorted,
        [](const fs::path& p) { return fs::is_directory(p); }
    );
}

/**
 * This function extracts the directory part of the passed path. For example, if the
 * parameter is 'C:\\OpenSpace\\foobar\\foo.txt', this function returns
 * 'C:\\OpenSpace\\foobar'.
 */
[[codegen::luawrap]] std::filesystem::path directoryForPath(std::filesystem::path file) {
    std::filesystem::path path = std::filesystem::path(std::move(file)).parent_path();
    return path;
}

/**
 * This function extracts the contents of a zip file. The first argument is the path to
 * the zip file. The second argument is the directory where to put the extracted files. If
 * the third argument is true, the compressed file will be deleted after the decompression
 * is finished.
 */
[[codegen::luawrap]] void unzipFile(std::string source, std::string destination,
                                    bool deleteSource = false)
{
    if (!std::filesystem::exists(source)) {
        throw ghoul::lua::LuaError("Source file was not found");
    }

    struct zip_t* z = zip_open(source.c_str(), 0, 'r');
    const bool is64 = zip_is64(z);
    zip_close(z);

    if (is64) {
        throw ghoul::lua::LuaError(std::format(
            "Error while unzipping '{}': Zip64 archives are not supported", source
        ));
    }

    int ret = zip_extract(source.c_str(), destination.c_str(), nullptr, nullptr);
    if (ret != 0) {
        throw ghoul::lua::LuaError(std::format(
            "Error while unzipping '{}': {}", source, ret
        ));
    }

    if (deleteSource && std::filesystem::is_regular_file(source)) {
        std::filesystem::remove(source);
    }
}

#include "scriptengine_lua_codegen.cpp"

} // namespace
