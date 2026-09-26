/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#ifndef __GHOUL___SHADERPREPROCESSOR___H__
#define __GHOUL___SHADERPREPROCESSOR___H__

#include <ghoul/filesystem/file.h>
#include <ghoul/misc/dictionary.h>
#include <filesystem>
#include <functional>
#include <string>
#include <vector>

namespace ghoul::opengl {

class ShaderPreprocessor {
public:
    using ShaderChangedCallback = std::function<void()>;

    explicit ShaderPreprocessor(std::filesystem::path shaderPath,
        Dictionary dictionary = Dictionary());

    const std::filesystem::path& filename() const;
    const Dictionary& dictionary() const;
    void setDictionary(Dictionary dictionary);
    void setCallback(ShaderChangedCallback changeCallback);
    std::string process();
    std::string includedFiles() const;

    /**
     * Adds the passed folder to the list of include paths that are checked when a shader
     * includes a file. The list of include paths is traversed in the order in which they
     * where added to this class. The folder in which the shader is located will always be
     * treated as if being on the top of the list.
     *
     * \param folderPath The folder that should be added to the list of include paths
     *
     * \pre \p folderPath must not be empty
     * \pre \p folderPath must be an existing directory
     * \pre \p folderPath must be a path without FileSystem tokens
     */
    static void addIncludePath(const std::filesystem::path& folderPath);

    static const std::vector<std::filesystem::path> includePaths();

private:
    std::vector<filesystem::File> _includedFiles;
    static std::vector<std::filesystem::path> _includePaths;
    std::filesystem::path _shaderPath;
    Dictionary _dictionary;
    ShaderChangedCallback _onChangeCallback;
};

} // namespace ghoul::opengl

#endif // __GHOUL___SHADERPREPROCESSOR___H__
