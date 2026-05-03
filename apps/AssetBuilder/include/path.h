/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_ASSETBUILDER___UTILS___H__
#define __OPENSPACE_ASSETBUILDER___UTILS___H__

#include <filesystem>
#include <string>

/**
 * Classification of dependency path strings.
 */
enum class PathType {
    /// Data-relative path (e.g. "textures/earth.png")
    Data,
    /// Jasset-relative path (starts with ./ or ../)
    Relative,
    /// Absolute filesystem path (e.g. "C:/full/path.jasset")
    Absolute
};

/**
 * Detects whether a dependency string is data-relative, jasset-relative, or absolute.
 *
 * \param path The dependency path string
 * \return Classified PathType for the given string
 */
PathType detectPathType(const std::string& path);

/**
 * Resolves a dependency string to an absolute filesystem path. Data-relative paths
 * resolve against \p dataRoot, jasset-relative paths resolve against \p assetDir, and
 * absolute paths are returned as-is (after canonicalization).
 *
 * \param dependency The dependency path string
 * \param dataRoot The OpenSpace data root directory
 * \param assetDirectory The parent directory of the current .jasset file
 * \return Canonicalized absolute path, or the raw resolved path on error
 */
std::filesystem::path resolvePath(const std::string& dependency,
    const std::filesystem::path& dataRoot, const std::filesystem::path& assetDirectory);

#endif // __OPENSPACE_ASSETBUILDER___UTILS___H__
