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

#include "utils.h"

PathType detectPathType(const std::string& path) {
    // Jasset-relative: ./foo, ../foo
    if (path.starts_with("./") || path.starts_with("../")) {
        return PathType::Relative;
    }
    // Absolute: C:/ on Windows, / on Unix
    if (std::filesystem::path(path).is_absolute()) {
        return PathType::Absolute;
    }
    return PathType::Data;
}

std::filesystem::path resolvePath(const std::string& dependency,
                                  const std::filesystem::path& dataRoot,
                                  const std::filesystem::path& assetDirectory)
{
    const PathType type = detectPathType(dependency);
    std::filesystem::path resolved(dependency);

    // Resolve relative paths against the appropriate root directory
    if (type == PathType::Data && !dataRoot.empty()) {
        resolved = dataRoot / resolved;
    }
    else if (type == PathType::Relative && !assetDirectory.empty()) {
        resolved = assetDirectory / resolved;
    }

    // Normalize the path (resolve .. and . segments)
    std::error_code error;
    std::filesystem::path canonical = std::filesystem::weakly_canonical(resolved, error);
    return error ? resolved : canonical;
}
