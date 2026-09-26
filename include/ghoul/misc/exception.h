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

#ifndef __GHOUL___EXCEPTION___H__
#define __GHOUL___EXCEPTION___H__

#include <stdexcept>

#include <filesystem>
#include <string>

namespace ghoul {

/**
 * Superclass for all exceptions that are thrown in Ghoul. The total message of the
 * exception consists of the `message` prefixed with the `component` that threw the
 * exception if it was set.
 */
struct RuntimeError : public std::runtime_error {
    /**
     * Main constructor constructing the exception with the provided message \p msg and
     * component \p comp.
     *
     * \param msg The main message of the exception
     * \param comp The optional component
     *
     * \pre \p msg must not be empty
     */
    explicit RuntimeError(std::string msg, std::string comp = "");

    ~RuntimeError() override = default;

    /// The main message describing the exception
    const std::string message;

    /// The name of the component that threw the exception
    const std::string component;
};

/**
 * Exception that is thrown if an IO access failed because a file could was not found.
 */
struct FileNotFoundError final : public RuntimeError {
    /**
     * Main constructor constructing the exception with the provided missing file \p f and
     * the component \p comp that threw the exception.
     *
     * \param f The file that was missing which caused this exception to be thrown
     * \param comp The optional compoment that caused this exception to be thrown
     *
     * \pre \p f must not be empty
     */
    explicit FileNotFoundError(std::filesystem::path f, std::string comp = "");

    /// The file that was missing
    const std::filesystem::path file;
};

} // namespace ghoul

#endif // __GHOUL___EXCEPTION___H__
