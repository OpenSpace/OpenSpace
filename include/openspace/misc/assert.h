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

#ifndef __OPENSPACE_CORE___ASSERT___H__
#define __OPENSPACE_CORE___ASSERT___H__

#include <string>
#include <stdexcept>

namespace openspace {

/**
 * Exception that gets thrown if an assertion is triggered and the user selects the
 * `AssertionException` option.
 */
struct AssertionException final : public std::runtime_error {
    explicit AssertionException(std::string exp, std::string msg, std::string file,
        std::string func, int line);
};

/**
 * Exception that gets thrown if switch - case statement is missing a case.
 */
struct MissingCaseException final : public std::logic_error {
    MissingCaseException();
};

/**
 * Internal assert command. Is called by the #assert_msg macro.
 *
 * \param expression The expression that caused the assertion
 * \param message The message that was provided for the assertion
 * \param file The file in which the assertion triggered
 * \param function The function in which the assertion triggered
 * \param line The line in the \p file that triggered the assertion
 */
void internalAssert(std::string expression, std::string message, std::string file,
    std::string function, int line);

} // namespace openspace

#if !(defined(NDEBUG) || defined(DEBUG)) || defined(OPENSPACE_ASSERT)
/**
* @defgroup ASSERT_MACRO_GROUP Assertion Macros
*
* @{
*/

#if defined(__GNUC__) || defined(__clang__)
#  define OS_ASSERT_FUNCTION __PRETTY_FUNCTION__
#else // ^^^^ GNUC || __clang__ // !(GNUC || __clang__) vvvv
#  define OS_ASSERT_FUNCTION __FUNCTION__
#endif // defined(__GNUC__) || defined(__clang__)

/**
 * This macro asserts on the `__condition__` and prints the optional `__message__`. In
 * addition, it gives the option of aborting, exiting, or ignoring the assertion. The
 * macro is optimized away in Release mode. Due to this fact, the `__condition__` must not
 * have any sideeffects.
 */

#ifdef OPENSPACE_THROW_ON_ASSERT
#define assert_msg(__condition__, __message__)                                           \
    do {                                                                                 \
        if (!(__condition__)) {                                                          \
            throw AssertionException(                                                    \
                #__condition__,                                                          \
                __message__,                                                             \
                __FILE__,                                                                \
                OS_ASSERT_FUNCTION,                                                      \
                __LINE__                                                                 \
            );                                                                           \
        }                                                                                \
    } while (false)
#else // ^^^^ OPENSPACE_THROW_ON_ASSERT // !OPENSPACE_THROW_ON_ASSERT vvvv
#define assert_msg(__condition__, __message__)                                           \
    do {                                                                                 \
        if (!(__condition__)) {                                                          \
            openspace::internalAssert(                                                   \
                #__condition__,                                                          \
                __message__,                                                             \
                __FILE__,                                                                \
                OS_ASSERT_FUNCTION,                                                      \
                __LINE__                                                                 \
            );                                                                           \
        }                                                                                \
    } while (false)

#endif // OPENSPACE_THROW_ON_ASSERT
#else // ^^^^ NDEBUG || DEBUG || OPENSPACE_ASSERT
      // !(NDEBUG || DEBUG || OPENSPACE_ASSERT) vvvv
#define assert_msg(__condition__, __message__) do {} while (false)
#endif // NDEBUG || DEBUG || OPENSPACE_ASSERT

/** @}  */

#endif // __OPENSPACE_CORE___ASSERT___H__
