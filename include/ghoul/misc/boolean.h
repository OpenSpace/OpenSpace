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

#ifndef __GHOUL___BOOLEAN___H__
#define __GHOUL___BOOLEAN___H__

namespace ghoul {

/**
 * This structure can be used to make a more expressive boolean parameter for methods.
 * Instead of using `bool`, which does not convey any direct meaning and forces the
 * programmer to read the documentation what the parameter does, this class can be
 * `typedef`ed to a meaningful name.
 *
 * For example:
 * ```
 * using AllowOverride = ghoul::Boolean;
 *
 * void foo(std::string value, AllowOverride override = AllowOverride::Yes);
 * ```
 * Though it is more verbal, it elimiates ambiguity regarding parameters. This class is
 * implicitly convertible to `bool`, but not the other way around. Furthermore,
 * it supports the `==`, `!=`, and `!` operators.
 *
 * When using the Boolean class, also consider the BooleanType defintion to create a
 * typesafe version of the usage describe above.
 */
struct Boolean {
    enum Value { Yes = 1, No = 0 };

    // Switch to this as soon as https://gcc.gnu.org/bugzilla/show_bug.cgi?id=104398
    // is fixed
    //enum class Value { Yes = 1, No = 0 };
    //using enum Value;

    /**
     * Non - explicit constructor so that we can automatically convert between different
     * aliases of Boolean.
     */
    constexpr Boolean(Value v) : value(v) {}

    /**
     * Explicit constructor to convert from `bool` into Boolean.
     */
    constexpr explicit Boolean(bool v) : value(v ? Yes : No) {}

    /**
     * This operator returns `true` if the stored value is equal to `Yes`.
     */
    constexpr operator bool() const noexcept { return value == Yes; }

    constexpr bool operator==(Boolean r) const noexcept { return value == r.value; }
    constexpr bool operator==(Boolean::Value r) const noexcept { return value == r; }

    constexpr bool operator!=(Boolean r) const noexcept { return value != r.value; }
    constexpr bool operator!=(Boolean::Value r) const noexcept { return value != r; }

    const Value value;
};

// This define can be used as a drop-in for the Boolean type to make it type-safe
#define BooleanType(__name__)                                                            \
struct __name__ {                                                                        \
    enum Value { Yes = 1, No = 0 };                                                      \
                                                                                         \
    constexpr __name__(Value v) : value(v) {}                                            \
                                                                                         \
    constexpr explicit __name__(bool v) : value(v ? Yes : No) {}                         \
                                                                                         \
    constexpr operator bool() const noexcept { return value == Yes; }                    \
                                                                                         \
    constexpr bool operator==(__name__ r) const noexcept { return value == r.value; }    \
    constexpr bool operator==(__name__::Value r) const noexcept { return value == r; }   \
    constexpr bool operator!=(__name__ r) const noexcept { return value != r.value; }    \
    constexpr bool operator!=(__name__::Value r) const noexcept { return value != r; }   \
                                                                                         \
    Value value;                                                                         \
}

} // namespace ghoul

#endif // __GHOUL___BOOLEAN___H__
