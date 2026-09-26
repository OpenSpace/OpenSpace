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

#ifndef __GHOUL___DEFER___H__
#define __GHOUL___DEFER___H__

namespace ghoul::internal {

/**
 * This internal struct is used to host the lambda expression that gets executed when the
 * destructor is called.
 */
template <typename T>
struct ScopeExit {
    ScopeExit(T lambda) : _lambda(std::move(lambda)) {}
    ~ScopeExit() {
        _lambda();
    }

    T _lambda;
};

/**
 * A second helper struct that makes it possible to pass a lambda expression to the
 * ScopeExit without the trailing ) that a constructor call would require. The choice of
 * operator is arbitrary.
 */
class ScopeExitHelper {
public:
    template <typename T>
    ScopeExit<T> operator<<(T t) {
        return t;
    }
};

} // namespace ghoul::internal

#define __MERGE_Defer(a,b)  a##b
#define __LABEL_Defer(a) __MERGE_Defer(scopeExit_, a)


/**
 * The 'defer' macro can be used to execute a piece of code at the end of a scope, for
 * example to guarantee that a `delete` function is called.
 * Example:
 * ```
 * int* i = new int;
 * defer { delete i; };
 * *i = 0;
 * ```
 */
#define defer auto __LABEL_Defer(__LINE__) = ghoul::internal::ScopeExitHelper() << [&]()

#endif // __GHOUL___DEFER___H__
