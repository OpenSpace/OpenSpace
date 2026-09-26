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

#ifndef __GHOUL___UNIFORMCACHE___H__
#define __GHOUL___UNIFORMCACHE___H__

// Dear future-me (abock)
// I gave up trying to make this prettier for a reason
// I tried moving the number of parameters into a parameter:
// #define InternalUniformCache_1(__x1__) struct { int __x1__ = -1 }
// #define InternalUniformCache_2(__x1__, __x2__) struct { int __x1__ = -1, __x2__ = -1 }
// #define UniformCache(n, ...) InternalUniformCache_##n(__VA_ARGS__)
// But that leads to struct { int __x1__, __x2__ = =1, = -1 } due to the way the parameter
// expansion works

// I tried making the function recursive as by
// https://github.com/pfultz2/Cloak/wiki/C-Preprocessor-tricks,-tips,-and-idioms
// but that code failed to work in Visual Studio

// I tried creating a new struct that autoinitializes to get rid of the = -1 at the end
// but that failed as it needs to be cast into an int& for the caching

// This solution is from here:
// https://stackoverflow.com/questions/11761703/overloading-macro-on-number-of-arguments
// The mutable was added to the struct to make them usable in `const` methods


// HOW TO ADD MORE PARAMETERS:
//   1. Add a new macro UniformCacheX, copying from X-1
//   2. Add vX variable to argument list
//   3. Add ,vX = -1 to the struct definition
//   4. Add _X before NAME in the GET_MACRO macro
//   5. Add UniformCacheX after __VA_ARGS__ in the UniformCache macro

// OBS: Refrain from adding new values to the struct, or at the very least, update the
//      updateUniformLocations method as it depends on the UniformCache's struct size

#include <ghoul/opengl/programobject.h>
#include <array>
#include <type_traits>

/**
 * This is just an empty tag that can be used to provide better error messages for the
 * updateUniformLocations method.
 */
struct UniformCacheBase {};

#define UniformCache1(v1)                                                                \
    struct : public UniformCacheBase {                                                   \
        int v1 = -1;                                                                     \
        const std::array<const char*, 1> UniformNames = { #v1 };                         \
    }

#define UniformCache2(v1, v2)                                                            \
    struct : public UniformCacheBase {                                                   \
        int v1 = -1, v2 = -1;                                                            \
        const std::array<const char*, 2> UniformNames = { #v1, #v2 };                    \
    }

#define UniformCache3(v1, v2, v3)                                                        \
    struct : public UniformCacheBase {                                                   \
        int v1 = -1, v2 = -1, v3 = -1;                                                   \
        const std::array<const char*, 3> UniformNames = { #v1, #v2, #v3 };               \
    }

#define UniformCache4(v1, v2, v3, v4)                                                    \
    struct : public UniformCacheBase {                                                   \
        int v1 = -1, v2 = -1, v3 = -1, v4 = -1;                                          \
        const std::array<const char*, 4> UniformNames = { #v1, #v2, #v3, #v4 };          \
    }

#define UniformCache5(v1, v2, v3, v4, v5)                                                \
    struct : public UniformCacheBase {                                                   \
        int v1 = -1, v2 = -1, v3 = -1, v4 = -1, v5 = -1;                                 \
        const std::array<const char*, 5> UniformNames = { #v1, #v2, #v3, #v4, #v5 };     \
}

#define UniformCache6(v1, v2, v3, v4, v5, v6)                                            \
    struct : public UniformCacheBase {                                                   \
        int v1 = -1, v2 = -1, v3 = -1, v4 = -1, v5 = -1, v6 = -1;                        \
        const std::array<const char*, 6> UniformNames = {                                \
            #v1, #v2, #v3, #v4, #v5, #v6                                                 \
        };                                                                               \
    }

#define UniformCache7(v1, v2, v3, v4, v5, v6, v7)                                        \
    struct : public UniformCacheBase {                                                   \
        int v1 = -1, v2 = -1, v3 = -1, v4 = -1, v5 = -1, v6 = -1, v7 = -1;               \
        const std::array<const char*, 7> UniformNames = {                                \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7                                            \
        };                                                                               \
    }

#define UniformCache8(v1, v2, v3, v4, v5, v6, v7, v8)                                    \
    struct : public UniformCacheBase {                                                   \
        int v1 = -1, v2 = -1, v3 = -1, v4 = -1, v5 = -1, v6 = -1, v7 = -1, v8 = -1;      \
        const std::array<const char*, 8> UniformNames = {                                \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8                                       \
        };                                                                               \
    }

#define UniformCache9(v1, v2, v3, v4, v5, v6, v7, v8, v9)                                \
    struct : public UniformCacheBase {                                                   \
        int v1 = -1, v2 = -1, v3 = -1, v4 = -1, v5 = -1, v6 = -1, v7 = -1, v8 = -1,      \
            v9 = -1;                                                                     \
        const std::array<const char*, 9> UniformNames = {                                \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9                                  \
        };                                                                               \
    }

#define UniformCache10(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)                          \
    struct : public UniformCacheBase {                                                   \
        int v1 = -1,  v2 = -1, v3 = -1, v4 = -1, v5 = -1, v6 = -1, v7 = -1, v8 = -1,     \
            v9 = -1, v10 = -1;                                                           \
        const std::array<const char*, 10> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10                            \
        };                                                                               \
    }

#define UniformCache11(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11)                     \
    struct : public UniformCacheBase {                                                   \
        int v1 = -1,  v2 = -1,  v3 = -1, v4 = -1, v5 = -1, v6 = -1, v7 = -1, v8 = -1,    \
            v9 = -1, v10 = -1, v11 = -1;                                                 \
        const std::array<const char*, 11> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11                      \
        };                                                                               \
    }

#define UniformCache12(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12)                \
    struct : public UniformCacheBase {                                                   \
        int v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1, v5 = -1, v6 = -1, v7 = -1, v8 = -1,   \
            v9 = -1, v10 = -1, v11 = -1, v12 = -1;                                       \
        const std::array<const char*, 12> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12                \
        };                                                                               \
    }

#define UniformCache13(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13)           \
    struct : public UniformCacheBase {                                                   \
        int v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1, v6 = -1, v7 = -1, v8 = -1,  \
            v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1;                             \
        const std::array<const char*, 13> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13          \
        };                                                                               \
    }

#define UniformCache14(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14)      \
    struct : public UniformCacheBase {                                                   \
        int v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1, v7 = -1, v8 = -1, \
            v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1;                   \
        const std::array<const char*, 14> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14    \
        };                                                                               \
    }

#define UniformCache15(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1, v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,         \
             v8 = -1, v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,         \
            v15 = -1;                                                                    \
        const std::array<const char*, 15> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15                                                                         \
        };                                                                               \
    }

#define UniformCache16(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16)                                                              \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1;                                                          \
        const std::array<const char*, 16> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16                                                                   \
        };                                                                               \
    }

#define UniformCache17(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17)                                                         \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1;                                                \
        const std::array<const char*, 17> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17                                                             \
        };                                                                               \
    }

#define UniformCache18(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18)                                                    \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1;                                      \
        const std::array<const char*, 18> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18                                                       \
        };                                                                               \
    }

#define UniformCache19(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19)                                               \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1;                            \
        const std::array<const char*, 19> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19                                                 \
        };                                                                               \
    }

#define UniformCache20(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20)                                          \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1;                  \
        const std::array<const char*, 20> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20                                           \
        };                                                                               \
    }

#define UniformCache21(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21)                                     \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1;        \
        const std::array<const char*, 21> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21                                     \
        };                                                                               \
    }

#define UniformCache22(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22)                                \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1;                                                                    \
        const std::array<const char*, 22> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22                               \
        };                                                                               \
    }

#define UniformCache23(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23)                           \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1;                                                          \
        const std::array<const char*, 23> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23                         \
        };                                                                               \
    }

#define UniformCache24(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23, v24)                      \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1, v24 = -1;                                                \
        const std::array<const char*, 24> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23, #v24                   \
        };                                                                               \
    }

#define UniformCache25(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23, v24, v25)                 \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1, v24 = -1, v25 = -1;                                      \
        const std::array<const char*, 25> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23, #v24, #v25             \
        };                                                                               \
    }

#define UniformCache26(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26)            \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1, v24 = -1, v25 = -1, v26 = -1;                            \
        const std::array<const char*, 26> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23, #v24, #v25, #v26       \
        };                                                                               \
    }

#define UniformCache27(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27)       \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1, v24 = -1, v25 = -1, v26 = -1, v27 = -1;                  \
        const std::array<const char*, 27> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23, #v24, #v25, #v26,      \
            #v27                                                                         \
        };                                                                               \
    }

#define UniformCache28(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28)  \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1, v24 = -1, v25 = -1, v26 = -1, v27 = -1, v28 = -1;        \
        const std::array<const char*, 28> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23, #v24, #v25, #v26,      \
            #v27, #v28                                                                   \
        };                                                                               \
    }
#define UniformCache29(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28,  \
                        v29)                                                             \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1, v24 = -1, v25 = -1, v26 = -1, v27 = -1, v28 = -1,        \
            v29 = -1;                                                                    \
        const std::array<const char*, 29> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23, #v24, #v25, #v26,      \
            #v27, #v28, #v29                                                             \
        };                                                                               \
    }
#define UniformCache30(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28,  \
                       v29, v30)                                                         \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1, v24 = -1, v25 = -1, v26 = -1, v27 = -1, v28 = -1,        \
            v29 = -1, v30 = -1;                                                          \
        const std::array<const char*, 30> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23, #v24, #v25, #v26,      \
            #v27, #v28, #v29, #v30                                                       \
        };                                                                               \
    }
#define UniformCache31(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28,  \
                       v29, v30, v31)                                                    \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1, v24 = -1, v25 = -1, v26 = -1, v27 = -1, v28 = -1,        \
            v29 = -1, v30 = -1, v31 = -1;                                                \
        const std::array<const char*, 31> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23, #v24, #v25, #v26,      \
            #v27, #v28, #v29, #v30, #v31                                                 \
        };                                                                               \
    }
#define UniformCache32(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28,  \
                       v29, v30, v31, v32)                                               \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1, v24 = -1, v25 = -1, v26 = -1, v27 = -1, v28 = -1,        \
            v29 = -1, v30 = -1, v31 = -1, v32 = -1;                                      \
        const std::array<const char*, 32> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23, #v24, #v25, #v26,      \
            #v27, #v28, #v29, #v30, #v31, #v32                                           \
        };                                                                               \
    }
#define UniformCache33(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28,  \
                       v29, v30, v31, v32, v33)                                          \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1, v24 = -1, v25 = -1, v26 = -1, v27 = -1, v28 = -1,        \
            v29 = -1, v30 = -1, v31 = -1, v32 = -1, v33 = -1;                            \
        const std::array<const char*, 33> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23, #v24, #v25, #v26,      \
            #v27, #v28, #v29, #v30, #v31, #v32, #v33                                     \
        };                                                                               \
    }
#define UniformCache34(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28,  \
                       v29, v30, v31, v32, v33, v34)                                     \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1, v24 = -1, v25 = -1, v26 = -1, v27 = -1, v28 = -1,        \
            v29 = -1, v30 = -1, v31 = -1, v32 = -1, v33 = -1, v34 = -1;                  \
        const std::array<const char*, 34> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23, #v24, #v25, #v26,      \
            #v27, #v28, #v29, #v30, #v31, #v32, #v33, #v34                               \
        };                                                                               \
    }
#define UniformCache35(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28,  \
                       v29, v30, v31, v32, v33, v34, v35)                                \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1, v24 = -1, v25 = -1, v26 = -1, v27 = -1, v28 = -1,        \
            v29 = -1, v30 = -1, v31 = -1, v32 = -1, v33 = -1, v34 = -1, v35 = -1;        \
        const std::array<const char*, 35> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23, #v24, #v25, #v26,      \
            #v27, #v28, #v29, #v30, #v31, #v32, #v33, #v34, #v35                         \
        };                                                                               \
    }
#define UniformCache36(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28,  \
                       v29, v30, v31, v32, v33, v34, v35, v36)                           \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1, v24 = -1, v25 = -1, v26 = -1, v27 = -1, v28 = -1,        \
            v29 = -1, v30 = -1, v31 = -1, v32 = -1, v33 = -1, v34 = -1, v35 = -1,        \
            v36 = -1;                                                                    \
        const std::array<const char*, 36> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23, #v24, #v25, #v26,      \
            #v27, #v28, #v29, #v30, #v31, #v32, #v33, #v34, #v35, #v36                   \
        };                                                                               \
    }
#define UniformCache37(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28,  \
                       v29, v30, v31, v32, v33, v34, v35, v36, v37)                      \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1, v24 = -1, v25 = -1, v26 = -1, v27 = -1, v28 = -1,        \
            v29 = -1, v30 = -1, v31 = -1, v32 = -1, v33 = -1, v34 = -1, v35 = -1,        \
            v36 = -1, v37 = -1;                                                          \
        const std::array<const char*, 37> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23, #v24, #v25, #v26,      \
            #v27, #v28, #v29, #v30, #v31, #v32, #v33, #v34, #v35, #v36, #v37             \
        };                                                                               \
    }
#define UniformCache38(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28,  \
                       v29, v30, v31, v32, v33, v34, v35, v36, v37, v38)                 \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1, v24 = -1, v25 = -1, v26 = -1, v27 = -1, v28 = -1,        \
            v29 = -1, v30 = -1, v31 = -1, v32 = -1, v33 = -1, v34 = -1, v35 = -1,        \
            v36 = -1, v37 = -1, v38 = -1;                                                \
        const std::array<const char*, 38> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23, #v24, #v25, #v26,      \
            #v27, #v28, #v29, #v30, #v31, #v32, #v33, #v34, #v35, #v36, #v37, #v38       \
        };                                                                               \
    }
#define UniformCache39(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28,  \
                       v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39)            \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1, v24 = -1, v25 = -1, v26 = -1, v27 = -1, v28 = -1,        \
            v29 = -1, v30 = -1, v31 = -1, v32 = -1, v33 = -1, v34 = -1, v35 = -1,        \
            v36 = -1, v37 = -1, v38 = -1, v39 = -1;                                      \
        const std::array<const char*, 39> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23, #v24, #v25, #v26,      \
            #v27, #v28, #v29, #v30, #v31, #v32, #v33, #v34, #v35, #v36, #v37, #v38,      \
            #v39                                                                         \
        };                                                                               \
    }
#define UniformCache40(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, \
                       v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28,  \
                       v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39, v40)       \
    struct : public UniformCacheBase {                                                   \
        int  v1 = -1,  v2 = -1,  v3 = -1,  v4 = -1,  v5 = -1,  v6 = -1,  v7 = -1,        \
             v8 = -1,  v9 = -1, v10 = -1, v11 = -1, v12 = -1, v13 = -1, v14 = -1,        \
            v15 = -1, v16 = -1, v17 = -1, v18 = -1, v19 = -1, v20 = -1, v21 = -1,        \
            v22 = -1, v23 = -1, v24 = -1, v25 = -1, v26 = -1, v27 = -1, v28 = -1,        \
            v29 = -1, v30 = -1, v31 = -1, v32 = -1, v33 = -1, v34 = -1, v35 = -1,        \
            v36 = -1, v37 = -1, v38 = -1, v39 = -1, v40 = -1;                            \
        const std::array<const char*, 40> UniformNames = {                               \
            #v1, #v2, #v3, #v4, #v5, #v6, #v7, #v8, #v9, #v10, #v11, #v12, #v13, #v14,   \
            #v15, #v16, #v17, #v18, #v19, #v20, #v21, #v22, #v23, #v24, #v25, #v26,      \
            #v27, #v28, #v29, #v30, #v31, #v32, #v33, #v34, #v35, #v36, #v37, #v38,      \
            #v39, #v40                                                                   \
        };                                                                               \
    }

namespace ghoul::opengl {

/**
 * Function that will update the uniform locations of the \p uniformCache based on
 * querying the \p uniformNames in the \p program. This method will essentially call
 * ProgramObject::uniformLocation on the passed parameters. The order of arguments in the
 * \p uniformNames has to be the same as the location definitions.
 *
 * \tparam T A type that was created through the UniformCache command
 * \tparam N The number of uniform values that should be specified
 */
template <typename T, size_t N> // @CONCEPTS
void updateUniformLocations(const ProgramObject& program, T& uniformCache,
                            const std::array<const char*, N>& uniformNames)
{
    static_assert(std::is_base_of_v<UniformCacheBase, T>);

    int* uniformLocations = reinterpret_cast<int*>(&uniformCache);
    for (int i = 0; i < N; i++) {
        // We skip empty uniform names
        if (uniformNames[i]) {
            uniformLocations[i] = program.uniformLocation(uniformNames[i]);
        }
    }
}

/**
 * Function that will update the uniform locations in the \p program of the
 * \p uniformCache based on the uniform names defined inside the \p uniformCache. This
 * method will essentially call ProgramObject::uniformLocation on the passed parameters.
 * The order of arguments in the \p uniformNames has to be the same as the location
 * definitions.
 *
 * \tparam T A type that was created through the UniformCache command
 */
template <typename T> // @CONCEPTS
void updateUniformLocations(const ProgramObject& program, T& uniformCache) {
    static_assert(std::is_base_of_v<UniformCacheBase, T>);

    int* uniformLocations = reinterpret_cast<int*>(&uniformCache);
    for (size_t i = 0; i < uniformCache.UniformNames.size(); i++) {
        // We skip empty uniform names
        if (uniformCache.UniformNames[i]) {
            uniformLocations[i] = program.uniformLocation(uniformCache.UniformNames[i]);
        }
    }
}

} // namespace ghoul::opengl


// The EXPAND macro is a fix for MSVC's interpretation of the C++11 standard of VA_ARGS
#define EXPAND(x) x
#define GET_MACRO( _1,  _2,  _3,  _4,  _5,  _6,  _7,  _8,  _9, _10, _11, _12, _13, _14,  \
                  _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28,  \
                  _29, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, NAME, ...) \
                  NAME

#define UniformCache(...)                                                                \
    EXPAND(                                                                              \
        GET_MACRO(                                                                       \
            __VA_ARGS__,                                                                 \
            UniformCache40, UniformCache39, UniformCache38, UniformCache37,              \
            UniformCache36, UniformCache35, UniformCache34, UniformCache33,              \
            UniformCache32, UniformCache31, UniformCache30, UniformCache29,              \
            UniformCache28, UniformCache27, UniformCache26, UniformCache25,              \
            UniformCache24, UniformCache23, UniformCache22, UniformCache21,              \
            UniformCache20, UniformCache19, UniformCache18, UniformCache17,              \
            UniformCache16, UniformCache15, UniformCache14, UniformCache13,              \
            UniformCache12, UniformCache11, UniformCache10, UniformCache9,               \
            UniformCache8,  UniformCache7,  UniformCache6,  UniformCache5,               \
            UniformCache4,  UniformCache3,  UniformCache2,  UniformCache1                \
        )(__VA_ARGS__)                                                                   \
    )

#endif // __GHOUL___UNIFORMCACHE___H__
