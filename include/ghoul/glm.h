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

#ifndef __GHOUL___GLM___H__
#define __GHOUL___GLM___H__

#include <ghoul/format.h>
#include <ghoul/misc/stringconversion.h>

#ifdef __unix__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#endif // __unix__


#ifndef GLM_META_PROG_HELPERS
#define GLM_META_PROG_HELPERS
#endif // GLM_META_PROG_HELPERS

#ifndef GLM_ENABLE_EXPERIMENTAL
#define GLM_ENABLE_EXPERIMENTAL
#endif // GLM_ENABLE_EXPERIMENTAL

#ifndef GLM_FORCE_CTOR_INIT
#define GLM_FORCE_CTOR_INIT
#endif // GLM_FORCE_CTOR_INIT

// Enabling swizzling breaks binary compatibility between glm::vecX and float* ?
// #ifndef GLM_SWIZZLE
// #define GLM_SWIZZLE
// #endif // GLM_SWIZZLE

#include <glm/glm.hpp>
#include <glm/gtc/quaternion.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/ext/matrix_common.hpp>
#include <glm/gtx/component_wise.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <string>

namespace {
    template <class T, class... Ts>
    struct is_any : std::disjunction<std::is_same<T, Ts>...> {};
} // namespace

namespace ghoul {

template <typename T>
struct glm_components : public std::integral_constant<glm::length_t, 0> {};

template <glm::length_t L, typename T, glm::qualifier Q>
struct glm_components<glm::vec<L, T, Q>> :
    public std::integral_constant<glm::length_t, L> {};

template <glm::length_t C, glm::length_t R, typename T, glm::qualifier Q>
struct glm_components<glm::mat<C, R, T, Q>> :
    public std::integral_constant<glm::length_t, C * R> {};

template <typename T>
constexpr bool isGlmMatrix() {
    return is_any<T,
        glm::mat2x2, glm::mat2x3, glm::mat2x4,
        glm::mat3x2, glm::mat3x3, glm::mat3x4,
        glm::mat4x2, glm::mat4x3, glm::mat4x4,
        glm::dmat2x2, glm::dmat2x3, glm::dmat2x4,
        glm::dmat3x2, glm::dmat3x3, glm::dmat3x4,
        glm::dmat4x2, glm::dmat4x3, glm::dmat4x4>::value;
}

template <typename T>
constexpr bool isGlmVector() {
    return is_any<T,
        glm::vec2, glm::vec3, glm::vec4,
        glm::ivec2, glm::ivec3, glm::ivec4,
        glm::dvec2, glm::dvec3, glm::dvec4,
        glm::uvec2, glm::uvec3, glm::uvec4>::value;
}

/**
 * Compute a quaternion that represents the rotation looking from \p eye to \p target,
 * with the specified \p up direction.
 */
template <typename valType>
glm::tquat<valType> lookAtQuaternion(const glm::tvec3<valType> eye,
                                     const glm::tvec3<valType> target,
                                     const glm::tvec3<valType> up)
{
    const glm::tmat4x4<valType> lookAtMat = glm::lookAt(eye, target, up);
    return glm::normalize(glm::inverse(glm::quat_cast(lookAtMat)));
}

/**
 * Check if quaternion \p q1 and \p q2 represent the same spatial orientation. The
 * precision of the check can be controlled using the \p precision parameter.
 */
template <typename valType>
bool isSameOrientation(const glm::tquat<valType> q1, const glm::tquat<valType> q2,
                       const valType precision)
{
    return 1.0 - std::abs(glm::dot(q1, q2)) < precision;
}

/**
 * Compute a view direction vector from a quaternion representing a rotation.
 */
inline glm::dvec3 viewDirection(const glm::dquat& q) {
    return glm::normalize(q * glm::dvec3(0.0, 0.0, -1.0));
}

template <typename valType>
glm::tmat2x2<valType> createFillMat2x2(valType v) {
    return glm::tmat2x2<valType>(v, v, v, v);
}

template <typename valType>
glm::tmat2x3<valType> createFillMat2x3(valType v) {
    return glm::tmat2x3<valType>(v, v, v, v, v, v);
}

template <typename valType>
glm::tmat2x4<valType> createFillMat2x4(valType v) {
    return glm::tmat2x4<valType>(v, v, v, v, v, v, v, v);
}

template <typename valType>
glm::tmat3x3<valType> createFillMat3x3(valType v) {
    return glm::tmat3x3<valType>(v, v, v, v, v, v, v, v, v);
}

template <typename valType>
glm::tmat3x2<valType> createFillMat3x2(valType v) {
    return glm::tmat3x2<valType>(v, v, v, v, v, v);
}

template <typename valType>
glm::tmat3x4<valType> createFillMat3x4(valType v) {
    return glm::tmat3x4<valType>(v, v, v, v, v, v, v, v, v, v, v, v);
}

template <typename valType>
glm::tmat4x4<valType> createFillMat4x4(valType v) {
    return glm::tmat4x4<valType>(v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v);
}

template <typename valType>
glm::tmat4x2<valType> createFillMat4x2(valType v) {
    return glm::tmat4x2<valType>(v, v, v, v, v, v, v, v);
}

template <typename valType>
glm::tmat4x3<valType> createFillMat4x3(valType v) {
    return glm::tmat4x3<valType>(v, v, v, v, v, v, v, v, v, v, v, v);
}

} // namespace ghoul

template <>
struct std::formatter<glm::bvec2> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::bvec2& _val, std::format_context& ctx) const {
        return std::format_to(ctx.out(), "{{{},{}}}", _val.x, _val.y);
    }
};

template <>
struct std::formatter<glm::bvec3> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::bvec3& _val, std::format_context& ctx) const {
        return std::format_to(ctx.out(), "{{{},{},{}}}", _val.x, _val.y, _val.z);
    }
};

template <>
struct std::formatter<glm::bvec4> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::bvec4& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{}}}", _val.x, _val.y, _val.z, _val.w
        );
    }
};

template <>
struct std::formatter<glm::vec2> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::vec2& _val, std::format_context& ctx) const {
        return std::format_to(ctx.out(), "{{{},{}}}", _val.x, _val.y);
    }
};

template <>
struct std::formatter<glm::vec3> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::vec3& _val, std::format_context& ctx) const {
        return std::format_to(ctx.out(), "{{{},{},{}}}", _val.x, _val.y, _val.z);
    }
};

template <>
struct std::formatter<glm::vec4> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::vec4& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{}}}", _val.x, _val.y, _val.z, _val.w
        );
    }
};

template <>
struct std::formatter<glm::quat> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::quat& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{}}}", _val.x, _val.y, _val.z, _val.w
        );
    }
};

template <>
struct std::formatter<glm::dvec2> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::dvec2& _val, std::format_context& ctx) const {
        return std::format_to(ctx.out(), "{{{},{}}}", _val.x, _val.y);
    }
};

template <>
struct std::formatter<glm::dvec3> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::dvec3& _val, std::format_context& ctx) const {
        return std::format_to(ctx.out(), "{{{},{},{}}}", _val.x, _val.y, _val.z);
    }
};

template <>
struct std::formatter<glm::dvec4> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::dvec4& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{}}}", _val.x, _val.y, _val.z, _val.w
        );
    }
};

template <>
struct std::formatter<glm::dquat> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::dquat& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{}}}", _val.x, _val.y, _val.z, _val.w
        );
    }
};

template <>
struct std::formatter<glm::ivec2> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::ivec2& _val, std::format_context& ctx) const {
        return std::format_to(ctx.out(), "{{{},{}}}", _val.x, _val.y);
    }
};

template <>
struct std::formatter<glm::ivec3> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::ivec3& _val, std::format_context& ctx) const {
        return std::format_to(ctx.out(), "{{{},{},{}}}", _val.x, _val.y, _val.z);
    }
};

template <>
struct std::formatter<glm::ivec4> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::ivec4& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{}}}", _val.x, _val.y, _val.z, _val.w
        );
    }
};

template <>
struct std::formatter<glm::uvec2> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::uvec2& _val, std::format_context& ctx) const {
        return std::format_to(ctx.out(), "{{{},{}}}", _val.x, _val.y);
    }
};

template <>
struct std::formatter<glm::uvec3> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::uvec3& _val, std::format_context& ctx) const {
        return std::format_to(ctx.out(), "{{{},{},{}}}", _val.x, _val.y, _val.z);
    }
};

template <>
struct std::formatter<glm::uvec4> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::uvec4& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{}}}", _val.x, _val.y, _val.z, _val.w
        );
    }
};

template <>
struct std::formatter<glm::mat2> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::mat2& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{}}}", _val[0].x, _val[0].y, _val[1].x, _val[1].y
        );
    }
};

template <>
struct std::formatter<glm::mat2x3> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::mat2x3& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{},{},{}}}",
            _val[0].x, _val[0].y, _val[0].z, _val[1].x, _val[1].y, _val[1].z
        );
    }
};

template <>
struct std::formatter<glm::mat2x4> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::mat2x4& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{},{},{},{},{}}}",
            _val[0].x, _val[0].y, _val[0].z, _val[0].w,
            _val[1].x, _val[1].y, _val[1].z, _val[1].w
        );
    }
};

template <>
struct std::formatter<glm::mat3x2> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::mat3x2& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{},{},{}}}",
            _val[0].x, _val[0].y, _val[1].x, _val[1].y, _val[2].x, _val[2].y
        );
    }
};

template <>
struct std::formatter<glm::mat3> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::mat3& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{},{},{},{},{},{}}}",
            _val[0].x, _val[0].y, _val[0].z,
            _val[1].x, _val[1].y, _val[1].z,
            _val[2].x, _val[2].y, _val[2].z
        );
    }
};

template <>
struct std::formatter<glm::mat3x4> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::mat3x4& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{},{},{},{},{},{},{},{},{}}}",
            _val[0].x, _val[0].y, _val[0].z, _val[0].w,
            _val[1].x, _val[1].y, _val[1].z, _val[1].w,
            _val[2].x, _val[2].y, _val[2].z, _val[2].w
        );
    }
};

template <>
struct std::formatter<glm::mat4x2> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::mat4x2& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{},{},{},{},{}}}",
            _val[0].x, _val[0].y,
            _val[1].x, _val[1].y,
            _val[2].x, _val[2].y,
            _val[3].x, _val[3].y
        );
    }
};

template <>
struct std::formatter<glm::mat4x3> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::mat4x3& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{},{},{},{},{},{},{},{},{}}}",
            _val[0].x, _val[0].y, _val[0].z,
            _val[1].x, _val[1].y, _val[1].z,
            _val[2].x, _val[2].y, _val[2].z,
            _val[3].x, _val[3].y, _val[3].z
        );
    }
};

template <>
struct std::formatter<glm::mat4> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::mat4& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}}}",
            _val[0].x, _val[0].y, _val[0].z, _val[0].w,
            _val[1].x, _val[1].y, _val[1].z, _val[1].w,
            _val[2].x, _val[2].y, _val[2].z, _val[2].w,
            _val[3].x, _val[3].y, _val[3].z, _val[3].w
        );
    }
};

template <>
struct std::formatter<glm::dmat2> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::dmat2& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{}}}", _val[0].x, _val[0].y, _val[1].x, _val[1].y
        );
    }
};

template <>
struct std::formatter<glm::dmat2x3> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::dmat2x3& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{},{},{}}}",
            _val[0].x, _val[0].y, _val[0].z, _val[1].x, _val[1].y, _val[1].z
        );
    }
};

template <>
struct std::formatter<glm::dmat2x4> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::dmat2x4& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{},{},{},{},{}}}",
            _val[0].x, _val[0].y, _val[0].z, _val[0].w,
            _val[1].x, _val[1].y, _val[1].z, _val[1].w
        );
    }
};

template <>
struct std::formatter<glm::dmat3x2> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::dmat3x2& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{},{},{}}}",
            _val[0].x, _val[0].y, _val[1].x, _val[1].y, _val[2].x, _val[2].y
        );
    }
};

template <>
struct std::formatter<glm::dmat3> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::dmat3& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{},{},{},{},{},{}}}",
            _val[0].x, _val[0].y, _val[0].z,
            _val[1].x, _val[1].y, _val[1].z,
            _val[2].x, _val[2].y, _val[2].z
        );
    }
};

template <>
struct std::formatter<glm::dmat3x4> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::dmat3x4& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{},{},{},{},{},{},{},{},{}}}",
            _val[0].x, _val[0].y, _val[0].z, _val[0].w,
            _val[1].x, _val[1].y, _val[1].z, _val[1].w,
            _val[2].x, _val[2].y, _val[2].z, _val[2].w
        );
    }
};

template <>
struct std::formatter<glm::dmat4x2> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::dmat4x2& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{},{},{},{},{}}}",
            _val[0].x, _val[0].y,
            _val[1].x, _val[1].y,
            _val[2].x, _val[2].y,
            _val[3].x, _val[3].y
        );
    }
};

template <>
struct std::formatter<glm::dmat4x3> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::dmat4x3& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{},{},{},{},{},{},{},{},{}}}",
            _val[0].x, _val[0].y, _val[0].z,
            _val[1].x, _val[1].y, _val[1].z,
            _val[2].x, _val[2].y, _val[2].z,
            _val[3].x, _val[3].y, _val[3].z
        );
    }
};

template <>
struct std::formatter<glm::dmat4> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const glm::dmat4& _val, std::format_context& ctx) const {
        return std::format_to(
            ctx.out(),
            "{{{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}}}",
            _val[0].x, _val[0].y, _val[0].z, _val[0].w,
            _val[1].x, _val[1].y, _val[1].z, _val[1].w,
            _val[2].x, _val[2].y, _val[2].z, _val[2].w,
            _val[3].x, _val[3].y, _val[3].z, _val[3].w
        );
    }
};





template <>
struct std::less<glm::vec2> {
    bool operator()(const glm::vec2& a, const glm::vec2& b) const {
        return a.x < b.x && a.x < b.y;
    }
};

template <>
struct std::less<glm::vec3> {
    bool operator()(const glm::vec3& a, const glm::vec3& b) const {
        return a.x < b.x && a.x < b.y && a.z < b.z;
    }
};

template <>
struct std::less<glm::vec4> {
    bool operator()(const glm::vec4& a, const glm::vec4& b) const {
        return a.x < b.x && a.x < b.y && a.z < b.z && a.w < b.w;
    }
};

template <>
struct std::less<glm::ivec2> {
    bool operator()(const glm::ivec2& a, const glm::ivec2& b) const {
        return a.x < b.x && a.x < b.y;
    }
};

template <>
struct std::less<glm::ivec3> {
    bool operator()(const glm::ivec3& a, const glm::ivec3& b) const {
        return a.x < b.x && a.x < b.y && a.z < b.z;
    }
};

template <>
struct std::less<glm::ivec4> {
    bool operator()(const glm::ivec4& a, const glm::ivec4& b) const {
        return a.x < b.x && a.x < b.y && a.z < b.z && a.w < b.w;
    }
};

template <>
struct std::less<glm::dvec2> {
    bool operator()(const glm::dvec2& a, const glm::dvec2& b) const {
        return a.x < b.x && a.x < b.y;
    }
};

template <>
struct std::less<glm::dvec3> {
    bool operator()(const glm::dvec3& a, const glm::dvec3& b) const {
        return a.x < b.x && a.x < b.y && a.z < b.z;
    }
};

template <>
struct std::less<glm::dvec4> {
    bool operator()(const glm::dvec4& a, const glm::dvec4& b) const {
        return a.x < b.x && a.x < b.y && a.z < b.z && a.w < b.w;
    }
};

template <>
struct std::less_equal<glm::vec2> {
    bool operator()(const glm::vec2& a, const glm::vec2& b) const {
        return a.x <= b.x && a.x <= b.y;
    }
};

template <>
struct std::less_equal<glm::vec3> {
    bool operator()(const glm::vec3& a, const glm::vec3& b) const {
        return a.x <= b.x && a.x <= b.y && a.z <= b.z;
    }
};

template <>
struct std::less_equal<glm::vec4> {
    bool operator()(const glm::vec4& a, const glm::vec4& b) const {
        return a.x <= b.x && a.x <= b.y && a.z <= b.z && a.w <= b.w;
    }
};

template <>
struct std::less_equal<glm::ivec2> {
    bool operator()(const glm::ivec2& a, const glm::ivec2& b) const {
        return a.x <= b.x && a.x <= b.y;
    }
};

template <>
struct std::less_equal<glm::ivec3> {
    bool operator()(const glm::ivec3& a, const glm::ivec3& b) const {
        return a.x <= b.x && a.x <= b.y && a.z <= b.z;
    }
};

template <>
struct std::less_equal<glm::ivec4> {
    bool operator()(const glm::ivec4& a, const glm::ivec4& b) const {
        return a.x <= b.x && a.x <= b.y && a.z <= b.z && a.w <= b.w;
    }
};

template <>
struct std::less_equal<glm::dvec2> {
    bool operator()(const glm::dvec2& a, const glm::dvec2& b) const {
        return a.x <= b.x && a.x <= b.y;
    }
};

template <>
struct std::less_equal<glm::dvec3> {
    bool operator()(const glm::dvec3& a, const glm::dvec3& b) const {
        return a.x <= b.x && a.x <= b.y && a.z <= b.z;
    }
};

template <>
struct std::less_equal<glm::dvec4> {
    bool operator()(const glm::dvec4& a, const glm::dvec4& b) const {
        return a.x <= b.x && a.x <= b.y && a.z <= b.z && a.w <= b.w;
    }
};

template <>
struct std::greater<glm::vec2> {
    bool operator()(const glm::vec2& a, const glm::vec2& b) const {
        return a.x > b.x && a.x > b.y;
    }
};

template <>
struct std::greater<glm::vec3> {
    bool operator()(const glm::vec3& a, const glm::vec3& b) const {
        return a.x > b.x && a.x > b.y && a.z > b.z;
    }
};

template <>
struct std::greater<glm::vec4> {
    bool operator()(const glm::vec4& a, const glm::vec4& b) const {
        return a.x > b.x && a.x > b.y && a.z > b.z && a.w > b.w;
    }
};

template <>
struct std::greater<glm::ivec2> {
    bool operator()(const glm::ivec2& a, const glm::ivec2& b) const {
        return a.x > b.x && a.x > b.y;
    }
};

template <>
struct std::greater<glm::ivec3> {
    bool operator()(const glm::ivec3& a, const glm::ivec3& b) const {
        return a.x > b.x && a.x > b.y && a.z > b.z;
    }
};

template <>
struct std::greater<glm::ivec4> {
    bool operator()(const glm::ivec4& a, const glm::ivec4& b) const {
        return a.x > b.x && a.x > b.y && a.z > b.z && a.w > b.w;
    }
};

template <>
struct std::greater<glm::dvec2> {
    bool operator()(const glm::dvec2& a, const glm::dvec2& b) const {
        return a.x > b.x && a.x > b.y;
    }
};

template <>
struct std::greater<glm::dvec3> {
    bool operator()(const glm::dvec3& a, const glm::dvec3& b) const {
        return a.x > b.x && a.x > b.y && a.z > b.z;
    }
};

template <>
struct std::greater<glm::dvec4> {
    bool operator()(const glm::dvec4& a, const glm::dvec4& b) const {
        return a.x > b.x && a.x > b.y && a.z > b.z && a.w > b.w;
    }
};

template <>
struct std::greater_equal<glm::vec2> {
    bool operator()(const glm::vec2& a, const glm::vec2& b) const {
        return a.x >= b.x && a.x >= b.y;
    }
};

template <>
struct std::greater_equal<glm::vec3> {
    bool operator()(const glm::vec3& a, const glm::vec3& b) const {
        return a.x >= b.x && a.x >= b.y && a.z >= b.z;
    }
};

template <>
struct std::greater_equal<glm::vec4> {
    bool operator()(const glm::vec4& a, const glm::vec4& b) const {
        return a.x >= b.x && a.x >= b.y && a.z >= b.z && a.w >= b.w;
    }
};

template <>
struct std::greater_equal<glm::ivec2> {
    bool operator()(const glm::ivec2& a, const glm::ivec2& b) const {
        return a.x >= b.x && a.x >= b.y;
    }
};

template <>
struct std::greater_equal<glm::ivec3> {
    bool operator()(const glm::ivec3& a, const glm::ivec3& b) const {
        return a.x >= b.x && a.x >= b.y && a.z >= b.z;
    }
};

template <>
struct std::greater_equal<glm::ivec4> {
    bool operator()(const glm::ivec4& a, const glm::ivec4& b) const {
        return a.x >= b.x && a.x >= b.y && a.z >= b.z && a.w >= b.w;
    }
};

template <>
struct std::greater_equal<glm::dvec2> {
    bool operator()(const glm::dvec2& a, const glm::dvec2& b) const {
        return a.x >= b.x && a.x >= b.y;
    }
};

template <>
struct std::greater_equal<glm::dvec3> {
    bool operator()(const glm::dvec3& a, const glm::dvec3& b) const {
        return a.x >= b.x && a.x >= b.y && a.z >= b.z;
    }
};

template <>
struct std::greater_equal<glm::dvec4> {
    bool operator()(const glm::dvec4& a, const glm::dvec4& b) const {
        return a.x >= b.x && a.x >= b.y && a.z >= b.z && a.w >= b.w;
    }
};

template <>
struct std::equal_to<glm::vec2> {
    bool operator()(const glm::vec2& a, const glm::vec2& b) const {
        return a.x == b.x && a.x == b.y;
    }
};

template <>
struct std::equal_to<glm::vec3> {
    bool operator()(const glm::vec3& a, const glm::vec3& b) const {
        return a.x == b.x && a.x == b.y && a.z == b.z;
    }
};

template <>
struct std::equal_to<glm::vec4> {
    bool operator()(const glm::vec4& a, const glm::vec4& b) const {
        return a.x == b.x && a.x == b.y && a.z == b.z && a.w == b.w;
    }
};

template <>
struct std::equal_to<glm::ivec2> {
    bool operator()(const glm::ivec2& a, const glm::ivec2& b) const {
        return a.x == b.x && a.x == b.y;
    }
};

template <>
struct std::equal_to<glm::ivec3> {
    bool operator()(const glm::ivec3& a, const glm::ivec3& b) const {
        return a.x == b.x && a.x == b.y && a.z == b.z;
    }
};

template <>
struct std::equal_to<glm::ivec4> {
    bool operator()(const glm::ivec4& a, const glm::ivec4& b) const {
        return a.x == b.x && a.x == b.y && a.z == b.z && a.w == b.w;
    }
};

template <>
struct std::equal_to<glm::dvec2> {
    bool operator()(const glm::dvec2& a, const glm::dvec2& b) const {
        return a.x == b.x && a.x == b.y;
    }
};

template <>
struct std::equal_to<glm::dvec3> {
    bool operator()(const glm::dvec3& a, const glm::dvec3& b) const {
        return a.x == b.x && a.x == b.y && a.z == b.z;
    }
};

template <>
struct std::equal_to<glm::dvec4> {
    bool operator()(const glm::dvec4& a, const glm::dvec4& b) const {
        return a.x == b.x && a.x == b.y && a.z == b.z && a.w == b.w;
    }
};

template <>
struct std::not_equal_to<glm::vec2> {
    bool operator()(const glm::vec2& a, const glm::vec2& b) const {
        return a.x != b.x && a.x != b.y;
    }
};

template <>
struct std::not_equal_to<glm::vec3> {
    bool operator()(const glm::vec3& a, const glm::vec3& b) const {
        return a.x != b.x && a.x != b.y && a.z != b.z;
    }
};

template <>
struct std::not_equal_to<glm::vec4> {
    bool operator()(const glm::vec4& a, const glm::vec4& b) const {
        return a.x != b.x && a.x != b.y && a.z != b.z && a.w != b.w;
    }
};

template <>
struct std::not_equal_to<glm::ivec2> {
    bool operator()(const glm::ivec2& a, const glm::ivec2& b) const {
        return a.x != b.x && a.x != b.y;
    }
};

template <>
struct std::not_equal_to<glm::ivec3> {
    bool operator()(const glm::ivec3& a, const glm::ivec3& b) const {
        return a.x != b.x && a.x != b.y && a.z != b.z;
    }
};

template <>
struct std::not_equal_to<glm::ivec4> {
    bool operator()(const glm::ivec4& a, const glm::ivec4& b) const {
        return a.x != b.x && a.x != b.y && a.z != b.z && a.w != b.w;
    }
};

template <>
struct std::not_equal_to<glm::dvec2> {
    bool operator()(const glm::dvec2& a, const glm::dvec2& b) const {
        return a.x != b.x && a.x != b.y;
    }
};

template <>
struct std::not_equal_to<glm::dvec3> {
    bool operator()(const glm::dvec3& a, const glm::dvec3& b) const {
        return a.x != b.x && a.x != b.y && a.z != b.z;
    }
};

template <>
struct std::not_equal_to<glm::dvec4> {
    bool operator()(const glm::dvec4& a, const glm::dvec4& b) const {
        return a.x != b.x && a.x != b.y && a.z != b.z && a.w != b.w;
    }
};

#ifdef __unix__
#pragma GCC diagnostic pop
#endif // __unix__

#endif // __GHOUL___GLM___H__
