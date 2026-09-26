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
 *****************************************************************************************
 * File based on 'easing.c' by Auerhaus Development, LLC and used under the Do What The  *
 * Fuck You Want To Public License, Version 2 as published by Sam Hocevar. See           *
 * http://sam.zoy.org/wtfpl/COPYING for more details.                                    *
 ****************************************************************************************/

#ifndef __GHOUL___EASING___H__
#define __GHOUL___EASING___H__

#include <string_view>

namespace ghoul {

enum class EasingFunction : uint8_t {
    // @FRAGILE(abock):  The cpp expects the values to be in this order and start at 0
    Linear = 0,
    QuadraticEaseIn,   QuadraticEaseOut,   QuadraticEaseInOut,
    CubicEaseIn,       CubicEaseOut,       CubicEaseInOut,
    QuarticEaseIn,     QuarticEaseOut,     QuarticEaseInOut,
    QuinticEaseIn,     QuinticEaseOut,     QuinticEaseInOut,
    SineEaseIn,        SineEaseOut,        SineEaseInOut,
    CircularEaseIn,    CircularEaseOut,    CircularEaseInOut,
    ExponentialEaseIn, ExponentialEaseOut, ExponentialEaseInOut,
    ElasticEaseIn,     ElasticEaseOut,     ElasticEaseInOut,
    BounceEaseIn,      BounceEaseOut,      BounceEaseInOut
};

/**
 * Function pointer representing any of the easing functions defined in this file.
 */
template <typename T> using EasingFunc = T(*)(T);

/**
 * Returns the easing function that is named by \p func.
 *
 * \param func The enum that defines the easing function that is returned by this function
 * \return A function pointer to the easing function that is to be called
 */
template <typename T>
[[nodiscard]] EasingFunc<T> easingFunction(EasingFunction func);

/**
 * Converts the passed \p func enum into a textual representation, which can be used for
 * the #easingFunctionFromName to recreate the same enum.
 *
 * \param func The easing function for which the name should be retrieved
 * \return The name of the easing function
 */
[[nodiscard]] std::string_view nameForEasingFunction(EasingFunction func);

/**
 * Returns the enum for the passed \p name of an easing function. If \p name is not a
 * valid easing function name, a ghoul::MissingCaseException is raised.
 *
 * \param name The name for which the easing function enum should be returned
 * \return The enum value for the easing function named \p name
 *
 * \throw std::invalid_argument If \p name is not a valid name for an easing function
 * \pre \p name must not be `nullptr`
 */
[[nodiscard]] EasingFunction easingFunctionFromName(std::string_view name);

/**
 * Returns whether \p name is naming a valid easing function. If this function returns
 * `true` and \p name is passed to #easingFunctionFromName, the function is guaranteed to
 * not throw.
 *
 * \param name The name for which should be tested whether it is a valid easing function
 *        name
 * \return `true` if \p name is a valid name for an easing function, `false` otherwise
 *
 * \pre \p name must not be `nullptr`
 */
[[nodiscard]] bool isValidEasingFunctionName(std::string_view name);

/**
 * Interpolates the parameter \p p, which has to be in [0,1] using the easing function
 * named by \p func, which is used to a call for #easingFunction to determine which easing
 * function is used.
 *
 * \param p The parameter to be interpolated in [0,1]
 * \param func The enum that defines the easing function that is used to ease/interpolate
 *        \p p
 * \return The interpolated value using the easing function defined by \p func
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T easing(T p, EasingFunction func);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the line
 * `y = x`.
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T linear(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the parabola
 * `y = x^2`.
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T quadraticEaseIn(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the parabola
 * `y = -x^2 + 2x`.
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T quadraticEaseOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the piecewise
 * quadratic:
 * ```
 * y = (1/2)((2x)^2)             ; [0, 0.5)
 * y = -(1/2)((2x-1)*(2x-3) - 1) ; [0.5, 1]
 * ```
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T quadraticEaseInOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the cubic
 * `y = x^3`.
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T cubicEaseIn(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the cubic
 * `y = (x - 1)^3 + 1`.
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T cubicEaseOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the piecewise
 * cubic:
 * ```
 * y = (1/2)((2x)^3)       ; [0, 0.5)
 * y = (1/2)((2x-2)^3 + 2) ; [0.5, 1]
 * ```
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T cubicEaseInOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the quartic
 * `y = x^4`.
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T quarticEaseIn(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the quartic
 * `y = 1 - (x - 1)^4`.
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T quarticEaseOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the piecewise
 * quartic:
 * ```
 * y = (1/2)((2x)^4)        ; [0, 0.5)
 * y = -(1/2)((2x-2)^4 - 2) ; [0.5, 1]
 * ```
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T quarticEaseInOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the quintic
 * `y = x^5`.
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T quinticEaseIn(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the quintic
 * `y = (x - 1)^5 + 1`.
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T quinticEaseOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the piecewise
 * quintic:
 * ```
 * y = (1/2)((2x)^5)       ; [0, 0.5)
 * y = (1/2)((2x-2)^5 + 2) ; [0.5, 1]
 * ```
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T quinticEaseInOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the
 * quarter-cycle of sine wave.
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T sineEaseIn(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the
 * quarter-cycle of sine wave (different phase).
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T sineEaseOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the half sine
 * wave.
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T sineEaseInOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the shifted
 * quadrant IV of unit circle.
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T circularEaseIn(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the shifted
 * quadrant II of unit circle.
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T circularEaseOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the piecewise
 * circular function:
 * ```
 * y = (1/2)(1 - sqrt(1 - 4x^2))           ; [0, 0.5)
 * y = (1/2)(sqrt(-(2x - 3)*(2x - 1)) + 1) ; [0.5, 1]
 * ```
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T circularEaseInOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the
 * exponential function: `y = 2^(10(x - 1))`
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T exponentialEaseIn(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the
 * exponential function: `y = -2^(-10x) + 1`
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T exponentialEaseOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the
 * piecewise exponential function:
 * ```
 * y = (1/2)2^(10(2x - 1))         ; [0,0.5)
 * y = -(1/2)*2^(-10(2x - 1))) + 1 ; [0.5,1]
 * ```
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T exponentialEaseInOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the damped
 * sine wave: `y = sin(13pi/2*x)*pow(2, 10 * (x - 1))`
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T elasticEaseIn(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the damped
 * sine wave: `y = sin(-13pi/2*(x + 1))*pow(2, -10x) + 1`
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T elasticEaseOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the piecewise
 * exponentially-damped sine wave:
 * ```
 * y = (1/2)*sin(13pi/2*(2*x))*pow(2, 10 * ((2*x) - 1))      ; [0,0.5)
 * y = (1/2)*(sin(-13pi/2*((2x-1)+1))*pow(2,-10(2*x-1)) + 2) ; [0.5, 1]
 * ```
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T elasticEaseInOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the
 * overshooting cubic: `y = x^3-x*sin(x*pi)`
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T backEaseIn(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the
 * overshooting cubic: `y = 1-((1-x)^3-(1-x)*sin((1-x)*pi))`
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T backEaseOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the piecewise
 * overshooting cubic function:
 * ```
 * y = (1/2)*((2x)^3-(2x)*sin(2*x*pi))           ; [0, 0.5)
 * y = (1/2)*(1-((1-x)^3-(1-x)*sin((1-x)*pi))+1) ; [0.5, 1]
 * ```
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T backEaseInOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the quadratic
 * equation: `ax^2 + bx + c`.
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T bounceEaseIn(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the quadratic
 * equation: `ax^2 + bx + c`.
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T bounceEaseOut(T p);

/**
 * Interpolates the parameter \p p, which has to be in [0,1]. Modeled after the quadratic
 * equation: `ax^2 + bx + c`.
 *
 * \tparam T The type of the value to be interpreted. Must be an arithmetic type
 * \param p The parameter to be interpolated in [0,1]
 * \return The interpolated value; in [0,1]
 *
 * \pre \p p must be in [0,1]
 */
template <typename T>
[[nodiscard]] T bounceEaseInOut(T p);

} // namespace ghoul

#include "easing.inl"

#endif // __GHOUL___EASING___H__
