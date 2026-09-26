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

#include <ghoul/misc/easing.h>

#include <ghoul/format.h>
#include <algorithm>
#include <array>
#include <type_traits>

namespace {
    constexpr std::array<std::string_view, 28> EaseFuncNames = {
        "Linear",
        "QuadraticEaseIn",   "QuadraticEaseOut",   "QuadraticEaseInOut",
        "CubicEaseIn",       "CubicEaseOut",       "CubicEaseInOut",
        "QuarticEaseIn",     "QuarticEaseOut",     "QuarticEaseInOut",
        "QuinticEaseIn",     "QuinticEaseOut",     "QuinticEaseInOut",
        "SineEaseIn",        "SineEaseOut",        "SineEaseInOut",
        "CircularEaseIn",    "CircularEaseOut",    "CircularEaseInOut",
        "ExponentialEaseIn", "ExponentialEaseOut", "ExponentialEaseInOut",
        "ElasticEaseIn",     "ElasticEaseOut",     "ElasticEaseInOut",
        "BounceEaseIn",      "BounceEaseOut",      "BounceEaseInOut"
    };
} // namespace

namespace ghoul {

std::string_view nameForEasingFunction(EasingFunction func) {
    return EaseFuncNames[static_cast<std::underlying_type_t<EasingFunction>>(func)];
}

EasingFunction easingFunctionFromName(std::string_view name) {
    for (size_t i = 0; i < EaseFuncNames.size(); i++) {
        if (EaseFuncNames[i] == name) {
            return EasingFunction(i);
        }
    }
    throw std::invalid_argument(std::format(
        "Could not find easing function with name '{}'", name
    ));
}

bool isValidEasingFunctionName(std::string_view name) {
    const auto it = std::find(EaseFuncNames.begin(), EaseFuncNames.end(), name);
    return it != EaseFuncNames.end();
}

namespace {
    // Just instantiate them once to see that they compile and don't throw warnings
    [[maybe_unused]] float _f = linear(0.f);
    [[maybe_unused]] float _f1 = quadraticEaseIn(0.f);
    [[maybe_unused]] float _f2 = quadraticEaseOut(0.f);
    [[maybe_unused]] float _f3 = quadraticEaseInOut(0.f);
    [[maybe_unused]] float _f4 = cubicEaseIn(0.f);
    [[maybe_unused]] float _f5 = cubicEaseOut(0.f);
    [[maybe_unused]] float _f6 = cubicEaseInOut(0.f);
    [[maybe_unused]] float _f7 = quarticEaseIn(0.f);
    [[maybe_unused]] float _f8 = quarticEaseOut(0.f);
    [[maybe_unused]] float _f9 = quarticEaseInOut(0.f);
    [[maybe_unused]] float _f10 = quinticEaseIn(0.f);
    [[maybe_unused]] float _f11 = quinticEaseOut(0.f);
    [[maybe_unused]] float _f12 = quinticEaseInOut(0.f);
    [[maybe_unused]] float _f13 = quinticEaseIn(0.f);
    [[maybe_unused]] float _f14 = quinticEaseOut(0.f);
    [[maybe_unused]] float _f15 = quinticEaseInOut(0.f);
    [[maybe_unused]] float _f16 = sineEaseIn(0.f);
    [[maybe_unused]] float _f17 = sineEaseOut(0.f);
    [[maybe_unused]] float _f18 = sineEaseInOut(0.f);
    [[maybe_unused]] float _f19 = circularEaseIn(0.f);
    [[maybe_unused]] float _f20 = circularEaseOut(0.f);
    [[maybe_unused]] float _f21 = circularEaseInOut(0.f);
    [[maybe_unused]] float _f22 = exponentialEaseIn(0.f);
    [[maybe_unused]] float _f23 = exponentialEaseOut(0.f);
    [[maybe_unused]] float _f24 = exponentialEaseInOut(0.f);
    [[maybe_unused]] float _f25 = elasticEaseIn(0.f);
    [[maybe_unused]] float _f26 = elasticEaseInOut(0.f);
    [[maybe_unused]] float _f27 = elasticEaseOut(0.f);
    [[maybe_unused]] float _f28 = bounceEaseIn(0.f);
    [[maybe_unused]] float _f29 = bounceEaseOut(0.f);
    [[maybe_unused]] float _f30 = bounceEaseInOut(0.f);

    [[maybe_unused]] double _d = linear(0.0);
    [[maybe_unused]] double _d1 = quadraticEaseIn(0.0);
    [[maybe_unused]] double _d2 = quadraticEaseOut(0.0);
    [[maybe_unused]] double _d3 = quadraticEaseInOut(0.0);
    [[maybe_unused]] double _d4 = cubicEaseIn(0.0);
    [[maybe_unused]] double _d5 = cubicEaseOut(0.0);
    [[maybe_unused]] double _d6 = cubicEaseInOut(0.0);
    [[maybe_unused]] double _d7 = quarticEaseIn(0.0);
    [[maybe_unused]] double _d8 = quarticEaseOut(0.0);
    [[maybe_unused]] double _d9 = quarticEaseInOut(0.0);
    [[maybe_unused]] double _d10 = quinticEaseIn(0.0);
    [[maybe_unused]] double _d11 = quinticEaseOut(0.0);
    [[maybe_unused]] double _d12 = quinticEaseInOut(0.0);
    [[maybe_unused]] double _d13 = quinticEaseIn(0.0);
    [[maybe_unused]] double _d14 = quinticEaseOut(0.0);
    [[maybe_unused]] double _d15 = quinticEaseInOut(0.0);
    [[maybe_unused]] double _d16 = sineEaseIn(0.0);
    [[maybe_unused]] double _d17 = sineEaseOut(0.0);
    [[maybe_unused]] double _d18 = sineEaseInOut(0.0);
    [[maybe_unused]] double _d19 = circularEaseIn(0.0);
    [[maybe_unused]] double _d20 = circularEaseOut(0.0);
    [[maybe_unused]] double _d21 = circularEaseInOut(0.0);
    [[maybe_unused]] double _d22 = exponentialEaseIn(0.0);
    [[maybe_unused]] double _d23 = exponentialEaseOut(0.0);
    [[maybe_unused]] double _d24 = exponentialEaseInOut(0.0);
    [[maybe_unused]] double _d25 = elasticEaseIn(0.0);
    [[maybe_unused]] double _d26 = elasticEaseInOut(0.0);
    [[maybe_unused]] double _d27 = elasticEaseOut(0.0);
    [[maybe_unused]] double _d28 = bounceEaseIn(0.0);
    [[maybe_unused]] double _d29 = bounceEaseOut(0.0);
    [[maybe_unused]] double _d30 = bounceEaseInOut(0.0);
} // namespace

} // namespace ghoul
