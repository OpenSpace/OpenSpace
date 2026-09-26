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

#include <ghoul/glm.h>
#include <ghoul/misc/assert.h>
#include <glm/gtc/constants.hpp>

namespace ghoul {

template <typename T>
EasingFunc<T> easingFunction(EasingFunction func) {
    switch (func) {
        case EasingFunction::Linear:               return linear<T>;
        case EasingFunction::QuadraticEaseIn:      return quadraticEaseIn<T>;
        case EasingFunction::QuadraticEaseOut:     return quadraticEaseOut<T>;
        case EasingFunction::QuadraticEaseInOut:   return quadraticEaseInOut<T>;
        case EasingFunction::CubicEaseIn:          return cubicEaseIn<T>;
        case EasingFunction::CubicEaseOut:         return cubicEaseOut<T>;
        case EasingFunction::CubicEaseInOut:       return cubicEaseInOut<T>;
        case EasingFunction::QuarticEaseIn:        return quarticEaseIn<T>;
        case EasingFunction::QuarticEaseOut:       return quarticEaseOut<T>;
        case EasingFunction::QuarticEaseInOut:     return quarticEaseInOut<T>;
        case EasingFunction::QuinticEaseIn:        return quinticEaseIn<T>;
        case EasingFunction::QuinticEaseOut:       return quinticEaseOut<T>;
        case EasingFunction::QuinticEaseInOut:     return quinticEaseInOut<T>;
        case EasingFunction::SineEaseIn:           return sineEaseIn<T>;
        case EasingFunction::SineEaseOut:          return sineEaseOut<T>;
        case EasingFunction::SineEaseInOut:        return sineEaseInOut<T>;
        case EasingFunction::CircularEaseIn:       return circularEaseIn<T>;
        case EasingFunction::CircularEaseOut:      return circularEaseOut<T>;
        case EasingFunction::CircularEaseInOut:    return circularEaseInOut<T>;
        case EasingFunction::ExponentialEaseIn:    return exponentialEaseIn<T>;
        case EasingFunction::ExponentialEaseOut:   return exponentialEaseOut<T>;
        case EasingFunction::ExponentialEaseInOut: return exponentialEaseInOut<T>;
        case EasingFunction::ElasticEaseIn:        return elasticEaseIn<T>;
        case EasingFunction::ElasticEaseOut:       return elasticEaseOut<T>;
        case EasingFunction::ElasticEaseInOut:     return elasticEaseInOut<T>;
        case EasingFunction::BounceEaseIn:         return bounceEaseIn<T>;
        case EasingFunction::BounceEaseOut:        return bounceEaseOut<T>;
        case EasingFunction::BounceEaseInOut:      return bounceEaseInOut<T>;
        default:                                   throw MissingCaseException();
    }
}

template <typename T>
T easing(T p, EasingFunction func) {
    EasingFunc<T> f = easingFunction<T>(func);
    return f(p);
}

template <typename T>
T linear(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    return p;
}

template <typename T>
T quadraticEaseIn(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    return p * p;
}

template <typename T>
T quadraticEaseOut(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    return -(p * (p - 2));
}

template <typename T>
T quadraticEaseInOut(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    if (p < 0.5) {
        return 2 * p * p;
    }
    else {
        return (-2 * p * p) + (4 * p) - 1;
    }
}

template <typename T>
T cubicEaseIn(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    return p * p * p;
}

template <typename T>
T cubicEaseOut(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    const T f = (p - 1);
    return f * f * f + 1;
}

template <typename T>
T cubicEaseInOut(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    if (p < 0.5) {
        return 4 * p * p * p;
    }
    else {
        const T f = ((2 * p) - 2);
        return T(0.5) * f * f * f + 1;
    }
}

template <typename T>
T quarticEaseIn(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    return p * p * p * p;
}

template <typename T>
T quarticEaseOut(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    const T f = (p - 1);
    return f * f * f * (1 - p) + 1;
}

template <typename T>
T quarticEaseInOut(T p)  {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    if (p < 0.5) {
        return 8 * p * p * p * p;
    }
    else {
        const T f = (p - 1);
        return -8 * f * f * f * f + 1;
    }
}

template <typename T>
T quinticEaseIn(T p)  {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    return p * p * p * p * p;
}

template <typename T>
T quinticEaseOut(T p)  {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    const T f = (p - 1);
    return f * f * f * f * f + 1;
}

template <typename T>
T quinticEaseInOut(T p)  {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    if (p < 0.5) {
        return 16 * p * p * p * p * p;
    }
    else {
        const T f = ((2 * p) - 2);
        return T(0.5) * f * f * f * f * f + 1;
    }
}

template <typename T>
T sineEaseIn(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    return glm::sin((p - 1) * glm::half_pi<T>()) + 1;
}

template <typename T>
T sineEaseOut(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    return glm::sin(p * glm::half_pi<T>());
}

template <typename T>
T sineEaseInOut(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    return T(0.5) * (1 - glm::cos(p * glm::pi<T>()));
}

template <typename T>
T circularEaseIn(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    return 1 - glm::sqrt(1 - (p * p));
}

template <typename T>
T circularEaseOut(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    return glm::sqrt((2 - p) * p);
}

template <typename T>
T circularEaseInOut(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    if (p < 0.5) {
        return T(0.5) * (1 - glm::sqrt(1 - 4 * (p * p)));
    }
    else {
        return T(0.5) * (glm::sqrt(-((2 * p) - 3) * ((2 * p) - 1)) + 1);
    }
}

template <typename T>
T exponentialEaseIn(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    return (p == 0.0) ? p : static_cast<T>(glm::pow(2, 10 * (p - 1)));
}

template <typename T>
T exponentialEaseOut(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    return (p == 1.0) ? p : static_cast<T>(1 - glm::pow(2, -10 * p));
}

template <typename T>
T exponentialEaseInOut(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    if (p == 0.0 || p == 1.0) {
        return p;
    }

    if (p < 0.5) {
        return static_cast<T>(0.5 * glm::pow(2, (20 * p) - 10));
    }
    else {
        return static_cast<T>(-0.5 * glm::pow(2, (-20 * p) + 10) + 1);
    }
}

template <typename T>
T elasticEaseIn(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    return static_cast<T>(
        glm::sin(13 * glm::half_pi<T>() * p) * glm::pow(2, 10 * (p - 1))
    );
}

template <typename T>
T elasticEaseOut(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    return static_cast<T>(
        glm::sin(-13 * glm::half_pi<T>() * (p + 1)) * glm::pow(2, -10 * p) + 1
    );
}

template <typename T>
T elasticEaseInOut(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    if (p < 0.5) {
        return static_cast<T>(
            0.5 * glm::sin(13 * glm::half_pi<T>() * (2 * p)) *
                glm::pow(2, 10 * ((2 * p) - 1))
        );
    }
    else {
        return static_cast<T>(
            0.5 * (glm::sin(-13 * glm::half_pi<T>() * ((2 * p - 1) + 1)) *
                glm::pow(2, -10 * (2 * p - 1)) + 2)
        );
    }
}

template <typename T>
T backEaseIn(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    return p * p * p - p * glm::sin(p * glm::pi<T>());
}

template <typename T>
T backEaseOut(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    const T f = (1 - p);
    return 1 - (f * f * f - f * glm::sin(f * glm::pi<T>()));
}

template <typename T>
T backEaseInOut(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    if (p < 0.5) {
        const T f = 2 * p;
        return 0.5 * (f * f * f - f * glm::sin(f * glm::pi<T>()));
    }
    else {
        const T f = (1 - (2*p - 1));
        return 0.5 * (1 - (f * f * f - f * glm::sin(f * glm::pi<T>()))) + 0.5;
    }
}

template <typename T>
T bounceEaseIn(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    return 1 - bounceEaseOut(1 - p);
}

template <typename T>
T bounceEaseOut(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    if (p < 4 / 11.0) {
        return static_cast<T>((121.0 * p * p) / 16.0);
    }
    else if (p < 8 / 11.0) {
        return static_cast<T>((363/40.0 * p * p) - (99/10.0 * p) + 17/5.0);
    }
    else if (p < 9 / 10.0) {
        return static_cast<T>((4356/361.0 * p * p) - (35442/1805.0 * p) + 16061/1805.0);
    }
    else {
        return static_cast<T>((54/5.0 * p * p) - (513/25.0 * p) + 268/25.0);
    }
}

template <typename T>
T bounceEaseInOut(T p) {
    ghoul_assert(p >= 0.f && p <= 1.f, "Interpolation variable p out of range [0,1]");
    if (p < 0.5) {
        return T(0.5) * bounceEaseIn(p * 2);
    }
    else {
        return T(0.5) * bounceEaseOut(p * 2 - 1) + T(0.5);
    }
}

} // namespace ghoul
