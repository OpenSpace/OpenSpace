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

namespace {
    template <typename T, typename V>
    T internalLinear(V t, const T& p0, const T& p1) {
        return t * p1 + (V(1.0) - t) * p0;
    }

    template <typename T, typename V>
    T internalCubicBezier(V t, const T& p0, const T& p1, const T& p2, const T& p3) {
        const V a = V(1.0) - t;
        return p0 * a * a * a
            + p1 * t * a * a * V(3.0)
            + p2 * t * t * a * V(3.0)
            + p3 * t * t * t;
    }

    template <typename T, typename V>
    T internalCatmullRom(V t, const T& p0, const T& p1, const T& p2, const T& p3) {
        const V t2 = t * t;
        const V t3 = t2 * t;

        return V(0.5) * (
            V(2.0) * p1 +
            t * (p2 - p0) +
            t2 * (V(2.0) * p0 - V(5.0) * p1 + V(4.0) * p2 - p3) +
            t3 * (V(3.0) * p1 - p0 - V(3.0) * p2 + p3)
        );
    }
} // namespace

namespace ghoul {

template <typename T>
T interpolateLinear(double t, const T& p0, const T& p1) {
    if constexpr (isGlmMatrix<T>() || isGlmVector<T>()) {
        return internalLinear(typename T::value_type(t), p0, p1);
    }
    else {
        return internalLinear(t, p0, p1);
    }
}

template <typename T>
T interpolateCubicBezier(double t, const T& p0, const T& p1, const T& p2, const T& p3) {
    if constexpr (isGlmMatrix<T>() || isGlmVector<T>()) {
        return internalCubicBezier(typename T::value_type(t), p0, p1, p2, p3);
    }
    else {
        return internalCubicBezier(t, p0, p1, p2, p3);
    }
}

template <typename T>
T interpolateCatmullRom(double t, const T& p0, const T& p1, const T& p2, const T& p3) {
    if constexpr (isGlmMatrix<T>() || isGlmVector<T>()) {
        return internalCatmullRom(typename T::value_type(t), p0, p1, p2, p3);
    }
    else {
        return internalCatmullRom(t, p0, p1, p2, p3);
    }
}

} // namespace ghoul
