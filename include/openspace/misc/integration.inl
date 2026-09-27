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

namespace ghoul {

template <typename T>
double integrateSimpsonsRule(double t0, double t1, int n, Integrand<T> f) {
    ghoul_assert(n >= 2, "Number of partitions, n, must be at least 2");

    // `n` must be an even number
    if (n % 2 != 0) {
        n += 1;
    }

    const double h = (t1 - t0) / static_cast<double>(n);
    const T endpoints = f(t0) + f(t1);
    T times4 = T(0);
    T times2 = T(0);

    // Weight 4
    for (int i = 1; i < n; i += 2) {
        double t = t0 + static_cast<double>(i) * h;
        times4 += f(t);
    }

    // Weight 2
    for (int i = 2; i < n; i += 2) {
        double t = t0 + static_cast<double>(i) * h;
        times2 += f(t);
    }

    return (h / 3.0) * (endpoints + 4.0 * times4 + 2.0 * times2);
}

template <typename T>
T integrateGaussianQuadrature(double t0, double t1, Integrand<T> f) {
    struct GaussLegendreCoefficient {
        double abscissa; // xi
        double weight;   // wi
    };

    static constexpr GaussLegendreCoefficient Coefficients[] = {
        { .abscissa = 0.0, .weight = 0.5688889 },
        { .abscissa = -0.5384693, .weight = 0.47862867 },
        { .abscissa = 0.5384693, .weight = 0.47862867 },
        { .abscissa = -0.90617985, .weight = 0.23692688 },
        { .abscissa = 0.90617985, .weight = 0.23692688 }
    };

    const double a = t0;
    const double b = t1;
    T sum = 0.0;
    for (const GaussLegendreCoefficient& coefficient : Coefficients) {
        // Change of interval from [-1, 1]  to [a, b] (also 0.5 * (b - a) below)
        double const t = 0.5 * ((b - a) * coefficient.abscissa + (b + a));
        sum += f(t) * coefficient.weight;
    }
    return 0.5 * (b - a) * sum;
}

} // namespace ghoul
