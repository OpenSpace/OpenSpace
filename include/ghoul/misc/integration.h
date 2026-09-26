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

#ifndef __GHOUL___INTEGRATION___H__
#define __GHOUL___INTEGRATION___H__

#include <functional>

namespace ghoul {

template <typename T> using Integrand = std::function<T(double)>;

/**
 * Compute the approximate integral of integrand \p f numerially using Simpson's Rule. The
 * interval for the integration is given by \p t0 and \p t1.
 *
 * \param t0 The lower bound for the integration interval
 * \param t1 The upper bound for the integration interval
 * \param n The resolution for the integration. Should be an even number
 * \param f The integrand for the integration, as a function of t (double)
 * \return The approximated integral of function \p f over the interval [t0, t1]
 */
template <typename T>
T integrateSimpsonsRule(double t0, double t1, int n, Integrand<T> f);

/**
 * Compute the approximate integral of integrand \p f numerially using 5-point Gaussian
 * quadrature with Legendre points. This should be exact for polyniomial functions of
 * degree 9 or less. https://en.wikipedia.org/wiki/Gaussian_quadrature
 * The interval for the integration is given by \p t0 and \p t1.
 *
 * \param t0 The lower bound for the integration interval
 * \param t1 The upper bound for the integration interval
 * \param f The integrand for the integration, as a function of t (double)
 * \return The approximated integral of function \p f over the interval [t0, t1]
 */
template <typename T>
T integrateGaussianQuadrature(double t0, double t1, Integrand<T> f);

} // namespace ghoul

#include "integration.inl"

#endif // __GHOUL___INTEGRATION___H__
