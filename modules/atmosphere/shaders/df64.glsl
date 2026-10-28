/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

/**
 * df64.glsl — Full-precision Dekker/Veltkamp two-float emulated 64-bit arithmetic.
 *
 * A "double-float" (df64) value is stored as a vec2(hi, lo) where:
 *   hi + lo == the exact double value (to ~48-53 bits of precision).
 *
 * References:
 *   Dekker (1971) — "A floating-point technique for extending the available precision"
 *   Veltkamp (1971) — Splitting of floating-point numbers
 *   Hida, Li & Bailey (2000) — "Algorithms for quad-double arithmetic"
 */

#ifndef _ATMOSPHERE___DF64___GLSL_
#define _ATMOSPHERE___DF64___GLSL_

// ---------------------------------------------------------------------------
// Scalar df64 arithmetic
// ---------------------------------------------------------------------------

// Two-sum: exact sum of two floats returned as (sum, roundingError).
// Satisfies: a + b == result.x + result.y exactly.
vec2 df64_twoSum(float a, float b) {
    float s = a + b;
    float v = s - a;
    float e = (a - (s - v)) + (b - v);
    return vec2(s, e);
}

// Two-diff: exact difference a - b as (diff, roundingError).
// Satisfies: a - b == result.x + result.y exactly.
vec2 df64_twoDiff(float a, float b) {
    float s = a - b;
    float v = s - a;
    float e = (a - (s - v)) - (b + v);
    return vec2(s, e);
}

// Veltkamp split: splits a float into two non-overlapping halves.
// For fp32 (24-bit mantissa), split at bit 12: SPLIT = 2^12 + 1 = 4097.
vec2 df64_split(float a) {
    const float C = 4097.0;
    float t  = C * a;
    float hi = t - (t - a);
    float lo = a - hi;
    return vec2(hi, lo);
}

// Exact product of two floats as (product, roundingError) using hardware FMA.
// Satisfies: a * b == result.x + result.y exactly, with zero intermediate overflow risk.
vec2 df64_twoProd(float a, float b) {
    float p = a * b;
    float e = fma(a, b, -p);
    return vec2(p, e);
}

// Add two df64 values: a + b
vec2 df64_add(vec2 a, vec2 b) {
    vec2 s = df64_twoSum(a.x, b.x);
    s.y   += a.y + b.y;
    return df64_twoSum(s.x, s.y);
}

// Subtract two df64 values: a - b
vec2 df64_sub(vec2 a, vec2 b) {
    vec2 s = df64_twoDiff(a.x, b.x);
    s.y   += a.y - b.y;
    return df64_twoSum(s.x, s.y);
}

// Multiply two df64 values: a * b
vec2 df64_mul(vec2 a, vec2 b) {
    vec2 p = df64_twoProd(a.x, b.x);
    p.y   += a.x * b.y + a.y * b.x;
    return df64_twoSum(p.x, p.y);
}

// Multiply a df64 value by a single float: a * b
vec2 df64_mul_float(vec2 a, float b) {
    vec2 p = df64_twoProd(a.x, b);
    p.y   += a.y * b;
    return df64_twoSum(p.x, p.y);
}

// Divide two df64 values: a / b
vec2 df64_div(vec2 a, vec2 b) {
    if (b.x == 0.0) return vec2(0.0);
    float q1 = a.x / b.x;
    vec2  r  = df64_sub(a, df64_mul_float(b, q1));
    float q2 = r.x / b.x;
    return df64_twoSum(q1, q2);
}

// sqrt of a df64 value
vec2 df64_sqrt(vec2 a) {
    if (a.x <= 0.0) return vec2(0.0);
    float x = inversesqrt(a.x);
    float y = a.x * x;
    vec2  ys = df64_twoProd(y, y);
    vec2  e  = df64_sub(a, ys);
    return df64_twoSum(y, e.x * (x * 0.5));
}

// Convert a df64 to a single float
float df64_toFloat(vec2 a) {
    return a.x + a.y;
}

// ---------------------------------------------------------------------------
// Vector & matrix df64 types and operations
// ---------------------------------------------------------------------------

struct df64_vec3 {
    vec3 hi;
    vec3 lo;
};

struct df64_vec4 {
    vec4 hi;
    vec4 lo;
};

// Subtract two df64_vec3 values: a - b
df64_vec3 df64_vec3_sub(df64_vec3 a, df64_vec3 b) {
    vec2 rx = df64_sub(vec2(a.hi.x, a.lo.x), vec2(b.hi.x, b.lo.x));
    vec2 ry = df64_sub(vec2(a.hi.y, a.lo.y), vec2(b.hi.y, b.lo.y));
    vec2 rz = df64_sub(vec2(a.hi.z, a.lo.z), vec2(b.hi.z, b.lo.z));
    return df64_vec3(vec3(rx.x, ry.x, rz.x), vec3(rx.y, ry.y, rz.y));
}

// Add two df64_vec3 values: a + b
df64_vec3 df64_vec3_add(df64_vec3 a, df64_vec3 b) {
    vec2 rx = df64_add(vec2(a.hi.x, a.lo.x), vec2(b.hi.x, b.lo.x));
    vec2 ry = df64_add(vec2(a.hi.y, a.lo.y), vec2(b.hi.y, b.lo.y));
    vec2 rz = df64_add(vec2(a.hi.z, a.lo.z), vec2(b.hi.z, b.lo.z));
    return df64_vec3(vec3(rx.x, ry.x, rz.x), vec3(rx.y, ry.y, rz.y));
}

// Multiply df64_vec3 by scalar float: a * f
df64_vec3 df64_vec3_mul_float(df64_vec3 a, float f) {
    vec2 rx = df64_mul_float(vec2(a.hi.x, a.lo.x), f);
    vec2 ry = df64_mul_float(vec2(a.hi.y, a.lo.y), f);
    vec2 rz = df64_mul_float(vec2(a.hi.z, a.lo.z), f);
    return df64_vec3(vec3(rx.x, ry.x, rz.x), vec3(rx.y, ry.y, rz.y));
}

// Euclidean length of a df64_vec3
float df64_vec3_length(df64_vec3 a) {
    vec2 x2 = df64_mul(vec2(a.hi.x, a.lo.x), vec2(a.hi.x, a.lo.x));
    vec2 y2 = df64_mul(vec2(a.hi.y, a.lo.y), vec2(a.hi.y, a.lo.y));
    vec2 z2 = df64_mul(vec2(a.hi.z, a.lo.z), vec2(a.hi.z, a.lo.z));
    vec2 len2 = df64_add(df64_add(x2, y2), z2);
    vec2 len = df64_sqrt(len2);
    return df64_toFloat(len);
}

// Recombine df64_vec3 to single-precision vec3
vec3 df64_vec3_to_vec3(df64_vec3 a) {
    return a.hi + a.lo;
}

// Recombine df64_vec4 to single-precision vec4
vec4 df64_vec4_to_vec4(df64_vec4 a) {
    return a.hi + a.lo;
}

// Multiply split dmat4 by single-precision vec4: M * v
df64_vec4 df64_mat4_mul_vec4(mat4 M_hi, mat4 M_lo, vec4 v) {
    df64_vec4 res;
    for (int i = 0; i < 4; ++i) {
        vec2 p0 = df64_mul_float(vec2(M_hi[0][i], M_lo[0][i]), v.x);
        vec2 p1 = df64_mul_float(vec2(M_hi[1][i], M_lo[1][i]), v.y);
        vec2 p2 = df64_mul_float(vec2(M_hi[2][i], M_lo[2][i]), v.z);
        vec2 p3 = df64_mul_float(vec2(M_hi[3][i], M_lo[3][i]), v.w);
        vec2 sum = df64_add(df64_add(p0, p1), df64_add(p2, p3));
        res.hi[i] = sum.x;
        res.lo[i] = sum.y;
    }
    return res;
}

// Multiply split dmat4 by df64_vec4: M * v
df64_vec4 df64_mat4_mul_df64_vec4(mat4 M_hi, mat4 M_lo, df64_vec4 v) {
    df64_vec4 res;
    for (int i = 0; i < 4; ++i) {
        vec2 p0 = df64_mul(vec2(M_hi[0][i], M_lo[0][i]), vec2(v.hi.x, v.lo.x));
        vec2 p1 = df64_mul(vec2(M_hi[1][i], M_lo[1][i]), vec2(v.hi.y, v.lo.y));
        vec2 p2 = df64_mul(vec2(M_hi[2][i], M_lo[2][i]), vec2(v.hi.z, v.lo.z));
        vec2 p3 = df64_mul(vec2(M_hi[3][i], M_lo[3][i]), vec2(v.hi.w, v.lo.w));
        vec2 sum = df64_add(df64_add(p0, p1), df64_add(p2, p3));
        res.hi[i] = sum.x;
        res.lo[i] = sum.y;
    }
    return res;
}

// Perspective projection: homogeneous coordinates to 3D Cartesian coordinates (v.xyz / v.w)
df64_vec3 df64_project(df64_vec4 v) {
    vec2 invW = df64_div(vec2(1.0, 0.0), vec2(v.hi.w, v.lo.w));
    vec2 x = df64_mul(vec2(v.hi.x, v.lo.x), invW);
    vec2 y = df64_mul(vec2(v.hi.y, v.lo.y), invW);
    vec2 z = df64_mul(vec2(v.hi.z, v.lo.z), invW);
    return df64_vec3(vec3(x.x, y.x, z.x), vec3(x.y, y.y, z.y));
}

#endif // _ATMOSPHERE___DF64___GLSL_
