/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

// Atmosphere Rendering Parameters 
uniform float Rg;
uniform float Rt;
uniform float AverageGroundReflectance;
uniform float HR;
uniform vec3 betaRayleigh;
uniform float HM;
uniform vec3 betaMieScattering;
uniform vec3 betaMieExtinction;
uniform float mieG;
uniform float sunRadiance;

const float ATM_EPSILON = 1.0;
// const float RL = Rt + 1.0;

// const float Rg = 6360.0;
// const float Rt = 6420.0;
// const float RL = 6421.0;
// const float ATM_EPSILON = 1.0;

// const float AVERAGE_GROUND_REFLECTANCE = 0.1;

// // Rayleigh
// const float HR = 8.0;
// const vec3 betaR = vec3(5.8e-3, 1.35e-2, 3.31e-2);

// // Mie
// // DEFAULT
// const float HM = 1.2;
// const vec3 betaMSca = vec3(4e-3);
// //const vec3 betaMSca = vec3(2e-5);
// const vec3 betaMEx = betaMSca / 0.9;
// const float mieG = 1.0;

// Integration steps
const int TRANSMITTANCE_STEPS = 500;
const int INSCATTER_INTEGRAL_SAMPLES = 50;
const int IRRADIANCE_INTEGRAL_SAMPLES = 32;
const int INSCATTER_SPHERICAL_INTEGRAL_SAMPLES = 16;

// The next values crash NVIDIA driver for Quadro K620 -- JCC
// const int TRANSMITTANCE_INTEGRAL_SAMPLES = 1000;
// const int INSCATTER_INTEGRAL_SAMPLES = 100;
// const int IRRADIANCE_INTEGRAL_SAMPLES = 64;
// const int INSCATTER_SPHERICAL_INTEGRAL_SAMPLES = 32;

const float M_PI = 3.141592657;

const int TRANSMITTANCE_W = 256;
const int TRANSMITTANCE_H = 64;

const int SKY_W = 64;
const int SKY_H = 16;

const int OTHER_TEXTURES_W = 64;
const int OTHER_TEXTURES_H = 16;


const int RES_R = 32;
const int RES_MU = 128;
const int RES_MU_S = 32;
const int RES_NU = 8;
