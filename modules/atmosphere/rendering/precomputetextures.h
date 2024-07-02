/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_MODULE_ATMOSPHERE___PRECOMPUTETEXTURES___H__
#define __OPENSPACE_MODULE_ATMOSPHERE___PRECOMPUTETEXTURES___H__

#include <ghoul/glm.h>

struct CPUTexture;

namespace openspace {

namespace atmosphere {

struct CPUTexture {
    enum class Format {
        RGB = 3,
        RGBA = 4
    };

    CPUTexture() = default;
    CPUTexture(int w, int h, Format c, double value = 255.0)
        : width{ w }, height{ h }, components{ static_cast<int>(c) } {
        data = std::vector<double>(width * height * components, value);
    }
    CPUTexture(const glm::ivec2& size, Format c, double value = 255.0)
        : CPUTexture(size.x, size.y, c, value) {}

    void createGPUTexture(std::string_view name) {
        // TODO: find better way without having to copy all data and cast it to floats
        // before uploading to GPU, e.g., reinterpret cast or something?
        std::vector<float> newdata(width * height * components, 0.f);
        int k = 0;
        for (double d : data) {
            newdata[k++] = static_cast<float>(d);
        }
        GLuint t = 0;
        glGenTextures(1, &t);
        glBindTexture(GL_TEXTURE_2D, t);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
        glTexImage2D(
            GL_TEXTURE_2D,
            0,
            (components == 3) ? GL_RGB32F : GL_RGBA32F,
            width,
            height,
            0,
            (components == 3) ? GL_RGB : GL_RGBA,
            GL_FLOAT,
            newdata.data()
        );

        if (glbinding::Binding::ObjectLabel.isResolved()) {
            glObjectLabel(GL_TEXTURE, t, static_cast<GLsizei>(name.size()), name.data());
        }
        glTex = t;
    }


    int width = 0;
    int height = 0;
    std::vector<double> data;
    int components = 0;
    GLuint glTex = 0;
};

using CPUTexture3D = std::vector<CPUTexture>;

double safeSqrt(double x);

namespace common {
    // In the following shaders r (altitude) is the length of vector/position x in the
    // atmosphere (or on the top of it when considering an observer in space), where the light
    // is coming from the opposite direction of the view direction, here the vector v or
    // viewDirection. Rg is the planet radius and Rt the atmosphere radius.

    // Calculate the distance of the ray starting at x (height r) until the planet's ground
    // or top of atmosphere
    // r := || vec(x) || e [0, Rt]
    // mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
    double rayDistance(double r, double mu, double Rt, double Rg);

     // biliniear interpolate a texture, clamps the texture coordinates to(0, size - 1)
     // Assumes x and y lookup coordinates are given as decimals (0,1)
    glm::dvec4 texture(const CPUTexture& tex, double x, double y);
    glm::dvec4 texture(const CPUTexture3D& tex, const glm::dvec3& pos);

    // Function to access the transmittance texture. Given r and mu, returns the
    // transmittance of a ray starting at vec(x), height r, and direction vec(v), mu, and
    // length until it hits the ground or the top of atmosphere.
    // r := height of starting point vect(x)
    // mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
    glm::dvec3 transmittance(const atmosphere::CPUTexture& tex, double r, double mu,
        double Rg, double Rt
    );

    // Given a position r and direction mu, calculates de transmittance along the ray with
    // length d. This function uses the propriety of Transmittance:
    // T(a,b) = TableT(a,v)/TableT(b, v)
    // r := height of starting point vect(x)
    // mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
    glm::dvec3 transmittance(const atmosphere::CPUTexture& tex, double r, double mu, double d,
        double Rg, double Rt);

    // Given the windows's fragment coordinates, for a defined view port, gives back the
    // interpolated r e [Rg, Rt] and mu, muSun amd nu e [-1, 1]
    // r := height of starting point vect(x)
    // mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
    // muSun := cosine of the zeith angle of vec(s). Or muSun = (vec(s) * vec(v))
    // nu := cosone of the angle between vec(s) and vec(v)
    // dhdH := it is a vec4. dhdH.x stores the dminT := Rt - r, dhdH.y stores the dH value
    //         (see paper), dhdH.z stores dminG := r - Rg and dhdH.w stores dh (see paper)
    glm::dvec3 unmappingMuMuSunNu(double r, const glm::dvec4& dhdH, int SAMPLES_MU, double Rg,
        double Rt, int SAMPLES_MU_S, int SAMPLES_NU, int x, int y);

    // Calculates Rayleigh phase function given the scattering cosine angle mu
    // mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
    double rayleighPhaseFunction(double mu);

    // Calculates Mie phase function given the scattering cosine angle mu
    // mu   := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v)) / r
    // mieG := mie phase function value
    double miePhaseFunction(double mu, double mieG);

    // Given the height rm view-zenith angle (cosine) mu, sun-zenith angle (cosine) muSun
    // and the angle (cosine) between the vec(s) and vec(v), nu, we access the 3D
    // textures and interpolate between them (r) to find the value for the 4D texture.
    // r := height of starting point vect(x)
    // mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
    // muSun := cosine of the zeith angle of vec(s). Or muSun = (vec(s) * vec(v))
    // nu := cosine of the angle between vec(s) and vec(v)
    glm::dvec4 texture4D(const CPUTexture3D& table, double r, double mu, double muSun,
        double nu, double Rg, int samplesMu, double Rt, int samplesR, int samplesMuS,
        int samplesNu);

} // namespace common

namespace transmittance {

void calculateTransmittance(CPUTexture& texture, double Rg, double Rt, double HR,
    const glm::dvec3 betaRayleigh, double HO, const glm::dvec3& betaOzoneExtinction,
    double HM, const glm::dvec3& betaMieExtinction, bool ozoneLayerEnabled);

} // namespace transmittance

namespace irradiance {
CPUTexture calculateIrradiance(const glm::ivec2& _tableSize);

CPUTexture calculateDeltaE(const glm::ivec2& deltaETableSize,
    const CPUTexture& transmittance, double Rg, double Rt);

void calculateDeltaE(int scatteringOrder, CPUTexture& deltaETexture,
    const CPUTexture3D& deltaSRTexture, const CPUTexture3D& deltaSMTexture, double Rg,
    double Rt, double mieG, const glm::ivec2 SKY, int SAMPLES_MU_S, int SAMPLES_NU,
    int SAMPLES_MU, int SAMPLES_R);

void calculateIrradiance(int scatteringOrder, CPUTexture& irradianceTexture,
    const CPUTexture& deltaETexture);

} // namespace irradiance

namespace inscattering {
std::pair<CPUTexture3D, CPUTexture3D> calculateDeltaS(
    const glm::ivec3& textureSize, const CPUTexture& transmittanceTexture, double Rg,
    double Rt, double HR, const glm::dvec3& betaRayleigh, double HM,
    const glm::dvec3& betaMiescattering, int SAMPLES_MU_S, int SAMPLES_NU, int SAMPLES_MU,
    bool ozoneLayerEnabled, double HO);

void calculateDeltaS(int inscatteringOrder, CPUTexture3D& deltaSRayleighTexture,
    const CPUTexture3D& deltaJTexture, const CPUTexture& transmittanceTexture, double Rg,
    double Rt, int SAMPLES_MU_S, int SAMPLES_NU, int SAMPLES_MU, int SAMPLES_R);

CPUTexture3D calculateInscattering(const CPUTexture3D& deltaSRayleighTable,
    const CPUTexture3D& deltaSMieTable, const glm::ivec3 textureSize, int SAMPLES_MU_S,
    int SAMPLES_NU, int SAMPLES_MU, int SAMPLES_R);

void calculateInscattering(int scatteringOrder, CPUTexture3D& inscatteringTexture,
    const CPUTexture3D& deltaSTexture, int SAMPLES_MU_S, int SAMPLES_NU, int SAMPLES_MU,
    int SAMPLES_R);

void calculateDeltaJ(int scatteringOrder, CPUTexture3D& deltaJ,
    const CPUTexture& deltaETexture, const CPUTexture3D& deltaSRTexture,
    const CPUTexture3D& deltaSMTexture, const CPUTexture& transmittanceTexture, double Rg,
    double RT, double averageGroundReflectance, double HR, const glm::dvec3& betaRayleigh,
    double HM, const glm::dvec3& betaMieScattering, double mieG, int SAMPLES_MU_S,
    int SAMPLES_NU, int SAMPLES_MU, int SAMPLES_R);

std::pair<double, glm::dvec4> step3DTexture(double Rg, double Rt, int rSamples, int layer);

std::pair<glm::dvec3, glm::dvec3> inscatter(double r, double mu, double muSun, double nu,
    double Rt, double Rg, const CPUTexture& transmittanceTexture, bool ozoneLayerEnabled,
    double HO, double HM, double HR, const glm::dvec3& betaRayleigh,
    const glm::dvec3& betaMieScattering);

glm::dvec3 inscatter(double r, double mu, double muSun, double nu, double Rt, double Rg,
    double averageGroundReflectance, const CPUTexture& transmittanceTexture, double mieG,
    bool firstIteration, const CPUTexture& deltaETexture,
    const CPUTexture3D& deltaSRTexture, const CPUTexture3D& deltaSMTexture,
    int SAMPLES_MU_S, int SAMPLES_NU, int SAMPLES_MU, int SAMPLES_R,
    const glm::dvec3& betaRayleigh, const glm::dvec3& betaMieScattering, double HR, double HM);

std::pair<glm::dvec3, glm::dvec3> integrand(double r, double mu, double muSun, double nu,
    double y, double Rg, double Rt, const CPUTexture& transmittanceTexture,
    bool ozoneLayerEnabled, double HO, double HM, double HR);


// Given the irradiance texture table, the cosine of zenith sun vector and the height of
// the observer (ray's stating point x), calculates the mapping for u_r and u_muSun and
// returns the value in the LUT
// lut   := OpenGL texture2D sampler (the irradiance texture deltaE)
// muSun := cosine of the zeith angle of vec(s). Or muSun = (vec(s) * vec(v))
// r     := height of starting point vect(x)
glm::dvec3 irradianceLUT(const CPUTexture& lut, double muSun, double r, double Rg, double Rt);

} // namespace inscattering

} // namespace atmosphere

} // namespace openspace

#endif // __OPENSPACE_MODULE_ATMOSPHERE___PRECOMPUTETEXTURES___H__
