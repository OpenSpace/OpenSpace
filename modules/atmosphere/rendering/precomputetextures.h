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
    CPUTexture(int w, int h, Format c, float value = 255.f)
        : width{ w }, height{ h }, components{ static_cast<int>(c) } {
        data = std::vector<float>(width * height * components, value);
    }
    CPUTexture(const glm::ivec2& size, Format c, float value = 255.f)
        : CPUTexture(size.x, size.y, c, value) {}

    int width = 0;
    int height = 0;
    std::vector<float> data;
    int components = 0;
};

using CPUTexture3D = std::vector<CPUTexture>;

namespace common {
    float rayDistance(float r, float mu, float Rt, float Rg);

     // biliniear interpolate a texture, clamps the texture coordinates to(0, size - 1)
     // Assumes x and y lookup coordinates are given as decimals (0,1)
    glm::vec4 texture(const CPUTexture& tex, float x, float y);
    glm::vec4 texture(const CPUTexture3D& tex, const glm::vec3& pos);

    // Function to access the transmittance texture. Given r and mu, returns the
    // transmittance of a ray starting at vec(x), height r, and direction vec(v), mu, and
    // length until it hits the ground or the top of atmosphere.
    // r := height of starting point vect(x)
    // mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
    glm::vec3 transmittance(const atmosphere::CPUTexture& tex, float r, float mu,
        float Rg, float Rt
    );

    // Given a position r and direction mu, calculates de transmittance along the ray with
    // length d. This function uses the propriety of Transmittance:
    // T(a,b) = TableT(a,v)/TableT(b, v)
    // r := height of starting point vect(x)
    // mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
    glm::vec3 transmittance(const atmosphere::CPUTexture& tex, float r, float mu, float d,
        float Rg, float Rt);

    // Given the windows's fragment coordinates, for a defined view port, gives back the
    // interpolated r e [Rg, Rt] and mu, muSun amd nu e [-1, 1]
    // r := height of starting point vect(x)
    // mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
    // muSun := cosine of the zeith angle of vec(s). Or muSun = (vec(s) * vec(v))
    // nu := cosone of the angle between vec(s) and vec(v)
    // dhdH := it is a vec4. dhdH.x stores the dminT := Rt - r, dhdH.y stores the dH value
    //         (see paper), dhdH.z stores dminG := r - Rg and dhdH.w stores dh (see paper)
    glm::vec3 unmappingMuMuSunNu(float r, const glm::vec4& dhdH, int SAMPLES_MU, float Rg,
        float Rt, int SAMPLES_MU_S, int SAMPLES_NU, int x, int y);

} // namespace common

namespace transmittance {

void calculateTransmittance(CPUTexture& texture, float Rg, float Rt, float HR,
    const glm::vec3 betaRayleigh, float HO, const glm::vec3& betaOzoneExtinction,
    float HM, const glm::vec3& betaMieExtinction, bool ozoneLayerEnabled);

} // namespace transmittance

namespace irradiance {
CPUTexture calculateIrradiance(const glm::ivec2& _tableSize);

CPUTexture calculateDeltaE(const glm::ivec2& deltaETableSize,
    const CPUTexture& transmittance, float Rg, float Rt);

} // namespace irradiance

namespace inscattering {
std::pair<CPUTexture3D, CPUTexture3D> calculateDeltaS(
    const glm::ivec3& textureSize, const CPUTexture& transmittanceTexture, float Rg,
    float Rt, float HR, const glm::vec3& betaRayleigh, float HM,
    const glm::vec3& betaMiescattering, int SAMPLES_MU_S, int SAMPLES_NU, int SAMPLES_MU,
    bool ozoneLayerEnabled, float HO);

CPUTexture3D calculateInscattering(const CPUTexture3D& deltaSRayleighTable,
    const CPUTexture3D& deltaSMieTable, const glm::ivec3 textureSize, int SAMPLES_MU_S,
    int SAMPLES_NU, int SAMPLES_MU, int SAMPLES_R);

std::pair<float, glm::vec4> step3DTexture(float Rg, float Rt, int rSamples, int layer);

std::pair<glm::vec3, glm::vec3> inscatter(float r, float mu, float muSun, float nu,
    float Rt, float Rg, const CPUTexture& transmittanceTexture, bool ozoneLayerEnabled,
    float HO, float HM, float HR, const glm::vec3& betaRayleigh,
    const glm::vec3& betaMieScattering);

std::pair<glm::vec3, glm::vec3> integrand(float r, float mu, float muSun, float nu,
    float y, float Rg, float Rt, const CPUTexture& transmittanceTexture,
    bool ozoneLayerEnabled, float HO, float HM, float HR);

} // namespace inscattering

} // namespace atmosphere

} // namespace openspace

#endif // __OPENSPACE_MODULE_ATMOSPHERE___PRECOMPUTETEXTURES___H__
