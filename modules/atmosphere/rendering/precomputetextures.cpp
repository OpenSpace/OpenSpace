#include "precomputetextures.h"

namespace openspace {
namespace atmosphere {

namespace common {
float rayDistance(float r, float mu, float Rt, float Rg) {
    const float ATM_EPSILON = 1.0f;
    // The light ray starting at the observer in/on the atmosphere can have to possible end
    // points: the top of the atmosphere or the planet ground. So the shortest path is the
    // one we are looking for, otherwise we may be passing through the ground

    // cosine law
    float atmRadiusEps2 = (Rt + ATM_EPSILON) * (Rt + ATM_EPSILON);
    float mu2 = mu * mu;
    float r2 = r * r;
    float rayDistanceAtmosphere = -r * mu + std::sqrt(r2 * (mu2 - 1.0) + atmRadiusEps2);
    float delta = r2 * (mu2 - 1.0) + Rg * Rg;

    // Ray may be hitting ground
    if (delta >= 0.f) {
        float rayDistanceGround = -r * mu - std::sqrt(delta);
        if (rayDistanceGround >= 0.) {
            return std::min(rayDistanceAtmosphere, rayDistanceGround);
        }
    }
    return rayDistanceAtmosphere;
}

glm::vec4 texture(const CPUTexture& tex, float x, float y)
{
    auto getColor = [&tex](int i, int j) {
        int index = (j * tex.width + i) * tex.components;
        return glm::vec4(
            tex.data[index],
            tex.data[index + 1],
            tex.data[index + 2],
            static_cast<CPUTexture::Format>(tex.components) == CPUTexture::Format::RGB ?
                1.f : tex.data[index + 3]
        );
    };

    //auto getWrappedIndex = [](float v, int max) {
    //    // v is within our texture, no need to wrap it
    //    if (v > 0 && v < max) {
    //        return static_cast<int>(v);
    //    }

    //    // max is 1 less than the width/height size, we add +1 to get the correct texel
    //    // after casting eg., -0.2 should wrap to the last element
    //    if (v < 0) {
    //        const int size = max + 1;
    //        float wrappedV = size - v;
    //        return static_cast<int>(wrappedV);
    //    }

    //    else {

    //    }
    //};

    // Scale lookup coordinates to match texture size
    x = x * (tex.width - 1);
    y = y * (tex.height - 1);

    // Calc integer coordinates of the four sourrounding pixels
    int x1 = std::clamp(static_cast<int>(x), 0, tex.width - 1);
    int y1 = std::clamp(static_cast<int>(y), 0, tex.height - 1);
    int x2 = std::clamp(x1 + 1, 0, tex.width - 1);
    int y2 = std::clamp(y1 + 1, 0, tex.height - 1);

    // Get fractional part of x and y
    float fx = x - x1;
    float fy = y - y1;

    // Get colors of the four sourrounding pixels
    glm::vec4 c11 = getColor(x1, y1);
    glm::vec4 c12 = getColor(x1, y2);
    glm::vec4 c21 = getColor(x2, y1);
    glm::vec4 c22 = getColor(x2, y2);

    // Interpolate the colors
    glm::vec4 c1, c2, result;
    c1 = glm::mix(c11, c21, fx);
    c2 = glm::mix(c21, c22, fx);
    result = glm::mix(c1, c2, fy);

    return result;
}

glm::vec4 texture(const CPUTexture3D& tex, const glm::vec3& pos)
{
    // Scale z lookup cordinate to match texture size, x and y are computed in the 2D func
    float z = pos.z * (tex.size() - 1);

    // Get integer coordinate of the two surrounding slices
    int z1 = std::clamp(static_cast<int>(z), 0, static_cast<int>(tex.size()) - 1);
    int z2 = std::clamp(z1 + 1, 0, static_cast<int>(tex.size()) - 1);

    // Get fractional part of z
    float fz = z - z1;

    // Perform bilinear interpolation on the two slices
    glm::vec4 c0 = texture(tex[z1], pos.x, pos.y);
    glm::vec4 c1 = texture(tex[z2], pos.x, pos.y);

    // Interpolate between the two slices along z
    glm::vec4 result = glm::mix(c0, c1, fz);

    return result;
}

glm::vec3 transmittance(const atmosphere::CPUTexture& tex, float r, float mu, float Rg,
                        float Rt)
{
    // Given the position x (here the altitude r) and the view angle v
    // (here the cosine(v)= mu), we map this
    float u_r = std::sqrt((r - Rg) / (Rt - Rg));
    // See Collienne to understand the mapping
    float u_mu = std::atan((mu + 0.15f) / 1.15f * tan(1.5f)) / 1.5f;
    return texture(tex, u_mu, u_r);
}

glm::vec3 transmittance(const atmosphere::CPUTexture& tex, float r, float mu, float d,
    float Rg, float Rt)
{
    // Here we use the transmittance property: T(x,v) = T(x,d)*T(d,v) to, given a distance
    // d, calculates that transmittance along that distance starting in x (height r):
    // T(x,d) = T(x,v)/T(d,v).
    //
    // From cosine law: c^2 = a^2 + b^2 - 2*a*b*cos(ab)
    float ri = std::sqrt(d * d + r * r + 2.0f * r * d * mu);
    // mu_i = (vec(d) dot vec(v)) / r_i
    //      = ((vec(x) + vec(d-x)) dot vec(v))/ r_i
    //      = (r*mu + d) / r_i
    float mui = (d + r * mu) / ri;

    // It's important to remember that we calculate the Transmittance table only for
    // zenith angles between 0 and pi/2+episilon. Then, if mu < 0.0, we just need to
    // invert the view direction and the start and end points between them, i.e., if
    // x --> x0, then x0-->x.
    // Also, let's use the property: T(a,c) = T(a,b)*T(b,c)
    // Because T(a,c) and T(b,c) are already in the table T, T(a,b) = T(a,c)/T(b,c).
    glm::vec3 res;
    if (mu > 0.0) {
        res = transmittance(tex, r, mu, Rg, Rt) / transmittance(tex, ri, mui, Rg, Rt);
    }
    else {
        res = transmittance(tex, ri, -mui, Rg, Rt) / transmittance(tex, r, -mu, Rg, Rt);
    }
    return glm::min(res, 1.0f);
}

glm::vec3 unmappingMuMuSunNu(float r, const glm::vec4& dhdH, int SAMPLES_MU, float Rg,
    float Rt, int SAMPLES_MU_S, int SAMPLES_NU, int x, int y)
{
    float mu;
    float muSun;
    float nu;
    // Window coordinates of pixel (uncentering also)
    //glm::vec2 fragment = gl_FragCoord.xy - vec2(0.5);

    // We dont need to uncenter since we already have a bottom
    // left corner
    glm::vec2 fragment{ x, y };

    // Pre-calculations
    float r2 = r * r;
    float Rg2 = Rg * Rg;

    float halfSAMPLE_MU = static_cast<float>(SAMPLES_MU) / 2.0f;
    // If the (vec(x) dot vec(v))/r is negative, i.e., the light ray has great probability
    // to touch the ground, we obtain mu considering the geometry of the ground
    if (fragment.y < halfSAMPLE_MU) {
        float ud = 1.0f - (fragment.y / (halfSAMPLE_MU - 1.0f));
        float d = std::min(std::max(dhdH.z, ud * dhdH.w), dhdH.w * 0.999f);
        // cosine law: Rg^2 = r^2 + d^2 - 2rdcos(pi-theta) where cosine(theta) = mu
        mu = (Rg2 - r2 - d * d) / (2.0f * r * d);
        // We can't handle a ray inside the planet, i.e., when r ~ Rg, so we check against
        // it. If that is the case, we approximate to a ray touching the ground.
        // cosine(pi-theta) = dh/r = sqrt(r^2-Rg^2)
        // cosine(theta) = - sqrt(1 - Rg^2/r^2)
        mu = std::min(mu, -std::sqrt(1.0f - (Rg2 / r2)) - 0.001f);
    }
    // The light ray is touching the atmosphere and not the ground
    else {
        float d = (fragment.y - halfSAMPLE_MU) / (halfSAMPLE_MU - 1.0f);
        d = std::min(std::max(dhdH.x, d * dhdH.y), dhdH.y * 0.999f);
        // cosine law: Rt^2 = r^2 + d^2 - 2rdcos(pi-theta) where cosine(theta) = mu
        mu = (Rt * Rt - r2 - d * d) / (2.0f * r * d);
    }

    float modValueMuSun = std::fmod(
        fragment.x,
        static_cast<float>(SAMPLES_MU_S)) / (static_cast<float>(SAMPLES_MU_S) - 1.0f
    );
    // The following mapping is different from the paper. See Collienne for an details.
    muSun = std::tan((2.0f * modValueMuSun - 1.0f + 0.26f) * 1.1f) / std::tan(1.26f * 1.1f);
    nu = -1.0f + std::floor(fragment.x / static_cast<float>(SAMPLES_MU_S)) /
        (static_cast<float>(SAMPLES_NU) - 1.0f) * 2.0f;

    return glm::vec3{ mu, muSun, nu };
}

float rayleighPhaseFunction(float mu)
{
    // return (3.0 / (16.0 * M_PI)) * (1.0 + mu * mu);
    return 0.0596831036f * (1.0f + mu * mu);;
}

float miePhaseFunction(float mu, float mieG)
{
    float mieG2 = mieG * mieG;
    return 0.1193662072f * (1.0f - mieG2) *
        std::pow(1.0f + mieG2 - 2.0f * mieG * mu, -1.5f) * (1.0f + mu * mu) / (2.0f + mieG2);
}

glm::vec4 texture4D(const CPUTexture3D& table, float r, float mu, float muSun, float nu,
    float Rg, int samplesMu, float Rt, int samplesR, int samplesMuS, int samplesNu)
{
    float r2 = r * r;
    float Rg2 = Rg * Rg;
    float Rt2 = Rt * Rt;
    float rho = std::sqrt(r2 - Rg2);
    float rmu = r * mu;
    float delta = rmu * rmu - r2 + Rg2;

    glm::vec4 cst = rmu < 0.f && delta > 0.f ?
        glm::vec4(1.f, 0.f, 0.f, 0.5f - 0.5f / static_cast<float>(samplesMu)) :
        glm::vec4(-1.f, Rt2 - Rg2, std::sqrt(Rt2 - Rg2), 0.5f + 0.5f /
            static_cast<float>(samplesMu));

    float u_r = 0.5f / static_cast<float>(samplesR) + rho / std::sqrt(Rt2 - Rg2) *
        (1.f - 1.f / static_cast<float>(samplesR));
    float u_mu = cst.w + (rmu * cst.x + std::sqrt(delta + cst.y)) /
        (rho + cst.z) * (0.5f - 1.f / samplesMu);
    float u_mu_s = 0.5f / float(samplesMuS) +
        (std::atan(std::max(muSun, -0.1975f) * std::tan(1.386f)) *
            0.9090909090909090f + 0.74f) *
        0.5f * (1.f - 1.f / static_cast<float>(samplesMuS));
    float t = (nu + 1.f) / 2.f * (static_cast<float>(samplesNu) - 1.f);
    float u_nu = std::floor(t);
    t = t - u_nu;

    glm::vec4 v1 = texture(table,
        glm::vec3((u_nu + u_mu_s) / static_cast<float>(samplesNu), u_mu, u_r));
    glm::vec4 v2 = texture(table,
        glm::vec3((u_nu + u_mu_s + 1.0) / float(samplesNu), u_mu, u_r));

    return glm::mix(v1, v2, t);
}

} // namespace common

namespace transmittance {
void calculateTransmittance(CPUTexture& texture, float Rg, float Rt, float HR,
    const glm::vec3 betaRayleigh, float HO, const glm::vec3& betaOzoneExtinction,
    float HM, const glm::vec3& betaMieExtinction, bool ozoneLayerEnabled)
{
    const int TransmittanceSteps = 500;

    // Optical depth by integration, from ray starting at point vec(x), i.e, height r and
    // angle mu (cosine of vec(v)) until top of atmosphere or planet's ground.
    // r := height of starting point vect(x)
    // mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
    // H := Thickness of atmosphere if its density were uniform (used for Rayleigh and Mie)
    auto opticalDepth = [&](float r, float mu, float H) -> float {
        float r2 = r * r;
        // Is ray below horizon? The transmittance table will have only the values for
        // transmittance starting at r (x) until the light ray touches the atmosphere or the
        // ground and only for view angles v between 0 and pi/2 + eps. That's because we can
        // calculate the transmittance for angles bigger than pi/2 just inverting the ray
        // direction and starting and ending points.

        // cosine law for triangles: y_i^2 = a^2 + b^2 - 2abcos(alpha)
        float cosZenithHorizon = -sqrt(1.0f - ((Rg * Rg) / r2));
        if (mu < cosZenithHorizon) {
            return 1e9f;
        }

        // Integrating using the Trapezoidal rule:
        // Integral(f(y)dy)(from a to b) = ((b-a)/2n_steps)*(Sum(f(y_i+1)+f(y_i)))
        float b_a = common::rayDistance(r, mu, Rt, Rg);
        float deltaStep = b_a / static_cast<float>(TransmittanceSteps);
        // cosine law
        float y_i = std::exp(-(r - Rg) / H);

        float accumulation = 0.0;
        for (int i = 1; i <= TransmittanceSteps; i++) {
            float x_i = static_cast<float>(i) * deltaStep;
            // cosine law for triangles: y_i^2 = a^2 + b^2 - 2abcos(alpha)
            // In this case, a = r, b = x_i and cos(alpha) = cos(PI-zenithView) = mu
            float y_ii = std::exp(-(std::sqrt(r2 + x_i * x_i + 2.0f * x_i * r * mu) - Rg) / H);
            accumulation += (y_ii + y_i);
            y_i = y_ii;
        }
        return accumulation * (b_a / (2.0f * TransmittanceSteps));
        };

    int k = 0;
    for (int y = 0; y < texture.height; y++) {
        for (int x = 0; x < texture.width; x++) {
            // In the shader this x and y here are actually gl_FragCoord.x, gl_FragCoord
            // assumes a lower-left origin and pixels centers are located at half-pixel
            // enters, thus we had 0.5 to x, y
            float u_mu = (x + 0.5f) / static_cast<float>(texture.width);
            float u_r = (y + 0.5f) / static_cast<float>(texture.height);

            // In the paper u_r^2 = (r^2-Rg^2)/(Rt^2-Rg^2)
            // So, extracting r from u_r in the above equation:
            float r = Rg + (u_r * u_r) * (Rt - Rg);

            // In the paper the Bruneton suggest mu = dot(v,x)/||x|| with ||v|| = 1.0
            // Later he proposes u_mu = (1-exp(-3mu-0.6))/(1-exp(-3.6))
            // But the below one is better. See Collienne.
            // One must remember that mu is defined from 0 to PI/2 + epsilon
            float muSun = -0.15f + std::tan(1.5f * u_mu) / std::tan(1.5f) * 1.15f;

            glm::vec3 ozoneContribution = glm::vec3(0.0f);
            if (ozoneLayerEnabled) {
                ozoneContribution = betaOzoneExtinction * 0.0000006f * opticalDepth(r, muSun, HO);
            }
            glm::vec3 opDepth = ozoneContribution;
            glm::vec3 opDepthBetaMie = betaMieExtinction * opticalDepth(r, muSun, HM);
            glm::vec3 opDepthBetaRay = betaRayleigh * opticalDepth(r, muSun, HR);

            //glm::vec3 color = glm::exp(-opDepth);
            glm::vec3 color = (opDepth + opDepthBetaMie + opDepthBetaRay);
            color = glm::exp(-color);

            texture.data[k] = color.r;
            texture.data[k + 1] = color.g;
            texture.data[k + 2] = color.b;
            k += texture.components;
        }
    }
}

} // namespace transmittance

namespace irradiance {
CPUTexture calculateIrradiance(const glm::ivec2& tableSize)
{
    // Irradiance start at 0 (?) (see pdf line 4 in 4.1)
    return CPUTexture(tableSize, CPUTexture::Format::RGB, 0.f);
}

CPUTexture calculateDeltaE(const glm::ivec2& deltaETableSize,
    const CPUTexture& transmittance, float Rg, float Rt)
{
    CPUTexture deltaETexture = CPUTexture(deltaETableSize, CPUTexture::Format::RGB);

    int k = 0;
    for (int y = 0; y < deltaETexture.height; y++) {
        for (int x = 0; x < deltaETexture.width; x++) {
            // See Bruneton and Collienne to understand the mapping
            // In the shader it was gl_FragCoord.x - 0.5 but since fragcoord assume
            // center voxel and we already have left centered voxels we don't have to subtract
            float muSun = -0.2f + x / (static_cast<float>(deltaETexture.width) - 1.0f) * 1.2f;
            float r = Rg + y / (static_cast<float>(deltaETexture.height)) * (Rt - Rg);
            //float r = Rg + y / (static_cast<float>(img.height)) * (Rt - Rg);

            // We are calculating the Irradiance for L0, i.e., only the radiance coming from the Sun
            // direction is accounted for:
            // E[L0](x,s) = L0*dot(w,n) or 0 (if v!=s or the sun is occluded).
            // Because we consider the planet as a perfect sphere and we are considering only single
            // scattering here, the dot product dot(w,n) is equal to dot(s,n) that is equal to
            // dot(s, r/||r||) = muSun.
            glm::vec3 color = common::transmittance(transmittance, r, muSun, Rg, Rt) * std::max(muSun, 0.0f);
            //color = glm::vec3(color.r, 0, 0);
            //img.data[k] = static_cast<unsigned int>(color.r * 255);
            //img.data[k + 1] = static_cast<unsigned int>(color.g * 255);
            //img.data[k + 2] = static_cast<unsigned int>(color.b * 255);
            deltaETexture.data[k] = color.r;
            deltaETexture.data[k + 1] = color.g;
            deltaETexture.data[k + 2] = color.b;
            k += deltaETexture.components;
        }
    }

    return deltaETexture;
}

void calculateDeltaE(int scatteringOrder, CPUTexture& deltaETexture,
    const CPUTexture3D& deltaSRTexture, const CPUTexture3D& deltaSMTexture, float Rg,
    float Rt, float mieG, const glm::ivec2 SKY, int SAMPLES_MU_S, int SAMPLES_NU,
    int SAMPLES_MU, int SAMPLES_R)
{
    const float M_PI = 3.141592657f;
    const int IRRADIANCE_INTEGRAL_SAMPLES = 32;
    // Spherical Coordinates Steps. phi in [0,2PI] and theta in [0, PI/2]
    const float stepPhi = (2.0 * M_PI) / float(IRRADIANCE_INTEGRAL_SAMPLES);
    const float stepTheta = M_PI / (2.0 * float(IRRADIANCE_INTEGRAL_SAMPLES));

    const bool firstIteration = scatteringOrder == 2;

    int k = 0;
    for (int y = 0; y < deltaETexture.height; y++) {
        for (int x = 0; x < deltaETexture.width; x++) {
            // See Bruneton and Collienne to understand the mapping.
            float muSun = -0.2f + x / (static_cast<float>(SKY.x) - 1.0f) * 1.2f;
            float r = Rg + y / (static_cast<float>(SKY.y) - 1.0f) * (Rt - Rg);

            // We know that muSun = cos(sigma) = s.z/||s||
            // But, ||s|| = 1, so s.z = muSun. Also,
            // ||s|| = 1, so s.x = sin(sigma) = sqrt(1-muSun^2) and s.y = 0.0
            glm::vec3 s = glm::vec3(
                std::max(std::sqrt(1.0f - muSun * muSun), 0.0f),
                0.0f,
                muSun
            );

            // In order to solve the integral from equation (15) we use the trapezoidal rule:
            // Integral(f(y)dy)(from a to b) = ((b-a)/2n_steps)*(Sum(f(y_i+1)+f(y_i)))
            glm::vec3 irradianceE = glm::vec3(0.0f);
            for (int iphi = 0; iphi < IRRADIANCE_INTEGRAL_SAMPLES; iphi++) {
                float phi = (static_cast<float>(iphi) + 0.5f) * stepPhi;
                for (int itheta = 0; itheta < IRRADIANCE_INTEGRAL_SAMPLES; itheta++) {
                    float theta = (static_cast<float>(itheta) + 0.5f) * stepTheta;
                    // spherical coordinates: dw = dtheta*dphi*sin(theta)*rho^2
                    // rho = 1, we are integrating over a unit sphere
                    float dw = stepTheta * stepPhi * std::sin(theta);
                    // w = (cos(phi) * sin(theta) * rho, sin(phi) * sin(theta) * rho, cos(theta) * rho)
                    glm::vec3 w = glm::vec3(
                        std::cos(phi) * std::sin(theta),
                        std::sin(phi) * std::sin(theta),
                        std::cos(theta)
                    );
                    float nu = glm::dot(s, w);

                    // The first iteration is different from the others as in the first iteration all
                    // the light arriving is coming from the initial pre-computed single scattered
                    // light. We stored these values in the deltaS textures (Ray and Mie), and in order
                    // to avoid problems with the high angle dependency in the phase functions, we don't
                    // include the phase functions on those tables (that's why we calculate them now)
                    if (firstIteration == 1) {
                        float phaseRay = common::rayleighPhaseFunction(nu);
                        float phaseMie = common::miePhaseFunction(nu, mieG);
                        glm::vec3 singleRay = glm::vec3(common::texture4D(deltaSRTexture,
                            r, w.z, muSun, nu, Rg, SAMPLES_MU, Rt, SAMPLES_R,
                            SAMPLES_MU_S, SAMPLES_NU)
                        );
                        glm::vec3 singleMie = glm::vec3(common::texture4D(deltaSMTexture,
                            r, w.z, muSun, nu, Rg, SAMPLES_MU, Rt, SAMPLES_R,
                            SAMPLES_MU_S, SAMPLES_NU)
                        );
                        // w.z is the cosine(theta) = mu for vec(w) and also vec(w) dot vec(n(xo))
                        irradianceE += (singleRay * phaseRay + singleMie * phaseMie) * w.z * dw;
                    }
                    else {
                        // On line 10 of the algorithm, the texture table deltaE is updated, so when we
                        // are not in the first iteration, we are getting the updated result of deltaE
                        // (not the single irradiance light but the accumulated (higher order) irradiance
                        // light. w.z is the cosine(theta) = mu for vec(w) and also vec(w) dot vec(n(xo))
                        irradianceE += glm::vec3(common::texture4D(deltaSRTexture, r, w.z,
                            muSun, nu, Rg, SAMPLES_MU, Rt, SAMPLES_R, SAMPLES_MU_S,
                            SAMPLES_NU)) * w.z * dw;
                    }
                }
            }

            deltaETexture.data[k] = irradianceE.r;
            deltaETexture.data[k + 1] = irradianceE.g;
            deltaETexture.data[k + 2] = irradianceE.b;
            k += deltaETexture.components;
        }
    }
}

} // namespace irradiance

namespace inscattering {
std::pair<CPUTexture3D, CPUTexture3D> calculateDeltaS(
    const glm::ivec3& textureSize, const CPUTexture& transmittanceTexture, float Rg,
    float Rt, float HR, const glm::vec3& betaRayleigh, float HM,
    const glm::vec3& betaMiescattering, int SAMPLES_MU_S, int SAMPLES_NU, int SAMPLES_MU,
    bool ozoneLayerEnabled, float HO)
{

    CPUTexture3D deltaSRayleigh(textureSize.z,
        CPUTexture{ textureSize.x, textureSize.y, CPUTexture::Format::RGB }
    );
    CPUTexture3D deltaSmie(textureSize.z,
        CPUTexture{ textureSize.x, textureSize.y, CPUTexture::Format::RGB }
    );

    for (int layer = 0; layer < textureSize.z; layer++) {
        std::pair<float, glm::vec4> v = step3DTexture(Rg, Rt, textureSize.z, layer);

        const float& r = v.first;
        const glm::vec4& dhdH = v.second;

        //auto [r, dhdH] = step3DTexture(Rg, Rt, textureSize.z,layer);

        int k = 0;
        for (int y = 0; y < textureSize.y; y++) {
            for (int x = 0; x < textureSize.x; x++) {
                // From the layer interpolation (see C++ code for layer to r) and the
                // textures parameters (uv), we unmapping mu, muSun and nu.
                glm::vec3 muMuSunNu = common::unmappingMuMuSunNu(r, dhdH, SAMPLES_MU, Rg,
                    Rt, SAMPLES_MU_S, SAMPLES_NU, x, y
                );

                float mu = muMuSunNu.x;
                float muSun = muMuSunNu.y;
                float nu = muMuSunNu.z;

                // Here we calculate the single inScattered light. Because this is a single
                // inscattering, the light that arrives at a point y in the path from the
                // eye to the infinity (top of atmosphere or planet's ground), comes only
                // from the light source, i.e., the sun. So, the there is no need to
                // integrate over the whole solid angle (4pi), we need only to consider
                // the Sun position (cosine of sun pos = muSun). Then, following the paper
                // notation:
                // S[L] = P_R*S_R[L0] + P_M*S_M[L0] + S[L*]
                // For single inscattering only:
                // S[L0] = P_R*S_R[L0] + P_M*S_M[L0]
                // In order to save memory, we just store the red component of S_M[L0],
                // and later we use the proportionality rule to calcule the other
                // components.
                //glm::vec3 S_R; // First Order Rayleigh InScattering
                //glm::vec3 S_M; // First Order Mie InScattering
                auto [S_R, S_M] = inscatter(r, mu, muSun, nu, Rt, Rg, transmittanceTexture,
                    ozoneLayerEnabled, HO, HM, HR, betaRayleigh, betaMiescattering);

                deltaSRayleigh[layer].data[k] = S_R.r; //S_R.r;
                deltaSRayleigh[layer].data[k + 1] = S_R.g; // S_R.g;
                deltaSRayleigh[layer].data[k + 2] = S_R.b; // S_R.b;

                deltaSmie[layer].data[k] = S_M.r;
                deltaSmie[layer].data[k + 1] = S_M.g;
                deltaSmie[layer].data[k + 2] = S_M.b;

                k += deltaSRayleigh[0].components;
            }
        }
    }
    return std::make_pair(deltaSRayleigh, deltaSmie);
}

CPUTexture3D calculateInscattering(const CPUTexture3D& deltaSRayleighTexture,
    const CPUTexture3D& deltaSMieTexture, const glm::ivec3 textureSize,
    int SAMPLES_MU_S, int SAMPLES_NU, int SAMPLES_MU, int SAMPLES_R)
{

    CPUTexture3D inScatteringTableTexture(textureSize.z,
        CPUTexture{ textureSize.x, textureSize.y, CPUTexture::Format::RGBA }
    );
    for (int layer = 0; layer < textureSize.z; layer++) {
        int k = 0;

        for (int y = 0; y < textureSize.y; y++) {
            for (int x = 0; x < textureSize.x; x++) {
                // First we convert the window's fragment coordinate to texel coordinates
                glm::vec3 rst = glm::vec3(x + 0.5f, y + 0.5f, layer + 0.5f) /
                    glm::vec3(
                        static_cast<float>(SAMPLES_MU_S * SAMPLES_NU),
                        static_cast<float>(SAMPLES_MU),
                        static_cast<float>(SAMPLES_R)
                    );

                glm::vec3 rayleighInscattering = common::texture(deltaSRayleighTexture, rst);
                float mieInscattering = common::texture(deltaSMieTexture, rst).r;

                // We are using only the red component of the Mie scattering. See the
                // Precomputed Atmosphere Scattering paper for details about the angular
                // precision
                inScatteringTableTexture[layer].data[k] = rayleighInscattering.r;
                inScatteringTableTexture[layer].data[k + 1] = rayleighInscattering.g;
                inScatteringTableTexture[layer].data[k + 2] = rayleighInscattering.b;
                inScatteringTableTexture[layer].data[k + 3] = mieInscattering;

                k += inScatteringTableTexture[0].components;
            }
        }
    }

    return inScatteringTableTexture;
}

void calculateDeltaJ(int scatteringOrder, CPUTexture3D& deltaJ,
    const CPUTexture& deltaETexture, const CPUTexture3D& deltaSRTexture,
    const CPUTexture3D& deltaSMTexture, const CPUTexture& transmittanceTexture,
    float Rg, float Rt, float averageGroundReflectance, float HR,
    const glm::vec3& betaRayleigh, float HM, const glm::vec3& betaMieScattering,
    float mieG, int SAMPLES_MU_S, int SAMPLES_NU, int SAMPLES_MU, int SAMPLES_R)
{

    const bool firstIteration = scatteringOrder == 2;

    for (int layer = 0; layer < SAMPLES_R; layer++) {
        auto [r, dhdH] = step3DTexture(Rg, Rt, SAMPLES_R, layer);

        int k = 0;
        for (int y = 0; y < deltaJ[0].height; y++) {
            for (int x = 0; x < deltaJ[0].width; x++) {
                // InScattering Radiance to be calculated at different points in the ray
                // path. Unmapping the variables from texture texels coordinates to
                // mapped coordinates
                glm::vec3 muMuSunNu = common::unmappingMuMuSunNu(r, dhdH, SAMPLES_MU, Rg,
                    Rt, SAMPLES_MU_S, SAMPLES_NU, x, y);
                float mu = muMuSunNu.r;
                float muSun = muMuSunNu.g;
                float nu = muMuSunNu.b;
                // Calculate the the light inScattered in direction
                // -vec(v) for the point at height r (vec(y) following Bruneton and Neyret's paper
                glm::vec3 radianceJ = inscatter(r, mu, muSun, nu, Rt, Rg,
                    averageGroundReflectance, transmittanceTexture, mieG, firstIteration,
                    deltaETexture, deltaSRTexture, deltaSMTexture, SAMPLES_MU_S,
                    SAMPLES_NU, SAMPLES_MU, SAMPLES_R, betaRayleigh, betaMieScattering,
                    HR, HM);

                deltaJ[layer].data[k] = radianceJ.r;
                deltaJ[layer].data[k + 1] = radianceJ.g;
                deltaJ[layer].data[k + 2] = radianceJ.b;

                k += deltaJ[0].components;
            }
        }
    }
}



std::pair<float, glm::vec4> step3DTexture(float Rg, float Rt, int rSamples, int layer)
{
    float atmospherePlanetRadius = Rg;
    float atmosphereRadius = Rt;
    //int _rSamples = textureSize.z;
    // See OpenGL redbook 8th Edition page 556 for Layered Rendering
    const float planet2 = atmospherePlanetRadius * atmospherePlanetRadius;
    const float diff = atmosphereRadius * atmosphereRadius - planet2;
    const float ri = static_cast<float>(layer) / static_cast<float>(rSamples - 1);
    float eps = 0.01f;
    if (layer > 0) {
        if (layer == (rSamples - 1)) {
            eps = -0.001f;
        }
        else {
            eps = 0.f;
        }
    }
    const float r = std::sqrt(planet2 + ri * ri * diff) + eps;
    const float dminG = r - atmospherePlanetRadius;
    const float dminT = atmosphereRadius - r;
    const float dh = std::sqrt(r * r - planet2);
    const float dH = dh + std::sqrt(diff);

    glm::vec4 dhdH{ dminT, dH, dminG, dh };
    return std::make_pair(r, dhdH);
}

std::pair<glm::vec3, glm::vec3> inscatter(float r, float mu, float muSun, float nu,
    float Rt, float Rg, const CPUTexture& transmittanceTexture, bool ozoneLayerEnabled,
    float HO, float HM, float HR, const glm::vec3& betaRayleigh,
    const glm::vec3& betaMieScattering)
{
    const int INSCATTER_INTEGRAL_SAMPLES = 50;

    // Let's calculate S_M and S_R by integration along the eye ray path inside the
    // atmosphere, given a position r, a view angle (cosine) mu, a sun position angle
    // (cosine) muSun, and the angle (cosine) between the sun position and the view
    // direction, nu. Integrating using the Trapezoidal rule:
    // Integral(f(y)dy)(from a to b) = (b-a)/2n_steps*(Sum(f(y_i+1)+f(y_i)))
    glm::vec3 S_R{ 0.f };
    glm::vec3 S_M{ 0.f };

    float rayDist = common::rayDistance(r, mu, Rt, Rg);
    float dy = rayDist / static_cast<float>(INSCATTER_INTEGRAL_SAMPLES);
    //glm::vec3 S_Ri;
    //glm::vec3 S_Mi;
    auto [S_Ri, S_Mi] = integrand(r, mu, muSun, nu, 0.0, Rg, Rt, transmittanceTexture,
        ozoneLayerEnabled, HO, HM, HR
    );

    for (int i = 1; i <= INSCATTER_INTEGRAL_SAMPLES; i++) {
        float yj = static_cast<float>(i) * dy;
        auto [S_Rj, S_Mj] = integrand(r, mu, muSun, nu, yj, Rg, Rt, transmittanceTexture,
            ozoneLayerEnabled, HO, HM, HR
        );
        S_R += (S_Ri + S_Rj);
        S_M += (S_Mi + S_Mj);
        S_Ri = S_Rj;
        S_Mi = S_Mj;
    }
    S_R *= betaRayleigh * (rayDist / (2.0f * static_cast<float>(INSCATTER_INTEGRAL_SAMPLES)));
    S_M *= betaMieScattering * (rayDist / (2.0f * static_cast<float>(INSCATTER_INTEGRAL_SAMPLES)));

    return std::make_pair(S_R, S_M);
}

glm::vec3 inscatter(float r, float mu, float muSun, float nu, float Rt, float Rg,
    float averageGroundReflectance, const CPUTexture& transmittanceTexture, float mieG,
    bool firstIteration, const CPUTexture& deltaETexture,
    const CPUTexture3D& deltaSRTexture, const CPUTexture3D& deltaSMTexture,
    int SAMPLES_MU_S, int SAMPLES_NU, int SAMPLES_MU, int SAMPLES_R,
    const glm::vec3& betaRayleigh, const glm::vec3& betaMieScattering, float HR, float HM)
{
    const float M_PI = 3.141592657;
    const int INSCATTER_SPHERICAL_INTEGRAL_SAMPLES = 16;
    // -- Spherical Coordinates Steps. phi e [0,2PI] and theta e [0, PI]
    const float stepPhi = (2.0 * M_PI) / float(INSCATTER_SPHERICAL_INTEGRAL_SAMPLES);
    const float stepTheta = M_PI / float(INSCATTER_SPHERICAL_INTEGRAL_SAMPLES);

    // Be sure to not get a cosine or height out of bounds
    r = glm::clamp(r, Rg, Rt);
    mu = glm::clamp(mu, -1.0f, 1.0f);
    muSun = glm::clamp(muSun, -1.0f, 1.0f);

    //  s sigma | theta v
    //   \      |      /
    //    \     |     /
    //     \    |    /
    //      \   |   /   theta + signam = ni
    //       \  |  /    cos(theta) = mu
    //        \ | /     cos(sigma) = muSun
    //         \|/      cos(ni)    = nu
    float mu2 = mu * mu;
    float muSun2 = muSun * muSun;
    float sinThetaSinSigma = std::sqrt(1.0f - mu2) * std::sqrt(1.0f - muSun2);
    // cos(sigma + theta) = cos(theta)cos(sigma)-sin(theta)sin(sigma)
    // cos(ni) = nu = mu * muSun - sqrt(1.0 - mu*mu)*sqrt(1.0 - muSun*muSun)
    // sin(theta) = sqrt(1.0 - mu*mu)
    // Now we make sure the angle between vec(s) and vec(v) is in the right range:
    nu = glm::clamp(nu, muSun * mu - sinThetaSinSigma, muSun * mu + sinThetaSinSigma);

    // Lets calculate the consine of the angle to the horizon:
    // theta is the angle between vec(v) and x
    // cos(PI-theta) = d/r
    // -cos(theta) = sqrt(r*r-Rg*Rg)/r
    float Rg2 = Rg * Rg;
    float r2 = r * r;
    float cosHorizon = -sqrt(r2 - Rg2) / r;

    // Now we get vec(v) and vec(s) from mu, muSun and nu:
    // Assuming:
    //              z |theta
    //                |\ vec(v) ||vec(v)|| = 1
    //                | \
    //                |__\_____x
    // sin(PI-theta) = x/||v|| => x = sin(theta) =? x = sqrt(1-mu*mu)
    // cos(PI-theta) = z/||v|| => z = cos(theta) = mu
    // v.y = 0 because ||v|| = 1
    glm::vec3 v = glm::vec3(sqrt(1.0f - mu2), 0.0f, mu);

    // To obtain vec(s), we use the following properties:
    // ||vec(s)|| = 1, ||vec(v)|| = 1
    // vec(s) dot vec(v) = cos(ni) = nu
    // Following the same idea for vec(v), we now that s.z = cos(sigma) = muSun
    // So, from vec(s) dot vec(v) = cos(ni) = nu we have,
    // s.x*v.x +s.y*v.y + s.z*v.z = nu
    // s.x = (nu - s.z*v.z)/v.x = (nu - mu*muSun)/v.x
    float sx = (v.x == 0.0f) ? 0.0f : (nu - muSun * mu) / v.x;
    // Also, ||vec(s)|| = 1, so:
    // 1 = sqrt(s.x*s.x + s.y*s.y + s.z*s.z)
    // s.y = sqrt(1 - s.x*s.x - s.z*s.z) = sqrt(1 - s.x*s.x - muSun*muSun)
    glm::vec3 s = glm::vec3(sx, std::sqrt(std::max(0.0f, 1.0f - sx * sx - muSun2)), muSun);


    // In order to integrate over 4PI, we scan the sphere using the spherical coordinates
    // previously defined
    glm::vec3 radianceJAcc = glm::vec3(0.0);
    for (int theta_i = 0; theta_i < INSCATTER_SPHERICAL_INTEGRAL_SAMPLES; theta_i++) {
        float theta = (theta_i + 0.5f) * stepTheta;
        float cosineTheta = std::cos(theta);
        float cosineTheta2 = cosineTheta * cosineTheta;
        float distanceToGround = 0.0f;
        float groundReflectance = 0.0f;
        glm::vec3 groundTransmittance = glm::vec3(0.0f);

        // If the ray w can see the ground we must compute the transmittance
        // effect from the starting point x to the ground point in direction -vec(v):
        if (cosineTheta < cosHorizon) { // ray hits ground
            // AverageGroundReflectance e [0,1]
            groundReflectance = averageGroundReflectance / M_PI;
            // From cosine law: Rg*Rg = r*r + distanceToGround*distanceToGround - 2*r*distanceToGround*cos(PI-theta)
            distanceToGround = -r * cosineTheta - std::sqrt(r2 * (cosineTheta2 - 1.0) + Rg2);
            //               |
            //               | theta
            //               |
            //               |\ distGround
            //            r  | \  alpha
            //               |  \/
            //               |  /
            //               | / Rg
            //               |/
            // So cos(alpha) = ((vec(x)+vec(dg)) dot -vec(distG))/(||(vec(x)+vec(distG))|| * ||vec(distG)||)
            //    cos(alpha) = (-r*distG*cos(theta) - distG*distG)/(Rg*distG)
            //      muGround = -(r*cos(theta) + distG)/Rg
            float muGround = -(r * cosineTheta + distanceToGround) / Rg;
            // We can use the same triangle in calculate the distanceToGround to calculate
            // the cosine of the angle between the ground touching point at height Rg and
            // the zenith angle
            // float muGround = (r2 - distanceToGround*distanceToGround - Rg2)/(2*distanceToGround*Rg);
            // Access the Transmittance LUT in order to calculate the transmittance from
            // the ground point Rg, thorugh the atmosphere, at a distance: distanceToGround
            groundTransmittance = common::transmittance(transmittanceTexture, Rg,
                muGround, distanceToGround, Rg, Rt);
        }

        for (int phi_i = 0; phi_i < INSCATTER_SPHERICAL_INTEGRAL_SAMPLES; ++phi_i) {
            float phi = (float(phi_i) + 0.5) * stepPhi;
            // spherical coordinates: dw = dtheta*dphi*sin(theta)*rho^2
            // rho = 1, we are integrating over a unit sphere
            float dw = stepTheta * stepPhi * std::sin(theta);
            // w = (rho*sin(theta)*cos(phi), rho*sin(theta)*sin(phi), rho*cos(theta))
            float sinPhi = std::sin(phi);
            float sinTheta = std::sin(theta);
            float cosPhi = std::cos(phi);
            glm::vec3 w = glm::vec3(sinTheta * cosPhi, sinTheta * sinPhi, cosineTheta);

            // We calculate the Rayleigh and Mie phase function for the new scattering
            // angle:
            // cos(angle between vec(v) and vec(w)), ||v|| = ||w|| = 1
            float nuWV = dot(v, w);
            float phaseRayleighWV = common::rayleighPhaseFunction(nuWV);
            float phaseMieWV = common::miePhaseFunction(nuWV, mieG);

            glm::vec3 groundNormal = (glm::vec3(0.f, 0.f, r) + distanceToGround * w) / Rg;
            glm::vec3 groundIrradiance = irradianceLUT(deltaETexture,
                glm::dot(groundNormal, s),
                Rg, Rg, Rt
            );

            // We finally calculate the radiance from the reflected ray from ground
            // (0.0 if not reflected)
            glm::vec3 radianceJ1 = groundTransmittance * groundReflectance * groundIrradiance;

            // We calculate the Rayleigh and Mie phase function for the new scattering
            // angle:
            // cos(angle between vec(s) and vec(w)), ||s|| = ||w|| = 1
            float nuSW = glm::dot(s, w);
            // The first iteration is different from the others. In the first iteration
            // all the light InScattered is coming from the initial pre-computed single
            // InScattered light. We stored these values in the deltaS textures
            // (Ray and Mie), and in order to avoid problems with the high angle
            // dependency in the phase functions, we don't include the phase functions on
            // those tables (that's why we calculate them now).
            if (firstIteration) {
                float phaseRaySW = common::rayleighPhaseFunction(nuSW);
                float phaseMieSW = common::miePhaseFunction(nuSW, mieG);
                // We can now access the values for the single InScattering in the
                // textures deltaS textures.
                glm::vec3 singleRay = glm::vec3(common::texture4D(deltaSRTexture, r, w.z,
                    muSun, nuSW, Rg, SAMPLES_MU, Rt, SAMPLES_R, SAMPLES_MU_S, SAMPLES_NU));
                glm::vec3 singleMie = glm::vec3(common::texture4D(deltaSMTexture, r, w.z,
                    muSun, nuSW, Rg, SAMPLES_MU, Rt, SAMPLES_R, SAMPLES_MU_S, SAMPLES_NU));

                // Initial InScattering including the phase functions
                radianceJ1 += singleRay * phaseRaySW + singleMie * phaseMieSW;
            }
            else {
                // On line 9 of the algorithm, the texture table deltaSR is updated, so
                // when we are not in the first iteration, we are getting the updated
                // result of deltaSR (not the single inscattered light but the accumulated
                // (higher order) inscattered light.
                // w.z is the cosine(theta) = mu for vec(w)
                radianceJ1 += glm::vec3(common::texture4D(deltaSRTexture, r, w.z, muSun,
                    nuSW, Rg, SAMPLES_MU, Rt, SAMPLES_R, SAMPLES_MU_S, SAMPLES_NU));
            }

            // Finally, we add the atmospheric scale height (See: Radiation Transfer on
            // the atmosphere and Ocean from Thomas and Stamnes, pg 9-10.
            radianceJAcc += radianceJ1 * (
                    betaRayleigh * std::exp(-(r - Rg) / HR) * phaseRayleighWV +
                    betaMieScattering * std::exp(-(r - Rg) / HM) * phaseMieWV
                ) * dw;
        }
    }

    return radianceJAcc;
}

std::pair<glm::vec3, glm::vec3> integrand(float r, float mu, float muSun, float nu,
    float y, float Rg, float Rt, const CPUTexture& transmittanceTexture,
    bool ozoneLayerEnabled, float HO, float HM, float HR)
{
    // The integral's integrand is the single inscattering radiance:
    // S[L0] = P_M*S_M[L0] + P_R*S_R[L0]
    // where S_M[L0] = T*(betaMScattering * exp(-h/H_M))*L0 and
    // S_R[L0] = T*(betaRScattering * exp(-h/H_R))*L0.
    // T = transmittance.
    // One must remember that because the occlusion on L0, the integrand here will be equal
    // to 0 in that cases. Also it is important to remember that the phase function for the
    // Rayleigh and Mie scattering are added during the rendering time to increase the
    // angular precision
    glm::vec3 S_R{ 0.f };
    glm::vec3 S_M{ 0.f };

    // cosine law
    float ri = std::max(std::sqrt(r * r + y * y + 2.0f * r * mu * y), Rg);

    // Considering the Sun as a parallel light source, thew vector s_i = s.
    // So muSun_i = (vec(y_i) dot vec(s))/r_i = ((vec(x) + vec(yi-x)) dot vec(s))/r_i
    // muSun_i = (vec(x) dot vec(s) + vec(yi-x) dot vec(s))/r_i = (r*muSun + yi*nu)/r_i
    float muSun_i = (nu * y + muSun * r) / ri;

    // If the muSun_i is smaller than the angle to horizon (no sun radiance hitting the
    // point y), we return S_R = S_M = 0.0.
    if (muSun_i >= -std::sqrt(1.0f - Rg * Rg / (ri * ri))) {
        // It's the transmittance from the point y (ri) to the top of atmosphere in direction
        // of the sun (muSun_i) and the transmittance from the observer at x (r) to y (ri).
        glm::vec3 transmittanceY =
            common::transmittance(transmittanceTexture, r, mu, y, Rg, Rt) *
            common::transmittance(transmittanceTexture, ri, muSun_i, Rg, Rt);
        // exp(-h/H)*T(x,v)
        if (ozoneLayerEnabled) {
            S_R = (std::exp(-(ri - Rg) / HO) + std::exp(-(ri - Rg) / HR)) * transmittanceY;
            S_M = std::exp(-(ri - Rg) / HM) * transmittanceY;
        }
        else {
            S_R = std::exp(-(ri - Rg) / HR) * transmittanceY;
            S_M = std::exp(-(ri - Rg) / HM) * transmittanceY;
        }
        // The L0 (sun radiance) is added in real-time.
    }
    return std::make_pair(S_R, S_M);
}

glm::vec3 irradianceLUT(const CPUTexture& lut, float muSun, float r, float Rg, float Rt)
{
    // See Bruneton paper and Coliene to understand the mapping
    float u_muSun = (muSun + 0.2f) / 1.2f;
    float u_r = (r - Rg) / (Rt - Rg);
    return glm::vec3(common::texture(lut, u_muSun, u_r));
}

} // namespace scattering

} // namespace atmosphere

} // namespace openspace
