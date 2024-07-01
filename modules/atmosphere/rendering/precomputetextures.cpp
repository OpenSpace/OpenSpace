#include "precomputetextures.h"

namespace openspace {
namespace atmosphere {

 double safeSqrt(double x) {
    return std::sqrt(std::max(x, 0.0));
}

namespace common {
double rayDistance(double r, double mu, double Rt, double Rg) {
    const double ATM_EPSILON = 1.0;
    // The light ray starting at the observer in/on the atmosphere can have to possible end
    // points: the top of the atmosphere or the planet ground. So the shortest path is the
    // one we are looking for, otherwise we may be passing through the ground

    // cosine law
    double atmRadiusEps2 = (Rt + ATM_EPSILON) * (Rt + ATM_EPSILON);
    double mu2 = mu * mu;
    double r2 = r * r;
    double rayDistanceAtmosphere = -r * mu + std::sqrt(r2 * (mu2 - 1.0) + atmRadiusEps2);
    double delta = r2 * (mu2 - 1.0) + Rg * Rg;

    // Ray may be hitting ground
    if (delta >= 0.0) {
        double rayDistanceGround = -r * mu - std::sqrt(delta);
        if (rayDistanceGround >= 0.0) {
            return std::min(rayDistanceAtmosphere, rayDistanceGround);
        }
    }
    return rayDistanceAtmosphere;
}

glm::dvec4 texture(const CPUTexture& tex, double x, double y)
{
    auto getColor = [&tex](int i, int j) {
        int index = (j * tex.width + i) * tex.components;
        return glm::dvec4(
            tex.data[index],
            tex.data[index + 1],
            tex.data[index + 2],
            static_cast<CPUTexture::Format>(tex.components) == CPUTexture::Format::RGB ?
                1.0 : tex.data[index + 3]
        );
    };

    //auto getWrappedIndex = [](double v, int max) {
    //    // v is within our texture, no need to wrap it
    //    if (v > 0 && v < max) {
    //        return static_cast<int>(v);
    //    }

    //    // max is 1 less than the width/height size, we add +1 to get the correct texel
    //    // after casting eg., -0.2 should wrap to the last element
    //    if (v < 0) {
    //        const int size = max + 1;
    //        double wrappedV = size - v;
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
    double fx = x - x1;
    double fy = y - y1;

    // Get colors of the four sourrounding pixels
    glm::dvec4 c11 = getColor(x1, y1);
    glm::dvec4 c12 = getColor(x1, y2);
    glm::dvec4 c21 = getColor(x2, y1);
    glm::dvec4 c22 = getColor(x2, y2);

    // Interpolate the colors
    glm::dvec4 c1, c2, result;
    c1 = glm::mix(c11, c21, fx);
    c2 = glm::mix(c21, c22, fx);
    result = glm::mix(c1, c2, fy);

    return result;
}

glm::dvec4 texture(const CPUTexture3D& tex, const glm::dvec3& pos)
{
    // Scale z lookup cordinate to match texture size, x and y are computed in the 2D func
    double z = pos.z * (tex.size() - 1);

    // Get integer coordinate of the two surrounding slices
    int z1 = std::clamp(static_cast<int>(z), 0, static_cast<int>(tex.size()) - 1);
    int z2 = std::clamp(z1 + 1, 0, static_cast<int>(tex.size()) - 1);

    // Get fractional part of z
    double fz = z - z1;

    // Perform bilinear interpolation on the two slices
    glm::dvec4 c0 = texture(tex[z1], pos.x, pos.y);
    glm::dvec4 c1 = texture(tex[z2], pos.x, pos.y);

    // Interpolate between the two slices along z
    glm::dvec4 result = glm::mix(c0, c1, fz);

    return result;
}

glm::dvec3 transmittance(const atmosphere::CPUTexture& tex, double r, double mu,
    double Rg, double Rt)
{
    // Given the position x (here the altitude r) and the view angle v
    // (here the cosine(v)= mu), we map this
    double u_r = safeSqrt((r - Rg) / (Rt - Rg));
    //double u_r = std::sqrt((r - Rg) / (Rt - Rg));
    //if ((Rt - Rg < 0.0)) {
    //    u_r = u_r;
    //}
    //if ((r - Rg) < 0.0) {
    //    u_r = 0.0;
    //}
    // See Collienne to understand the mapping
    double u_mu = std::atan((mu + 0.15) / 1.15 * tan(1.5)) / 1.5;
    return texture(tex, u_mu, u_r);
}

glm::dvec3 transmittance(const atmosphere::CPUTexture& tex, double r, double mu, double d,
    double Rg, double Rt)
{
    // Here we use the transmittance property: T(x,v) = T(x,d)*T(d,v) to, given a distance
    // d, calculates that transmittance along that distance starting in x (height r):
    // T(x,d) = T(x,v)/T(d,v).
    //
    // From cosine law: c^2 = a^2 + b^2 - 2*a*b*cos(ab)
    double ri = std::sqrt(d * d + r * r + 2.0 * r * d * mu);
    // mu_i = (vec(d) dot vec(v)) / r_i
    //      = ((vec(x) + vec(d-x)) dot vec(v))/ r_i
    //      = (r*mu + d) / r_i
    double mui = (d + r * mu) / ri;

    // It's important to remember that we calculate the Transmittance table only for
    // zenith angles between 0 and pi/2+episilon. Then, if mu < 0.0, we just need to
    // invert the view direction and the start and end points between them, i.e., if
    // x --> x0, then x0-->x.
    // Also, let's use the property: T(a,c) = T(a,b)*T(b,c)
    // Because T(a,c) and T(b,c) are already in the table T, T(a,b) = T(a,c)/T(b,c).
    glm::dvec3 res;
    if (mu > 0.0) {
        res = transmittance(tex, r, mu, Rg, Rt) / transmittance(tex, ri, mui, Rg, Rt);
    }
    else {
        res = transmittance(tex, ri, -mui, Rg, Rt) / transmittance(tex, r, -mu, Rg, Rt);
    }
    return glm::min(res, 1.0);
}

glm::dvec3 unmappingMuMuSunNu(double r, const glm::dvec4& dhdH, int SAMPLES_MU, double Rg,
    double Rt, int SAMPLES_MU_S, int SAMPLES_NU, int x, int y)
{
    double mu;
    double muSun;
    double nu;
    // Window coordinates of pixel (uncentering also)
    //glm::vec2 fragment = gl_FragCoord.xy - vec2(0.5);

    // We dont need to uncenter since we already have a bottom-left corner origin
    glm::dvec2 fragment{ x, y };

    // Pre-calculations
    double r2 = r * r;
    double Rg2 = Rg * Rg;

    double halfSAMPLE_MU = static_cast<double>(SAMPLES_MU) / 2.0;
    // If the (vec(x) dot vec(v))/r is negative, i.e., the light ray has great probability
    // to touch the ground, we obtain mu considering the geometry of the ground
    if (fragment.y < halfSAMPLE_MU) {
        double ud = 1.0 - (fragment.y / (halfSAMPLE_MU - 1.0));
        double d = std::min(std::max(dhdH.z, ud * dhdH.w), dhdH.w * 0.999);
        // cosine law: Rg^2 = r^2 + d^2 - 2rdcos(pi-theta) where cosine(theta) = mu
        mu = (Rg2 - r2 - d * d) / (2.0 * r * d);
        // We can't handle a ray inside the planet, i.e., when r ~ Rg, so we check against
        // it. If that is the case, we approximate to a ray touching the ground.
        // cosine(pi-theta) = dh/r = sqrt(r^2-Rg^2)
        // cosine(theta) = - sqrt(1 - Rg^2/r^2)
        mu = std::min(mu, -std::sqrt(1.0 - (Rg2 / r2)) - 0.001);
    }
    // The light ray is touching the atmosphere and not the ground
    else {
        double d = (fragment.y - halfSAMPLE_MU) / (halfSAMPLE_MU - 1.0);
        d = std::min(std::max(dhdH.x, d * dhdH.y), dhdH.y * 0.999);
        // cosine law: Rt^2 = r^2 + d^2 - 2rdcos(pi-theta) where cosine(theta) = mu
        mu = (Rt * Rt - r2 - d * d) / (2.0 * r * d);
    }

    double modValueMuSun = std::fmod(
        fragment.x,
        static_cast<double>(SAMPLES_MU_S)) / (static_cast<double>(SAMPLES_MU_S) - 1.0
    );
    // The following mapping is different from the paper. See Collienne for an details.
    muSun = std::tan((2.0 * modValueMuSun - 1.0 + 0.26) * 1.1) / std::tan(1.26 * 1.1);
    nu = -1.0 + std::floor(fragment.x / static_cast<double>(SAMPLES_MU_S)) /
        (static_cast<double>(SAMPLES_NU) - 1.0) * 2.0;

    return glm::dvec3{ mu, muSun, nu };
}

double rayleighPhaseFunction(double mu)
{
    // return (3.0 / (16.0 * M_PI)) * (1.0 + mu * mu);
    return 0.0596831036 * (1.0 + mu * mu);;
}

double miePhaseFunction(double mu, double mieG)
{
    double mieG2 = mieG * mieG;
    return 0.1193662072 * (1.0 - mieG2) *
        std::pow(1.0 + mieG2 - 2.0 * mieG * mu, -1.5) * (1.0 + mu * mu) / (2.0 + mieG2);
}

glm::dvec4 texture4D(const CPUTexture3D& table, double r, double mu, double muSun,
    double nu, double Rg, int samplesMu, double Rt, int samplesR, int samplesMuS,
    int samplesNu)
{
    double r2 = r * r;


    double Rg2 = Rg * Rg;
    double Rt2 = Rt * Rt;
    double rho = safeSqrt(r2 - Rg2);
    double rmu = r * mu;
    double delta = rmu * rmu - r2 + Rg2; //discriminant

    glm::dvec4 cst = rmu < 0.0 && delta > 0.0 ?
        glm::dvec4(1.0, 0.0, 0.0, 0.5 - 0.5 / static_cast<double>(samplesMu)) :
        glm::dvec4(-1.0, Rt2 - Rg2, std::sqrt(Rt2 - Rg2), 0.5 + 0.5 /
            static_cast<double>(samplesMu));

    double u_r = 0.5 / static_cast<double>(samplesR) + rho / std::sqrt(Rt2 - Rg2) *
        (1.0 - 1.0 / static_cast<double>(samplesR));

    // this checks the rho + cst.z if that is 0 so we avoid dividng by zero introducing
    // nans. See https://github.com/ebruneton/precomputed_atmospheric_scattering/blob/d9954923ccd810be2d4443268a182bcb96544c1e/atmosphere/functions.glsl#L795
    double dmin = r - Rg;
    double dmax = rho;
    double u_mu = cst.w + (std::abs(dmax - dmin) < 1e-9 ? 0.0 :
        (rmu * cst.x + safeSqrt(delta + cst.y)) / (rho + cst.z)) *
        (0.5 - 1. / samplesMu);

    double u_mu_s = 0.5 / static_cast<double>(samplesMuS) +
        (std::atan(std::max(muSun, -0.1975) * std::tan(1.386)) *
            0.9090909090909090 + 0.74) *
        0.5f * (1.f - 1.f / static_cast<double>(samplesMuS));
    double t = (nu + 1.f) / 2.f * (static_cast<double>(samplesNu) - 1.f);
    double u_nu = std::floor(t);
    t = t - u_nu;

    glm::dvec4 v1 = texture(table,
        glm::dvec3((u_nu + u_mu_s) / static_cast<double>(samplesNu), u_mu, u_r));
    glm::dvec4 v2 = texture(table,
        glm::dvec3((u_nu + u_mu_s + 1.0) / static_cast<double>(samplesNu), u_mu, u_r));

    return glm::mix(v1, v2, t);
}

} // namespace common

namespace transmittance {
void calculateTransmittance(CPUTexture& texture, double Rg, double Rt, double HR,
    const glm::dvec3 betaRayleigh, double HO, const glm::dvec3& betaOzoneExtinction,
    double HM, const glm::dvec3& betaMieExtinction, bool ozoneLayerEnabled)
{
    const int TransmittanceSteps = 500;

    // Optical depth by integration, from ray starting at point vec(x), i.e, height r and
    // angle mu (cosine of vec(v)) until top of atmosphere or planet's ground.
    // r := height of starting point vect(x)
    // mu := cosine of the zeith angle of vec(v). Or mu = (vec(x) * vec(v))/r
    // H := Thickness of atmosphere if its density were uniform (used for Rayleigh and Mie)
    auto opticalDepth = [&](double r, double mu, double H) -> double {
        double r2 = r * r;
        // Is ray below horizon? The transmittance table will have only the values for
        // transmittance starting at r (x) until the light ray touches the atmosphere or the
        // ground and only for view angles v between 0 and pi/2 + eps. That's because we can
        // calculate the transmittance for angles bigger than pi/2 just inverting the ray
        // direction and starting and ending points.

        // cosine law for triangles: y_i^2 = a^2 + b^2 - 2abcos(alpha)
        double cosZenithHorizon = -std::sqrt(1.0 - ((Rg * Rg) / r2));
        if (mu < cosZenithHorizon) {
            return 1e9;
        }

        // Integrating using the Trapezoidal rule:
        // Integral(f(y)dy)(from a to b) = ((b-a)/2n_steps)*(Sum(f(y_i+1)+f(y_i)))
        double b_a = common::rayDistance(r, mu, Rt, Rg);
        double deltaStep = b_a / static_cast<double>(TransmittanceSteps);
        // cosine law
        double y_i = std::exp(-(r - Rg) / H);

        double accumulation = 0.0;
        for (int i = 1; i <= TransmittanceSteps; i++) {
            double x_i = static_cast<double>(i) * deltaStep;
            // cosine law for triangles: y_i^2 = a^2 + b^2 - 2abcos(alpha)
            // In this case, a = r, b = x_i and cos(alpha) = cos(PI-zenithView) = mu
            double y_ii = std::exp(-(std::sqrt(r2 + x_i * x_i + 2.0 * x_i * r * mu) - Rg) / H);
            accumulation += (y_ii + y_i);
            y_i = y_ii;
        }
        return accumulation * (b_a / (2.0 * TransmittanceSteps));
        };

    int k = 0;
    for (int y = 0; y < texture.height; y++) {
        for (int x = 0; x < texture.width; x++) {
            // x and y correspond to the gl_FragCoord.xy which assumes a lower-left origin
            // and pixels centers are located at half-pixel centers, our x and y have
            // their centers at the lower-left origin thus we add 0.5 to compensate
            double u_mu = (x + 0.5f) / static_cast<double>(texture.width);
            double u_r = (y + 0.5f) / static_cast<double>(texture.height);

            // In the paper u_r^2 = (r^2-Rg^2)/(Rt^2-Rg^2)
            // So, extracting r from u_r in the above equation:
            double r = Rg + (u_r * u_r) * (Rt - Rg);

            // In the paper the Bruneton suggest mu = dot(v,x)/||x|| with ||v|| = 1.0
            // Later he proposes u_mu = (1-exp(-3mu-0.6))/(1-exp(-3.6))
            // But the below one is better. See Collienne.
            // One must remember that mu is defined from 0 to PI/2 + epsilon
            double muSun = -0.15 + std::tan(1.5 * u_mu) / std::tan(1.5) * 1.15;

            glm::dvec3 ozoneContribution = glm::dvec3(0.0);
            if (ozoneLayerEnabled) {
                ozoneContribution = betaOzoneExtinction * 0.0000006 * opticalDepth(r, muSun, HO);
            }
            glm::dvec3 opDepth = ozoneContribution;
            glm::dvec3 opDepthBetaMie = betaMieExtinction * opticalDepth(r, muSun, HM);
            glm::dvec3 opDepthBetaRay = betaRayleigh * opticalDepth(r, muSun, HR);

            //glm::dvec3 color = glm::exp(-opDepth);
            glm::dvec3 color = (opDepth + opDepthBetaMie + opDepthBetaRay);
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
    return CPUTexture(tableSize, CPUTexture::Format::RGB, 0.0);
}

CPUTexture calculateDeltaE(const glm::ivec2& deltaETableSize,
    const CPUTexture& transmittance, double Rg, double Rt)
{
    CPUTexture deltaETexture = CPUTexture(deltaETableSize, CPUTexture::Format::RGB);

    int k = 0;
    for (int y = 0; y < deltaETexture.height; y++) {
        for (int x = 0; x < deltaETexture.width; x++) {
            // See Bruneton and Collienne to understand the mapping
            // In the shader it was gl_FragCoord.x - 0.5 but since fragcoord assume
            // center voxel and we already have left centered voxels we don't have to subtract
            double muSun = -0.2 + x / (static_cast<double>(deltaETexture.width) - 1.0) * 1.2;
            double r = Rg + y / (static_cast<double>(deltaETexture.height)) * (Rt - Rg);
            //double r = Rg + y / (static_cast<double>(img.height)) * (Rt - Rg);

            // We are calculating the Irradiance for L0, i.e., only the radiance coming from the Sun
            // direction is accounted for:
            // E[L0](x,s) = L0*dot(w,n) or 0 (if v!=s or the sun is occluded).
            // Because we consider the planet as a perfect sphere and we are considering only single
            // scattering here, the dot product dot(w,n) is equal to dot(s,n) that is equal to
            // dot(s, r/||r||) = muSun.
            glm::dvec3 color = common::transmittance(transmittance, r, muSun, Rg, Rt) * std::max(muSun, 0.0);
            //color = glm::dvec3(color.r, 0, 0);
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
    const CPUTexture3D& deltaSRTexture, const CPUTexture3D& deltaSMTexture, double Rg,
    double Rt, double mieG, const glm::ivec2 SKY, int SAMPLES_MU_S, int SAMPLES_NU,
    int SAMPLES_MU, int SAMPLES_R)
{
    const double M_PI = 3.141592657;
    const int IRRADIANCE_INTEGRAL_SAMPLES = 32;
    // Spherical Coordinates Steps. phi in [0,2PI] and theta in [0, PI/2]
    const double stepPhi = (2.0 * M_PI) / static_cast<double>(IRRADIANCE_INTEGRAL_SAMPLES);
    const double stepTheta = M_PI / (2.0 * static_cast<double>(IRRADIANCE_INTEGRAL_SAMPLES));

    const bool firstIteration = scatteringOrder == 2;

    int k = 0;
    for (int y = 0; y < deltaETexture.height; y++) {
        for (int x = 0; x < deltaETexture.width; x++) {
            // See Bruneton and Collienne to understand the mapping.
            double muSun = -0.2 + x / (static_cast<double>(SKY.x) - 1.0) * 1.2;
            double r = Rg + y / (static_cast<double>(SKY.y) - 1.0) * (Rt - Rg);

            // We know that muSun = cos(sigma) = s.z/||s||
            // But, ||s|| = 1, so s.z = muSun. Also,
            // ||s|| = 1, so s.x = sin(sigma) = sqrt(1-muSun^2) and s.y = 0.0
            glm::dvec3 s = glm::dvec3(
                std::max(std::sqrt(1.0 - muSun * muSun), 0.0),
                0.0,
                muSun
            );

            // In order to solve the integral from equation (15) we use the trapezoidal rule:
            // Integral(f(y)dy)(from a to b) = ((b-a)/2n_steps)*(Sum(f(y_i+1)+f(y_i)))
            glm::dvec3 irradianceE = glm::dvec3(0.0f);
            for (int iphi = 0; iphi < IRRADIANCE_INTEGRAL_SAMPLES; iphi++) {
                double phi = (static_cast<double>(iphi) + 0.5) * stepPhi;
                for (int itheta = 0; itheta < IRRADIANCE_INTEGRAL_SAMPLES; itheta++) {
                    double theta = (static_cast<double>(itheta) + 0.5) * stepTheta;
                    // spherical coordinates: dw = dtheta*dphi*sin(theta)*rho^2
                    // rho = 1, we are integrating over a unit sphere
                    double dw = stepTheta * stepPhi * std::sin(theta);
                    // w = (cos(phi) * sin(theta) * rho, sin(phi) * sin(theta) * rho, cos(theta) * rho)
                    glm::dvec3 w = glm::dvec3(
                        std::cos(phi) * std::sin(theta),
                        std::sin(phi) * std::sin(theta),
                        std::cos(theta)
                    );
                    double nu = glm::dot(s, w);

                    // The first iteration is different from the others as in the first iteration all
                    // the light arriving is coming from the initial pre-computed single scattered
                    // light. We stored these values in the deltaS textures (Ray and Mie), and in order
                    // to avoid problems with the high angle dependency in the phase functions, we don't
                    // include the phase functions on those tables (that's why we calculate them now)
                    if (firstIteration) {
                        double phaseRay = common::rayleighPhaseFunction(nu);
                        double phaseMie = common::miePhaseFunction(nu, mieG);
                        glm::dvec3 singleRay = glm::dvec3(common::texture4D(deltaSRTexture,
                            r, w.z, muSun, nu, Rg, SAMPLES_MU, Rt, SAMPLES_R,
                            SAMPLES_MU_S, SAMPLES_NU)
                        );
                        glm::dvec3 singleMie = glm::dvec3(common::texture4D(deltaSMTexture,
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
                        irradianceE += glm::dvec3(common::texture4D(deltaSRTexture, r, w.z,
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

void calculateIrradiance(int scatteringOrder, CPUTexture& irradianceTexture,
    const CPUTexture& deltaETexture)
{
    int k = 0;
    for (int y = 0; y < irradianceTexture.height; y++) {
        for (int x = 0; x < irradianceTexture.width; x++) {

            //glm::vec2 uv = glm::vec2((x + 0.5f) / static_cast<double>(deltaETexture.width),
            //    (y + 0.5f) / static_cast<double>(deltaETexture.height));

            glm::dvec2 uv = glm::dvec2(x + 0.5, y + 0.5) /
                glm::dvec2(deltaETexture.width, deltaETexture.height);

            glm::dvec4 color = common::texture(deltaETexture, uv.x, uv.y);

            irradianceTexture.data[k] += color.r;
            irradianceTexture.data[k + 1] += color.g;
            irradianceTexture.data[k + 2] += color.b;
            k += irradianceTexture.components;
        }
    }
}

} // namespace irradiance

namespace inscattering {
std::pair<CPUTexture3D, CPUTexture3D> calculateDeltaS(
    const glm::ivec3& textureSize, const CPUTexture& transmittanceTexture, double Rg,
    double Rt, double HR, const glm::dvec3& betaRayleigh, double HM,
    const glm::dvec3& betaMiescattering, int SAMPLES_MU_S, int SAMPLES_NU, int SAMPLES_MU,
    bool ozoneLayerEnabled, double HO)
{

    CPUTexture3D deltaSRayleigh(textureSize.z,
        CPUTexture{ textureSize.x, textureSize.y, CPUTexture::Format::RGB }
    );
    CPUTexture3D deltaSmie(textureSize.z,
        CPUTexture{ textureSize.x, textureSize.y, CPUTexture::Format::RGB }
    );

    for (int layer = 0; layer < textureSize.z; layer++) {
        std::pair<double, glm::dvec4> v = step3DTexture(Rg, Rt, textureSize.z, layer);

        const double& r = v.first;
        const glm::dvec4& dhdH = v.second;

        //auto [r, dhdH] = step3DTexture(Rg, Rt, textureSize.z,layer);

        int k = 0;
        for (int y = 0; y < textureSize.y; y++) {
            for (int x = 0; x < textureSize.x; x++) {
                // From the layer interpolation (see C++ code for layer to r) and the
                // textures parameters (uv), we unmapping mu, muSun and nu.
                glm::dvec3 muMuSunNu = common::unmappingMuMuSunNu(r, dhdH, SAMPLES_MU, Rg,
                    Rt, SAMPLES_MU_S, SAMPLES_NU, x, y
                );

                double mu = muMuSunNu.x;
                double muSun = muMuSunNu.y;
                double nu = muMuSunNu.z;

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
                //glm::dvec3 S_R; // First Order Rayleigh InScattering
                //glm::dvec3 S_M; // First Order Mie InScattering
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

void calculateDeltaS(int inscatteringOrder, CPUTexture3D& deltaSRayleighTexture,
    const CPUTexture3D& deltaJTexture, const CPUTexture& transmittanceTexture, double Rg,
    double Rt, int SAMPLES_MU_S, int SAMPLES_NU, int SAMPLES_MU, int SAMPLES_R)
{
    const int INSCATTER_INTEGRAL_SAMPLES = 50;

    // The integrand here is the f(y) of the trapezoidal rule:
    auto integrand = [&transmittanceTexture, &deltaJTexture, Rg, Rt, SAMPLES_MU, SAMPLES_R,
        SAMPLES_MU_S, SAMPLES_NU](double r, double mu, double muSun, double nu, double dist) ->
        glm::dvec3
    {
        // We can calculate r_i by the cosine law: r_i^2=dist^2 + r^2 - 2*r*dist*cos(PI-theta)
        double r_i = std::sqrt(r * r + dist * dist + 2.0 * r * dist * mu);
        // r_i can be found using the dot product:
        // vec(y_i) dot vec(dist) = cos(theta_i) * ||vec(y_i)|| * ||vec(dist)||
        // But vec(y_i) = vec(x) + vec(dist), also: vec(x) dot vec(dist) = cos(theta) = mu
        // So, cos(theta_i) = mu_i = (r*dist**mu + dist*2)/(r_i*dist)
        double mu_i = (r * mu + dist) / r_i;
        // muSun_i can also be found by the dot product:
        // cos(sigma_i) = muSun_i = (vec(s) dot vec(y_i))/(||vec(y_i)|| * ||vec(s)||)
        // But vec(y_i) = vec(x) + vec(dist), and vec(x) dot vec(s) = muSun, cos(sigma_i + theta_i) = nu
        double muSun_i = (r * muSun + dist * nu) / r_i;
        // The irradiance attenuated from point r until y (y-x = dist)
        return
            common::transmittance(transmittanceTexture, r, mu, dist, Rg, Rt) *
            glm::dvec3(common::texture4D(deltaJTexture, r_i, mu_i, muSun_i, nu, Rg,
                SAMPLES_MU, Rt, SAMPLES_R, SAMPLES_MU_S, SAMPLES_NU));
    };

    auto inscatter = [Rg, Rt, integrand](double r, double mu, double muSun, double nu) -> glm::dvec3 {
        glm::dvec3 inScatteringRadiance = glm::dvec3(0.0);
        double dy = common::rayDistance(r, mu, Rt, Rg) / static_cast<double>(INSCATTER_INTEGRAL_SAMPLES);
        glm::dvec3 inScatteringRadiance_i = integrand(r, mu, muSun, nu, 0.0f);

        // In order to solve the integral from equation (11) we use the trapezoidal rule:
        // Integral(f(y)dy)(from a to b) = ((b-a)/2n_steps)*(Sum(f(y_i+1)+f(y_i)))
        // where y_i+1 = y_j
        for (int i = 1; i <= INSCATTER_INTEGRAL_SAMPLES; i++) {
            double y_j = i * dy;
            glm::dvec3 inScatteringRadiance_j = integrand(r, mu, muSun, nu, y_j);
            inScatteringRadiance += (inScatteringRadiance_i + inScatteringRadiance_j) / 2.0 * dy;
            inScatteringRadiance_i = inScatteringRadiance_j;
        }
        return inScatteringRadiance;
    };


    for (int layer = 0; layer < deltaSRayleighTexture.size(); layer++) {
        int k = 0;
        for (int y = 0; y < deltaSRayleighTexture[0].height; y++) {
            for (int x = 0; x < deltaSRayleighTexture[0].width; x++) {
                auto [r, dhdH] = step3DTexture(Rg, Rt, SAMPLES_R, layer);
                glm::dvec3 MuMuSunNu = common::unmappingMuMuSunNu(r, dhdH, SAMPLES_MU, Rg,
                    Rt, SAMPLES_MU_S, SAMPLES_NU, x, y
                );
                double mu = MuMuSunNu.r;
                double muSun = MuMuSunNu.g;
                double nu = MuMuSunNu.b;

                glm::dvec3 result = inscatter(r, mu, muSun, nu);
                if (std::isnan(result.r) || std::isnan(result.g) || std::isnan(result.b)) {
                    result.r = result.x;
                }

                deltaSRayleighTexture[layer].data[k] = result.r;
                deltaSRayleighTexture[layer].data[k + 1] = result.g;
                deltaSRayleighTexture[layer].data[k + 2] = result.b;

                k += deltaSRayleighTexture[0].components;
            }
        }
    }
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
                glm::dvec3 rst = glm::dvec3(x + 0.5, y + 0.5, layer + 0.5) /
                    glm::dvec3(
                        static_cast<double>(SAMPLES_MU_S * SAMPLES_NU),
                        static_cast<double>(SAMPLES_MU),
                        static_cast<double>(SAMPLES_R)
                    );

                glm::dvec3 rayleighInscattering = glm::dvec3(common::texture(deltaSRayleighTexture, rst));
                double mieInscattering = common::texture(deltaSMieTexture, rst).r;

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

void calculateInscattering(int scatteringOrder, CPUTexture3D& inscatteringTexture,
    const CPUTexture3D& deltaSTexture, int SAMPLES_MU_S, int SAMPLES_NU, int SAMPLES_MU,
    int SAMPLES_R)
{
    for (int layer = 0; layer < SAMPLES_R; layer++) {

        int k = 0;
        for (int y = 0; y < inscatteringTexture[0].height; y++) {
            for (int x = 0; x < inscatteringTexture[0].width; x++) {
                double nu = -1. +
                    std::floor(static_cast<double>(x) / static_cast<double>(SAMPLES_MU_S)) /
                    (static_cast<double>(SAMPLES_NU) - 1.0) * 2.0;

                glm::dvec3 uvw = glm::dvec3(x + 0.5, y + 0.5, layer + 0.5) /
                    glm::dvec3(
                        static_cast<double>(SAMPLES_MU_S * SAMPLES_NU),
                        static_cast<double>(SAMPLES_MU),
                        static_cast<double>(SAMPLES_R)
                );

                // See Bruneton and Neyret paper, "Angular Precision" paragraph to understanding why we
                // are dividing the S[L*] by the Rayleigh phase function.
                glm::dvec3 color = glm::dvec3(common::texture(deltaSTexture, uvw)) /
                    common::rayleighPhaseFunction(nu);

                inscatteringTexture[layer].data[k] += color.r;
                inscatteringTexture[layer].data[k + 1] += color.g;
                inscatteringTexture[layer].data[k + 2] += color.b;
                inscatteringTexture[layer].data[k + 3] = 0.0;
                k += inscatteringTexture[0].components;
            }
        }
    }
}

void calculateDeltaJ(int scatteringOrder, CPUTexture3D& deltaJ,
    const CPUTexture& deltaETexture, const CPUTexture3D& deltaSRTexture,
    const CPUTexture3D& deltaSMTexture, const CPUTexture& transmittanceTexture,
    double Rg, double Rt, double averageGroundReflectance, double HR,
    const glm::dvec3& betaRayleigh, double HM, const glm::dvec3& betaMieScattering,
    double mieG, int SAMPLES_MU_S, int SAMPLES_NU, int SAMPLES_MU, int SAMPLES_R)
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
                glm::dvec3 muMuSunNu = common::unmappingMuMuSunNu(r, dhdH, SAMPLES_MU, Rg,
                    Rt, SAMPLES_MU_S, SAMPLES_NU, x, y);
                double mu = muMuSunNu.r;
                double muSun = muMuSunNu.g;
                double nu = muMuSunNu.b;
                // Calculate the the light inScattered in direction
                // -vec(v) for the point at height r (vec(y) following Bruneton and Neyret's paper
                glm::dvec3 radianceJ = inscatter(r, mu, muSun, nu, Rt, Rg,
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



std::pair<double, glm::dvec4> step3DTexture(double Rg, double Rt, int rSamples, int layer)
{
    double atmospherePlanetRadius = Rg;
    double atmosphereRadius = Rt;
    //int _rSamples = textureSize.z;
    // See OpenGL redbook 8th Edition page 556 for Layered Rendering
    const double planet2 = atmospherePlanetRadius * atmospherePlanetRadius;
    const double diff = atmosphereRadius * atmosphereRadius - planet2;
    const double ri = static_cast<double>(layer) / static_cast<double>(rSamples - 1);
    double eps = 0.01;
    if (layer > 0) {
        if (layer == (rSamples - 1)) {
            eps = -0.001;
        }
        else {
            eps = 0.0;
        }
    }
    const double r = std::sqrt(planet2 + ri * ri * diff) + eps;
    const double dminG = r - atmospherePlanetRadius;
    const double dminT = atmosphereRadius - r;
    const double dh = std::sqrt(r * r - planet2);
    const double dH = dh + std::sqrt(diff);

    glm::dvec4 dhdH{ dminT, dH, dminG, dh };
    return std::make_pair(r, dhdH);
}

std::pair<glm::dvec3, glm::dvec3> inscatter(double r, double mu, double muSun, double nu,
    double Rt, double Rg, const CPUTexture& transmittanceTexture, bool ozoneLayerEnabled,
    double HO, double HM, double HR, const glm::dvec3& betaRayleigh,
    const glm::dvec3& betaMieScattering)
{
    const int INSCATTER_INTEGRAL_SAMPLES = 50;

    // Let's calculate S_M and S_R by integration along the eye ray path inside the
    // atmosphere, given a position r, a view angle (cosine) mu, a sun position angle
    // (cosine) muSun, and the angle (cosine) between the sun position and the view
    // direction, nu. Integrating using the Trapezoidal rule:
    // Integral(f(y)dy)(from a to b) = (b-a)/2n_steps*(Sum(f(y_i+1)+f(y_i)))
    glm::dvec3 S_R{ 0.0 };
    glm::dvec3 S_M{ 0.0 };

    double rayDist = common::rayDistance(r, mu, Rt, Rg);
    double dy = rayDist / static_cast<double>(INSCATTER_INTEGRAL_SAMPLES);
    //glm::dvec3 S_Ri;
    //glm::dvec3 S_Mi;
    auto [S_Ri, S_Mi] = integrand(r, mu, muSun, nu, 0.0, Rg, Rt, transmittanceTexture,
        ozoneLayerEnabled, HO, HM, HR
    );

    for (int i = 1; i <= INSCATTER_INTEGRAL_SAMPLES; i++) {
        double yj = static_cast<double>(i) * dy;
        auto [S_Rj, S_Mj] = integrand(r, mu, muSun, nu, yj, Rg, Rt, transmittanceTexture,
            ozoneLayerEnabled, HO, HM, HR
        );
        S_R += (S_Ri + S_Rj);
        S_M += (S_Mi + S_Mj);
        S_Ri = S_Rj;
        S_Mi = S_Mj;
    }
    S_R *= betaRayleigh * (rayDist / (2.0 * static_cast<double>(INSCATTER_INTEGRAL_SAMPLES)));
    S_M *= betaMieScattering * (rayDist / (2.0 * static_cast<double>(INSCATTER_INTEGRAL_SAMPLES)));

    return std::make_pair(S_R, S_M);
}

glm::dvec3 inscatter(double r, double mu, double muSun, double nu, double Rt, double Rg,
    double averageGroundReflectance, const CPUTexture& transmittanceTexture, double mieG,
    bool firstIteration, const CPUTexture& deltaETexture,
    const CPUTexture3D& deltaSRTexture, const CPUTexture3D& deltaSMTexture,
    int SAMPLES_MU_S, int SAMPLES_NU, int SAMPLES_MU, int SAMPLES_R,
    const glm::dvec3& betaRayleigh, const glm::dvec3& betaMieScattering, double HR, double HM)
{
    const double M_PI = 3.141592657;
    const int INSCATTER_SPHERICAL_INTEGRAL_SAMPLES = 16;
    // -- Spherical Coordinates Steps. phi e [0,2PI] and theta e [0, PI]
    const double stepPhi = (2.0 * M_PI) / static_cast<double>(INSCATTER_SPHERICAL_INTEGRAL_SAMPLES);
    const double stepTheta = M_PI / static_cast<double>(INSCATTER_SPHERICAL_INTEGRAL_SAMPLES);

    // Be sure to not get a cosine or height out of bounds
    r = glm::clamp(r, Rg, Rt);
    mu = glm::clamp(mu, -1.0, 1.0);
    muSun = glm::clamp(muSun, -1.0, 1.0);

    //  s sigma | theta v
    //   \      |      /
    //    \     |     /
    //     \    |    /
    //      \   |   /   theta + signam = ni
    //       \  |  /    cos(theta) = mu
    //        \ | /     cos(sigma) = muSun
    //         \|/      cos(ni)    = nu
    double mu2 = mu * mu;
    double muSun2 = muSun * muSun;
    double sinThetaSinSigma = std::sqrt(1.0 - mu2) * std::sqrt(1.0 - muSun2);
    // cos(sigma + theta) = cos(theta)cos(sigma)-sin(theta)sin(sigma)
    // cos(ni) = nu = mu * muSun - sqrt(1.0 - mu*mu)*sqrt(1.0 - muSun*muSun)
    // sin(theta) = sqrt(1.0 - mu*mu)
    // Now we make sure the angle between vec(s) and vec(v) is in the right range:
    nu = glm::clamp(nu, muSun * mu - sinThetaSinSigma, muSun * mu + sinThetaSinSigma);

    // Lets calculate the consine of the angle to the horizon:
    // theta is the angle between vec(v) and x
    // cos(PI-theta) = d/r
    // -cos(theta) = sqrt(r*r-Rg*Rg)/r
    double Rg2 = Rg * Rg;
    double r2 = r * r;
    double cosHorizon = -std::sqrt(r2 - Rg2) / r;

    // Now we get vec(v) and vec(s) from mu, muSun and nu:
    // Assuming:
    //              z |theta
    //                |\ vec(v) ||vec(v)|| = 1
    //                | \
    //                |__\_____x
    // sin(PI-theta) = x/||v|| => x = sin(theta) =? x = sqrt(1-mu*mu)
    // cos(PI-theta) = z/||v|| => z = cos(theta) = mu
    // v.y = 0 because ||v|| = 1
    glm::dvec3 v = glm::dvec3(sqrt(1.0 - mu2), 0.0, mu);

    // To obtain vec(s), we use the following properties:
    // ||vec(s)|| = 1, ||vec(v)|| = 1
    // vec(s) dot vec(v) = cos(ni) = nu
    // Following the same idea for vec(v), we now that s.z = cos(sigma) = muSun
    // So, from vec(s) dot vec(v) = cos(ni) = nu we have,
    // s.x*v.x +s.y*v.y + s.z*v.z = nu
    // s.x = (nu - s.z*v.z)/v.x = (nu - mu*muSun)/v.x
    double sx = (v.x == 0.0) ? 0.0 : (nu - muSun * mu) / v.x;
    // Also, ||vec(s)|| = 1, so:
    // 1 = sqrt(s.x*s.x + s.y*s.y + s.z*s.z)
    // s.y = sqrt(1 - s.x*s.x - s.z*s.z) = sqrt(1 - s.x*s.x - muSun*muSun)
    glm::dvec3 s = glm::dvec3(sx, std::sqrt(std::max(0.0, 1.0 - sx * sx - muSun2)), muSun);


    // In order to integrate over 4PI, we scan the sphere using the spherical coordinates
    // previously defined
    glm::dvec3 radianceJAcc = glm::dvec3(0.0);
    for (int theta_i = 0; theta_i < INSCATTER_SPHERICAL_INTEGRAL_SAMPLES; theta_i++) {
        double theta = (theta_i + 0.5) * stepTheta;
        double cosineTheta = std::cos(theta);
        double cosineTheta2 = cosineTheta * cosineTheta;
        double distanceToGround = 0.0;
        double groundReflectance = 0.0;
        glm::dvec3 groundTransmittance = glm::dvec3(0.0);

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
            double muGround = -(r * cosineTheta + distanceToGround) / Rg;
            // We can use the same triangle in calculate the distanceToGround to calculate
            // the cosine of the angle between the ground touching point at height Rg and
            // the zenith angle
            // double muGround = (r2 - distanceToGround*distanceToGround - Rg2)/(2*distanceToGround*Rg);
            // Access the Transmittance LUT in order to calculate the transmittance from
            // the ground point Rg, thorugh the atmosphere, at a distance: distanceToGround
            groundTransmittance = common::transmittance(transmittanceTexture, Rg,
                muGround, distanceToGround, Rg, Rt);
        }

        for (int phi_i = 0; phi_i < INSCATTER_SPHERICAL_INTEGRAL_SAMPLES; ++phi_i) {
            double phi = (static_cast<double>(phi_i) + 0.5) * stepPhi;
            // spherical coordinates: dw = dtheta*dphi*sin(theta)*rho^2
            // rho = 1, we are integrating over a unit sphere
            double dw = stepTheta * stepPhi * std::sin(theta);
            // w = (rho*sin(theta)*cos(phi), rho*sin(theta)*sin(phi), rho*cos(theta))
            double sinPhi = std::sin(phi);
            double sinTheta = std::sin(theta);
            double cosPhi = std::cos(phi);
            glm::dvec3 w = glm::dvec3(sinTheta * cosPhi, sinTheta * sinPhi, cosineTheta);

            // We calculate the Rayleigh and Mie phase function for the new scattering
            // angle:
            // cos(angle between vec(v) and vec(w)), ||v|| = ||w|| = 1
            double nuWV = dot(v, w);
            double phaseRayleighWV = common::rayleighPhaseFunction(nuWV);
            double phaseMieWV = common::miePhaseFunction(nuWV, mieG);

            glm::dvec3 groundNormal = (glm::dvec3(0.0, 0.0, r) + distanceToGround * w) / Rg;
            glm::dvec3 groundIrradiance = irradianceLUT(deltaETexture,
                glm::dot(groundNormal, s),
                Rg, Rg, Rt
            );

            // We finally calculate the radiance from the reflected ray from ground
            // (0.0 if not reflected)
            glm::dvec3 radianceJ1 = groundTransmittance * groundReflectance * groundIrradiance;

            // We calculate the Rayleigh and Mie phase function for the new scattering
            // angle:
            // cos(angle between vec(s) and vec(w)), ||s|| = ||w|| = 1
            double nuSW = glm::dot(s, w);
            // The first iteration is different from the others. In the first iteration
            // all the light InScattered is coming from the initial pre-computed single
            // InScattered light. We stored these values in the deltaS textures
            // (Ray and Mie), and in order to avoid problems with the high angle
            // dependency in the phase functions, we don't include the phase functions on
            // those tables (that's why we calculate them now).
            if (firstIteration) {
                double phaseRaySW = common::rayleighPhaseFunction(nuSW);
                double phaseMieSW = common::miePhaseFunction(nuSW, mieG);
                // We can now access the values for the single InScattering in the
                // textures deltaS textures.
                glm::dvec3 singleRay = glm::dvec3(common::texture4D(deltaSRTexture, r, w.z,
                    muSun, nuSW, Rg, SAMPLES_MU, Rt, SAMPLES_R, SAMPLES_MU_S, SAMPLES_NU));
                glm::dvec3 singleMie = glm::dvec3(common::texture4D(deltaSMTexture, r, w.z,
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
                radianceJ1 += glm::dvec3(common::texture4D(deltaSRTexture, r, w.z, muSun,
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

std::pair<glm::dvec3, glm::dvec3> integrand(double r, double mu, double muSun, double nu,
    double y, double Rg, double Rt, const CPUTexture& transmittanceTexture,
    bool ozoneLayerEnabled, double HO, double HM, double HR)
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
    glm::dvec3 S_R{ 0.0 };
    glm::dvec3 S_M{ 0.0 };

    // cosine law
    double ri = std::max(std::sqrt(r * r + y * y + 2.0 * r * mu * y), Rg);

    // Considering the Sun as a parallel light source, thew vector s_i = s.
    // So muSun_i = (vec(y_i) dot vec(s))/r_i = ((vec(x) + vec(yi-x)) dot vec(s))/r_i
    // muSun_i = (vec(x) dot vec(s) + vec(yi-x) dot vec(s))/r_i = (r*muSun + yi*nu)/r_i
    double muSun_i = (nu * y + muSun * r) / ri;

    // If the muSun_i is smaller than the angle to horizon (no sun radiance hitting the
    // point y), we return S_R = S_M = 0.0.
    if (muSun_i >= -safeSqrt(1.0 - Rg * Rg / (ri * ri))) {
        // It's the transmittance from the point y (ri) to the top of atmosphere in direction
        // of the sun (muSun_i) and the transmittance from the observer at x (r) to y (ri).
        glm::dvec3 transmittanceY =
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

glm::dvec3 irradianceLUT(const CPUTexture& lut, double muSun, double r, double Rg, double Rt)
{
    // See Bruneton paper and Coliene to understand the mapping
    double u_muSun = (muSun + 0.2) / 1.2;
    double u_r = (r - Rg) / (Rt - Rg);
    return glm::dvec3(common::texture(lut, u_muSun, u_r));
}

} // namespace scattering

} // namespace atmosphere

} // namespace openspace
