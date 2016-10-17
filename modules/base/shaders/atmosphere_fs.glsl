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

#define EPSILON 0.0001f

// Sun Irradiance
const float ISun = 50.0;
const uint numberOfShadows = 1;

struct ShadowRenderingStruct {
        float xu, xp;
        float rs, rc;
        vec3 sourceCasterVec;
        vec3 casterPositionVec;
        bool isShadowing;
};

uniform ShadowRenderingStruct shadowDataArray[numberOfShadows];

uniform mat4 completeInverse;
uniform mat4 projInverse;

uniform vec4 campos;
uniform vec4 objpos;
uniform vec3 sun_pos;

uniform vec4 cameraPosObj;
uniform vec4 planetPositionObj;
uniform vec3 sunPositionObj;

uniform bool _performShading = true;
uniform float transparency;
uniform int shadows;

uniform float screenX;
uniform float screenY;
uniform float screenWIDTH;
uniform float screenHEIGHT;

uniform float time;
uniform sampler2D texture1;
uniform sampler2D nightTex;
uniform sampler2D cloudsTexture;

uniform sampler2D reflectanceTexture;
uniform sampler2D transmittanceTexture;
uniform sampler2D irradianceTexture;
uniform sampler3D inscatterTexture;

in vec2 vs_st;
in vec2 vs_nightTex;
in vec4 vs_normal;
in vec4 vs_position;
in vec4 vs_posWorld;
 
#include "hdr.glsl"
#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"
#include "atmosphere_common.glsl"

vec4 butterworthFunc(const float d, const float r, const float n) {
    return vec4(vec3(sqrt(r/(r + pow(d, 2*n)))), 1.0);    
}

/*******************************************************************************
 ****** ALL CALCULATIONS FOR ATMOSPHERE ARE KM AND IN OBJECT SPACE SYSTEM ******
 *******************************************************************************/

/* Calculates the intersection of the view ray direction with the atmosphere and 
 * returns the first intersection (0.0 when inside atmosphere): offset
 * and the second intersection: maxLength
 */

struct Ray {
  vec4 origin;
  vec4 direction;
};

struct Ellipsoid {
  vec4 center;
  vec4 size;
};

bool intersectEllipsoid(const Ray ray, const Ellipsoid ellipsoid, out float offset, out float maxLength) {
  vec4 O_C = ray.origin-ellipsoid.center;
  vec4 dir = normalize(ray.direction);

  offset    = 0.0f;
  maxLength = 0.0f;

  float a =
    ((dir.x*dir.x)/(ellipsoid.size.x*ellipsoid.size.x))
    + ((dir.y*dir.y)/(ellipsoid.size.y*ellipsoid.size.y))
    + ((dir.z*dir.z)/(ellipsoid.size.z*ellipsoid.size.z));
  float b =
    ((2.f*O_C.x*dir.x)/(ellipsoid.size.x*ellipsoid.size.x))
    + ((2.f*O_C.y*dir.y)/(ellipsoid.size.y*ellipsoid.size.y))
    + ((2.f*O_C.z*dir.z)/(ellipsoid.size.z*ellipsoid.size.z));
  float c =
    ((O_C.x*O_C.x)/(ellipsoid.size.x*ellipsoid.size.x))
    + ((O_C.y*O_C.y)/(ellipsoid.size.y*ellipsoid.size.y))
    + ((O_C.z*O_C.z)/(ellipsoid.size.z*ellipsoid.size.z))
    - 1.f;
  
  float d = ((b*b)-(4.f*a*c));
  if ( d<0.f || a==0.f || b==0.f || c==0.f )
    return false;
  
  d = sqrt(d);
  
  float t1 = (-b+d)/(2.f*a);
  float t2 = (-b-d)/(2.f*a);
  
  if( t1<=EPSILON && t2<=EPSILON )
    return false; // both intersections are behind the ray origin

  bool back = (t1<=EPSILON || t2<=EPSILON); // If only one intersection (t>0) then we are inside the ellipsoid and the intersection is at the back of the ellipsoid
  float t=0.f;
  if( t1<=EPSILON )
    t = t2;
  else
    if( t2<=EPSILON )
      t = t1;
    else
      t=(t1<t2) ? t1 : t2;
  
  if( t<EPSILON ) return false; // Too close to intersection

  vec4 intersection = ray.origin + t*dir;
  vec4 normal = intersection-ellipsoid.center;
  normal.x = 2.f*normal.x/(ellipsoid.size.x*ellipsoid.size.x);
  normal.y = 2.f*normal.y/(ellipsoid.size.y*ellipsoid.size.y);
  normal.z = 2.f*normal.z/(ellipsoid.size.z*ellipsoid.size.z);
  
  normal.w = 0.f;
  normal *= (back) ? -1.f : 1.f;
  normal = normalize(normal);

  
  return true;
}

bool intersectAtmosphere(const vec4 planetPos, const vec3 rayDirection, const float sphereRadius, 
                        out float offset, out float maxLength) {
    offset = 0.0f;
    maxLength = 0.0f;
    
    vec3 l   =  planetPos.xyz - cameraPosObj.xyz;
    float s  = dot(l, rayDirection);
    float l2 = dot(l, l);
    
    // sphereRadius in Km
    float r  = sphereRadius - EPSILON; // EPSILON to avoid surface acne
    float r2 = r * r;
        
    if (l2 <= r2) {
        // ray origin inside sphere
        float m2 = l2 - (s*s);
        float q = sqrt(r2 - m2);
        maxLength = s + q;
        
        return true;
    }
    else if (s >= 0.0) {
        // ray outside sphere
        float m2 = l2 - (s*s);
        if (m2 <= r2) {
            // ray hits atmosphere
            float q = sqrt(r2 - m2);
            offset = s-q;
            maxLength = (s+q)-offset;
            
            return true;
        }
    }
    
    return false;
}
 
// Rayleigh phase function
float phaseFunctionR(float mu) {
    return (3.0 / (16.0 * M_PI)) * (1.0 + mu * mu);
}

// Mie phase function
float phaseFunctionM(float mu) {
    return 1.5 * 1.0 / (4.0 * M_PI) * (1.0 - mieG*mieG) * pow(1.0 + (mieG*mieG) - 2.0*mieG*mu, -3.0/2.0) * (1.0 + mu * mu) / (2.0 + mieG*mieG);
}

float opticalDepth(float H, float r, float mu, float d) {
    float a = sqrt((0.5/H)*r);
    vec2 a01 = a*vec2(mu, mu + d / r);
    vec2 a01s = sign(a01);
    vec2 a01sq = a01*a01;
    float x = a01s.y > a01s.x ? exp(a01sq.x) : 0.0;
    vec2 y = a01s / (2.3193*abs(a01) + sqrt(1.52*a01sq + 4.0)) * vec2(1.0, exp(-d/H*(d/(2.0*r)+mu)));
    return sqrt((6.2831*H)*r) * exp((Rg-r)/H) * (x + dot(y, vec2(1.0, -1.0)));
}

vec4 texture4D(sampler3D table, float r, float mu, float muS, float nu)
{
    float H = sqrt(Rt * Rt - Rg * Rg);
    float rho = sqrt(r * r - Rg * Rg);
    float rmu = r * mu;
    float delta = rmu * rmu - r * r + Rg * Rg;
    vec4 cst = rmu < 0.0 && delta > 0.0 ? vec4(1.0, 0.0, 0.0, 0.5 - 0.5 / float(RES_MU)) : vec4(-1.0, H * H, H, 0.5 + 0.5 / float(RES_MU));
    float uR = 0.5 / float(RES_R) + rho / H * (1.0 - 1.0 / float(RES_R));
    float uMu = cst.w + (rmu * cst.x + sqrt(delta + cst.y)) / (rho + cst.z) * (0.5 - 1.0 / float(RES_MU));
    float uMuS = 0.5 / float(RES_MU_S) + (atan(max(muS, -0.1975) * tan(1.26 * 1.1)) / 1.1 + (1.0 - 0.26)) * 0.5 * (1.0 - 1.0 / float(RES_MU_S));
    float lerp = (nu + 1.0) / 2.0 * (float(RES_NU) - 1.0);
    float uNu = floor(lerp);
    lerp = lerp - uNu;
    return texture(table, vec3((uNu + uMuS) / float(RES_NU), uMu, uR)) * (1.0 - lerp) +
           texture(table, vec3((uNu + uMuS + 1.0) / float(RES_NU), uMu, uR)) * lerp;
}

vec3 analyticTransmittance(float r, float mu, float d) {
    return exp(- betaR * opticalDepth(HR, r, mu, d) - betaMEx * opticalDepth(HM, r, mu, d));
}

vec3 getMie(vec4 rayMie) {
    return rayMie.rgb * rayMie.a / max(rayMie.r, 1e-4) * (betaR.r / betaR);
}

vec2 getTransmittanceUV(float r, float mu) {
    float uR, uMu;
    uR = sqrt((r - Rg) / (Rt - Rg));
    uMu = atan((mu + 0.15) / (1.0 + 0.15) * tan(1.5)) / 1.5;
    return vec2(uMu, uR);
}

vec3 transmittanceFromTexture(float r, float mu) {
    vec2 uv = getTransmittanceUV(r, mu);
    return texture(transmittanceTexture, uv).rgb;
}

vec3 transmittanceWithShadow(float r, float mu) {
    return mu < -sqrt(1.0 - (Rg / r) * (Rg / r)) ? vec3(0.0) : transmittanceFromTexture(r, mu);
}

vec3 transmittance(float r, float mu, vec3 v, vec3 x0) {
    vec3 result;
    float r1 = length(x0);
    float mu1 = dot(x0, v) / r;
    if (mu > 0.0) {
        result = min(transmittanceFromTexture(r, mu) / 
        transmittanceFromTexture(r1, mu1), 1.0);
    } else {
        result = min(transmittanceFromTexture(r1, -mu1) / 
        transmittanceFromTexture(r, -mu), 1.0);
    }
    return result;
}

vec2 getIrradianceUV(float r, float muS) {
    float uR = (r - Rg) / (Rt - Rg);
    float uMuS = (muS + 0.2) / (1.0 + 0.2);
    return vec2(uMuS, uR);
}

vec3 irradiance(sampler2D sampler, float r, float muS) {
    vec2 uv = getIrradianceUV(r, muS);
    return texture(sampler, uv).rgb;
}

/* 
 * Calculates the light scattering in the view direction comming from other 
 * light rays scattered in the atmosphere.
 * The view direction here is the ray: x + tv, s is the sun direction,
 * r and mu the position and zenith cossine angle as in the paper.
 */
vec3 inscatterLight(inout vec3 x, inout float t, vec3 v, vec3 s, 
                    out float r, out float mu, out vec3 attenuation) {
    vec3 result;
    r = length(x);
    mu = dot(x, v) / r;
    float d = -r * mu - sqrt(r * r * (mu * mu - 1.0) + Rt * Rt);
    if (d > 0.0) { 
        x += d * v;
        t -= d;
        mu = (r * mu + d) / Rt;
        r = Rt;
    }
    // Intersects atmosphere?
    if (r <= Rt) { 
        float nu = dot(v, s);
        float muS = dot(x, s) / r;
        float phaseR = phaseFunctionR(nu);
        float phaseM = phaseFunctionM(nu);
        vec4 inscatter = max(texture4D(inscatterTexture, r, mu, muS, nu), 0.0);
        if (t > 0.0) {
            vec3 x0    = x + t * v;
            float r0   = length(x0);
            float rMu0 = dot(x0, v);
            float mu0  = rMu0 / r0;
            float muS0 = dot(x0, s) / r0;
            
            attenuation = analyticTransmittance(r, mu, t);
            //attenuation = transmittance(r, mu, v, x+t*v);
            
            //The following Code is generating surface acne on atmosphere. JCC
            // We need a better acne avoindance constant (0.01). Done!! Adaptive from distance to x
            if (r0 > Rg + 0.1*r) {
                inscatter = max(inscatter - attenuation.rgbr * texture4D(inscatterTexture, r0, mu0, muS0, nu), 0.0);
                const float EPS = 0.004;
                float muHoriz = -sqrt(1.0 - (Rg / r) * (Rg / r));
                if (abs(mu - muHoriz) < EPS) {
                    float a = ((mu - muHoriz) + EPS) / (2.0 * EPS);

                    mu = muHoriz - EPS;
                    r0 = sqrt(r * r + t * t + 2.0 * r * t * mu);
                    mu0 = (r * mu + t) / r0;
                    vec4 inScatter0 = texture4D(inscatterTexture, r, mu, muS, nu);
                    vec4 inScatter1 = texture4D(inscatterTexture, r0, mu0, muS0, nu);
                    vec4 inScatterA = max(inScatter0 - attenuation.rgbr * inScatter1, 0.0);

                    mu = muHoriz + EPS;
                    r0 = sqrt(r * r + t * t + 2.0 * r * t * mu);
                    mu0 = (r * mu + t) / r0;
                    inScatter0 = texture4D(inscatterTexture, r, mu, muS, nu);
                    inScatter1 = texture4D(inscatterTexture, r0, mu0, muS0, nu);
                    vec4 inScatterB = max(inScatter0 - attenuation.rgbr * inScatter1, 0.0);

                    inscatter = mix(inScatterA, inScatterB, a);
                }
            }
        }
        inscatter.w *= smoothstep(0.00, 0.02, muS);
        result = max(inscatter.rgb * phaseR + getMie(inscatter) * phaseM, 0.0);
        
    } else {
        // No intersection with earth
        result = vec3(0.0);
    }
    return result * ISun;
}

vec3 groundColor(vec3 x, float t, vec3 v, vec3 s, float r, float mu, vec3 attenuation)
{
    vec3 result;
    // Ray hits ground
    if (t > 0.0) {
        vec3 x0 = x + t * v;
        float r0 = length(x0);
        vec3 n = x0 / r0;
        
        // Fixing texture coordinates:
        vec4 reflectance = texture(reflectanceTexture, vs_st) * vec4(0.2, 0.2, 0.2, 1.0);
        
        // The following code is generating surface acne in ground. 
        // It is only necessary inside atmosphere rendering. JCC
        // if (r0 > Rg + 0.01) {
        //     reflectance = vec4(0.4, 0.4, 0.4, 0.0);
        // }

        float muS = dot(n, s);
        vec3 sunLight = transmittanceWithShadow(r0, muS);
        
        vec3 groundSkyLight = irradiance(irradianceTexture, r0, muS);
        
        vec4 clouds = vec4(0.85)*texture(cloudsTexture, vs_st);
        vec3 groundColor = (reflectance.rgb + clouds.rgb) * 
        (max(muS, 0.0) * sunLight + groundSkyLight) * ISun / M_PI;
        
        // Yellowish reflection from sun on oceans and rivers
        if (reflectance.w > 0.0) {
            vec3 h = normalize(s - v);
            float fresnel = 0.02 + 0.98 * pow(1.0 - dot(-v, h), 5.0);
            float waterBrdf = fresnel * pow(max(dot(h, n), 0.0), 150.0);
            groundColor += reflectance.w * max(waterBrdf, 0.0) * sunLight * ISun;
        }

        result = attenuation * groundColor;
    } else { 
        // No hit
        result = vec3(0.0);
    }
    return result;
}

vec3 sunColor(vec3 x, float t, vec3 v, vec3 s, float r, float mu) {
    if (t > 0.0) {
        return vec3(0.0);
    } else {
        vec3 transmittance = r <= Rt ? transmittanceWithShadow(r, mu) : vec3(1.0);
        float isun = step(cos(M_PI / 180.0), dot(v, s)) * ISun; 
        return transmittance * isun;
    }
}

/***********************************************************************
 ******* CALCULATIONS FOR SHADOWS ARE IN WORLD SPACE IN METERS *********
 ***********************************************************************/
 // TODO: Change calculations for view space in KM.
vec4 calcShadow(const ShadowRenderingStruct shadowInfoArray[numberOfShadows], const vec3 position) {
    if (shadowInfoArray[0].isShadowing) {
        vec3 pc = shadowInfoArray[0].casterPositionVec - position;
        vec3 sc_norm = normalize(shadowInfoArray[0].sourceCasterVec); // we can pass this normalized to the shader
        vec3 pc_proj = dot(pc, sc_norm) * sc_norm;
        vec3 d = pc - pc_proj;
        
        float length_d = length(d);
        float length_pc_proj = length(pc_proj);
        
        float r_p_pi = shadowInfoArray[0].rc * (length_pc_proj + shadowInfoArray[0].xp) / shadowInfoArray[0].xp;
        
        //float r_u_pi = shadowInfoArray[0].rc * (length_pc_proj + shadowInfoArray[0].xu) / shadowInfoArray[0].xu;
        float r_u_pi = shadowInfoArray[0].rc * (shadowInfoArray[0].xu - length_pc_proj) / shadowInfoArray[0].xu;
        
        if ( length_d < r_u_pi ) { // umbra
            //return vec4(0.0, 0.0, 0.0, 1.0);
            //return vec4(1.0, 0.0, 0.0, 1.0);
            return butterworthFunc(length_d, r_u_pi, 4.0);
        }
        else if ( length_d < r_p_pi ) {// penumbra
            //return vec4(0.5, 0.5, 0.5, 1.0);
            //return vec4(0.0, 1.0, 0.0, 1.0);
            return vec4(vec3(length_d/r_p_pi), 1.0);
        }
    }
     
    return vec4(1.0);
}

Fragment getFragment() {
    vec4 position = vs_position;
    float depth = pscDepth(position);
    vec4 diffuse = texture(texture1, vs_st);
    vec4 diffuse2 = texture(nightTex, vs_st);
    vec4 clouds = texture(cloudsTexture, vs_st);

    Fragment frag;
    if (_performShading) {
        // atmosphere
        vec4 viewport = vec4(screenX, screenY, screenWIDTH, screenHEIGHT);
        vec4 ndcPos;
        ndcPos.xy = ((2.0 * gl_FragCoord.xy) - (2.0 * viewport.xy)) / (viewport.zw) - 1;
        //ndcPos.x = ((2.0f * gl_FragCoord.x) - (2.0f * viewport.x)) / viewport.z - 1.0f;
        //ndcPos.y = 1.0f - (2.0f * gl_FragCoord.y) / viewport.w;
        ndcPos.z = (2.0f * gl_FragCoord.z - gl_DepthRange.near - gl_DepthRange.far) / 
        (gl_DepthRange.far - gl_DepthRange.near);
        ndcPos.w = 1.0f;
        vec4 clipPos = ndcPos / gl_FragCoord.w;
        //vec4 clipPos = ndcPos;
        //clipPos.z = -1.0;
        //clipPos.w = 1.0;
        vec4 projCoords = projInverse * clipPos;
        vec4 viewDirection =  normalize(completeInverse * vec4(projCoords.xyz, 0.0));
        vec3 v = normalize(viewDirection.xyz);
        
        float offset, maxLength;
        vec4 ppos = vec4(0.0);
        //if (intersectAtmosphere(planetPositionObj, v, Rt, offset, maxLength)) {
        if (intersectAtmosphere(ppos, v, Rg, offset, maxLength)) {
            // Following paper nomenclature
            float t  = offset;
            vec3 x   = cameraPosObj.xyz;// + offset * v;
            float r  = length(x);
            float mu = dot(x, v) / r;
            vec3 s   = normalize(sunPositionObj);
            
            vec3 attenuation;
            vec3 inscatterColor = inscatterLight(x, t, v, s, r, mu, attenuation); 
            vec3 groundColor = groundColor(x, t, v, s, r, mu, attenuation);
            vec3 sunColor = sunColor(x, t, v, s, r, mu); 
            
            //diffuse = HDR(vec4(sunColor + groundColor + inscatterColor, 1.0));      
            //diffuse = HDR(vec4(sunColor, 1.0)); 
            //diffuse = HDR(vec4(groundColor, 1.0)); 
            //diffuse = HDR(vec4(inscatterColor, 1.0)); 
            
            //diffuse = HDR(vec4(sunColor + groundColor + inscatterColor, 1.0) + diffuse2); 
            diffuse = HDR((vec4(sunColor + groundColor + inscatterColor, 1.0) + diffuse2) *
                        calcShadow(shadowDataArray, vs_posWorld.xyz) ); 
        }
        // else
        //     diffuse = HDR(diffuse);
    }
    
    diffuse[3] = transparency;
    frag.color = diffuse;
    frag.depth = depth;

    return frag;
}
