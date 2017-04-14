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

#version 400

#define EPSILON 0.0001f

// Double Precision Versions:
uniform dmat4 dSgctProjectionMatrix;
uniform dmat4 dInverseTransformMatrix;
uniform dmat4 dScaleTransformMatrix;
uniform dmat4 dInverseScaleTransformMatrix;
uniform dmat4 dObjToWorldTransform;
uniform dmat4 dWorldToObjectTransform;
uniform dmat4 dWorldToOsEyeTransform;
uniform dmat4 dOsEyeToWorldTransform; // OS Eye to World
uniform dmat4 dOsEyeToSGCTEyeTranform; // OS Eye to SGCT Eye
uniform dmat4 dSgctEyeToOSEyeTranform; // SGCT Eye to OS Eye
uniform dmat4 dSgctEyeToClipTranform; // SGCT Eye to SGCT Project Clip
uniform dmat4 dInverseSgctProjectionMatrix; // Clip to SGCT Eye
uniform dmat4 dInverseCamRotTransform;

// Double Precision Versions:
uniform dvec4 dObjpos;
uniform dvec3 dCampos;
uniform dmat3 dCamrot;

uniform dvec3 sunDirectionObj;

uniform bool _performShading = true;
/*
uniform float transparency;
uniform int shadows;

uniform float screenX;
uniform float screenY;
uniform float screenWIDTH;
uniform float screenHEIGHT;

uniform vec2 depthrange;

uniform float time;
*/


uniform sampler2D reflectanceTexture;
uniform sampler2D irradianceTexture;
uniform sampler3D inscatterTexture;
 
#include "hdr.glsl"
#include "atmosphere_common.glsl"

layout(location = 0) out vec4 renderTarget;

in vec3 interpolatedNDCPos;
//in vec4 vertexPosObjVS;


/*******************************************************************************
 ****** ALL CALCULATIONS FOR ATMOSPHERE ARE KM AND IN OBJECT SPACE SYSTEM ******
 *******************************************************************************/

/* Calculates the intersection of the view ray direction with the atmosphere and 
 * returns the first intersection (0.0 when inside atmosphere): offset
 * and the second intersection: maxLength
 */

struct dRay {
  dvec4 origin;
  dvec4 direction;
};

struct Ellipsoid {
  dvec4 center;
  dvec4 size;
};

bool dIntersectEllipsoid(const dRay ray, const Ellipsoid ellipsoid, out double offset, out double maxLength) {
  dvec4 O_C = ray.origin - ellipsoid.center;
  dvec4 dir = normalize(ray.direction);

  offset    = 0.0f;
  maxLength = 0.0f;

  double a =
    ((dir.x*dir.x)/(ellipsoid.size.x*ellipsoid.size.x))
    + ((dir.y*dir.y)/(ellipsoid.size.y*ellipsoid.size.y))
    + ((dir.z*dir.z)/(ellipsoid.size.z*ellipsoid.size.z));
  double b =
    ((2.f*O_C.x*dir.x)/(ellipsoid.size.x*ellipsoid.size.x))
    + ((2.f*O_C.y*dir.y)/(ellipsoid.size.y*ellipsoid.size.y))
    + ((2.f*O_C.z*dir.z)/(ellipsoid.size.z*ellipsoid.size.z));
  double c =
    ((O_C.x*O_C.x)/(ellipsoid.size.x*ellipsoid.size.x))
    + ((O_C.y*O_C.y)/(ellipsoid.size.y*ellipsoid.size.y))
    + ((O_C.z*O_C.z)/(ellipsoid.size.z*ellipsoid.size.z))
    - 1.f;
  
  double d = ((b * b)-(4.0 * a * c));
  if ( d < 0.f || a == 0.f || b == 0.f || c == 0.f )
    return false;
  
  d = sqrt(d);
  
  double t1 = (-b+d) / (2.0 * a);
  double t2 = (-b-d) / (2.0 * a);
  
  if ( t1 <= EPSILON && t2 <= EPSILON )
    return false; // both intersections are behind the ray origin

  // If only one intersection (t>0) then we are inside the ellipsoid and the intersection is at the back of the ellipsoid
  bool back = (t1 <= EPSILON || t2 <= EPSILON); 
  double t  = 0.0;
  if ( t1 <= EPSILON ) {
    t = t2;
  } else {
    if( t2 <= EPSILON )
      t = t1;
    else
      t = (t1 < t2) ? t1 : t2;
  }

  if ( t<EPSILON ) 
    return false; // Too close to intersection

  dvec4 intersection = ray.origin + t * dir;
  dvec4 normal       = intersection - ellipsoid.center;
  normal.x = 2.0 * normal.x / (ellipsoid.size.x * ellipsoid.size.x);
  normal.y = 2.0 * normal.y / (ellipsoid.size.y * ellipsoid.size.y);
  normal.z = 2.0 * normal.z / (ellipsoid.size.z * ellipsoid.size.z);
  
  normal.w = 0.0;
  normal  *= (back) ? -1.0 : 1.0;
  normal  = normalize(normal);
  
  return true;
}

/* Function to calculate the initial intersection of the eye (camera) ray
 * with the atmosphere.
 * In (all parameters in the same coordinate system and same units):
 * - planet position
 * - ray direction (normalized)
 * - eye position
 * - atmosphere radius
 * Out: true if an intersection happens, false otherwise
 * - inside: true if the ray origin is inside atmosphere, false otherwise
 * - offset: the initial intersection distance from eye position when 
 *           the eye is outside the atmosphere
 * - maxLength : the second intersection distance from eye position when the
 *               eye is outside the atmosphere or the initial (and only) 
 *               intersection of the ray with atmosphere when the eye position
 *               is inside atmosphere.
 */
bool dAtmosphereIntersection(const dvec3 planetPosition, const dRay ray, const double atmRadius,
                            out bool inside, out double offset, out double maxLength ) {
  dvec3  l  = planetPosition - ray.origin.xyz;
  double s  = dot(l, ray.direction.xyz);
  double l2 = dot(l, l);
  double r2 = (atmRadius - EPSILON) *  (atmRadius - EPSILON); // avoiding surface acne

  // Ray origin (eye position) is behind sphere
  if ((s < 0.0) && (l2 > r2)) {
    inside    = false;
    offset    = 0.0;
    maxLength = 0.0;
    return false;
  }

  double m2 = l2 - s*s;

  // Ray misses atmospere
  if (m2 > r2) {
    inside    = false;
    offset    = 0.0;
    maxLength = 0.0;
    return false;
  }

  // We already now the ray hits the atmosphere

  // If q = 0.0f, there is only one intersection
  double q = sqrt(r2 - m2);

  // If l2 < r2, the ray origin is inside the sphere
  if (l2 > r2) {
    inside    = false;
    offset    = s - q;
    maxLength = s + q;
  } else {
    inside    = true;
    offset    = -1.0;
    maxLength = s + q;
  }
  
  return true;
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

vec3 analyticTransmittance(float r, float mu, float d) {
  return exp(- betaRayleigh * opticalDepth(HR, r, mu, d) -
             betaMieExtinction * opticalDepth(HM, r, mu, d));
}

// vec2 getIrradianceUV(float r, float muSun) {
//   float uR = (r - Rg) / (Rt - Rg);
//   float uMuS = (muSun + 0.2) / (1.0 + 0.2);
//   return vec2(uMuS, uR);
// }

vec3 irradiance(sampler2D sampler, const float r, const float muSun) {
  float u_r     = (r - Rg) / (Rt - Rg);
  float u_muSun = (muSun + 0.2) / (1.0 + 0.2);
  return texture(sampler, vec2(u_muSun, u_r)).rgb;
}

/* 
 * Calculates the light scattering in the view direction comming from other 
 * light rays scattered in the atmosphere.
 * Following the paper:  S[L]|x - T(x,xs) * S[L]|xs
 * The view direction here is the ray: x + tv, s is the sun direction,
 * r and mu the position and zenith cosine angle as in the paper.
 * Arguments:
 * x := camera position
 * t := ray displacement variable after calculating the intersection with the 
 * atmosphere. It is the distance from the camera to the last intersection with
 * the atmosphere
 * v := view direction (ray's direction) (normalized)
 * s := Sun direction (normalized)
 * r := out of ||x|| inside atmosphere (or top of atmosphere)
 * mu := out of cosine of the zenith view angle
 * attenuation := out of transmittance T(x,x0). This will be used later when
 * calculating the reflectance R[L].
 */
vec3 inscatterRadiance(inout vec3 x, inout float t, const vec3 v, const vec3 s, 
                    out float r, out float mu, out vec3 attenuation) {
  vec3 radiance;
  
  r  = length(x);
  mu = dot(x, v) / r;

  float mu2 = mu * mu;
  float r2  = r * r;
  float Rt2 = Rt * Rt;
  float Rg2 = Rg * Rg;

  // Dist stores the distance from the camera position
  // to the first (the only one in some cases) intersection of the
  // light ray and the top of atmosphere.
  
  // From the cosine law for x0 at top of atmosphere:
  // Rt^2 = r^2 + dist^2 - 2*r*dist*cos(PI - theta)
  // Pay attentation to the -sqrt, it means we are 
  // considering the distance from observer to the 
  // first intersection with the atmosphere.
  float dist = -r * mu - sqrt(r2 * (mu2 - 1.0f) + Rt2);
  
  // Are we at space?
  if (dist > 0.0f) {
    // Because we are at space, we must obtain the vector x,
    // the correct cosine of between x and v and the right height r,
    // with the x in top of atmosphere.
    // What we do is to move from the starting point x (camera position)
    // to the point on the atmosphere. So, because we have a new x,
    // we must also calculate the new cosine between x and v. s is the
    // same because we consider the Sun as a parallel ray light source.
    t -= dist;
    x += dist * v;
    // mu(x0 and v)
    // cos(theta') = (x0 dot v)/(||x0||*||v||) = ((x + dist*v) dot v)/(Rt * 1)
    // cos(theta') = mu' = (r*mu + dist)/Rt
    mu  = (r * mu + dist) / Rt;
    mu2 = mu * mu;
    r   = Rt;
    r2  = r * r;    
  }  

  // Intersects atmosphere?
  if (r <= Rt + EPSILON) { 
    float nu                = dot(v, s);
    float muSun             = dot(x, s) / r;
    float rayleighPhase     = rayleighPhaseFunction(nu);
    float miePhase          = miePhaseFunction(nu);
    
    // S[L](x,s,v)
    vec4 inscatterRadiance = max(texture4D(inscatterTexture, r, mu, muSun, nu), 0.0);
    
    // After removing the initial path from camera pos to top of atmosphere or the
    // current camera position if inside atmosphere, t > 0
    if (t > 0.0) {
      // Here we must test if we are hitting the ground:
      bool  insideATM    = false;
      double offset      = 0.0f;
      double maxLength   = 0.0f;
      dRay ray;
      ray.direction = vec4(v, 0.0);
      ray.origin = vec4(x, 1.0);     
      bool  hitGround = dAtmosphereIntersection(vec3(0.0), ray,  Rg,
                                                insideATM, offset, maxLength);
      if (hitGround) {
        t = float(offset); 
      }
      // Calculate the zenith angles for x0 and v, s:
      vec3  x0     = x + t * v;
      float r0     = length(x0);
      float mu0    = dot(x0, v) / r0;
      float muSun0 = dot(x0, s) / r0;

      // Transmittance from point r, direction mu, distance t
      // By Analytical calculation
      attenuation = analyticTransmittance(r, mu, t);

      // By Texture Access
      //attenuation = transmittance(r, mu, v, x0);
      
      //The following Code is generating surface acne on atmosphere. JCC
      // We need a better acne avoidance constant (0.01). Done!! Adaptive from distance to x
      //if (r0 > Rg + (0.1f * r)) {
      // It r0 > Rg it means the ray hits something inside the atmosphere. So we need to
      // remove the inScattering contribution from the main ray from the hitting point
      // to the end of the ray.
      
      if (r0 > Rg + (0.01f)) {
        // Here we use the idea of S[L](a->b) = S[L](b->a), and get the S[L](x0, v, s)
        // Then we calculate S[L] = S[L]|x - T(x, x0)*S[L]|x0
        inscatterRadiance = max(inscatterRadiance - attenuation.rgbr * texture4D(inscatterTexture, r0, mu0, muSun0, nu), 0.0);
        
        // cos(PI-thetaH) = dist/r
        // cos(thetaH) = - dist/r
        // muHorizon = -sqrt(r^2-Rg^2)/r = -sqrt(1-(Rg/r)^2)
        float muHorizon = -sqrt(1.0f - (Rg2 / r2));

        // In order to avoid imprecision problems near horizon,
        // we interpolate between two points: above and below horizon
        const float INTERPOLATION_EPS = 0.004f; // precision const from Brunetton
        if (abs(mu - muHorizon) < INTERPOLATION_EPS) {
          // We want an interpolation value close to 1/2, so the
          // contribution of each radiance value is almost the same
          // or it has a havey weight if from above or below horizon
          float interpolationValue = ((mu - muHorizon) + INTERPOLATION_EPS) / (2.0f * INTERPOLATION_EPS);

          float t2 = t * t;
          
          // Above Horizon
          mu  = muHorizon - INTERPOLATION_EPS;
          //r0  = sqrt(r * r + t * t + 2.0f * r * t * mu);
          // From cosine law where t = distance between x and x0
          // r0^2 = r^2 + t^2 - 2 * r * t * cos(PI-theta)
          r0  = sqrt(r2 + t2 + 2.0f * r * t * mu);
          // From the dot product: cos(theta0) = (x0 dot v)/(||ro||*||v||)
          // mu0 = ((x + t) dot v) / r0
          // mu0 = (x dot v + t dot v) / r0
          // mu0 = (r*mu + t) / r0
          mu0 = (r * mu + t) / r0;
          vec4 inScatterAboveX  = texture4D(inscatterTexture, r, mu, muSun, nu);
          vec4 inScatterAboveXs = texture4D(inscatterTexture, r0, mu0, muSun0, nu);
          // Attention for the attenuation.r value applied to the S_Mie
          vec4 inScatterAbove = max(inScatterAboveX - attenuation.rgbr * inScatterAboveXs, 0.0f);

          // Below Horizon
          mu  = muHorizon + INTERPOLATION_EPS;
          r0  = sqrt(r2 + t2 + 2.0f * r * t * mu);
          mu0 = (r * mu + t) / r0;
          vec4 inScatterBelowX  = texture4D(inscatterTexture, r, mu, muSun, nu);
          vec4 inScatterBelowXs = texture4D(inscatterTexture, r0, mu0, muSun0, nu);
          // Attention for the attenuation.r value applied to the S_Mie
          vec4 inScatterBelow = max(inScatterBelowX - attenuation.rgbr * inScatterBelowXs, 0.0);

          // Interpolate between above and below inScattering radiance
          inscatterRadiance = mix(inScatterAbove, inScatterBelow, interpolationValue);
        }
      }
    }
      
    // The w component of inscatterRadiance has stored the Cm,r value (Cm = Sm[L0])
    // So, we must reintroduce the Mie inscatter by the proximity rule as described in the
    // paper by Bruneton and Neyret in "Angular precision" paragraph:
    
    // Hermite interpolation between two values
    // This step is done because imprecision problems happen when the Sun is slightly below
    // the horizon. When this happen, we avoid the Mie scattering contribution.
    inscatterRadiance.w *= smoothstep(0.0f, 0.02f, muSun);
    vec3 inscatterMie    = inscatterRadiance.rgb * inscatterRadiance.a / max(inscatterRadiance.r, 1e-4) *
      (betaRayleigh.r / betaRayleigh);
    
    radiance = max(inscatterRadiance.rgb * rayleighPhase + inscatterMie * miePhase, 0.0f);    
    
  } else {
    // No intersection with atmosphere
    // The ray is traveling on space
    radiance = vec3(0.0f);
  }
  
  
  // Finally we add the Lsun (all calculations are done with no Lsun so
  // we can change it on the fly with no precomputations)
  return radiance * sunRadiance;
  
}

/* 
 * Calculates the light reflected in the view direction comming from other 
 * light rays integrated over the hemispehre plus the direct light (L0) from Sun.
 * Following the paper: R[L]= R[L0]+R[L*] 
 * The the ray is x + tv, v the view direction, s is the sun direction,
 * r and mu the position and zenith cosine angle as in the paper.
 * As for all calculations in the atmosphere, the center of the coordinate system
 * is the planet's center of coordiante system, i.e., the planet's position is (0,0,0).
 * Arguments:
 * x := camera position
 * t := ray displacement variable. Here, differently from the inScatter light calculation,
 * the position of the camera is already offset (on top of atmosphere) or inside 
 * the atmosphere.
 * v := view direction (ray's direction) (normalized)
 * s := Sun direction (normalized)
 * r := ||x|| inside atmosphere (or top of atmosphere). r <= Rt here.
 * mu := cosine of the zenith view angle
 * attenuationXtoX0 := transmittance T(x,x0)
 */
vec3 groundColor(const vec3 x, const float t, const vec3 v, const vec3 s, const float r,
                 const float mu, const vec3 attenuationXtoX0)
{
  vec3 reflectedRadiance = vec3(0.0f);

  float d = length(x + t*v);
  float x_0 = sqrt(r*r + d*d - 2*r*d*mu);
  
  // Ray hits planet's surface
  //if (t > 0.0f) {    
  if (x_0 >= Rg) {    
    // First we obtain the ray's end point on the surface
    vec3  x0 = x + t * v;
    float r0 = length(x0);
    // Normal of intersection point.
    // TODO: Change it to globebrowser
    vec3  n  = x0 / r0;
    //vec3  n  = -x0 / r0;

    // Old deferred:    
    vec2 coords = vec2(atan(n.y, n.x), acos(n.z)) * vec2(0.5, 1.0) / M_PI + vec2(0.5, 0.0);
    //vec2 coords = vec2(0.5 + (atan(n.z, n.x))/(2*M_PI), 0.5 - asin(n.y)/(M_PI));
    vec4 reflectance = texture2D(reflectanceTexture, coords) * vec4(0.2, 0.2, 0.2, 1.0);
    
    // Initial ground radiance (the surface color)
    //vec4 reflectance = texture(reflectanceTexture, vs_st) * vec4(0.2, 0.2, 0.2, 1.0);
    
    // The following code is generating surface acne in ground. 
    // It is only necessary inside atmosphere rendering. JCC
    // If r0 > Rg + EPS (we are not intersecting the ground),
    // we get a constant initial ground radiance
    //if (r0 > Rg + 0.01) {
    //    reflectance = vec4(0.4, 0.4, 0.4, 0.0);
    //}
    
    // L0 is not included in the irradiance texture.
    // We first calculate the light attenuation from the top of the atmosphere
    // to x0. 
    float muSun           = dot(n, s);
    // Is direct Sun light arriving at x0? If not, there is no direct light from Sun (shadowed)
    vec3  transmittanceL0 = muSun < -sqrt(1.0f - ((Rg * Rg) / (r0 * r0))) ? vec3(0.0f) : transmittanceLUT(r0, muSun);

    // E[L*] at x0
    vec3 irradianceReflected = irradiance(irradianceTexture, r0, muSun);
    
    // Adding clouds texture
    //vec4 clouds = vec4(0.85)*texture(cloudsTexture, vs_st);

    // R[L0] + R[L*]
    //vec3 groundRadiance = (reflectance.rgb + clouds.rgb) * 
    //  (max(muSun, 0.0) * transmittanceL0 + irradianceReflected) * sunRadiance / M_PI;

    vec3 groundRadiance = reflectance.rgb * 
      (max(muSun, 0.0) * transmittanceL0 + irradianceReflected) * sunRadiance / M_PI;
    
    // Yellowish specular reflection from sun on oceans and rivers
    if (reflectance.w > 0.0) {
      vec3  h         = normalize(s - v);
      // Fresnell Schlick's approximation
      float fresnel   = 0.02f + 0.98f * pow(1.0f - dot(-v, h), 5.0f);
      // Walter BRDF approximation
      float waterBrdf = fresnel * pow(max(dot(h, n), 0.0f), 150.0f);
      // Adding Fresnell and Water BRDFs approximation to the final surface color
      // (After adding the sunRadiance and the attenuation of the Sun through atmosphere)
      groundRadiance += reflectance.w * max(waterBrdf, 0.0) * transmittanceL0 * sunRadiance;
    }

    // Finally, we attenuate the surface Radiance from the the point x0 to the camera location.
    reflectedRadiance = attenuationXtoX0 * groundRadiance;
  } else { // ray looking at the sky
        reflectedRadiance = vec3(0.0f);
  }

  // Returns reflectedRadiance = 0.0 if the ray doesn't hit the ground.
  return reflectedRadiance;
}

/* 
 * Calculates the Sun color.
 * The the ray is x + tv, v the view direction, s is the sun direction,
 * r and mu the position and zenith cosine angle as in the paper.
 * As for all calculations in the atmosphere, the center of the coordinate system
 * is the planet's center of coordiante system, i.e., the planet's position is (0,0,0).
 * Arguments:
 * x := camera position
 * t := ray displacement variable. Here, differently from the inScatter light calculation,
 * the position of the camera is already offset (on top of atmosphere) or inside 
 * the atmosphere.
 * v := view direction (ray's direction) (normalized)
 * s := Sun direction (normalized)
 * r := ||x|| inside atmosphere (or top of atmosphere). r <= Rt here.
 * mu := cosine of the zenith view angle
 * attenuation := transmittance T(x,x0)
 */
vec3 sunColor(const vec3 x, const float t, const vec3 v, const vec3 s, const float r, const float mu) {
  if (t > 0.0f) {
    return vec3(0.0f);
  } else {
    vec3 transmittance = (r <= Rt) ? 
          (mu < -sqrt(1.0f - (Rg/r)/(Rg/r)) ? vec3(0.0f) : transmittanceLUT(r, mu)) :
          vec3(1.0f);
    float sunColor = step(cos(M_PI / 180.0), dot(v, s)) * sunRadiance; 

    return transmittance * sunColor;
    }
}

/*
 * Calculates Intersection Ray by walking through
 * all the graphic pipile transformations in the 
 * opposite direction.
 * Instead of passing through all the pipeline,
 * it starts at NDC from the interpolated
 * positions from the screen quad.
 * This method avoids matrices multiplications
 * wherever is possible.
 */
void dCalculateRay2(out dRay ray, out dvec4 planetPositionObjectCoords) {
  // ======================================
  // ======= Avoiding Some Matrices =======

  // NDC to clip coordinates (gl_FragCoord.w = 1.0/w_clip)
  // Using the interpolated coords:
  // Assuming Red Book is right: z_ndc e [0, 1] and not [-1, 1]
  dvec4 clipCoords = dvec4(interpolatedNDCPos, 1.0) / gl_FragCoord.w; 
  // This next line is needed because OS or SGCT is not inverting Y axis from 
  // window space. 
  clipCoords.y = (-interpolatedNDCPos.y) / gl_FragCoord.w;
 
  // Clip to SGCT Eye
  dvec4 sgctEyeCoords = dInverseSgctProjectionMatrix * clipCoords;
  //sgctEyeCoords /= sgctEyeCoords.w;
  sgctEyeCoords.w = 1.0;
  
  // SGCT Eye to OS Eye (This is SGCT eye to OS eye)
  dvec4 osEyeCoords = dSgctEyeToOSEyeTranform * sgctEyeCoords;
    
  // OS Eye to World coords
  // Now we execute the transformations with no matrices:
  dvec4 ttmp         = dInverseScaleTransformMatrix * osEyeCoords;
  dvec3 ttmp2        = dmat3(dInverseCamRotTransform) * dvec3(ttmp);
  dvec4 worldCoords  = dvec4(dCampos + ttmp2, 1.0);
    
  // World to Object
  dvec4 objectCoords = dInverseTransformMatrix * dvec4(-dObjpos.xyz + worldCoords.xyz, 1.0);

  // Planet Position in Object Space
  planetPositionObjectCoords = dInverseTransformMatrix * dvec4(-dObjpos.xyz + dObjpos.xyz, 1.0);

  // Camera Position in Object Space
  dvec4 cameraPositionInObject = dInverseTransformMatrix * dvec4(-dObjpos.xyz + dCampos, 1.0);
    
  // ============================
  // ====== Building Ray ========
  // Ray in object space (in KM)
  ray.origin    = cameraPositionInObject / dvec4(1000.0, 1000.0, 1000.0, 1.0);
  ray.direction = dvec4(normalize(objectCoords.xyz - cameraPositionInObject.xyz), 0.0);
}

// Double Version
void main() {
  double depth = 0.0;  
  if (_performShading) {
    
    // Ray in object space
    dRay ray;
    dvec4 planetPositionObjectCoords = dvec4(0.0);
    dCalculateRay2(ray, planetPositionObjectCoords);
    //dCalculateInterpolatedRay(ray, planetPositionObjectCoords);
    
    bool  insideATM    = false;
    double offset      = 0.0f;
    double maxLength   = 0.0f;     
    bool  intersectATM = dAtmosphereIntersection(planetPositionObjectCoords.xyz, ray,  Rt,
                                                insideATM, offset, maxLength );
    if ( intersectATM ) {
      //renderTarget = vec4(1.0, 0.0, 0.0, 1.0);      
      //renderTarget = vec4(offset/maxLength, offset/maxLength, offset/maxLength, 1.0);
      //return;
      
      // Following paper nomenclature      
      double t = 0.0;            
      if ( offset != -1.0 ) {
        // Camera is inside Atmosphere
        t = offset;
      }
      // Moving camera to top of Atmosphere if needed
      vec3  x  = vec3(ray.origin.xyz);
      float r  = length(x);
      vec3  v  = vec3(ray.direction.xyz);
      float mu = dot(x, v) / r;
      vec3  s  = vec3(sunDirectionObj);
      
      float tF = float(maxLength);
      vec3 attenuation;

      //renderTarget =  vec4(analyticTransmittance(r, mu, tF).xyz, 1.0);      
      //renderTarget = vec4(s, 1.0);
      //renderTarget = HDR(vec4(abs(mu*mu), abs(mu*mu), abs(mu*mu), 1.0));
      //renderTarget = HDR(vec4(abs(Rt*Rt), abs(Rt*Rt), abs(Rt*Rt), 1.0));
      //renderTarget = HDR(vec4(abs(Rg*Rg), abs(Rg*Rg), abs(Rg*Rg), 1.0));
      //renderTarget = HDR(vec4(normalize(vec3(abs(r), abs(r), abs(r))), 1.0));
      //renderTarget = HDR(vec4(normalize(ray.origin.xyz + t * ray.direction.xyz), 1.0));
      //renderTarget = HDR(vec4(vec3(length(ray.origin.xyz + t * ray.direction.xyz)), 1.0));
      //float nu                = dot(v, s);//float(dot(vec3(ray.direction.xyz), s));
      //float muSun             = dot(x, s) / r;
      //renderTarget = vec4(nu, nu, nu, 1.0);
      //renderTarget = HDR(vec4(muSun, muSun, muSun, 1.0));
      //renderTarget = HDR(vec4(abs(nu), abs(nu), abs(nu), 1.0));
      //renderTarget = vec4(abs(muSun), abs(muSun), abs(muSun), 1.0);
      //renderTarget = vec4(vec3(max(texture4D(inscatterTexture, r, mu, muSun, nu), 0.0)), 1.0);

      vec3 inscatterColor = inscatterRadiance(x, tF, v, s, r, mu, attenuation); 
      vec3 groundColor = groundColor(x, tF, v, s, r, mu, attenuation);
      vec3 sunColor = sunColor(x, tF, v, s, r, mu); 
      
      //renderTarget = vec4(HDR(inscatterColor), 1.0); 
      //renderTarget = vec4(HDR(groundColor), 1.0); 
      //renderTarget = vec4(groundColor, 1.0); 
      //renderTarget = vec4(HDR(sunColor), 1.0); 
      //renderTarget = vec4(HDR(sunColor), 1.0); 
      renderTarget = vec4(HDR(inscatterColor + groundColor + inscatterColor), 1.0);       
      
    } else {
      renderTarget = vec4(0.0, 0.0, 0.0, 1.0);
    }
    
  } else {
    renderTarget = vec4(0.5, 0.5, 0.5, 1.0);
  }
}

