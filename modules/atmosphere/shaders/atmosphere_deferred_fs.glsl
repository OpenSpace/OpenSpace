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

// Sun Irradiance
const float ISun = 40.0;

uniform mat4 sgctProjectionMatrix;
uniform mat4 inverseTransformMatrix;
uniform mat4 scaleTransformMatrix;
uniform mat4 objToWorldTransform;
uniform mat4 worldToObjectTransform;
uniform mat4 worldToEyeTransform;
uniform mat4 eyeToWorldTransform;
uniform mat4 eyeToViewTranform;
uniform mat4 viewToEyeTranform;
uniform mat4 inverseSgctProjectionMatrix;

uniform mat4 completeVertexTransform;
uniform mat4 inverseCompleteVertexTransform;

uniform vec4 cameraPositionObjectCoords;

//uniform vec4 campos;
uniform vec4 objpos;
uniform vec3 campos;
uniform mat3 camrot;
//uniform vec3 sun_pos;

uniform bool _performShading = true;
uniform float transparency;
uniform int shadows;

uniform float screenX;
uniform float screenY;
uniform float screenWIDTH;
uniform float screenHEIGHT;

uniform vec2 depthrange;

uniform float time;

uniform sampler2D reflectanceTexture;
//uniform sampler2D transmittanceTexture;
uniform sampler2D irradianceTexture;
uniform sampler3D inscatterTexture;
 
#include "hdr.glsl"
//#include "PowerScaling/powerScaling_fs.hglsl"
//#include "fragment.glsl"
#include "atmosphere_common.glsl"


layout(location = 0) out vec4 renderTarget;

in vec3 interpolatedNDCPos;
in vec4 vertexPosObjVS;
in vec3 interpolatedRayDirection;

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

bool algebraicIntersecSphere(const Ray ray, const float SphereRadius, const vec4 SphereCenter, 
                             out float offset, out float maxLength) 
{
  vec3 L = ray.origin.xyz - SphereCenter.xyz;
  float B = 2 * dot(ray.direction.xyz, L);
  float C = dot(L, L) - (SphereRadius*SphereRadius);
  float delta = B*B - 4*C;

  if ( delta < 0.0 ) { // no intersection
    return false;
  }
  else if ( delta == 0.0 ) { // one intersection;
    offset = maxLength = -B/2.0;
  } else {
    float tmpB = -B * 0.5;
    float root = sqrt(delta) * 0.5;
    float t0 = tmpB - root;
    float t1 = tmpB + root;

    if ( t0 < t1 ) {
      offset = t0;
      maxLength = t1;
    } else {
      offset = t1;
      maxLength = t0;
    }
  } 
  return true;
}

bool geometricIntersecSphere(const Ray ray, const float SphereRadius, const dvec4 SphereCenter, 
                             out double offset, out double maxLength) {
  // Ray's direction must be normalized.
  dvec4 OC   = SphereCenter - ray.origin;
  double L2  = dot(OC.xyz, OC.xyz);
  double Sr2 = SphereRadius * SphereRadius;

  if ( L2 < Sr2 ) // Ray origin inside sphere.
    return false; // TODO: Bust be handled later

  double t_ca = dot(OC.xyz, ray.direction.xyz);

  if ( t_ca < 0.0 ) // Sphere's center lies behind the rays origin.
    return false; // TODO: Handle inside sphere.

  double t_2hc = Sr2 - L2 + (t_ca * t_ca);

  if ( t_2hc < 0.0 ) // Ray misses the sphere
    return false;

  double t_hc = sqrt(t_2hc);

  offset = t_ca - t_hc;
  maxLength = t_ca + t_hc;
    
  return true;
}

bool intersectEllipsoid(const Ray ray, const Ellipsoid ellipsoid, out double offset, out double maxLength) {
  dvec4 O_C = ray.origin-ellipsoid.center;
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
  
  double d = ((b*b)-(4.f*a*c));
  if ( d<0.f || a==0.f || b==0.f || c==0.f )
    return false;
  
  d = sqrt(d);
  
  double t1 = (-b+d)/(2.f*a);
  double t2 = (-b-d)/(2.f*a);
  
  if( t1<=EPSILON && t2<=EPSILON )
    return false; // both intersections are behind the ray origin

  bool back = (t1<=EPSILON || t2<=EPSILON); // If only one intersection (t>0) then we are inside the ellipsoid and the intersection is at the back of the ellipsoid
  double t=0.f;
  if( t1<=EPSILON )
    t = t2;
  else
    if( t2<=EPSILON )
      t = t1;
    else
      t=(t1<t2) ? t1 : t2;
  
  if( t<EPSILON ) return false; // Too close to intersection

  dvec4 intersection = ray.origin + t*dir;
  dvec4 normal = intersection-ellipsoid.center;
  normal.x = 2.f*normal.x/(ellipsoid.size.x*ellipsoid.size.x);
  normal.y = 2.f*normal.y/(ellipsoid.size.y*ellipsoid.size.y);
  normal.z = 2.f*normal.z/(ellipsoid.size.z*ellipsoid.size.z);
  
  normal.w = 0.f;
  normal *= (back) ? -1.f : 1.f;
  normal = normalize(normal);

  
  return true;
}

bool intersectAtmosphere(const dvec4 planetPos, const dvec3 rayDirection, const double sphereRadius, 
                         out double offset, out double maxLength) {
  offset = 0.0f;
  maxLength = 0.0f;

  // REVIEW
  //dvec3 l   =  planetPos.xyz - cameraPositionObject.xyz;
  dvec3 l   =  planetPos.xyz;
  double s  = dot(l, rayDirection);
  double l2 = dot(l, l);
    
  // sphereRadius in Km
  double r  = sphereRadius - EPSILON; // EPSILON to avoid surface acne
  double r2 = r * r;
        
  if (l2 <= r2) {
    // ray origin inside sphere
    double m2 = l2 - (s*s);
    double q = sqrt(r2 - m2);
    maxLength = s + q;
        
    return true;
  }
  else if (s >= 0.0) {
    // ray outside sphere
    double m2 = l2 - (s*s);
    if (m2 <= r2) {
      // ray hits atmosphere
      double q = sqrt(r2 - m2);
      offset = s-q;
      maxLength = (s+q)-offset;
            
      return true;
    }
  }
    
  return false;
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
 *               eye is inside the atmosphere or the initial (and only) 
 *               intersection of the ray with atmosphere when the eye position
 *               is inside atmosphere.
 */
bool atmosphereIntersection(const vec3 planetPosition, const Ray ray, const float atmRadius,
                            out bool inside, out float offset, out float maxLength ) {
  vec3  l  = planetPosition - ray.origin.xyz;
  float s  = dot(l, ray.direction.xyz);
  float l2 = dot(l, l);
  float r2 = (atmRadius - EPSILON) *  (atmRadius - EPSILON); // avoiding surface acne

  // Ray origin (eye position) is behind sphere
  if ((s < 0.0f) && (l2 > r2)) {
    inside    = false;
    offset    = 0.0f;
    maxLength = 0.0f;
    return false;
  }

  float m2 = l2 - s*s;

  // Ray misses atmospere
  if (m2 > r2) {
    inside    = false;
    offset    = 0.0f;
    maxLength = 0.0f;
    return false;
  }

  // We already now the ray hits the atmosphere

  // If q = 0.0f, there is only one intersection
  float q = sqrt(r2 - m2);

  // If l2 < r2, the ray origin is inside the sphere
  if (l2 > r2) {
    inside    = false;
    offset    = s - q;
    maxLength = s + q;
  } else {
    inside    = true;
    offset    = 0.0f;
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

vec2 getIrradianceUV(float r, float muSun) {
  float uR = (r - Rg) / (Rt - Rg);
  float uMuS = (muSun + 0.2) / (1.0 + 0.2);
  return vec2(uMuS, uR);
}

vec3 irradiance(sampler2D sampler, float r, float muSun) {
  vec2 uv = getIrradianceUV(r, muSun);
  return texture(sampler, uv).rgb;
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

  // From the cosine law for x0 at top of atmosphere:
  // Rt^2 = r^2 + dist^2 - 2*r*dist*cos(PI - theta)
  float dist = -r * mu - sqrt(r2 * (mu2 - 1.0f) + Rt2);

  // Dist stores the distance from the camera position
  // to the first (the only in some cases) intersection of the
  // light ray and the top of atmosphere.
  
  // Are we at space?
  if (dist > 0.0f) {
    // Because we are at space, we must obtain the vector x,
    // the correct cosine of between x and v and the right height r,
    // with the x in top of atmosphere.
    // What we do is to move from the starting point x (camera position)
    // to the point on the atmosphere. So, because we have a new x,
    // we must also calculate the new cosine between x and v. s is the
    // same because we consider the Sun as a parallel ray light source.
    x += dist * v;
    t -= dist;
    // mu(x0 and v)
    // cos(theta') = (x0 dot v)/(||x0||*||v||) = ((x + dist*v) dot v)/(Rt * 1)
    // cos(theta') = mu' = (r*mu + dist)/Rt
    mu = (r * mu + dist) / Rt;
    mu2 = mu * mu;
    r  = Rt;
    r2 = r * r;    
  }
  
  // Intersects atmosphere?
  if (r <= Rt) { 
    float nu                = dot(v, s);
    float muSun             = dot(x, s) / r;
    float rayleighPhase     = rayleighPhaseFunction(nu);
    float miePhase          = miePhaseFunction(nu);

    // S[L](x,s,v)
    vec4  inscatterRadiance = max(texture4D(inscatterTexture, r, mu, muSun, nu), 0.0);
    
    // After removing the initial path from camera pos to top of atmosphere or the
    // current camera position if inside atmosphere, t > 0
    if (t > 0.0) {
      // Calculate the zenith angles for x0 and v, s:
      vec3  x0     = x + t * v;
      float r0     = length(x0);
      float mu0    = dot(x0, v) / r0;
      float muSun0 = dot(x0, s) / r0;

      // Transmittance from point r, direction mu, distance t
      attenuation = analyticTransmittance(r, mu, t);
      //attenuation = transmittance(r, mu, v, x+t*v);
      
      //The following Code is generating surface acne on atmosphere. JCC
      // We need a better acne avoidance constant (0.01). Done!! Adaptive from distance to x
      if (r0 > Rg + (0.1f * r)) {
      // It r0 > Rg it means the ray hits something inside the atmosphere. So we need to
      // remove the inScattering contribution from the main ray from the hitting point
      // to the end of the ray.
      //if (r0 > Rg + (0.01f)) {
        // Here we use the idea of S[L](a->b) = S[L](b->a), and get the S[L](x0, v, s)
        // Then we calculate S[L] = S[L]|x - T(x, x0)*S[L]|x0
        inscatterRadiance = max(inscatterRadiance - attenuation.rgbr * texture4D(inscatterTexture, r0, mu0, muSun0, nu), 0.0);

        // cos(PI-thetaH) = dist/r
        // cos(thetaH) = - dist/r
        // muHorizon = -sqrt(r^2-Rg^2)/r = -sqrt(1-(Rg/r)^2)
        float muHorizon = -sqrt(1.0f - (Rg2 / r2));

        // In order to avoid imprecision problems near horizon,
        // we interpolate between two points: above and below horizon
        const float INTERPOLATION_EPS = 0.004f; // precision const
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
          //r0  = sqrt(r * r + t * t + 2.0f * r * t * mu);
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
  
  // Ray hits planet's surface
  if (t > 0.0f) {
    // First we obtain the ray's end point on the surface
    vec3  x0 = x + t * v;
    float r0 = length(x0);
    // Normal of intersection point.
    // TODO: Change it to globebrowser
    vec3  n  = x0 / r0;

    // Old deferred:
    vec2 coords = vec2(atan(n.y, n.x), acos(n.z)) * vec2(0.5, 1.0) / M_PI + vec2(0.5, 0.0);
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
    vec3  transmittance = (r <= Rt) ? (mu < -sqrt(1.0f - (Rg*Rg)/(r*r)) ? vec3(0.0f) : transmittanceLUT(r, mu)) :
      vec3(1.0f);
    float sunRadiance   = step(cos(M_PI / 180.0), dot(v, s)) * sunRadiance; 

    return transmittance * sunRadiance;
  }
}

/*
 * Calculates Intersection Ray by walking through
 * all the graphic pipile transformations in the 
 * opposite direction.
 */
void calculateRay(out Ray ray, out vec4 planetPositionObjectCoords) {
  
  // Fragment to window coordinates
  vec4 windowCoords = vec4(0.0);
  
  windowCoords.x = gl_FragCoord.x + 0.5; // +0.5 because the fragment has non-integer coords by default
  windowCoords.y = screenHEIGHT - gl_FragCoord.y - 0.5; // +0.5 because the fragment has non-integer coords by default
  windowCoords.z = gl_FragCoord.z; // z can be 0.0 or 1.0. We chose 1.0 to avoid math problems.
  windowCoords.w = gl_FragCoord.w; // remember: gl_FragCoord.w = 1.0/w_clip
  
  // Window to NDC coordinates
  vec4 viewPort = vec4(screenX, screenY, screenWIDTH, screenHEIGHT);
  vec4 ndcCoords = vec4(0.0, 0.0, 0.0, 1.0);
  ndcCoords.xy = (2.0 * (windowCoords.xy - viewPort.xy) / viewPort.zw) - vec2(1.0);
  
  // The ndcCoords for z are only need if we want something inside the
  // view frustum. In this case we just want the position in the
  // near plane, that is z = -1.0
  float f_plus_n = gl_DepthRange.far + gl_DepthRange.near;
  float f_minus_n = gl_DepthRange.far - gl_DepthRange.near;
  ndcCoords.z = (2.0 * windowCoords.z - f_plus_n) / f_minus_n;
  
  // NDC to clip coordinates (gl_FragCoord.w = 1.0/w_clip)
  vec4 clipCoords = ndcCoords / gl_FragCoord.w;
  
  // Clip to SGCT Eye
  vec4 sgctEyeCoords = inverseSgctProjectionMatrix * clipCoords;
  
  // SGCT Eye to OS Eye (This is SGCT eye to OS eye)
  vec4 osEyeCoords = viewToEyeTranform * sgctEyeCoords;
  
  // OS Eye to World
  vec4 worldCoords = eyeToWorldTransform * osEyeCoords;   
  
  // World to Object
  vec4 objectCoords = worldToObjectTransform * worldCoords;
  
  // Planet Position in Object Space
  planetPositionObjectCoords = worldToObjectTransform * vec4(objpos.xyz, 1.0);
  
  // Camera Position in Object Space
  vec4 cameraPositionObject = worldToObjectTransform * vec4(campos.xyz, 1.0);
      
  // ============================
  // ====== Building Ray ========
  // Ray in object space
  ray.origin    = cameraPositionObject;
  ray.direction = vec4(normalize(objectCoords.xyz - cameraPositionObject.xyz), 0.0); 
}

/*
 * Calculates Intersection Ray by walking through
 * all the graphic pipile transformations in the 
 * opposite direction.
 * Instead of passing through all the pipeline,
 * it starts at NDC from the interpolated
 * positions from the screen quad.
 */
void calculateInterpolatedRay(out Ray ray, out vec4 planetPositionObjectCoords) {
  // NDC to Clip coords
  vec4 clipCoords = vec4(interpolatedNDCPos, 1.0) / gl_FragCoord.w;
    
  // Clip to SGCT Eye
  vec4 sgctEyeCoords = inverseSgctProjectionMatrix * clipCoords;

  // SGCT Eye to OS Eye (This is SGCT eye to OS eye)
  vec4 osEyeCoords = viewToEyeTranform * sgctEyeCoords;
    
  // OS Eye to World coords
  // Now we execute the transformations with no matrices:
  vec4 ttmp   = inverse(scaleTransformMatrix) * osEyeCoords;
  vec3 ttmp2  = inverse(camrot) * vec3(ttmp);
  vec4 ttmp3  = vec4(campos + ttmp2, 1.0);

  vec4 worldCoords = ttmp3;

  // World to Object coords
  vec4 objectCoords = inverseTransformMatrix * vec4(-objpos.xyz + worldCoords.xyz, 1.0);

  // Planet Position in Object Space
  planetPositionObjectCoords = inverseTransformMatrix * vec4(-objpos.xyz + objpos.xyz, 1.0);

  // Camera Position in Object Space
  vec4 cameraPositionInObject = inverseTransformMatrix * vec4(-objpos.xyz + campos.xyz, 1.0);    
    
  // ============================
  // ====== Building Ray ========
  // Ray in object space
  ray.origin    = cameraPositionInObject;
  ray.direction = vec4(normalize(objectCoords.xyz - cameraPositionInObject.xyz), 0.0);    
}


/*
 * Calculates Intersection Ray by walking through
 * all the graphic pipile transformations in the 
 * opposite direction.
 * This method avoids matrices multiplications
 * wherever is possible.
 */
void calculateRay2(out Ray ray, out vec4 planetPositionObjectCoords) {
  // ======================================
  // ======= Avoiding Some Matrices =======

  // NDC to clip coordinates (gl_FragCoord.w = 1.0/w_clip)
  // Using the interpolated coords:
  // Assuming Red Book is right: z_ndc e [0, 1] and not [-1, 1]
  vec4 clipCoords = vec4(interpolatedNDCPos, 1.0) / gl_FragCoord.w;
 
  // Clip to SGCT Eye
  vec4 sgctEyeCoords = inverseSgctProjectionMatrix * clipCoords;
  sgctEyeCoords.w = 1.0;
  
  // SGCT Eye to OS Eye (This is SGCT eye to OS eye)
  vec4 osEyeCoords = viewToEyeTranform * sgctEyeCoords;
    
  // OS Eye to World coords
  // Now we execute the transformations with no matrices:
  vec4 ttmp   = inverse(scaleTransformMatrix) * osEyeCoords;
  vec3 ttmp2  = inverse(camrot) * vec3(ttmp);
  vec4 ttmp3  = vec4(campos + ttmp2, 1.0);

  vec4 worldCoords = ttmp3;
    
  // World to Object
  vec4 objectCoords = inverseTransformMatrix * vec4(-objpos.xyz + worldCoords.xyz, 1.0);

  // Planet Position in Object Space
  planetPositionObjectCoords = inverseTransformMatrix * vec4(-objpos.xyz + objpos.xyz, 1.0);

  // Camera Position in Object Space
  vec4 cameraPositionInObject = inverseTransformMatrix * vec4(-objpos.xyz + campos, 1.0);
    
  // ============================
  // ====== Building Ray ========
  // Ray in object space
  ray.origin    = cameraPositionInObject;
  ray.direction = vec4(normalize(objectCoords.xyz - cameraPositionInObject.xyz), 0.0);

  renderTarget = vec4(0.5 * interpolatedNDCPos.xyz + vec3(0.5), 1.0);
}

/*
 * Calculates Intersection Ray by walking through
 * all the graphic pipile transformations in the 
 * opposite direction.
 * Khornos way.
 */
void calculateRay3(out Ray ray, out vec4 planetPositionObjectCoords) {

  vec4 viewPort = vec4(screenX, screenY, screenWIDTH, screenHEIGHT);
  vec4 ndcPos;
  ndcPos.xy = ((2.0 * gl_FragCoord.xy) - (2.0 * viewPort.xy)) / (viewPort.zw) - 1;
  ndcPos.z = (2.0 * gl_FragCoord.z - gl_DepthRange.near - gl_DepthRange.far) /
    (gl_DepthRange.far - gl_DepthRange.near);
  ndcPos.w = 1.0;
    
  vec4 clipPos = ndcPos / gl_FragCoord.w;
    
  // Clip to SGCT Eye
  vec4 sgctEyeCoords = inverseSgctProjectionMatrix * clipPos;
    
  // SGCT Eye to OS Eye (This is SGCT eye to OS eye)
  vec4 osEyeCoords = viewToEyeTranform * sgctEyeCoords;
    
  // OS Eye to World
  vec4 worldCoords = eyeToWorldTransform * osEyeCoords;   
    
  // World to Object
  vec4 objectCoords = worldToObjectTransform * worldCoords;

  // Planet Position in Object Space
  planetPositionObjectCoords = worldToObjectTransform * vec4(objpos.xyz, 1.0);

  // Camera Position in Object Space
  vec4 cameraOriginObjectCoords = worldToObjectTransform * vec4(campos.xyz, 1.0);

  // ============================
  // ====== Building Ray ========
  // Ray in object space
  ray.origin = cameraOriginObjectCoords;
  ray.direction = vec4(normalize(objectCoords.xyz - cameraOriginObjectCoords.xyz), 0.0);    
}

/*
 * Calculates Intersection Ray by walking through
 * all the graphic pipile transformations in the 
 * opposite direction.
 * Optimized Khronos way.
 */
void calculateRay4(out Ray ray, out vec4 planetPositionObjectCoords) {
  
  // ================================
  // ======== From Kronos ===========
  vec4 viewPort = vec4(screenX, screenY, screenWIDTH, screenHEIGHT);
  vec3 ndcPos;
  ndcPos.xy = ((2.0 * gl_FragCoord.xy) - (2.0 * viewPort.xy)) / (viewPort.zw) - 1;
  ndcPos.z = (2.0 * gl_FragCoord.z - depthrange.x - depthrange.y) /
  (depthrange.y - depthrange.x);

  vec4 clipPos;
  clipPos.w = sgctProjectionMatrix[3][2] / (ndcPos.z - (sgctProjectionMatrix[2][2] / sgctProjectionMatrix[2][3]));
  clipPos.xyz = ndcPos * clipPos.w;

  // Clip to SGCT Eye
  vec4 sgctEyeCoords = inverseSgctProjectionMatrix * clipPos;
  
  // SGCT Eye to OS Eye (This is SGCT eye to OS eye)
  vec4 osEyeCoords = viewToEyeTranform * sgctEyeCoords;
    
  // OS Eye to World coords
  // Now we execute the transformations with no matrices:
  vec4 ttmp   = inverse(scaleTransformMatrix) * osEyeCoords;
  vec3 ttmp2  = inverse(camrot) * vec3(ttmp);
  vec4 ttmp3  = vec4(campos + ttmp2, 1.0);

  vec4 worldCoords = ttmp3;

  // World to Object coords
  vec4 objectCoords = inverseTransformMatrix * vec4(-objpos.xyz + worldCoords.xyz, 1.0);

  // Planet Position in Object Space
  planetPositionObjectCoords = inverseTransformMatrix * vec4(-objpos.xyz + objpos.xyz, 1.0);

  // Camera Position in Object Space
  vec4 cameraOriginObjectCoords = inverseTransformMatrix * vec4(-objpos.xyz + campos.xyz, 1.0);    
    
  // ============================
  // ====== Building Ray ========
  // Ray in object space
  ray.origin    = cameraOriginObjectCoords;
  ray.direction = vec4(normalize(objectCoords.xyz - cameraOriginObjectCoords.xyz), 0.0);    
}



void main() {
  //vec4 position = vs_position;
  float depth = 0.0;
  // vec4 diffuse = texture(texture1, vs_st);
  // vec4 diffuse2 = texture(nightTex, vs_st);
  // vec4 clouds = texture(cloudsTexture, vs_st);
  
  if (_performShading) {
    
    // Ray in object space
    Ray ray;
    vec4 planetPositionObjectCoords = vec4(0.0);
    //calculateRay(ray, planetPositionObjectCoords);
    calculateRay2(ray, planetPositionObjectCoords);
    //calculateInterpolatedRay(ray, planetPositionObjectCoords);
    //calculateRay3(ray, planetPositionObjectCoords);
    //calculateRay4(ray, planetPositionObjectCoords);
    
    bool  insideATM    = false;
    float offset       = 0.0f;
    float maxLength    = 0.0f;     
    bool  intersectATM = atmosphereIntersection(planetPositionObjectCoords.xyz, ray,  Rt*1000.0,
                                                insideATM, offset, maxLength );
    //bool intersectATM = algebraicIntersecSphere(ray, Rt*1000.0, planetPositionObjectCoords, offset, maxLength);

    // if ( intersectATM ) {
    //   renderTarget = vec4(1.0, 0.0, 0.0, 1.0);
    // } else {
    //   renderTarget = vec4(0.0, 0.0, 0.0, 1.0);
    // }

    // Debugging:
    //renderTarget = vec4(interpolatedNDCPos.xy*0.5 + vec2(0.5), 0.0, 1.0);
    //renderTarget = vec4(ndcCoords.xy*0.5 + vec2(0.5), 0.0, 1.0);
    //renderTarget = vec4(normalize(sgctEyeCoords.xyz) * 0.5 + vec3(0.5), 1.0);
    //renderTarget = vec4(osEyeCoords.xyz * 0.5 + 0.5, 1.0);
    //vec2 temp = farPlaneObjectPos.xy;
    //vec2 temp = sgctEyeCoords.xy;
    //vec2 temp = osEyeCoords.xy;
    //vec2 temp = worldCoords.xy;
    // if (temp.x > temp.y)
    //   temp /= temp.x;
    // else
    //   temp /= temp.y;
    //renderTarget = vec4(temp, 0.0, 1.0);

    //renderTarget = vec4(ray.direction.xyz, 1.0);
    //  renderTarget = vec4(normalize(sgctEyeCoords).xyz, 1.0);
    //renderTarget = vec4(inverseSgctProjectionMatrix[2][1], 0.0, 0.0, 1.0);

    //renderTarget = vec4(0.5*normalize(worldCoords.xyz) + vec3(0.5), 1.0);
  } else {
    renderTarget = vec4(0.5, 0.5, 0.5, 1.0);
  }

  // Testing Uniforms:
  //renderTarget.xyz = vec3(1.0f);
  //renderTarget.xyz = vec3(Rg/6378.1366);
  //0renderTarget.xyz = vec3(Rt/6420.0);
  //renderTarget.xyz = vec3(AverageGroundReflectance/0.1f);
  //renderTarget.xyz = vec3(HR/8.0f);
  //renderTarget.xyz = vec3(HM/1.2f);
  //renderTarget.xyz = vec3(mieG/1.0f);
  //renderTarget.xyz = vec3(sunRadiance/50.0f);
  //renderTarget.xyz = vec3(betaRayleigh.x/5.8e-3, betaRayleigh.y/1.35e-2, betaRayleigh.z/3.31e-2);
  //renderTarget.xyz = vec3(betaMieScattering.x/4e-3, betaMieScattering.y/4e-3, betaMieScattering.z/4e-3);
  //renderTarget.xyz = vec3(betaMieExtinction.x/(betaMieScattering.x/0.9), betaMieExtinction.y/(betaMieScattering.y/0.9),
  //                   betaMieExtinction.z/(betaMieScattering.z/0.9));
  //renderTarget.xyz = vec3(mieG);

  //renderTarget = vec4(interpolatedRayDirection * 0.5 + 0.5, 1.0);
}
