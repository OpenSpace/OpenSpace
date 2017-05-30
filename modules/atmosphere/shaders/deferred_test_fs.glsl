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
#version __CONTEXT__
//#version 400

const int RenderablePlanet = 1;
const int RenderableGlobe  = 2;

#define EPSILON 0.0001f
#include "floatoperations.glsl"

#include "hdr.glsl"
#include "atmosphere_common.glsl"

// Atmosphere applied over a RenderablePlanet or
// a RenderableGlobe
uniform int RenderableClass;

uniform sampler2D irradianceTexture;
uniform sampler3D inscatterTexture;
uniform sampler2DMS mainPositionTexture;
uniform sampler2DMS mainNormalReflectanceTexture;
uniform sampler2DMS mainColorTexture;

uniform int nAaSamples;

//layout(location = 0) out vec4 renderTarget;
out vec4 renderTarget;

in vec3 interpolatedNDCPos;

// Model Transform Matrix Used for Globe Rendering
uniform dmat4 ModelTransformMatrix;
uniform dmat4 dInverseTransformMatrix;
uniform dmat4 dInverseScaleTransformMatrix;
uniform dmat4 dInverseSgctEyeToWorldTranform; // SGCT Eye to OS World
uniform dmat4 dSgctEyeToOSEyeTranform; // SGCT Eye to OS Eye
uniform dmat4 dInverseSgctProjectionMatrix; // Clip to SGCT Eye
uniform dmat4 dInverseCamRotTransform;

// Double Precision Versions:
uniform dvec4 dObjpos;
uniform dvec3 dCampos;

uniform dvec3 sunDirectionObj;

uniform dvec3 ellipsoidRadii;

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
  dvec3 size;
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

  offset = t;
  // dvec4 intersection = ray.origin + t * dir;
  // dvec4 normal       = intersection - ellipsoid.center;
  // normal.x = 2.0 * normal.x / (ellipsoid.size.x * ellipsoid.size.x);
  // normal.y = 2.0 * normal.y / (ellipsoid.size.y * ellipsoid.size.y);
  // normal.z = 2.0 * normal.z / (ellipsoid.size.z * ellipsoid.size.z);
  
  // normal.w = 0.0;
  // normal  *= (back) ? -1.0 : 1.0;
  // normal  = normalize(normal);
  
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
    offset    = 0.0;
    maxLength = s + q;
  }
  
  return true;
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
void dCalculateRayRenderableGlobe(out dRay ray, out dvec4 planetPositionObjectCoords, 
                                  out dvec4 cameraPositionInObject) {
  // ======================================
  // ======= Avoiding Some Matrices =======

  // NDC to clip coordinates (gl_FragCoord.w = 1.0/w_clip)
  // Using the interpolated coords:
  // Assuming Red Book is right: z_ndc e [0, 1] and not [-1, 1]
  dvec4 clipCoords = dvec4(interpolatedNDCPos, 1.0) / gl_FragCoord.w; 
  // This next line is needed because OS or SGCT is not inverting Y axis from 
  // window space. 
  //clipCoords.y = (-interpolatedNDCPos.y) / gl_FragCoord.w;
 
  // Clip to SGCT Eye
  dvec4 sgctEyeCoords = dInverseSgctProjectionMatrix * clipCoords;
  //sgctEyeCoords /= sgctEyeCoords.w;
  sgctEyeCoords.w = 1.0;
  
  // SGCT Eye to OS Eye
  dvec4 tOSEyeCoordsInv = dSgctEyeToOSEyeTranform * sgctEyeCoords;
  
  // OS Eye to World coords
  dvec4 tmpRInv = dInverseCamRotTransform * tOSEyeCoordsInv;
  dvec4 worldCoords= dvec4(dvec3(tmpRInv) + dCampos, 1.0);
  
  // World to Object
  dvec4 objectCoords = dInverseTransformMatrix * worldCoords;

  // Planet Position in Object Space
  // JCC: Applying the inverse of the model transformation on the object postion in World 
  // space results in imprecision. 
  planetPositionObjectCoords = dvec4(0.0,0.0,0.0,1.0);//dInverseTransformMatrix * dvec4(dObjpos.xyz, 1.0);

  // Camera Position in Object Space
  cameraPositionInObject = dInverseTransformMatrix * dvec4(dCampos, 1.0);
    
  // ============================
  // ====== Building Ray ========
  // Ray in object space (in KM)
  ray.origin    = cameraPositionInObject / dvec4(1000.0, 1000.0, 1000.0, 1.0);
  ray.direction = dvec4(normalize(objectCoords.xyz - cameraPositionInObject.xyz), 0.0);
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
void dCalculateRayRenderablePlanet(out dRay ray, out dvec4 planetPositionObjectCoords, 
                                   out dvec4 cameraPositionInObject) {
  // ======================================
  // ======= Avoiding Some Matrices =======

  // NDC to clip coordinates (gl_FragCoord.w = 1.0/w_clip)
  // Using the interpolated coords:
  // Assuming Red Book is right: z_ndc e [0, 1] and not [-1, 1]
  dvec4 clipCoords = dvec4(interpolatedNDCPos, 1.0) / gl_FragCoord.w; 
  // This next line is needed because OS or SGCT is not inverting Y axis from 
  // window space. 
  //clipCoords.y = (-interpolatedNDCPos.y) / gl_FragCoord.w;
 
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
  planetPositionObjectCoords =  dvec4(0.0,0.0,0.0,1.0);//dInverseTransformMatrix * dvec4(-dObjpos.xyz + dObjpos.xyz, 1.0);

  // Camera Position in Object Space
  cameraPositionInObject = dInverseTransformMatrix * dvec4(-dObjpos.xyz + dCampos, 1.0);
    
  // ============================
  // ====== Building Ray ========
  // Ray in object space (in KM)
  ray.origin    = cameraPositionInObject / dvec4(1000.0, 1000.0, 1000.0, 1.0);
  ray.direction = dvec4(normalize(objectCoords.xyz - cameraPositionInObject.xyz), 0.0);
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
 * the atmosphere. If the ray hits the ground, t is updated to the correct value
 * v := view direction (ray's direction) (normalized)
 * s := Sun direction (normalized)
 * r := out of ||x|| inside atmosphere (or top of atmosphere)
 * mu := out of cosine of the zenith view angle
 * attenuation := out of transmittance T(x,x0). This will be used later when
 * calculating the reflectance R[L].
 */
vec3 inscatterRadiance(inout vec3 x, inout float t, inout float irradianceFactor,
                       const vec3 v, const vec3 s, out float r, out float mu,
                       out vec3 attenuation, const vec3 fragPosObj,
                       const double maxLength, const double pixelDepth,
                       const vec4 spaceColor) {
  vec3 radiance;
  
  r  = length(x);
  mu = dot(x, v) / r;

  float mu2           = mu * mu;
  float r2            = r * r;
  float Rt2           = Rt * Rt;
  float Rg2           = Rg * Rg;
  float nu            = dot(v, s);
  float muSun         = dot(x, s) / r;
  float rayleighPhase = rayleighPhaseFunction(nu);
  float miePhase      = miePhaseFunction(nu);
    
  // S[L](x,s,v)
  // I.e. the next line has the scattering light for the "infinite" ray passing 
  // through the atmosphere. If this ray hits something inside the atmosphere,
  // we will subtract the attenuated scattering light from that path in the
  // current path.
  vec4 inscatterRadiance = max(texture4D(inscatterTexture, r, mu, muSun, nu), 0.0);    
    
  // After removing the initial path from camera pos to top of atmosphere (for an
  // observer in the space) we test if the light ray is hitting the atmosphere
  vec3  x0     = fragPosObj;
  float r0     = length(fragPosObj);
  float muSun0 = dot(fragPosObj, s) / r0;
  //vec3  x0     = x + float(pixelDepth) * v;      
  float mu0    = dot(x0, v) / r0;
  
  if ((pixelDepth > 0.0) && pixelDepth < maxLength) {    
    t = float(pixelDepth);  

    // Transmittance from point r, direction mu, distance t
    // By Analytical calculation
    attenuation = analyticTransmittance(r, mu, t);

    // Here we use the idea of S[L](a->b) = S[L](b->a), and get the S[L](x0, v, s)
    // Then we calculate S[L] = S[L]|x - T(x, x0)*S[L]|x0        
    // The "infinite" ray hist something inside the atmosphere, so we need to remove
    // the unsused contribution to the final radiance.
    vec4 inscatterFromSurface = texture4D(inscatterTexture, r0, mu0, muSun0, nu);
    inscatterRadiance = max(inscatterRadiance - attenuation.rgbr * inscatterFromSurface, 0.0);

    // We set the irradianceFactor to 1.0 so the reflected irradiance will be considered
    // when calculating the reflected light on the ground.
    irradianceFactor = 1.0;
  } else {
    attenuation = analyticTransmittance(r, mu, t);
  }
      
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
  
  // Finally we add the Lsun (all calculations are done with no Lsun so
  // we can change it on the fly with no precomputations)
  // return radiance * sunRadiance;

  if (pixelDepth < maxLength) {
    //return spaceColor.rgb + (1.0 - spaceColor.a)*(radiance * sunRadiance);
    return radiance * sunRadiance;
  } else {
    //return radiance * sunRadiance;
    //return spaceColor.rgb + (radiance * sunRadiance);
    vec3 finalScatteringRadiance = radiance * sunRadiance;
    float atmAlpha = length(finalScatteringRadiance);
    return spaceColor.rgb + finalScatteringRadiance;
  }
  
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
                 const float mu, const vec3 attenuationXtoX0, const vec4 groundColor, 
                 const vec4 normalReflectance, const float irradianceFactor)
{
  vec3 reflectedRadiance = vec3(0.0f);

  // First we obtain the ray's end point on the surface
  vec3  x0 = x + t * v;
  float r0 = length(x0);
  // Normal of intersection point.
  vec3  n  = normalReflectance.xyz;
  vec4 reflectance = groundColor * vec4(0.4);
  //reflectance.w = 1.0;
            
  // L0 is not included in the irradiance texture.
  // We first calculate the light attenuation from the top of the atmosphere
  // to x0. 
  float muSun = max(dot(n, s), 0.0);
  // Is direct Sun light arriving at x0? If not, there is no direct light from Sun (shadowed)
  vec3  transmittanceL0 = muSun < -sqrt(1.0f - ((Rg * Rg) / (r0 * r0))) ? vec3(0.0f) : transmittanceLUT(r0, muSun);

  // E[L*] at x0
  vec3 irradianceReflected = irradiance(irradianceTexture, r0, muSun) * irradianceFactor;
    
  // R[L0] + R[L*]
  vec3 groundRadiance = reflectance.rgb * (muSun * transmittanceL0 + irradianceReflected)
    * sunRadiance / M_PI;
    
  // Specular reflection from sun on oceans and rivers  
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
vec3 sunColor(const vec3 x, const float t, const vec3 v, const vec3 s, const float r,
              const float mu, const float irradianceFactor) {
  vec3 transmittance = (r <= Rt) ? ( mu < -sqrt(1.0f - (Rg*Rg)/(r*r)) ? vec3(0.0f) : transmittanceLUT(r, mu)) : vec3(1.0f);  
  float sunFinalColor = step(cos(M_PI / 180.0), dot(v, s)) * sunRadiance * (1.0 - irradianceFactor); 

  return transmittance * sunFinalColor;      
}

void main() {
    //float geoDepth = 0.0;
    vec4 meanColor = vec4(0.0);
    vec4 meanNormal = vec4(0.0);
    vec4 meanPosition = vec4(0.0);
    //vec4 positionArray[nAaSamples];
    vec4 positionArray[8];
    float maxAlpha = -1.0;
    for (int i = 0; i < nAaSamples; i++) {
      meanNormal   += texelFetch(mainNormalReflectanceTexture, ivec2(gl_FragCoord), i);
      vec4 color = texelFetch(mainColorTexture, ivec2(gl_FragCoord), i);
      if ( color.a > maxAlpha )
        maxAlpha = color.a;
      meanColor += color;
      meanPosition += texelFetch(mainPositionTexture, ivec2(gl_FragCoord), i);
      positionArray[i] = texelFetch(mainPositionTexture, ivec2(gl_FragCoord), i);
      //     geoDepth += denormalizeFloat(texelFetch(mainDepthTexture, ivec2(gl_FragCoord), i).x);
    }
    meanColor    /= nAaSamples;
    meanNormal   /= nAaSamples;
    meanPosition /= nAaSamples;
    // geoDepth /= nAaSamples;

    meanColor.a = maxAlpha;
    
    meanNormal.xyz = normalize(meanNormal.xyz);
    
    // Ray in object space
    dRay ray;
    dvec4 planetPositionObjectCoords = dvec4(0.0);
    dvec4 cameraPositionInObject = dvec4(0.0);
    
    if (RenderableClass == RenderablePlanet) {
      // Get the ray from camera to atm in object space
      dCalculateRayRenderablePlanet(ray, planetPositionObjectCoords, cameraPositionInObject);
    
      bool  insideATM    = false;
      double offset      = 0.0;
      double maxLength   = 0.0;     
      bool  intersectATM = false;
      
      intersectATM = dAtmosphereIntersection(planetPositionObjectCoords.xyz, ray,  Rt-10*EPSILON,
                                            insideATM, offset, maxLength );
      if ( intersectATM ) {
        /*
        vec4 farthestPosition = vec4(0.0);
        float farthest = -1.0;
        for (int i = 0; i < nAaSamples; i++) {
          float tmpDistance = float(distance(dCampos.xyz, dvec3(positionArray[i].xyz)));
          if ( positionArray[i].w > 0.0 && tmpDistance >= farthest ) {
            farthest = tmpDistance;
            farthestPosition = positionArray[i];
          }
        }
        dvec3 tmpPos = dmat3(dInverseCamRotTransform) * dvec3(dInverseScaleTransformMatrix * farthestPosition);
        */
        dvec3 tmpPos = dmat3(dInverseCamRotTransform) * dvec3(dInverseScaleTransformMatrix * meanPosition);
        dvec4 fragWorldCoords  = dvec4(dCampos + tmpPos, 1.0); // Fragment in World Coords
        dvec4 fragObjectCoords = dInverseTransformMatrix * dvec4(-dObjpos.xyz + fragWorldCoords.xyz, 1.0);
        double pixelDepth = distance(cameraPositionInObject.xyz, fragObjectCoords.xyz);

        // All calculations are done in Km:
        pixelDepth /= 1000.0; 
        fragObjectCoords.xyz /= 1000.0;
        
        // Now we check is if the atmosphere is occluded, i.e., if the distance to the pixel 
        // in the depth buffer is less than the distance to the atmosphere then the atmosphere
        // is occluded
        // Fragments positions into G-Buffer are written in OS Eye Space (Camera Rig Coords)
        // when using their positions later, one must convert them to the planet's coords 

        if ((pixelDepth > 0.0) && pixelDepth < offset) {        
          renderTarget = meanColor;          
        } else {
          // Following paper nomenclature      
          double t = offset;                  
          vec3 attenuation;     

          // Moving observer from camera location to top atmosphere
          vec3  x  = vec3(ray.origin.xyz + t*ray.direction.xyz);
          float r  = 0.0;//length(x);
          vec3  v  = vec3(ray.direction.xyz);
          float mu = 0.0;//dot(x, v) / r;
          vec3  s  = vec3(sunDirectionObj);
          float tF = float(maxLength - t);

          // Because we may move the camera origin to the top of atmosphere 
          // we also need to adjust the pixelDepth for this offset so the
          // next comparison with the planet's ground make sense:
          pixelDepth -= offset;

          float irradianceFactor = 0.0;
          
          vec3 inscatterColor = inscatterRadiance(x, tF, irradianceFactor, v,
                                                  s, r, mu, attenuation, 
                                                  vec3(fragObjectCoords.xyz),
                                                  maxLength, pixelDepth,
                                                  meanColor); 
          vec3 groundColor    = groundColor(x, tF, v, s, r, mu, attenuation,
                                            meanColor, meanNormal, irradianceFactor);
          vec3 sunColor       = sunColor(x, tF, v, s, r, mu, irradianceFactor); 
          
          vec4 finalRadiance = vec4(HDR(inscatterColor + groundColor + sunColor), 1.0);
                  
          renderTarget = finalRadiance;
        }      
      } else {
        //renderTarget = vec4(1.0, 1.0, 0.0, 1.0);      
        //renderTarget = vec4(0.0);      
        renderTarget = meanColor;
      }
    } else if ( RenderableClass == RenderableGlobe) {
      // Get the ray from camera to atm in object space
      dCalculateRayRenderableGlobe(ray, planetPositionObjectCoords, cameraPositionInObject);
    
      bool  insideATM    = false;
      double offset      = 0.0;
      double maxLength   = 0.0;     

      bool  intersectATM = false;

      // Instead of ray-ellipsoid intersection lets transform the ray to a sphere:
      dRay transfRay;
      transfRay.origin = ray.origin;
      transfRay.direction = ray.direction;

      // transfRay.origin.z *= 1000.0/ellipsoidRadii.x;
      // transfRay.direction.z *= 1000.0/ellipsoidRadii.x;        
      // transfRay.origin.x *= 1000.0/ellipsoidRadii.y;
      // transfRay.direction.x *= 1000.0/ellipsoidRadii.y;    
      // transfRay.origin.y *= 1000.0/ellipsoidRadii.z;
      // transfRay.direction.y *= 1000.0/ellipsoidRadii.z;
      // transfRay.direction.xyz = normalize(transfRay.direction.xyz);

      // intersectATM = dAtmosphereIntersection(planetPositionObjectCoords.xyz, transfRay, 1.0,
      //                                               insideATM, offset, maxLength );

      // intersectATM = dAtmosphereIntersection(planetPositionObjectCoords.xyz, transfRay,  Rt+EPSILON,
      //                                       insideATM, offset, maxLength );
    
      intersectATM = dAtmosphereIntersection(planetPositionObjectCoords.xyz, transfRay,  Rt-10*EPSILON,
                                            insideATM, offset, maxLength );

      if ( intersectATM ) {
        // Now we check is if the atmosphere is occluded, i.e., if the distance to the pixel 
        // in the depth buffer is less than the distance to the atmosphere then the atmosphere
        // is occluded
        // Fragments positions into G-Buffer are written in OS Eye Space (Camera Rig Coords)
        // when using their positions later, one must convert them to the planet's coords
        
        // OS Eye to World coords

        // Version when no milkway is present (performance hit)
        /*
        vec4 farthestPosition = vec4(0.0);
        float farthest = -1.0;
        for (int i = 0; i < nAaSamples; i++) {
          float tmpDistance = float(distance(dCampos.xyz, dvec3(positionArray[i].xyz)));
          if ( positionArray[i].w > 0.0 && tmpDistance >= farthest ) {
            farthest = tmpDistance;
            farthestPosition = positionArray[i];
          }
        }
        dvec4 tmpRInvPos            = dInverseCamRotTransform * farthestPosition;        
        */
        // Version with milkway enabled
        dvec4 tmpRInvPos            = dInverseCamRotTransform * meanPosition;
        
        dvec4 fragWorldCoords       = dvec4(dvec3(tmpRInvPos) + dCampos, 1.0);
        //dvec4 tmpRInvNormal         = dInverseCamRotTransform * meanNormal;
        //dvec4 fragNormalWorldCoords = dvec4(dvec3(tmpRInvNormal) + dCampos, 1.0);

        // World to Object (Normal and Position in meters)
        dvec4 fragObjectCoords       = dInverseTransformMatrix * fragWorldCoords;
        //dvec4 fragNormalObjectCoords = dInverseTransformMatrix * fragNormalWorldCoords;

        // Normal in Object Space already (changed 05/26/2017).
        //dvec4 fragNormalObjectCoords = dvec4(normalize(meanNormal.xyz), 1.0);

        // Distance of the pixel in the gBuffer to the observer
        double pixelDepth = distance(cameraPositionInObject.xyz, fragObjectCoords.xyz);
        
        // All calculations are done in Km:
        pixelDepth /= 1000.0;
        fragObjectCoords.xyz /= 1000.0;
        
        if (meanPosition.xyz != vec3(0.0) && (pixelDepth < offset)) {        
          renderTarget = meanColor;
        } else {
          // Following paper nomenclature      
          double t = offset;                  
          vec3 attenuation;     

          // Moving observer from camera location to top atmosphere
          // If the observer is already inside the atm, offset = 0.0
          // and no changes at all.
          vec3  x  = vec3(ray.origin.xyz + t*ray.direction.xyz);
          float r  = 0.0;//length(x);
          vec3  v  = vec3(ray.direction.xyz);
          float mu = 0.0;//dot(x, v) / r;
          vec3  s  = vec3(sunDirectionObj);
          float tF = float(maxLength - t);

          // Because we may move the camera origin to the top of atmosphere 
          // we also need to adjust the pixelDepth for this offset so the
          // next comparison with the planet's ground make sense:
          pixelDepth -= offset;

          float irradianceFactor = 0.0;

          vec3 inscatterColor = inscatterRadiance(x, tF, irradianceFactor, v,
                                                  s, r, mu, attenuation, 
                                                  vec3(fragObjectCoords.xyz),
                                                  maxLength, pixelDepth,
                                                  meanColor); 
          vec3 groundColor    = groundColor(x, tF, v, s, r, mu, attenuation,
                                            meanColor, meanNormal, irradianceFactor);
          vec3 sunColor       = sunColor(x, tF, v, s, r, mu, irradianceFactor); 
          
          //vec4 finalRadiance = vec4(HDR(inscatterColor + sunColor), 1.0);
          //finalRadiance = mix(finalRadiance, meanColor);
          //vec4 finalRadiance = vec4(inscatterColor, 1.0);
          //vec4 finalRadiance = vec4(HDR(groundColor), 1.0);
          //vec4 finalRadiance = vec4(HDR(inscatterColor), 1.0);
          //vec4 finalRadiance = vec4(HDR(inscatterColor + meanColor.xyz), meanColor.w);
          //vec4 finalRadiance = vec4(HDR(sunColor), 1.0);
          //vec4 finalRadiance = vec4(sunColor, 1.0);
          //vec4 finalRadiance = vec4(HDR(inscatterColor + groundColor), 1.0);
          
          // The meanColor is temporary here
          vec4 finalRadiance = vec4(HDR(inscatterColor + groundColor + sunColor), 1.0);          
          
          renderTarget = finalRadiance;
          //renderTarget = vec4(1.0 - HDR(vec3(pixelDepth/100)),1.0);
        }
      } else {
        renderTarget = vec4(HDR(meanColor.xyz), meanColor.a);        
      }
    } else {
      // No ATM defined.
      renderTarget = vec4(HDR(meanColor.xyz), meanColor.a);
    }

                
}

