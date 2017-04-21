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

uniform bool  _performShading = true;
uniform float transparency;
uniform int   shadows;

uniform float screenX;
uniform float screenY;
uniform float screenWIDTH;
uniform float screenHEIGHT;

uniform float time;
uniform sampler2D texture1;
uniform sampler2D nightTex;
uniform sampler2D cloudsTexture;

uniform sampler2D reflectanceTexture;
uniform sampler2D irradianceTexture;
uniform sampler3D inscatterTexture;

// Texture coordinates
in vec2 vs_st;
in vec4 vs_normal;
// vs_position is the object position in Eye space but in PSC coords.
in vec4 vs_position;
// Vertex position in world coordinates in meters (no powerscalling coors).    
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
// For the atmosphere being correctly rendered, one must use the planet's center
// as the center of the coordinate system.


/* Calculates the intersection of the view ray direction with the atmosphere and 
 * returns the first intersection (0.0 when inside atmosphere): offset
 * and the second intersection: maxLength
 */
struct Ray {
  vec4 origin;
  vec4 direction;
};

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
  } else if (s >= 0.0) {
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
    mu  = (r * mu + dist) / Rt;
    mu2 = mu * mu;
    r   = Rt;
    r2  = r * r;    
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
    
    // Initial ground radiance (the surface color)
    vec4 reflectance = texture(reflectanceTexture, vs_st) * vec4(0.2, 0.2, 0.2, 1.0);
    
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
    vec4 clouds = vec4(0.85)*texture(cloudsTexture, vs_st);

    // R[L0] + R[L*]
    vec3 groundRadiance = (reflectance.rgb + clouds.rgb) * 
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

  // Vertex position in PSC coordinates in Eye space
  vec4 position = vs_position;
  float depth   = pscDepth(position);
  vec4 diffuse  = texture(texture1, vs_st);
  vec4 diffuse2 = texture(nightTex, vs_st);
  vec4 clouds   = texture(cloudsTexture, vs_st);
  
  Fragment frag;

  // Shading is enabled
  if (_performShading) {
    // atmosphere
    // vec4 viewport = vec4(screenX, screenY, screenWIDTH, screenHEIGHT);
    // vec4 ndcPos;
    // ndcPos.xy = ((2.0f * gl_FragCoord.xy) - (2.0f * viewport.xy)) / (viewport.zw) - 1.0f;
    // ndcPos.z = (2.0f * gl_FragCoord.z - gl_DepthRange.near - gl_DepthRange.far) / 
    //   (gl_DepthRange.far - gl_DepthRange.near);
    // ndcPos.w = 1.0f;
    // vec4 clipPos = ndcPos / gl_FragCoord.w;
    // vec4 projCoords = projInverse * clipPos;
    // vec4 viewDirection =  normalize(completeInverse * vec4(projCoords.xyz, 0.0));
    // vec3 v = normalize(viewDirection.xyz);
    
    // float offset, maxLength;
    // vec4 ppos = vec4(0.0);

    // if (intersectAtmosphere(ppos, v, Rg, offset, maxLength)) {    
    //   // Following paper nomenclature
    //   float t  = offset;
    //   vec3  x  = cameraPosObj.xyz;
    //   float r  = length(x);
    //   float mu = dot(x, v) / r;
    //   vec3  s  = normalize(sunPositionObj);
      
    //   vec3 attenuation;
    //   vec3 inscatterColor = inscatterRadiance(x, t, v, s, r, mu, attenuation); 
    //   vec3 groundColor = groundColor(x, t, v, s, r, mu, attenuation);
    //   vec3 sunColor = sunColor(x, t, v, s, r, mu); 
      
    //   //diffuse = vec4(HDR(sunColor + groundColor + inscatterColor), 1.0));      
    //   //diffuse = vec4(HDR(inscatterColor), 1.0)); 
    //   //diffuse = vec4(HDR(groundColor), 1.0); 
    //   //diffuse = vec4(HDR(sunColor), 1.0));       
      
    //   //diffuse = HDR(vec4(sunColor + groundColor + inscatterColor, 1.0) + diffuse2); 
    //   vec4 finalRadiance = calcShadow(shadowDataArray, vs_posWorld.xyz) * 
    //                         (vec4(sunColor + groundColor + inscatterColor, 1.0) + diffuse2);
                     
    //   diffuse = vec4(HDR(finalRadiance.xyz), finalRadiance.w); 
    // }

    // directional lighting
    vec3 origin = vec3(0.0);
    vec4 spec = vec4(0.0);
       
    vec3 n = normalize(vs_normal.xyz);
    //vec3 e = normalize(camdir);
    vec3 l_pos = vec3(sun_pos); // sun.
    vec3 l_dir = normalize(l_pos-objpos.xyz);
    float intensity = min(max(5*dot(n,l_dir), 0.0), 1);
    float darkSide  = min(max(5*dot(n,-l_dir), 0.0), 1);
        
    float shine = 0.0001;

    vec4 specular = vec4(0.5);
    vec4 ambient = vec4(0.0,0.0,0.0,transparency);
        
    vec4 daytex = max(intensity * diffuse, ambient);
    vec4 mixtex = mix(diffuse, diffuse2,  (1+dot(n,-l_dir))/2);
        
    diffuse = calcShadow(shadowDataArray, vs_posWorld.xyz) * (daytex*2 + mixtex)/3;  
  }

  diffuse[3] = transparency;
  frag.color = diffuse;
  frag.depth = depth;  

  return frag;
}
