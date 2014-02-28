#version 400 core

out vec4 FragColor;

uniform sampler3D Density;
uniform vec3 LightPosition = vec3(0.25, 1.0, 3.0);
uniform vec3 LightIntensity = vec3(15.0);
uniform float Absorption = 1.0;
uniform mat4 Modelview;
uniform float FocalLength;
uniform vec2 WindowSize;
uniform vec3 RayOrigin;

const float maxDist = sqrt(2.0);
const int numSamples = 128;
const float stepSize = maxDist/float(numSamples);
const int numLightSamples = 32;
const float lscale = maxDist / float(numLightSamples);
const float densityFactor = 5;

struct Ray {
    vec3 Origin;
    vec3 Dir;
};

struct AABB {
    vec3 Min;
    vec3 Max;
};

bool IntersectBox(Ray r, AABB aabb, out float t0, out float t1)
{
    vec3 invR = 1.0 / r.Dir;
    vec3 tbot = invR * (aabb.Min-r.Origin);
    vec3 ttop = invR * (aabb.Max-r.Origin);
    vec3 tmin = min(ttop, tbot);
    vec3 tmax = max(ttop, tbot);
    vec2 t = max(tmin.xx, tmin.yz);
    t0 = max(t.x, t.y);
    t = min(tmax.xx, tmax.yz);
    t1 = min(t.x, t.y);
    return t0 <= t1;
}

void main() {
    vec3 rayDirection;
    rayDirection.xy = 2.0 * gl_FragCoord.xy / WindowSize - 1.0;
    rayDirection.z = -FocalLength;
    rayDirection = (vec4(rayDirection, 0) * Modelview).xyz;

    Ray eye = Ray( RayOrigin, normalize(rayDirection) );
    AABB aabb = AABB(vec3(-1.0), vec3(+1.0));

    float tnear, tfar;
    IntersectBox(eye, aabb, tnear, tfar);
    if (tnear < 0.0) tnear = 0.0;

    vec3 rayStart = eye.Origin + eye.Dir * tnear;
    vec3 rayStop = eye.Origin + eye.Dir * tfar;
    rayStart = 0.5 * (rayStart + 1.0);
    rayStop = 0.5 * (rayStop + 1.0);

    vec3 pos = rayStart;
    vec3 step = normalize(rayStop-rayStart) * stepSize;
    float travel = distance(rayStop, rayStart);
    float T = 1.0;
    vec3 Lo = vec3(0.0);

    for (int i=0; i < numSamples && travel > 0.0; ++i, pos += step, travel -= stepSize) {

        float density = texture(Density, pos).x * densityFactor;
        if (density <= 0.0)
            continue;

        T *= 1.0-density*stepSize*Absorption;
        if (T <= 0.01)
            break;

        vec3 lightDir = normalize(LightPosition-pos)*lscale;
        float Tl = 1.0;
        vec3 lpos = pos + lightDir;

        for (int s=0; s < numLightSamples; ++s) {
            float ld = texture(Density, lpos).x;
            Tl *= 1.0-Absorption*stepSize*ld;
            if (Tl <= 0.01) 
            lpos += lightDir;
        }

        vec3 Li = LightIntensity*Tl;
        Lo += Li*T*density*stepSize;
    }

    FragColor.rgb = Lo;
    FragColor.a = 1-T;

    FragColor = vec4(1.0, 0.0, 1.0, 1.0);
}