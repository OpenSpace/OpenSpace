#pragma once

#include <core/md_vec_math.h>
#include <core/md_bitfield.h>
#include <md_molecule.h>

#include <math.h>

// http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl

inline vec3_t rgb_to_hsv(vec3_t c) {
    const vec4_t K = {0.0f, -1.0f / 3.0f, 2.0f / 3.0f, -1.0f};
    const vec4_t p = vec4_lerp({c.z, c.y, K.w, K.z}, {c.y, c.z, K.x, K.y}, stepf(c.z, c.y));
    const vec4_t q = vec4_lerp({p.x, p.y, p.w, c.x}, {c.x, p.y, p.z, p.x}, stepf(p.x, c.x));

    const float d = q.x - MIN(q.w, q.y);
    const float e = 1.0e-10f;
    return {ABS(q.z + (q.w - q.y) / (6.0f * d + e)), d / (q.x + e), q.x};
}

inline vec3_t hsv_to_rgb(vec3_t c) {
    vec4_t K = {1.0f, 2.0f / 3.0f, 1.0f / 3.0f, 3.0f};
    vec3_t p = vec3_abs(vec3_fract(vec3_t{c.x, c.x, c.x} + vec3_from_vec4(K)) * 6.0f - vec3_t{K.w, K.w, K.w});
    return c.z * vec3_lerp(vec3_t{K.x, K.x, K.x}, vec3_clamp_f(p - vec3_t{K.x, K.x, K.x}, 0.0f, 1.0f), c.y);
}

constexpr inline vec3_t hcl_to_rgb(vec3_t HCL) {
    constexpr float HCLgamma = 3;
    constexpr float HCLy0 = 100;
    constexpr float HCLmaxL = 0.530454533953517f;  // == exp(HCLgamma / HCLy0) - 0.5

    vec3_t RGB = vec3_t{};
    if (HCL.z != 0) {
        float H = HCL.x;
        float C = HCL.y;
        float L = HCL.z * HCLmaxL;
        float Q = expf((1.f - C / (2.f * L)) * (HCLgamma / HCLy0));
        float U = (2.f * L - C) / (2.f * Q - 1.f);
        float V = C / Q;
        float T = tanf((H + MIN(fractf(2.f * H) / 4.f, fractf(-2.f * H) / 8.f)) * PI * 2.f);
        H *= 6.f;
        if (H <= 1.f) {
            RGB.x = 1.f;
            RGB.y = T / (1.f + T);
        } else if (H <= 2.f) {
            RGB.x = (1.f + T) / T;
            RGB.y = 1.f;
        } else if (H <= 3.f) {
            RGB.y = 1.f;
            RGB.z = 1.f + T;
        } else if (H <= 4.f) {
            RGB.y = 1.f / (1.f + T);
            RGB.z = 1.f;
        } else if (H <= 5.f) {
            RGB.x = -1.f / T;
            RGB.z = 1.f;
        } else {
            RGB.x = 1.f;
            RGB.z = -T;
        }
        RGB = RGB * V + U;
    }
    return RGB;
}

inline vec3_t rgb_to_hcl(vec3_t rgb) {
    constexpr float HCLgamma = 3.f;
    constexpr float HCLy0 = 100.f;
    constexpr float HCLmaxL = 0.530454533953517f;  // == exp(HCLgamma / HCLy0) - 0.5

    vec3_t HCL;
    float H = 0;
    float U = MIN(rgb.x, MIN(rgb.y, rgb.z));
    float V = MAX(rgb.x, MAX(rgb.y, rgb.z));
    float Q = HCLgamma / HCLy0;
    HCL.y = V - U;
    if (HCL.y != 0) {
        H = atan2f(rgb.y - rgb.z, rgb.x - rgb.y) / 3.1515926535f;
        Q *= U / V;
    }
    Q = expf(Q);
    HCL.x = fractf(H / 2.f - MIN(fractf(H), fractf(-H)) / 6.f);
    HCL.y *= Q;
    HCL.z = lerp(-U, V, Q) / (HCLmaxL * 2.f);
    return HCL;
}

// clang-format off
inline vec3_t rgb_to_XYZ(vec3_t rgb) {
    constexpr mat3_t RGB_2_XYZ = {0.4124564f, 0.3575761f, 0.1804375f,
        0.2126729f, 0.7151522f, 0.0721750f,
        0.0193339f, 0.1191920f, 0.9503041f};
    return RGB_2_XYZ * rgb;
}

inline vec3_t XYZ_to_rgb(vec3_t XYZ) {
    constexpr mat3_t XYZ_2_RGB = { 3.2404542f, -1.5371385f, -0.4985314f,
        -0.9692660f,  1.8760108f,  0.0415560f,
        0.0556434f, -0.2040259f,  1.0572252f};
    return XYZ_2_RGB * XYZ;
}
// clang-format on

inline vec3_t XYZ_to_Lab(vec3_t XYZ) {
    const auto f = [](float t) {
        const float d = 6.f / 29.f;
        return t > d * d * d ? powf(t, 1.0f / 3.0f) : (t / (3.f * d * d) + 4.f / 29.f);
    };

    const float Xn = 0.950489f;  // reference white
    const float Yn = 1.0f;
    const float Zn = 0.825188f;
    const float fx = f(XYZ.x / Xn);
    const float fy = f(XYZ.y / Yn);
    const float fz = f(XYZ.z / Zn);
    const float L = 116.f * fy - 16.f;  // maximum L = 100
    const float a = 500.f * (fx - fy);
    const float b = 200.f * (fy - fz);

    return {L, a, b};
}

inline vec3_t Lab_to_XYZ(vec3_t Lab) {
    const auto f = [](float t) {
        const float d = 6.f / 29.f;
        return t > d ? t * t * t : 3.0f * d * d * (t - 4.f / 29.f);
    };

    const float Xn = 0.950489f;  // reference white
    const float Yn = 1.0f;
    const float Zn = 0.825188f;
    const float X = Xn * f((Lab.x + 16.f) / 116.f + Lab.y / 500.f);
    const float Y = Yn * f((Lab.x + 16.f) / 116.f);
    const float Z = Zn * f((Lab.x + 16.f) / 116.f - Lab.z / 200.f);

    return {X, Y, Z};
}

inline vec3_t rgb_to_Lab(vec3_t rgb) { return XYZ_to_Lab(rgb_to_XYZ(rgb)); }
inline vec3_t Lab_to_rgb(vec3_t Lab) { return XYZ_to_rgb(Lab_to_XYZ(Lab)); }

inline vec3_t hcl_to_rgb(float h, float c, float l) { return hcl_to_rgb({h, c, l}); }
inline vec3_t rgb_to_hcl(float r, float g, float b) { return rgb_to_hcl({r, g, b}); }

enum class ColorMapping { Uniform, Cpk, AtomIndex, ResId, ResIndex, ChainId, ChainIndex, SecondaryStructure, Property };

inline vec4_t color_from_hash(uint32_t hash, uint32_t num_bins = 0) {
    constexpr float chroma = 0.8f;
    constexpr float luminance = 1.0f;
    const uint32_t mod = num_bins == 0 ? 0xFFFFFFFFU : num_bins;
    const float hue = (hash % mod) / (float)mod;
    const vec3_t rgb = hcl_to_rgb({hue, chroma, luminance});

    return vec4_from_vec3(rgb, 1);
}

constexpr inline vec4_t convert_color(uint32_t rgba) {
    return { (float)((rgba >> 0) & 0xFF) / 255.f, (float)((rgba >> 8) & 0xFF) / 255.f, (float)((rgba >> 16) & 0xFF) / 255.f, (float)((rgba >> 24) & 0xFF) / 255.f };
}

constexpr inline uint32_t convert_color(vec4_t color) {
    uint32_t out = 0;
    out |= ((uint32_t)(CLAMP(color.x, 0.0f, 1.0f) * 255.0f + 0.5f)) << 0;
    out |= ((uint32_t)(CLAMP(color.y, 0.0f, 1.0f) * 255.0f + 0.5f)) << 8;
    out |= ((uint32_t)(CLAMP(color.z, 0.0f, 1.0f) * 255.0f + 0.5f)) << 16;
    out |= ((uint32_t)(CLAMP(color.w, 0.0f, 1.0f) * 255.0f + 0.5f)) << 24;
    return out;
}

void color_atoms_uniform(uint32_t* colors, int64_t count, vec4_t uniform_color, const md_bitfield_t* mask = NULL);
void color_atoms_cpk(uint32_t* colors, int64_t count, const md_molecule_t& mol);
void color_atoms_idx(uint32_t* colors, int64_t count, const md_molecule_t& mol);
void color_atoms_residue_id(uint32_t* colors, int64_t count, const md_molecule_t& mol);
void color_atoms_residue_index(uint32_t* colors, int64_t count, const md_molecule_t& mol);
void color_atoms_chain_id(uint32_t* colors, int64_t count, const md_molecule_t& mol);
void color_atoms_chain_index(uint32_t* colors, int64_t count, const md_molecule_t& mol);
void color_atoms_secondary_structure(uint32_t* colors, int64_t count, const md_molecule_t& mol);

void filter_colors(uint32_t* colors, int64_t num_colors, const md_bitfield_t* mask);
void desaturate_colors(uint32_t* colors, const md_bitfield_t* mask, float scale);

vec3_t magma_color_scale(float t);
vec3_t inferno_color_scale(float t);
vec3_t plasma_color_scale(float t);
vec3_t viridis_color_scale(float t);

vec3_t orange_color_scale(float t);
vec3_t green_color_scale(float t);

vec4_t qualitative_color_scale(int idx);