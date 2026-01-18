/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

// The following code was taken from VIAMD (https://github.com/scanberg/viamd), MIT.

#include <modules/molecule/mol/viamd/coloring.h>
#include <md_util.h>
#include <core/md_bitfield.h>

static uint32_t constexpr crc32_tab[] = {
    0x00000000, 0x77073096, 0xee0e612c, 0x990951ba, 0x076dc419, 0x706af48f, 0xe963a535, 0x9e6495a3, 0x0edb8832, 0x79dcb8a4, 0xe0d5e91e, 0x97d2d988, 0x09b64c2b, 0x7eb17cbd, 0xe7b82d07, 0x90bf1d91,
    0x1db71064, 0x6ab020f2, 0xf3b97148, 0x84be41de, 0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7, 0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec, 0x14015c4f, 0x63066cd9, 0xfa0f3d63, 0x8d080df5,
    0x3b6e20c8, 0x4c69105e, 0xd56041e4, 0xa2677172, 0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b, 0x35b5a8fa, 0x42b2986c, 0xdbbbc9d6, 0xacbcf940, 0x32d86ce3, 0x45df5c75, 0xdcd60dcf, 0xabd13d59,
    0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116, 0x21b4f4b5, 0x56b3c423, 0xcfba9599, 0xb8bda50f, 0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924, 0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d,
    0x76dc4190, 0x01db7106, 0x98d220bc, 0xefd5102a, 0x71b18589, 0x06b6b51f, 0x9fbfe4a5, 0xe8b8d433, 0x7807c9a2, 0x0f00f934, 0x9609a88e, 0xe10e9818, 0x7f6a0dbb, 0x086d3d2d, 0x91646c97, 0xe6635c01,
    0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e, 0x6c0695ed, 0x1b01a57b, 0x8208f4c1, 0xf50fc457, 0x65b0d9c6, 0x12b7e950, 0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49, 0x8cd37cf3, 0xfbd44c65,
    0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2, 0x4adfa541, 0x3dd895d7, 0xa4d1c46d, 0xd3d6f4fb, 0x4369e96a, 0x346ed9fc, 0xad678846, 0xda60b8d0, 0x44042d73, 0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9,
    0x5005713c, 0x270241aa, 0xbe0b1010, 0xc90c2086, 0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f, 0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17, 0x2eb40d81, 0xb7bd5c3b, 0xc0ba6cad,
    0xedb88320, 0x9abfb3b6, 0x03b6e20c, 0x74b1d29a, 0xead54739, 0x9dd277af, 0x04db2615, 0x73dc1683, 0xe3630b12, 0x94643b84, 0x0d6d6a3e, 0x7a6a5aa8, 0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1,
    0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d, 0x806567cb, 0x196c3671, 0x6e6b06e7, 0xfed41b76, 0x89d32be0, 0x10da7a5a, 0x67dd4acc, 0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5,
    0xd6d6a3e8, 0xa1d1937e, 0x38d8c2c4, 0x4fdff252, 0xd1bb67f1, 0xa6bc5767, 0x3fb506dd, 0x48b2364b, 0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60, 0xdf60efc3, 0xa867df55, 0x316e8eef, 0x4669be79,
    0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236, 0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f, 0xc5ba3bbe, 0xb2bd0b28, 0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31, 0x2cd99e8b, 0x5bdeae1d,
    0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x026d930a, 0x9c0906a9, 0xeb0e363f, 0x72076785, 0x05005713, 0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38, 0x92d28e9b, 0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21,
    0x86d3d2d4, 0xf1d4e242, 0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b, 0x6fb077e1, 0x18b74777, 0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c, 0x8f659eff, 0xf862ae69, 0x616bffd3, 0x166ccf45,
    0xa00ae278, 0xd70dd2ee, 0x4e048354, 0x3903b3c2, 0xa7672661, 0xd06016f7, 0x4969474d, 0x3e6e77db, 0xaed16a4a, 0xd9d65adc, 0x40df0b66, 0x37d83bf0, 0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
    0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605, 0xcdd70693, 0x54de5729, 0x23d967bf, 0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94, 0xb40bbe37, 0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d};

constexpr uint32_t crc32impl(uint32_t prevCrc, const char* str, size_t size) { return !size ? prevCrc : crc32impl((prevCrc >> 8) ^ crc32_tab[(prevCrc ^ *str) & 0xff], str + 1, size - 1); }

constexpr uint32_t crc32(const char* ptr, size_t size) { return crc32impl(0xffffffff, ptr, size) ^ 0xffffffff; }

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
        float T = tanf((H + MIN(fractf(2.f * H) / 4.f, fractf(-2.f * H) / 8.f)) * static_cast<float>(PI) * 2.f);
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

static inline void set_colors(uint32_t* colors, int64_t count, uint32_t color) {
    for (int64_t i = 0; i < count; ++i) {
        colors[i] = color;
    }
}

void color_atoms_uniform(uint32_t* colors, int64_t count, uint32_t rgba, const md_bitfield_t* mask) {
    if (mask) {
        int64_t beg_bit = mask->beg_bit;
        int64_t end_bit = mask->end_bit;
        while ((beg_bit = md_bitfield_scan(mask, beg_bit, end_bit)) != 0) {
            int64_t i = beg_bit - 1;
            colors[i] = rgba;
        }
    } else {
        set_colors(colors, count, rgba);
    }
}

void color_atoms_cpk(uint32_t* colors, int64_t count, const md_molecule_t& mol) {
    // In the case of coarsegrained molecules, CPK is a bit illdefined as it operates on elements.
    // What we do instead is to try and ad-hoc guess elements from the atom names.
    if (count == 0)
        return;

    md_element_t* elements = 0;
    const md_element_t* elem = mol.atom.element;
    
    if (!elem) {
        elements = md_array_create(md_element_t, count, default_temp_allocator);
        md_util_element_guess(elements, md_array_size(elements), &mol);
        elem = elements;
    }
    
    for (int64_t i = 0; i < count; i++) {
        colors[i] = md_util_element_cpk_color(elem[i]);
    }
    
    md_array_free(elements, default_temp_allocator);
}

void color_atoms_idx(uint32_t* colors, int64_t count, const md_molecule_t&) {
    for (int i = 0; i < (int)count; ++i) {
        colors[i] = convert_color(color_from_hash(crc32((char*)&i, sizeof(i))));
    }
}

void color_atoms_residue_id(uint32_t* colors, int64_t count, const md_molecule_t& mol) {
    set_colors(colors, count, 0xFFFFFFFFU);
    if (mol.residue.name && mol.residue.atom_range) {
        for (int64_t i = 0; i < mol.residue.count; i++) {
            str_t str = mol.residue.name[i];
            const uint32_t color = convert_color(color_from_hash(crc32(str.ptr, str.len), 30));
            set_colors(colors + mol.residue.atom_range[i].beg, mol.residue.atom_range[i].end - mol.residue.atom_range[i].beg, color);
        }
    }
}
void color_atoms_residue_index(uint32_t* colors, int64_t count, const md_molecule_t& mol) {
    set_colors(colors, count, 0xFFFFFFFFU);
    if (mol.residue.atom_range) {
        for (int64_t i = 0; i < mol.residue.count; i++) {
            const uint32_t color = convert_color(color_from_hash(crc32((char*)&i, sizeof(i))));
            set_colors(colors + mol.residue.atom_range[i].beg, mol.residue.atom_range[i].end - mol.residue.atom_range[i].beg, color);
        }
    }
}
void color_atoms_chain_id(uint32_t* colors, int64_t count, const md_molecule_t& mol) {
    set_colors(colors, count, 0xFFFFFFFFU);
    if (mol.chain.atom_range) {
        for (int64_t i = 0; i < mol.chain.count; i++) {
            str_t str = mol.chain.id[i];
            const uint32_t color = convert_color(color_from_hash(crc32(str.ptr, str.len), 30));
            set_colors(colors + mol.chain.atom_range[i].beg, mol.chain.atom_range[i].end - mol.chain.atom_range[i].beg, color);
        }
    }
}

void color_atoms_chain_index(uint32_t* colors, int64_t count, const md_molecule_t& mol) {
    set_colors(colors, count, 0xFFFFFFFFU);
    if (mol.chain.atom_range) {
        for (int i = 0; i < (int)mol.chain.count; i++) {
            //const float hue = (float)i / (float)(MIN(mol.chain.count, 16));
            //const float sat = 0.8f;
            //const float val = 1.0f;
            const uint32_t color = convert_color(color_from_hash(crc32((char*)&i, sizeof(i))));
            //const uint32_t color = convert_color(vec4_from_vec3(hsv_to_rgb({hue, sat, val}), 1.0f));
            set_colors(colors + mol.chain.atom_range[i].beg, mol.chain.atom_range[i].end - mol.chain.atom_range[i].beg, color);
        }
    }
}

void color_atoms_secondary_structure(uint32_t* colors, int64_t count, const md_molecule_t& mol) {
    const uint32_t color_unknown = 0x22222222;
    const uint32_t color_coil    = 0xDDDDDDDD;
    const uint32_t color_helix   = 0xFF22DD22;
    const uint32_t color_sheet   = 0xFFDD2222;

    set_colors(colors, count, color_unknown);
    if (mol.backbone.secondary_structure) {
        for (int64_t i = 0; i < mol.backbone.count; i++) {
            const auto w = convert_color((uint32_t)mol.backbone.secondary_structure[i]);
            const auto color = w.x * convert_color(color_coil) + w.y * convert_color(color_helix) + w.z * convert_color(color_sheet);
            md_residue_idx_t res_idx = mol.backbone.residue_idx[i];
            set_colors(colors + mol.residue.atom_range[res_idx].beg, mol.residue.atom_range[res_idx].end - mol.residue.atom_range[res_idx].beg, convert_color(color));
        }
    }
}
