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

#ifndef __LAYERED_TEXTURES_H__
#define __LAYERED_TEXTURES_H__

#include <memory>
#include <vector>
#include <string>

#include <ghoul/opengl/programobject.h>

#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalarproperty.h>

#include <modules/globebrowsing/layered_rendering/perlayersetting.h>

namespace openspace {
    
    class LayeredTextures {

    public:

        static const size_t NUM_TEXTURE_CATEGORIES = 7;
        static const size_t MAX_NUM_TEXTURES_PER_CATEGORY = 5;

        static const size_t NUM_SETTINGS_PER_CATEGORY = 3;

        static const size_t NUM_TILE_DATA_VARIABLES = 5;
        static const size_t NUM_BLEND_TEXTURES = 3;
        static const size_t NUM_LAYER_SETTINGS_VARIABLES = 3;
        
        enum GlslKeyPrefixes
        {
            lastLayerIndex,
            use,
            blend,
        };

        enum TextureCategory {
            ColorTextures,
            GrayScaleTextures,
            GrayScaleOverlays,
            NightTextures,
            WaterMasks,
            Overlays,
            HeightMaps,
        };

        /**
        * Each texture can have these uniform variables associated with it in the shader
        * code.
        *
        * <code>textureSampler</code> is the actual texture that can be sampled in the
        * shader program. The associated GLSL type is <code>sampler2D</code>.
        * <code>depthTransform_depthScale</code> specifies the scale part of the depth
        * transform. Useful for height maps. The associated GLSL type is
        * <code>float</code>.
        * <code>depthTransform_depthOffset</code> specifies the offset part of the depth
        * transform. Useful for height maps. The associated GLSL type is
        * <code>float</code>.
        * <code>uvTransform_uvOffset</code> specifies an offset that can be used when
        * sampling from the texture. The associated GLSL type is <code>vec2</code>.
        * <code>uvTransform_uvScale</code> specifies a scale that can be used when
        * sampling from the texture. The associated GLSL type is <code>vec2</code>.
        *
        * The corresponding struct in GLSL code for storing these data is a
        * <code>Tile</code>. The names of the uniforms are the ones specified in 
        * <code>glslTileDataNames</code>.
        */
        enum GlslTileDataId {
            textureSampler,
            depthTransform_depthScale,
            depthTransform_depthOffset,
            uvTransform_uvOffset,
            uvTransform_uvScale,
        };

        /**
        * These suffixes are used when naming <code>Tile</code>s in GLSL code. The names
        * of the <code>Tile</code>s is one of
        * <code>LayeredTextures::TEXTURE_CATEGORY_NAMES</code> followed by the suffixes
        * defined in <code>blendLayerSuffixes</code>.
        */
        enum BlendLayerSuffixes {
            none,
            Parent1,
            Parent2,
        };
        
        enum LayerSettingsIds {
            opacity,
            gamma,
            multiplier,
        };

        static const std::string glslKeyPrefixes[NUM_SETTINGS_PER_CATEGORY];
        static const std::string TEXTURE_CATEGORY_NAMES[NUM_TEXTURE_CATEGORIES];
        static const std::string glslTileDataNames[NUM_TILE_DATA_VARIABLES];
        static const std::string blendLayerSuffixes[NUM_BLEND_TEXTURES];
        static const std::string layerSettingsIds[NUM_LAYER_SETTINGS_VARIABLES];
    };

    class PerLayerSettings {
    public:
        PerLayerSettings()
        {
            // Here, all the per layer settings are specified and added
            array[LayeredTextures::LayerSettingsIds::opacity] = std::make_shared<PerLayerFloatSetting>(
                LayeredTextures::layerSettingsIds[LayeredTextures::LayerSettingsIds::opacity],
                LayeredTextures::layerSettingsIds[LayeredTextures::LayerSettingsIds::opacity],
                1,
                0,
                1);
            array[LayeredTextures::LayerSettingsIds::gamma] = std::make_shared<PerLayerFloatSetting>(
                LayeredTextures::layerSettingsIds[LayeredTextures::LayerSettingsIds::gamma],
                LayeredTextures::layerSettingsIds[LayeredTextures::LayerSettingsIds::gamma],
                1,
                0,
                5);
            array[LayeredTextures::LayerSettingsIds::multiplier] = std::make_shared<PerLayerFloatSetting>(
                LayeredTextures::layerSettingsIds[LayeredTextures::LayerSettingsIds::multiplier],
                LayeredTextures::layerSettingsIds[LayeredTextures::LayerSettingsIds::multiplier],
                1,
                0,
                20);

            // Make sure all settings have been spacified and added
            for (int i = 0; i < LayeredTextures::NUM_LAYER_SETTINGS_VARIABLES; ++i) {
                ghoul_assert(array[i], "The setting " +
                    LayeredTextures::layerSettingsIds[i] + "is not specified!");
            }
        }

        std::array<std::shared_ptr<PerLayerSetting>,
            LayeredTextures::NUM_LAYER_SETTINGS_VARIABLES> array;
    };

} // namespace openspace
#endif  // __LAYERED_TEXTURES_H__
