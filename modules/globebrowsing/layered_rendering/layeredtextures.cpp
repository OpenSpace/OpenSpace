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

#include <modules/globebrowsing/layered_rendering/layeredtextures.h>

namespace {
    const std::string _loggerCat = "LayeredTextures";
}

namespace openspace {
namespace globebrowsing {

    const size_t LayeredTextures::NUM_TEXTURE_CATEGORIES;
    const size_t LayeredTextures::MAX_NUM_TEXTURES_PER_CATEGORY;
    const size_t LayeredTextures::NUM_SETTINGS_PER_CATEGORY;
    const size_t LayeredTextures::NUM_TILE_DATA_VARIABLES;
    const size_t LayeredTextures::NUM_BLEND_TEXTURES;
    const size_t LayeredTextures::NUM_LAYER_SETTINGS_VARIABLES;

    const std::string LayeredTextures::TEXTURE_CATEGORY_NAMES[NUM_TEXTURE_CATEGORIES] =
    {
        "ColorTextures",
        "GrayScaleTextures",
        "GrayScaleOverlays",
        "NightTextures",
        "WaterMasks",
        "Overlays",
        "HeightMaps",
    };

    const std::string LayeredTextures::glslKeyPrefixes[NUM_SETTINGS_PER_CATEGORY] =
    {
        "lastLayerIndex",
        "use",
        "blend",
    };

    const std::string LayeredTextures::glslTileDataNames[
        NUM_TILE_DATA_VARIABLES] =
    {
        "textureSampler",
        "depthTransform.depthScale",
        "depthTransform.depthOffset",
        "uvTransform.uvOffset",
        "uvTransform.uvScale"
    };

    const std::string LayeredTextures::blendLayerSuffixes[
        NUM_BLEND_TEXTURES] =
    {
        "",
        "Parent1",
        "Parent2",
    };
    
    const std::string LayeredTextures::layerSettingsIds[
        NUM_LAYER_SETTINGS_VARIABLES] =
    {
        "opacity",
        "gamma",
        "multiplier",
    };

    PerLayerSettings::PerLayerSettings()
    {
        // Here, all the per layer settings are specified and added
        _array[LayeredTextures::LayerSettingsIds::opacity] = std::make_shared<PerLayerFloatSetting>(
            LayeredTextures::layerSettingsIds[LayeredTextures::LayerSettingsIds::opacity],
            LayeredTextures::layerSettingsIds[LayeredTextures::LayerSettingsIds::opacity],
            1,
            0,
            1);
        _array[LayeredTextures::LayerSettingsIds::gamma] = std::make_shared<PerLayerFloatSetting>(
            LayeredTextures::layerSettingsIds[LayeredTextures::LayerSettingsIds::gamma],
            LayeredTextures::layerSettingsIds[LayeredTextures::LayerSettingsIds::gamma],
            1,
            0,
            5);
        _array[LayeredTextures::LayerSettingsIds::multiplier] = std::make_shared<PerLayerFloatSetting>(
            LayeredTextures::layerSettingsIds[LayeredTextures::LayerSettingsIds::multiplier],
            LayeredTextures::layerSettingsIds[LayeredTextures::LayerSettingsIds::multiplier],
            1,
            0,
            20);

        // Make sure all settings have been spacified and added
        for (int i = 0; i < LayeredTextures::NUM_LAYER_SETTINGS_VARIABLES; ++i) {
            ghoul_assert(_array[i], "The setting " +
                LayeredTextures::layerSettingsIds[i] + "is not specified!");
        }
    }

    PerLayerSettings::~PerLayerSettings() {};

    const std::array<std::shared_ptr<PerLayerSetting>,
        LayeredTextures::NUM_LAYER_SETTINGS_VARIABLES>& PerLayerSettings::array() const {
            return _array;
        }

}  // namespace globebrowsing
}  // namespace openspace
