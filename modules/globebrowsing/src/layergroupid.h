/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___LAYERGROUPID___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___LAYERGROUPID___H__

#include <ghoul/misc/stringconversion.h>
#include <string>

namespace openspace::globebrowsing::layergroupid {

static constexpr int NUM_LAYER_GROUPS = 5;
static constexpr std::string_view LAYER_GROUP_IDENTIFIERS[NUM_LAYER_GROUPS] = {
    "HeightLayers",
    "ColorLayers",
    "Overlays",
    "NightLayers",
    "WaterMasks"
};

static constexpr std::string_view LAYER_GROUP_NAMES[NUM_LAYER_GROUPS] = {
    "Height Layers",
    "Color Layers",
    "Overlays",
    "Night Layers",
    "Water Masks"
};

enum GroupID {
    HeightLayers,
    ColorLayers,
    Overlays,
    NightLayers,
    WaterMasks,
    Unknown
};

static constexpr int NUM_LAYER_TYPES = 10;
static constexpr std::string_view LAYER_TYPE_NAMES[NUM_LAYER_TYPES] = {
    "DefaultTileLayer",
    "SingleImageTileLayer",
    "ImageSequenceTileLayer",
    "SizeReferenceTileLayer",
    "TemporalTileLayer",
    "TileIndexTileLayer",
    "ByIndexTileLayer",
    "ByLevelTileLayer",
    "SolidColor",
    "SpoutImageTileLayer"
};

/**
 This enumeration is specified explicitly since it is used in the shader as well.
 */
enum class TypeID {
    Unknown = -1,
    DefaultTileLayer = 0,
    SingleImageTileLayer = 1,
    ImageSequenceTileLayer = 2,
    SizeReferenceTileLayer = 3,
    TemporalTileLayer = 4,
    TileIndexTileLayer = 5,
    ByIndexTileLayer = 6,
    ByLevelTileLayer = 7,
    SolidColor = 8,
    SpoutImageTileLayer = 9
};

static constexpr int NUM_ADJUSTMENT_TYPES = 3;
static constexpr std::string_view ADJUSTMENT_TYPE_NAMES[NUM_ADJUSTMENT_TYPES] = {
    "None",
    "ChromaKey",
    "TransferFunction",
};

/**
 This enumeration is specified explicitly since it is used in the shader as well.
 */
enum class AdjustmentTypeID {
    None = 0,
    ChromaKey = 1,
    TransferFunction = 2,
};

static constexpr int NUM_BLEND_MODES = 5;
static constexpr std::string_view BLEND_MODE_NAMES[NUM_BLEND_MODES] = {
    "Normal",
    "Multiply",
    "Add",
    "Subtract",
    "Color",
};

/**
 This enumeration is specified explicitly since it is used in the shader as well.
 */
enum class BlendModeID {
    Normal = 0,
    Multiply = 1,
    Add = 2,
    Subtract = 3,
    Color = 4,
};

} // namespace openspace::globebrowsing::layergroupid

namespace ghoul {

template <>
constexpr openspace::globebrowsing::layergroupid::TypeID from_string(
                                                                  std::string_view string)
{
    for (int i = 0; i < openspace::globebrowsing::layergroupid::NUM_LAYER_TYPES; ++i) {
        if (string == openspace::globebrowsing::layergroupid::LAYER_TYPE_NAMES[i]) {
            return static_cast<openspace::globebrowsing::layergroupid::TypeID>(i);
        }
    }
    return openspace::globebrowsing::layergroupid::TypeID::Unknown;
}

template <>
constexpr openspace::globebrowsing::layergroupid::GroupID from_string(
                                                                  std::string_view string)
{
    for (int i = 0; i < openspace::globebrowsing::layergroupid::NUM_LAYER_GROUPS; ++i) {
        if (string == openspace::globebrowsing::layergroupid::LAYER_GROUP_IDENTIFIERS[i])
        {
            return static_cast<openspace::globebrowsing::layergroupid::GroupID>(i);
        }
    }
    return openspace::globebrowsing::layergroupid::GroupID::Unknown;
}

template <>
constexpr openspace::globebrowsing::layergroupid::AdjustmentTypeID from_string(
                                                                  std::string_view string)
{
    for (int i = 0; i < openspace::globebrowsing::layergroupid::NUM_ADJUSTMENT_TYPES; ++i)
    {
        if (string == openspace::globebrowsing::layergroupid::ADJUSTMENT_TYPE_NAMES[i]) {
            return
                static_cast<openspace::globebrowsing::layergroupid::AdjustmentTypeID>(i);
        }
    }
    return openspace::globebrowsing::layergroupid::AdjustmentTypeID::None;
}

template <>
constexpr openspace::globebrowsing::layergroupid::BlendModeID from_string(
                                                                  std::string_view string)
{
    for (int i = 0; i < openspace::globebrowsing::layergroupid::NUM_BLEND_MODES; ++i) {
        if (string == openspace::globebrowsing::layergroupid::BLEND_MODE_NAMES[i]) {
            return static_cast<openspace::globebrowsing::layergroupid::BlendModeID>(i);
        }
    }
    return openspace::globebrowsing::layergroupid::BlendModeID::Normal;
}

} // ghoul


#endif // __OPENSPACE_MODULE_GLOBEBROWSING___LAYERGROUPID___H__
