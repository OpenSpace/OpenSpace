/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <string>

namespace openspace::globebrowsing::layergroupid {

static const int NUM_LAYER_GROUPS = 5;
static const std::string LAYER_GROUP_NAMES[NUM_LAYER_GROUPS] = {
    "HeightLayers",
    "ColorLayers",
    "Overlays",
    "NightLayers",
    "WaterMasks"
};

enum GroupID {
    HeightLayers,
    ColorLayers,
    Overlays,
    NightLayers,
    WaterMasks,
    Unknown,
};

static const int NUM_LAYER_TYPES = 8;
static const std::string LAYER_TYPE_NAMES[NUM_LAYER_TYPES] = {
    "DefaultTileLayer",
    "SingleImageTileLayer",
    "SizeReferenceTileLayer",
    "TemporalTileLayer",
    "TileIndexTileLayer",
    "ByIndexTileLayer",
    "ByLevelTileLayer",
    "SolidColor",
};

/**
 This enumeration is specified explicitly since it is used in the shader as well.
 */
enum class TypeID {
    Unknown = -1,
    DefaultTileLayer = 0,
    SingleImageTileLayer = 1,
    SizeReferenceTileLayer = 2,
    TemporalTileLayer = 3,
    TileIndexTileLayer = 4,
    ByIndexTileLayer = 5,
    ByLevelTileLayer = 6,
    SolidColor = 7,
};

static const int NUM_ADJUSTMENT_TYPES = 3;
static const std::string ADJUSTMENT_TYPE_NAMES[NUM_ADJUSTMENT_TYPES] = {
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

static const int NUM_BLEND_MODES = 5;
static const std::string BLEND_MODE_NAMES[NUM_BLEND_MODES] = {
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

static TypeID getTypeIDFromTypeString(std::string typeString) {
    for (int i = 0; i < NUM_LAYER_TYPES; ++i) {
        if (typeString == LAYER_TYPE_NAMES[i]) {
            return static_cast<TypeID>(i);
        }
    }
    return TypeID::Unknown;
}

static layergroupid::GroupID getGroupIDFromName(std::string layerGroupName) {
    for (int i = 0; i < layergroupid::NUM_LAYER_GROUPS; ++i) {
        if (layerGroupName == layergroupid::LAYER_GROUP_NAMES[i]) {
            return static_cast<layergroupid::GroupID>(i);
        }
    }
    return GroupID::Unknown;
}

static layergroupid::AdjustmentTypeID getAdjustmentTypeIDFromName(
    std::string adjustmentTypeName)
{
    for (int i = 0; i < layergroupid::NUM_ADJUSTMENT_TYPES; ++i) {
        if (adjustmentTypeName == layergroupid::ADJUSTMENT_TYPE_NAMES[i]) {
            return static_cast<layergroupid::AdjustmentTypeID>(i);
        }
    }
    return AdjustmentTypeID::None;
}

static layergroupid::BlendModeID getBlendModeIDFromName(
    std::string blendModeName)
{
    for (int i = 0; i < layergroupid::NUM_BLEND_MODES; ++i) {
        if (blendModeName == layergroupid::BLEND_MODE_NAMES[i]) {
            return static_cast<layergroupid::BlendModeID>(i);
        }
    }
    return BlendModeID::Normal;
}

} // namespace openspace::globebrowsing::layergroupid

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___LAYERGROUPID___H__
