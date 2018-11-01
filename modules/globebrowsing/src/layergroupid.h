/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
static constexpr const char* LAYER_GROUP_IDENTIFIERS[NUM_LAYER_GROUPS] = {
    "HeightLayers",
    "ColorLayers",
    "Overlays",
    "NightLayers",
    "WaterMasks"
};

static constexpr const char* LAYER_GROUP_NAMES[NUM_LAYER_GROUPS] = {
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
    Unknown,
};

static constexpr int NUM_LAYER_TYPES = 8;
static constexpr const char* LAYER_TYPE_NAMES[NUM_LAYER_TYPES] = {
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

static constexpr int NUM_ADJUSTMENT_TYPES = 3;
static constexpr const char* ADJUSTMENT_TYPE_NAMES[NUM_ADJUSTMENT_TYPES] = {
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
static constexpr const char* BLEND_MODE_NAMES[NUM_BLEND_MODES] = {
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
openspace::globebrowsing::layergroupid::TypeID from_string(const std::string& string);

template <>
openspace::globebrowsing::layergroupid::GroupID from_string(const std::string& string);

template <>
openspace::globebrowsing::layergroupid::AdjustmentTypeID from_string(
    const std::string& string);

template <>
openspace::globebrowsing::layergroupid::BlendModeID from_string(
    const std::string& string);

} // ghoul


#endif // __OPENSPACE_MODULE_GLOBEBROWSING___LAYERGROUPID___H__
