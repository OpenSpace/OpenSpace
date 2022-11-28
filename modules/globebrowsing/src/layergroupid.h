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
#include <array>
#include <string>

namespace openspace::globebrowsing::layers {

struct Group {
    enum class ID {
        HeightLayers = 0,
        ColorLayers,
        Overlays,
        NightLayers,
        WaterMasks,
        Unknown
    };

    ID id;
    std::string_view identifier;
    std::string_view name;
};

constexpr std::array<Group, 5> Groups = {
    Group{ Group::ID::HeightLayers, "HeightLayers", "Height Layers" },
    Group{ Group::ID::ColorLayers, "ColorLayers", "Color Layers" },
    Group{ Group::ID::Overlays, "Overlays", "Overlays" },
    Group{ Group::ID::NightLayers, "NightLayers", "Night Layers" },
    Group{ Group::ID::WaterMasks, "WaterMasks", "Water Masks" }
};



struct Layer {
    enum class ID {
        DefaultTileLayer = 0,
        SingleImageTileLayer,
        ImageSequenceTileLayer,
        SizeReferenceTileLayer,
        TemporalTileLayer,
        TileIndexTileLayer,
        ByIndexTileLayer,
        ByLevelTileLayer,
        SolidColor,
        SpoutImageTileLayer,
        VideoTileLayer,
        Unknown
    };

    ID id;
    std::string_view identifier;
};

constexpr std::array<Layer, 11> Layers = {
    Layer{ Layer::ID::DefaultTileLayer, "DefaultTileLayer" },
    Layer{ Layer::ID::SingleImageTileLayer, "SingleImageTileLayer" },
    Layer{ Layer::ID::ImageSequenceTileLayer, "ImageSequenceTileLayer" },
    Layer{ Layer::ID::SizeReferenceTileLayer, "SizeReferenceTileLayer" },
    Layer{ Layer::ID::TemporalTileLayer, "TemporalTileLayer" },
    Layer{ Layer::ID::TileIndexTileLayer, "TileIndexTileLayer" },
    Layer{ Layer::ID::ByIndexTileLayer, "ByIndexTileLayer" },
    Layer{ Layer::ID::ByLevelTileLayer, "ByLevelTileLayer" },
    Layer{ Layer::ID::SolidColor, "SolidColor" },
    Layer{ Layer::ID::SpoutImageTileLayer, "SpoutImageTileLayer" },
    Layer{ Layer::ID::VideoTileLayer, "VideoTileLayer" }
};



struct Adjustment {
    enum class ID {
        None = 0,
        ChromaKey,
        TransferFunction,
    };

    ID id;
    std::string_view identifier;
};

constexpr std::array<Adjustment, 3> Adjustments = {
    Adjustment{ Adjustment::ID::None, "None" },
    Adjustment{ Adjustment::ID::ChromaKey, "ChromaKey" },
    Adjustment{ Adjustment::ID::TransferFunction, "TransferFunction" }
};



struct Blend {
    enum class ID {
        Normal = 0,
        Multiply = 1,
        Add = 2,
        Subtract = 3,
        Color = 4,
    };

    ID id;
    std::string_view identifier;
};

constexpr std::array<Blend, 5> Blends = {
    Blend{ Blend::ID::Normal, "Normal" },
    Blend{ Blend::ID::Multiply, "Multiply" },
    Blend{ Blend::ID::Add, "Add" },
    Blend{ Blend::ID::Subtract, "Subtract" },
    Blend{ Blend::ID::Color, "Color" }
};

} // namespace openspace::globebrowsing::layers

namespace ghoul {

template <>
constexpr openspace::globebrowsing::layers::Layer::ID from_string(std::string_view string)
{
    auto it = std::find_if(
        openspace::globebrowsing::layers::Layers.begin(),
        openspace::globebrowsing::layers::Layers.end(),
        [&string](const openspace::globebrowsing::layers::Layer& li) {
            return li.identifier == string;
        }
    );
    return it != openspace::globebrowsing::layers::Layers.end() ?
        it->id :
        openspace::globebrowsing::layers::Layer::ID::Unknown;
}

template <>
constexpr openspace::globebrowsing::layers::Group::ID from_string(std::string_view string)
{
    auto it = std::find_if(
        openspace::globebrowsing::layers::Groups.begin(),
        openspace::globebrowsing::layers::Groups.end(),
        [&string](const openspace::globebrowsing::layers::Group& gi) {
            return gi.identifier == string;
        }
    );
    return it != openspace::globebrowsing::layers::Groups.end() ?
        it->id :
        openspace::globebrowsing::layers::Group::ID::Unknown;
}

template <>
constexpr openspace::globebrowsing::layers::Adjustment::ID from_string(
                                                                  std::string_view string)
{
    auto it = std::find_if(
        openspace::globebrowsing::layers::Adjustments.begin(),
        openspace::globebrowsing::layers::Adjustments.end(),
        [&string](const openspace::globebrowsing::layers::Adjustment& ai) {
            return ai.identifier == string;
        }
    );
    return it != openspace::globebrowsing::layers::Adjustments.end() ?
        it->id :
        openspace::globebrowsing::layers::Adjustment::ID::None;
}

template <>
constexpr openspace::globebrowsing::layers::Blend::ID from_string(std::string_view string)
{
    auto it = std::find_if(
        openspace::globebrowsing::layers::Blends.begin(),
        openspace::globebrowsing::layers::Blends.end(),
        [&string](const openspace::globebrowsing::layers::Blend& bi) {
            return bi.identifier == string;
        }
    );
    return it != openspace::globebrowsing::layers::Blends.end() ?
        it->id :
        openspace::globebrowsing::layers::Blend::ID::Normal;
}

} // ghoul

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___LAYERGROUPID___H__
