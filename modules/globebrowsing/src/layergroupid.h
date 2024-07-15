/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
    Group {
        .id = Group::ID::HeightLayers,
        .identifier = "HeightLayers",
        .name = "Height Layers"
    },
    Group {
        .id = Group::ID::ColorLayers,
        .identifier = "ColorLayers",
        .name = "Color Layers"
    },
    Group {
        .id = Group::ID::Overlays,
        .identifier = "Overlays",
        .name = "Overlays"
    },
    Group {
        .id = Group::ID::NightLayers,
        .identifier = "NightLayers",
        .name = "Night Layers"
    },
    Group {
        .id = Group::ID::WaterMasks,
        .identifier = "WaterMasks",
        .name = "Water Masks"
    }
};



struct Layer {
    enum class ID {
        DefaultTileProvider = 0,
        SingleImageProvider,
        ImageSequenceTileProvider,
        SizeReferenceTileProvider,
        TemporalTileProvider,
        TileIndexTileProvider,
        TileProviderByIndex,
        TileProviderByLevel,
        SolidColor,
        SpoutImageProvider,
        VideoTileProvider
    };

    ID id;
    std::string_view identifier;
};

constexpr std::array<Layer, 11> Layers = {
    Layer {
        .id = Layer::ID::DefaultTileProvider,
        .identifier = "DefaultTileProvider"
    },
    Layer {
        .id = Layer::ID::SingleImageProvider,
        .identifier = "SingleImageProvider"
    },
    Layer {
        .id = Layer::ID::ImageSequenceTileProvider,
        .identifier = "ImageSequenceTileProvider"
    },
    Layer {
        .id = Layer::ID::SizeReferenceTileProvider,
        .identifier = "SizeReferenceTileProvider"
    },
    Layer {
        .id = Layer::ID::TemporalTileProvider,
        .identifier = "TemporalTileProvider"
    },
    Layer {
        .id = Layer::ID::TileIndexTileProvider,
        .identifier = "TileIndexTileProvider"
    },
    Layer {
        .id = Layer::ID::TileProviderByIndex,
        .identifier = "TileProviderByIndex"
    },
    Layer {
        .id = Layer::ID::TileProviderByLevel,
        .identifier = "TileProviderByLevel"
    },
    Layer {
        .id = Layer::ID::SolidColor,
        .identifier = "SolidColor"
    },
    Layer {
        .id = Layer::ID::SpoutImageProvider,
        .identifier = "SpoutImageProvider"
    },
    Layer {
        .id = Layer::ID::VideoTileProvider,
        .identifier = "VideoTileProvider"
    }
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
    Adjustment {
        .id = Adjustment::ID::None,
        .identifier = "None"
    },
    Adjustment {
        .id = Adjustment::ID::ChromaKey,
        .identifier = "ChromaKey"
    },
    Adjustment {
        .id = Adjustment::ID::TransferFunction,
        .identifier = "TransferFunction"
    }
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
    Blend { .id = Blend::ID::Normal, .identifier = "Normal" },
    Blend { .id = Blend::ID::Multiply, .identifier = "Multiply" },
    Blend { .id = Blend::ID::Add, .identifier = "Add" },
    Blend { .id = Blend::ID::Subtract, .identifier = "Subtract" },
    Blend { .id = Blend::ID::Color, .identifier = "Color" }
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

    if (it != openspace::globebrowsing::layers::Layers.end()) {
        return it->id;
    }
    else {
        throw ghoul::RuntimeError(std::format(
            "Could not find Layer of type '{}'", string
        ));
    }
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

    if (it != openspace::globebrowsing::layers::Groups.end()) {
        return it->id;
    }
    else {
        throw ghoul::RuntimeError(std::format(
            "Could not find Group of type '{}'", string
        ));
    }
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
