/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/globebrowsing/src/layer.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <modules/globebrowsing/src/layergroup.h>
#include <modules/globebrowsing/src/layermanager.h>
#include <modules/globebrowsing/src/tileindex.h>
#include <modules/globebrowsing/src/tiletextureinitdata.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>

namespace openspace::globebrowsing {

namespace {
    constexpr const char* _loggerCat = "Layer";

    constexpr const char* KeyIdentifier = "Identifier";
    constexpr const char* KeyName = "Name";
    constexpr const char* KeyDesc = "Description";
    constexpr const char* KeyLayerGroupID = "LayerGroupID";
    constexpr const char* KeyAdjustment = "Adjustment";

    constexpr openspace::properties::Property::PropertyInfo TypeInfo = {
        "Type",
        "Type",
        "The type of this Layer. This value is a read-only property and thus cannot be "
        "changed."
    };

    constexpr openspace::properties::Property::PropertyInfo BlendModeInfo = {
        "BlendMode",
        "Blend Mode",
        "This value specifies the blend mode that is applied to this layer. The blend "
        "mode determines how this layer is added to the underlying layers beneath."
    };

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "If this value is enabled, the layer will be used for the final composition of "
        "the planet. If this value is disabled, the layer will be ignored in the "
        "composition."
    };

    constexpr openspace::properties::Property::PropertyInfo ResetInfo = {
        "Reset",
        "Reset",
        "If this value is triggered, this layer will be reset. This will delete the "
        "local cache for this layer and will trigger a fresh load of all tiles."
    };

    constexpr openspace::properties::Property::PropertyInfo RemoveInfo = {
        "Remove",
        "Remove",
        "If this value is triggered, a script will be executed that will remove this "
        "layer before the next frame."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "If the 'Type' of this layer is a solid color, this value determines what this "
        "solid color is."
    };

    constexpr openspace::properties::Property::PropertyInfo GuiDescriptionInfo = {
        "GuiDescription",
        "Gui Description",
        "This is the description for the scene graph node to be shown in the gui "
        "example: Earth is a special place",
        openspace::properties::Property::Visibility::Hidden
    };

    struct [[codegen::Dictionary(Layer), codegen::noexhaustive()]] Parameters {
        // The unique identifier for this layer. May not contain '.' or spaces
        std::string identifier;

        // A human-readable name for the user interface. If this is omitted, the
        // identifier is used instead
        std::optional<std::string> name;

        // A human-readable description of the layer to be used in informational texts
        // presented to the user
        std::optional<std::string> description;

        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color;

        // Specifies the type of layer that is to be added. If this value is not
        // specified, the layer is a DefaultTileLayer
        std::optional<std::string> type [[codegen::inlist("DefaultTileLayer",
            "SingleImageTileLayer", "SizeReferenceTileLayer", "TemporalTileLayer",
            "TileIndexTileLayer", "ByIndexTileLayer", "ByLevelTileLayer", "SolidColor")]];

        // Determine whether the layer is enabled or not. If this value is not specified,
        // the layer is disabled
        std::optional<bool> enabled;

        // Determines whether the downloaded tiles should have a padding added to the
        // borders
        std::optional<bool> padTiles;

        struct Settings {
            // The opacity value of the layer
            std::optional<float> opacity [[codegen::inrange(0.0, 1.0)]];

            // The gamma value that is applied to each pixel of the layer
            std::optional<float> gamma;

            // The multiplicative factor that is applied to each pixel of the layer
            std::optional<float> multiplier;

            // An additive offset that is applied to each pixel of the layer
            std::optional<float> offset;
        };
        // Specifies the render settings that should be applied to this layer
        std::optional<Settings> settings;

        struct LayerAdjustment {
            enum class Type {
                None,
                ChromaKey,
                TransferFunction
            };

            // Specifies the type of the adjustment that is applied
            std::optional<Type> type;

            // Specifies the chroma key used when selecting 'ChromaKey' for the 'Type'
            std::optional<glm::dvec3> chromaKeyColor;

            // Specifies the tolerance to match the color to the chroma key when the
            // 'ChromaKey' type is selected for the 'Type'
            std::optional<double> chromaKeyTolerance;
        };
        // Parameters that set individual adjustment parameters for this layer
        std::optional<LayerAdjustment> adjustment;

        enum class BlendMode {
            Normal,
            Multiply,
            Add,
            Subtract,
            Color
        };
        // Sets the blend mode of this layer to determine how it interacts with other
        // layers on top of this
        std::optional<BlendMode> blendMode;

        // If the primary layer creation fails, this layer is used as a fallback
        std::optional<std::monostate>
            fallback [[codegen::reference("globebrowsing_layer")]];
    };
#include "layer_codegen.cpp"
} // namespace

documentation::Documentation Layer::Documentation() {
    documentation::Documentation doc = codegen::doc<Parameters>();
    doc.id = "globebrowsing_layer";
    return doc;
}

Layer::Layer(layergroupid::GroupID id, const ghoul::Dictionary& layerDict,
             LayerGroup& parent)
    : properties::PropertyOwner({
        layerDict.value<std::string>(KeyIdentifier),
        layerDict.hasKey(KeyName) ? layerDict.value<std::string>(KeyName) : "",
        layerDict.hasKey(KeyDesc) ? layerDict.value<std::string>(KeyDesc) : ""
    })
    , _parent(parent)
    , _typeOption(TypeInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _blendModeOption(BlendModeInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _enabled(EnabledInfo, false)
    , _reset(ResetInfo)
    , _remove(RemoveInfo)
    , _guiDescription(GuiDescriptionInfo)
    , _solidColor(ColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _layerGroupId(id)

{
    const Parameters p = codegen::bake<Parameters>(layerDict);

    layergroupid::TypeID typeID;
    if (p.type.has_value()) {
        typeID = ghoul::from_string<layergroupid::TypeID>(*p.type);
        if (typeID == layergroupid::TypeID::Unknown) {
            throw ghoul::RuntimeError("Unknown layer type!");
        }
    }
    else {
        typeID = layergroupid::TypeID::DefaultTileLayer;
    }

    initializeBasedOnType(typeID, layerDict);

    _enabled = p.enabled.value_or(_enabled);

    if (p.description.has_value()) {
        _guiDescription = description();
        addProperty(_guiDescription);
    }

    const bool padTiles = p.padTiles.value_or(true);

    TileTextureInitData initData = tileTextureInitData(_layerGroupId, padTiles);
    _padTilePixelStartOffset = initData.tilePixelStartOffset;
    _padTilePixelSizeDifference = initData.tilePixelSizeDifference;

    if (p.settings.has_value()) {
        _renderSettings.opacity = p.settings->opacity.value_or(_renderSettings.opacity);
        _renderSettings.gamma = p.settings->gamma.value_or(_renderSettings.gamma);
        _renderSettings.multiplier =
            p.settings->multiplier.value_or(_renderSettings.multiplier);
        _renderSettings.offset = p.settings->offset.value_or(_renderSettings.offset);
    }
    if (layerDict.hasValue<ghoul::Dictionary>(KeyAdjustment)) {
        _layerAdjustment.setValuesFromDictionary(
            layerDict.value<ghoul::Dictionary>(KeyAdjustment)
        );
    }

    // Add options to option properties
    for (int i = 0; i < layergroupid::NUM_LAYER_TYPES; ++i) {
        _typeOption.addOption(i, layergroupid::LAYER_TYPE_NAMES[i]);
    }
    _typeOption.setValue(static_cast<int>(typeID));
    _type = static_cast<layergroupid::TypeID>(_typeOption.value());

    for (int i = 0; i < layergroupid::NUM_BLEND_MODES; ++i) {
        _blendModeOption.addOption(i, layergroupid::BLEND_MODE_NAMES[i]);
    }

    // Initialize blend mode
    if (p.blendMode.has_value()) {
        switch (*p.blendMode) {
            case Parameters::BlendMode::Normal:
                _blendModeOption = static_cast<int>(layergroupid::BlendModeID::Normal);
                break;
            case Parameters::BlendMode::Multiply:
                _blendModeOption = static_cast<int>(layergroupid::BlendModeID::Multiply);
                break;
            case Parameters::BlendMode::Add:
                _blendModeOption = static_cast<int>(layergroupid::BlendModeID::Add);
                break;
            case Parameters::BlendMode::Subtract:
                _blendModeOption = static_cast<int>(layergroupid::BlendModeID::Subtract);
                break;
            case Parameters::BlendMode::Color:
                _blendModeOption = static_cast<int>(layergroupid::BlendModeID::Color);
                break;
        }
    }
    else {
        _blendModeOption = static_cast<int>(layergroupid::BlendModeID::Normal);
    }

    // On change callbacks definitions
    _enabled.onChange([&]() {
        if (_onChangeCallback) {
            _onChangeCallback(this);
        }
    });

    _reset.onChange([&]() {
        if (_tileProvider) {
            tileprovider::reset(*_tileProvider);
        }
    });

    _remove.onChange([&]() {
        if (_tileProvider) {
            tileprovider::reset(*_tileProvider);
            _parent.deleteLayer(identifier());
        }
    });

    _typeOption.onChange([&]() {
        switch (type()) {
            // Intentional fall through. Same for all tile layers
            case layergroupid::TypeID::DefaultTileLayer:
            case layergroupid::TypeID::SingleImageTileLayer:
            case layergroupid::TypeID::SizeReferenceTileLayer:
            case layergroupid::TypeID::TemporalTileLayer:
            case layergroupid::TypeID::TileIndexTileLayer:
            case layergroupid::TypeID::ByIndexTileLayer:
            case layergroupid::TypeID::ByLevelTileLayer:
                if (_tileProvider) {
                    removePropertySubOwner(*_tileProvider);
                }
                break;
            case layergroupid::TypeID::SolidColor:
                removeProperty(_solidColor);
                break;
            default:
                break;
        }

        _type = static_cast<layergroupid::TypeID>(_typeOption.value());
        initializeBasedOnType(type(), {});
        addVisibleProperties();
        if (_onChangeCallback) {
            _onChangeCallback(this);
        }
    });

    _blendModeOption.onChange([&]() {
        if (_onChangeCallback) {
            _onChangeCallback(this);
        }
    });

    _layerAdjustment.onChange([&]() {
        if (_onChangeCallback) {
            _onChangeCallback(this);
        }
    });

    _typeOption.setReadOnly(true);

    // Add the properties
    addProperty(_typeOption);
    addProperty(_blendModeOption);
    addProperty(_enabled);
    addProperty(_reset);
    addProperty(_remove);

    _solidColor.setViewOption(properties::Property::ViewOptions::Color);

    addVisibleProperties();

    addPropertySubOwner(_renderSettings);
    addPropertySubOwner(_layerAdjustment);
}

void Layer::initialize() {
    ZoneScoped

    if (_tileProvider) {
        tileprovider::initialize(*_tileProvider);
    }
}

void Layer::deinitialize() {
    if (_tileProvider) {
        tileprovider::deinitialize(*_tileProvider);
    }
}

ChunkTilePile Layer::chunkTilePile(const TileIndex& tileIndex, int pileSize) const {
    ZoneScoped

    if (_tileProvider) {
        return tileprovider::chunkTilePile(*_tileProvider, tileIndex, pileSize);
    }
    else {
        ChunkTilePile chunkTilePile;
        std::fill(chunkTilePile.begin(), chunkTilePile.end(), std::nullopt);
        for (int i = 0; i < pileSize; ++i) {
            ChunkTile tile;
            tile.uvTransform = TileUvTransform{ { 0, 0 }, { 1, 1 } };
            chunkTilePile[i] = tile;
        }
        return chunkTilePile;
    }
}

Tile::Status Layer::tileStatus(const TileIndex& index) const {
    return _tileProvider ?
        tileprovider::tileStatus(*_tileProvider, index) :
        Tile::Status::Unavailable;
}

layergroupid::TypeID Layer::type() const {
    return _type;
}

layergroupid::BlendModeID Layer::blendMode() const {
    return static_cast<layergroupid::BlendModeID>(_blendModeOption.value());
}

TileDepthTransform Layer::depthTransform() const {
    return _tileProvider ?
        tileprovider::depthTransform(*_tileProvider) :
        TileDepthTransform{ 1.f, 0.f };
}

void Layer::setEnabled(bool enabled) {
    _enabled = enabled;
}

bool Layer::enabled() const {
    return _enabled;
}

tileprovider::TileProvider* Layer::tileProvider() const {
    return _tileProvider.get();
}

glm::vec3 Layer::solidColor() const {
    return _solidColor;
}

const LayerRenderSettings& Layer::renderSettings() const {
    return _renderSettings;
}

const LayerAdjustment& Layer::layerAdjustment() const {
    return _layerAdjustment;
}

void Layer::onChange(std::function<void(Layer*)> callback) {
    _onChangeCallback = std::move(callback);
}

int Layer::update() {
    ZoneScoped

    if (_tileProvider) {
        return tileprovider::update(*_tileProvider);
    }
    else {
        return 0;
    }
}

glm::ivec2 Layer::tilePixelStartOffset() const {
    return _padTilePixelStartOffset;
}

glm::ivec2 Layer::tilePixelSizeDifference() const {
    return _padTilePixelSizeDifference;
}

glm::vec2 Layer::tileUvToTextureSamplePosition(const TileUvTransform& uvTransform,
                                               const glm::vec2& tileUV,
                                               const glm::uvec2& resolution)
{
    glm::vec2 uv = uvTransform.uvOffset + uvTransform.uvScale * tileUV;

    const glm::vec2 sourceSize = glm::vec2(resolution) +
                                 glm::vec2(_padTilePixelSizeDifference);
    const glm::vec2 currentSize = glm::vec2(resolution);
    const glm::vec2 sourceToCurrentSize = currentSize / sourceSize;
    return sourceToCurrentSize * (uv - glm::vec2(_padTilePixelStartOffset) / sourceSize);
}

void Layer::initializeBasedOnType(layergroupid::TypeID id, ghoul::Dictionary initDict) {
    switch (id) {
        // Intentional fall through. Same for all tile layers
        case layergroupid::TypeID::DefaultTileLayer:
        case layergroupid::TypeID::SingleImageTileLayer:
        case layergroupid::TypeID::SizeReferenceTileLayer:
        case layergroupid::TypeID::TemporalTileLayer:
        case layergroupid::TypeID::TileIndexTileLayer:
        case layergroupid::TypeID::ByIndexTileLayer:
        case layergroupid::TypeID::ByLevelTileLayer: {
            // We add the id to the dictionary since it needs to be known by
            // the tile provider
            initDict.setValue(KeyLayerGroupID, static_cast<int>(_layerGroupId));
            if (initDict.hasKey(KeyName) && initDict.hasValue<std::string>(KeyName)) {
                std::string name = initDict.value<std::string>(KeyName);
                LDEBUG("Initializing tile provider for layer: '" + name + "'");
            }
            _tileProvider = tileprovider::createFromDictionary(id, std::move(initDict));
            break;
        }
        case layergroupid::TypeID::SolidColor: {
            if (initDict.hasValue<glm::dvec3>(ColorInfo.identifier)) {
                _solidColor = initDict.value<glm::dvec3>(ColorInfo.identifier);
            }
            break;
        }
        default:
            throw ghoul::MissingCaseException();
    }
}

void Layer::addVisibleProperties() {
    switch (type()) {
        // Intentional fall through. Same for all tile layers
        case layergroupid::TypeID::DefaultTileLayer:
        case layergroupid::TypeID::SingleImageTileLayer:
        case layergroupid::TypeID::SizeReferenceTileLayer:
        case layergroupid::TypeID::TemporalTileLayer:
        case layergroupid::TypeID::TileIndexTileLayer:
        case layergroupid::TypeID::ByIndexTileLayer:
        case layergroupid::TypeID::ByLevelTileLayer: {
            if (_tileProvider) {
                addPropertySubOwner(*_tileProvider);
            }
            break;
        }
        case layergroupid::TypeID::SolidColor: {
            addProperty(_solidColor);
            break;
        }
        default:
            break;
    }
}

} // namespace openspace::globebrowsing
