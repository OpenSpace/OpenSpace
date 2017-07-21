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

#include <modules/globebrowsing/rendering/layer/layer.h>

#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

namespace openspace::globebrowsing {

namespace {
    const char* _loggerCat = "Layer";

    const char* keyName = "Name";
    const char* keyEnabled = "Enabled";
    const char* keyLayerGroupID = "LayerGroupID";
    const char* keySettings = "Settings";
    const char* keyAdjustment = "Adjustment";
    const char* KeyBlendMode = "BlendMode";
} // namespace

Layer::Layer(layergroupid::GroupID id, const ghoul::Dictionary& layerDict)
    : properties::PropertyOwner(layerDict.value<std::string>(keyName))
    , _typeOption(
        "type",
        "Type",
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _blendModeOption(
        "blendMode",
        "Blend Mode",
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _enabled(properties::BoolProperty("enabled", "Enabled", false))
    , _reset("reset", "Reset")
    , _tileProvider(nullptr)
    , _otherTypesProperties{
        properties::Vec3Property (
        "color",
        "Color",
        glm::vec4(1.f, 1.f, 1.f, 1.f),
        glm::vec4(0.f),
        glm::vec4(1.f))
    }
    , _layerGroupId(id)
{
    layergroupid::TypeID typeID = parseTypeIdFromDictionary(layerDict);
    if (typeID == layergroupid::TypeID::Unknown) {
        throw ghoul::RuntimeError("Unknown layer type!");
    }

    initializeBasedOnType(typeID, layerDict);

    bool enabled = false; // defaults to false if unspecified
    layerDict.getValue(keyEnabled, enabled);
    _enabled.setValue(enabled);

    // Initialize settings
    ghoul::Dictionary settingsDict;
    if (layerDict.getValue(keySettings, settingsDict)) {
        _renderSettings.setValuesFromDictionary(settingsDict);
    }

    // Initiallize layer adjustment
    ghoul::Dictionary adjustmentDict;
    if (layerDict.getValue(keyAdjustment, adjustmentDict)) {
        _layerAdjustment.setValuesFromDictionary(adjustmentDict);
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
    std::string blendModeName;
    if (layerDict.getValue(KeyBlendMode, blendModeName)) {
        layergroupid::BlendModeID blendModeID =
            layergroupid::getBlendModeIDFromName(blendModeName);
        _blendModeOption.setValue(static_cast<int>(blendModeID));
    }
    else {
        _blendModeOption.setValue(static_cast<int>(layergroupid::BlendModeID::Normal));
    }

    // On change callbacks definitions
    _enabled.onChange([&](){
        if (_onChangeCallback) {
            _onChangeCallback();
        }
    });

    _reset.onChange([&](){
        if (_tileProvider) {
            _tileProvider->reset();
        }
    });

    _typeOption.onChange([&](){
        removeVisibleProperties();
        _type = static_cast<layergroupid::TypeID>(_typeOption.value());
        ghoul::Dictionary dict;
        initializeBasedOnType(type(), dict);
        addVisibleProperties();
        if (_onChangeCallback) {
            _onChangeCallback();
        }
    });

    _blendModeOption.onChange([&](){
        if (_onChangeCallback) {
            _onChangeCallback();
        }
    });

    _layerAdjustment.onChange([&](){
        if (_onChangeCallback) {
            _onChangeCallback();
        }
    });

    _typeOption.setReadOnly(true);

    // Add the properties
    addProperty(_typeOption);
    addProperty(_blendModeOption);
    addProperty(_enabled);
    addProperty(_reset);

    _otherTypesProperties.color.setViewOption(properties::Property::ViewOptions::Color);

    addVisibleProperties();

    addPropertySubOwner(_renderSettings);
    addPropertySubOwner(_layerAdjustment);
}

ChunkTilePile Layer::getChunkTilePile(const TileIndex& tileIndex, int pileSize) const {
    if (_tileProvider) {
        return _tileProvider->getChunkTilePile(tileIndex, pileSize);
    }
    else {   
        ChunkTilePile chunkTilePile;
        chunkTilePile.resize(pileSize);
        for (int i = 0; i < pileSize; ++i) {
            chunkTilePile[i].tile = Tile::TileUnavailable;
            chunkTilePile[i].uvTransform.uvOffset = { 0, 0 };
            chunkTilePile[i].uvTransform.uvScale = { 1, 1 };
        }
        return chunkTilePile;
    }
}

Tile::Status Layer::getTileStatus(const TileIndex& index) const {
    if (_tileProvider) {
        return _tileProvider->getTileStatus(index);
    }
    else {
        return Tile::Status::Unavailable;
    }
}

layergroupid::TypeID Layer::type() const {
    return _type;
};

layergroupid::BlendModeID Layer::blendMode() const {
    return static_cast<layergroupid::BlendModeID>(_blendModeOption.value());
};


TileDepthTransform Layer::depthTransform() const {
    if (_tileProvider) {
        return _tileProvider->depthTransform();
    }
    else {
        return {1.0f, 0.0f};
    }
}

bool Layer::enabled() const {
    return _enabled.value();
}

tileprovider::TileProvider* Layer::tileProvider() const {
    return _tileProvider.get();
}

const Layer::OtherTypesProperties& Layer::otherTypesProperties() const {
    return _otherTypesProperties;
}

const LayerRenderSettings& Layer::renderSettings() const {
    return _renderSettings;
}

const LayerAdjustment& Layer::layerAdjustment() const {
    return _layerAdjustment;
}

void Layer::onChange(std::function<void(void)> callback) {
    _onChangeCallback = callback;
}

void Layer::update() {
    if (_tileProvider) {
        _tileProvider->update();
    }
}

layergroupid::TypeID Layer::parseTypeIdFromDictionary(
    const ghoul::Dictionary& initDict) const
{
    if (initDict.hasKeyAndValue<std::string>("Type")) {
        const std::string typeString = initDict.value<std::string>("Type");
        return layergroupid::getTypeIDFromTypeString(typeString);
    }
    else {
        return layergroupid::TypeID::DefaultTileLayer;
    }
}

void Layer::initializeBasedOnType(layergroupid::TypeID typeId, ghoul::Dictionary initDict) {
    switch (typeId) {
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
            ghoul::Dictionary tileProviderInitDict = initDict;
            tileProviderInitDict.setValue(keyLayerGroupID, _layerGroupId);
            if (tileProviderInitDict.hasKeyAndValue<std::string>(keyName)) {
                std::string name;
                tileProviderInitDict.getValue(keyName, name);
                LDEBUG("Initializing tile provider for layer: '" + name + "'"); 
            }
            _tileProvider = std::shared_ptr<tileprovider::TileProvider>(
                tileprovider::TileProvider::createFromDictionary(typeId, tileProviderInitDict)
            );
            break;
        }
        case layergroupid::TypeID::SolidColor:
            break;
        default:
            throw ghoul::RuntimeError("Unable to create layer. Unknown type.");
            break;
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
        case layergroupid::TypeID::ByLevelTileLayer:
            if (_tileProvider) {
                addPropertySubOwner(*_tileProvider);
            }
            break;
        case layergroupid::TypeID::SolidColor:
            addProperty(_otherTypesProperties.color);
        default:
            break;
    }
}

void Layer::removeVisibleProperties() {
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
            removeProperty(_otherTypesProperties.color);
            break;
        default:
            break;
    }
}

} // namespace openspace::globebrowsing
