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

#include <modules/globebrowsing/src/tileprovider/tileproviderbydate.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timemanager.h>
#include <functional>

namespace {
    struct [[codegen::Dictionary(TileProviderByDate)]] Parameters {
        // Specifies the list of tile providers and for which times they are used for. The
        // tile provider with the earliest time will be used for all dates prior to that
        // date and the provider with the latest time will be used for all dates
        // afterwards. In between, a provider is used from the specified time until the
        // time of the next provider
        std::map<std::string, ghoul::Dictionary> providers;
    };
#include "tileproviderbydate_codegen.cpp"
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation TileProviderByDate::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_tileproviderbydate");
}

TileProviderByDate::TileProviderByDate(const ghoul::Dictionary& dictionary) {
    ZoneScoped;

    Parameters p = codegen::bake<Parameters>(dictionary);

    // For now we need to inject the LayerGroupID this way. We don't want it to be part of
    // the parameters struct as that would mean it would be visible to the end user, which
    // we don't want since this value just comes from whoever creates it, not the user
    ghoul_assert(dictionary.hasValue<int>("LayerGroupID"), "No Layer Group ID provided");
    const layers::Group::ID group = static_cast<layers::Group::ID>(
        dictionary.value<int>("LayerGroupID")
    );

    for (std::pair<const std::string, ghoul::Dictionary>& prov : p.providers) {
        prov.second.setValue("LayerGroupID", static_cast<int>(group));

        // Pass down the caching information from the enclosing dictionary
        if (dictionary.hasValue<std::string>("GlobeName")) {
            prov.second.setValue("GlobeName", dictionary.value<std::string>("GlobeName"));
        }
        layers::Layer::ID typeID = layers::Layer::ID::DefaultTileProvider;

        if (prov.second.hasValue<std::string>("Type")) {
            const std::string type = prov.second.value<std::string>("Type");
            typeID = ghoul::from_string<layers::Layer::ID>(type);
        }

        std::unique_ptr<TileProvider> tp = createFromDictionary(typeID, prov.second);
        const std::string provId = prov.second.value<std::string>("Identifier");
        tp->setIdentifier(provId);
        const std::string providerName = prov.second.value<std::string>("Name");
        tp->setGuiName(providerName);
        addPropertySubOwner(tp.get());

        const double time = SpiceManager::ref().ephemerisTimeFromDate(prov.first);
        _tileProviders.emplace_back(time, std::move(tp));
    }

    // After we added all tile providers, we need to sort them as they might have been
    // given in an arbitrary order
    std::sort(
        _tileProviders.begin(),
        _tileProviders.end(),
        [](const Provider& lhs, const Provider& rhs) {
            return lhs.startTime < rhs.startTime;
        }
    );
}

void TileProviderByDate::internalInitialize() {
    for (const Provider& p : _tileProviders) {
        p.tileProvider->initialize();
    }
}

void TileProviderByDate::internalDeinitialize() {
    for (const Provider& p : _tileProviders) {
        p.tileProvider->deinitialize();
    }
}

Tile TileProviderByDate::tile(const TileIndex& tileIndex) {
    ZoneScoped;

    return _currentTileProvider ? _currentTileProvider->tile(tileIndex) : Tile();
}

Tile::Status TileProviderByDate::tileStatus(const TileIndex& index) {
    return
        _currentTileProvider ?
        _currentTileProvider->tileStatus(index) :
        Tile::Status::Unavailable;
}


TileDepthTransform TileProviderByDate::depthTransform() {
    return _currentTileProvider ?
        _currentTileProvider->depthTransform() :
        TileDepthTransform(0.f, 1.f);
}

void TileProviderByDate::update() {
    if (_tileProviders.empty()) {
        // If there are no tile providers, then there is no work to be done
        _currentTileProvider = nullptr;
        return;
    }

    // First update all tile providers
    for (const Provider& p : _tileProviders) {
        p.tileProvider->update();
    }

    // Then check update our current tile provider pointer
    const double time = global::timeManager->time().j2000Seconds();
    ghoul_assert(!_tileProviders.empty(), "There should be tile providers at this point");

    // We use the first tileprovider for all times before the beginning start
    if (time < _tileProviders.begin()->startTime) {
        _currentTileProvider = _tileProviders.begin()->tileProvider.get();
        return;
    }

    // And the last tileprovider for all times after the end
    if (time > _tileProviders.back().startTime) {
        _currentTileProvider = _tileProviders.back().tileProvider.get();
        return;
    }

    _currentTileProvider = _tileProviders.begin()->tileProvider.get();
    for (const Provider& p : _tileProviders) {
        if (p.startTime > time) {
            // We have found the first entry that is larger than the current time, which
            // means that we are done
            return;
        }

        _currentTileProvider = p.tileProvider.get();
    }
}

void TileProviderByDate::reset() {
    for (const Provider& p : _tileProviders) {
        p.tileProvider->reset();
    }
}

int TileProviderByDate::minLevel() {
    return _currentTileProvider ? _currentTileProvider->minLevel() : 1;
}

int TileProviderByDate::maxLevel() {
    return _currentTileProvider ? _currentTileProvider->maxLevel() : 1;
}

float TileProviderByDate::noDataValueAsFloat() {
    return _currentTileProvider ? _currentTileProvider->noDataValueAsFloat() : 1;
}

} // namespace openspace::globebrowsing
