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

#include <modules/softwareintegration/utils/syncablestorage.h>

#include <openspace/util/syncbuffer.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/logging/logmanager.h>

namespace {
    
constexpr const char* _loggerCat = "SyncableStorage";

} // namespace

namespace openspace {

namespace softwareintegration::storage {

// Anonymous namespace
namespace {
    
    const std::unordered_map<std::string, Key> _keyStringFromKey{
        { "DataPoints", Key::DataPoints },
        { "VelocityData", Key::VelocityData },
        { "Colormap", Key::Colormap },
        { "ColormapAttributeData", Key::ColormapAttrData },
        { "LinearSizeAttrData", Key::LinearSizeAttrData },
        { "Unknown", Key::Unknown },
    };

} // namespace

bool hasStorageKey(const std::string& key) {
    return _keyStringFromKey.count(key) > 0;
}

Key getStorageKey(const std::string& key) {
    if (hasStorageKey(key)) {
        return _keyStringFromKey.at(key);
    }

    return Key::Unknown;
}

std::string getStorageKeyString(const Key key) {
    auto it = std::find_if(
        _keyStringFromKey.begin(),
        _keyStringFromKey.end(),
        [key](const std::pair<const std::string, Key>& p) {
            return key == p.second;
        }
    );
    if (it == _keyStringFromKey.end()) return "";
    return it->first;
}

} // namespace softwareintegration::storage

using namespace softwareintegration;

/* ============== SyncEngine functions ============== */
void SyncableStorage::encode(SyncBuffer* syncBuffer) {
    ZoneScopedN("SyncableStorage::encode");

    encodeStorage(syncBuffer);
}

void SyncableStorage::decode(SyncBuffer* syncBuffer) {
    ZoneScopedN("SyncableStorage::decode");

    decodeStorage(syncBuffer);
}

/* ================================================== */

bool SyncableStorage::isDirty(const Identifier& identifier, const storage::Key storageKey) {
    auto simpDataKeys = simpDataKeysFromStorageKey(storageKey);
    for (auto key : simpDataKeys) {
        if (!count(identifier, key)) {
            return false;
        }

        if (_storage.find(identifier)->second.find(key)->second.dirty) {
            return true;
        } 
    }
    return false;
}

void SyncableStorage::setLoaded(const Identifier& identifier, const storage::Key storageKey) {
    auto simpDataKeys = simpDataKeysFromStorageKey(storageKey);
    for (auto key : simpDataKeys) {
        if (!count(identifier, key)) {
            LERROR(std::format(
                "SceneGraphNode {} has no data with key '{}' in the syncable data storage",
                identifier,
                simp::getStringFromDataKey(key)
            ));
            return;
        }
        _storage.find(identifier)->second.find(key)->second.hasLoaded = true;
    }
}

bool SyncableStorage::hasLoaded(const Identifier& identifier, const storage::Key storageKey) {
	auto simpDataKeys = simpDataKeysFromStorageKey(storageKey);
    for (auto key : simpDataKeys) {
        if (!count(identifier, key)) {
            return false;
        }

        if (!_storage.find(identifier)->second.find(key)->second.hasLoaded) {
            return false;
        } 
    }
    return true;
}

void SyncableStorage::store(
    const Identifier& identifier,
    const simp::DataKey key,
    const std::vector<std::byte>& data
) {
    LDEBUG(std::format("Storing data in float data storage: {}-{}", identifier, simp::getStringFromDataKey(key)));
    std::lock_guard guard(_mutex);
    insertAssign(identifier, key, { data });
}

void SyncableStorage::encodeStorage(SyncBuffer* syncBuffer, bool skipNonSynced) {
    std::lock_guard guard(_mutex);

    syncBuffer->encode(static_cast<uint16_t>(_storage.size()));

    for (auto& [identifier, sgnStorage] : _storage) {
        syncBuffer->encode(identifier);

        syncBuffer->encode(static_cast<uint16_t>(sgnStorage.size()));

        for (auto& [key, storageEntry] : sgnStorage) {
            if (skipNonSynced) {
                bool& isSyncDirty = storageEntry.syncDirty;
                syncBuffer->encode(isSyncDirty);
                if (!isSyncDirty) continue;
                isSyncDirty = false;
            }

            syncBuffer->encode(static_cast<uint8_t>(key));

            syncBuffer->encode(storageEntry.data);
        }
    }
}

void SyncableStorage::decodeStorage(SyncBuffer* syncBuffer, bool skipNonSynced) {
    std::lock_guard guard(_mutex);

    uint16_t nSGNs;
    syncBuffer->decode(nSGNs);

    for (uint16_t i = 0; i < nSGNs; ++i) {
        std::string identifier;
        syncBuffer->decode(identifier);

        uint16_t nStorageEntries;
        syncBuffer->decode(nStorageEntries);

        for (uint16_t j = 0; j < nStorageEntries; ++j) {
            if (skipNonSynced) {
                bool isSyncDirty;
                syncBuffer->decode(isSyncDirty);
                if (!isSyncDirty) continue;
            }

            uint8_t keyRaw;
            syncBuffer->decode(keyRaw);
            auto key = static_cast<simp::DataKey>(keyRaw);

            std::vector<std::byte> dataEntry{};
            syncBuffer->decode(dataEntry);

            insertAssign(identifier, key, Value{ dataEntry });
        }
    }
}

void SyncableStorage::store(const std::vector<std::byte>& storageDump) {
    ZoneScopedN("SyncableStorage::store");
    auto syncBuffer = new SyncBuffer{ 0 };
    syncBuffer->setData(storageDump);
    decodeStorage(syncBuffer, false);
}

void SyncableStorage::dump(std::vector<std::byte>& storageDump) {
    ZoneScopedN("SyncableStorage::dump");

    auto syncBuffer = new SyncBuffer{ 0 };
    encodeStorage(syncBuffer, false);
    storageDump = syncBuffer->data();
}

std::vector<SyncableStorage::Identifier> SyncableStorage::getAllIdentifiers() {
    std::vector<Identifier> identifiers;
    identifiers.reserve(_storage.size());
    for (auto [identifier, sceneStorage] : _storage) {
        identifiers.push_back(identifier);
    }
    return std::move(identifiers);
}

/* =============== Utility functions ================ */
void SyncableStorage::insertAssign(const Identifier& identifier, const simp::DataKey key, const Value& value) {
    if (count(identifier)) {
        if (count(identifier, key)) {
            _storage.find(identifier)->second.find(key)->second = value;
        }
        else {
            _storage.find(identifier)->second.emplace(key, value);
        }
    }
    else {
        SceneStorage newSceneStorage{ { key, value } };
        _storage.emplace(identifier, std::move(newSceneStorage));
    }
}

size_t SyncableStorage::count(const Identifier& identifier) {
    return _storage.count(identifier);
}

size_t SyncableStorage::count(const Identifier& identifier, const simp::DataKey key) {
    auto sceneIt = _storage.find(identifier);
    if (sceneIt == _storage.end()) return 0;

    return sceneIt->second.count(key);
}

std::vector<simp::DataKey> SyncableStorage::simpDataKeysFromStorageKey(const storage::Key key) {
    switch (key) {
        case storage::Key::DataPoints: {
            return { simp::DataKey::X, simp::DataKey::Y, simp::DataKey::Z };
        }
        case storage::Key::VelocityData:{
            return { simp::DataKey::U, simp::DataKey::V, simp::DataKey::W };
        }
        case storage::Key::Colormap:{
            return { 
                simp::DataKey::ColormapReds,
                simp::DataKey::ColormapGreens,
                simp::DataKey::ColormapBlues,
                simp::DataKey::ColormapAlphas
            };
        }
        case storage::Key::ColormapAttrData:{
            return { simp::DataKey::ColormapAttributeData };
        }
        case storage::Key::LinearSizeAttrData:{
            return { simp::DataKey::LinearSizeAttributeData };
        }
        default: { // Unknown
            LERROR(std::format(
                "There's no storage key '{}'",
                storage::getStorageKeyString(key)
            ));
            return {};
        }
    }
}

/**
 * Fetches float data for 1 or more dimensions of data
 *
 */
bool SyncableStorage::fetchDimFloatData(
    const Identifier& identifier, 
    const std::vector<simp::DataKey> dimDataKeys,
    std::vector<float>& resultingData
) {
    // Fetch all values from storage
    size_t nBytesPerDim = 0;
    size_t nDimensions = dimDataKeys.size();
    size_t nValues = dimDataKeys.size();

    if (!count(identifier)) {
        LERROR(std::format(
            "SceneGraphNode {} is missing from the syncable data storage", 
            identifier
        ));
        return false;
    }

    auto& sceneStorage = _storage.at(identifier);

    for (size_t i = 0; i < nDimensions; i++) {
        if (!count(identifier, dimDataKeys[i])) {
            LERROR(std::format(
                "SceneGraphNode {} is missing {} from the syncable data storage", 
                identifier, simp::getStringFromDataKey(dimDataKeys[i])
            ));
            return false;
        }

        auto dimValues = sceneStorage.at(dimDataKeys[i]).data;
        
        // All dimensions must have same length
        if (i != 0 && nBytesPerDim != dimValues.size()) {
            LERROR(std::format(
                "Error while trying to fetch float data."
                "The dimensions of values does not have the same length."
            ));
            return false;
        }

        nBytesPerDim = dimValues.size();
        nValues = (nBytesPerDim / 4);
    }
    
    resultingData.resize(nValues * nDimensions);
    size_t nAddedValues = 0;

    for (size_t i = 0; i < nDimensions; ++i) {
        auto dataOnDim = sceneStorage.at(dimDataKeys[i]).data;
        size_t offset = 0;
        size_t index = i;

        while (offset < nBytesPerDim) {
            float value;
            try {
                // We can use readValue() (which converts to big endian) 
                // because we haven't changed from big (network) endian 
                // to little endian yet for the data in syncable storage
                simp::readValue(
                    dataOnDim,
                    offset,
                    value
                );
            }
            catch (const simp::SimpError& err) {
                LERROR(std::format(
                    "Couldn't parse value on offset {} from storage for {}: {}", 
                    offset, simp::getStringFromDataKey(dimDataKeys[i]), err.message
                ));
                resultingData.clear();
                return false;
            }

            resultingData[index] = value;
            ++nAddedValues;
            // Advance nDimensions (nDimension represent stride)
            index += nDimensions;
        }
    }

    if (nAddedValues != resultingData.size()) {
        std::string dataKeysString = "(";
        for (size_t i = 0; i < dimDataKeys.size(); ++i) {
            std::string prefix = (i != 0 ? ", " : "");
            dataKeysString += prefix + simp::getStringFromDataKey(dimDataKeys[i]);
        }
        dataKeysString += ")";
        LERROR(std::format(
            "Mismatch in number of values in syncable storage ({}) and loaded values ({}), when loading {}.", 
            resultingData.size(), nAddedValues, dataKeysString
        ));
        resultingData.clear();
        return false;
    }

    // Set all values to not dirty
    for (auto dimDataKey : dimDataKeys) {
        sceneStorage.at(dimDataKey).dirty = false;
    }

    return true;
}

/* ================================================== */

} // namespace openspace
