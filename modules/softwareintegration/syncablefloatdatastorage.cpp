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

#include <modules/softwareintegration/syncablefloatdatastorage.h>

#include <modules/softwareintegration/utils.h>
#include <openspace/util/syncbuffer.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/logging/logmanager.h>

namespace {
    
constexpr const char* _loggerCat = "SyncableFloatDataStorage";

} // namespace

namespace openspace {

using namespace softwareintegration;

/* ============== SyncEngine functions ============== */
void SyncableFloatDataStorage::encode(SyncBuffer* syncBuffer) {
    ZoneScopedN("SyncableFloatDataStorage::encode");

    encodeStorage(syncBuffer);
}

void SyncableFloatDataStorage::decode(SyncBuffer* syncBuffer) {
    ZoneScopedN("SyncableFloatDataStorage::decode");

    decodeStorage(syncBuffer);
}

void SyncableFloatDataStorage::postSync(bool isMaster) {
    if (isMaster) {
        std::lock_guard guard(_mutex);
        for (auto& sgnStorage : _storage) {
            for (auto& storageEntry : sgnStorage.second) {
                if (storageEntry.second.syncDirty && storageEntry.second.hasEncoded) {
                    storageEntry.second.syncDirty = false;
                    storageEntry.second.hasEncoded = false;
                }
            }
        }
    }
}

/* ================================================== */

const SyncableFloatDataStorage::ValueData& SyncableFloatDataStorage::fetch(
    const Identifier& identifier,
    const storage::Key key
) {
    LDEBUG(fmt::format("Loading data from float data storage: {}-{}", identifier, storage::getStorageKeyString(key)));
    std::lock_guard guard(_mutex);
    if (!count(identifier)) {
        LERROR(fmt::format(
            "Could not find any data for SceneGraphNode '{}' in the centralized data storage",
            identifier
        ));
        return ValueData{};
    }

    if (!count(identifier, key)) {
        LERROR(fmt::format(
            "SceneGraphNode {} has no data with key '{}' in the centralized data storage", identifier,
            storage::getStorageKeyString(key)
        ));
        return ValueData{};
    }

    auto& value = _storage.find(identifier)->second.find(key)->second;

    value.dirty = false;

    return value.data;
}

bool SyncableFloatDataStorage::isDirty(const Identifier& identifier, const storage::Key key) {
    if (!count(identifier, key)) {
        return false;
    }

    return _storage.find(identifier)->second.find(key)->second.dirty;
}

void SyncableFloatDataStorage::setLoaded(const Identifier& identifier, const storage::Key key) {
	if (!count(identifier, key)) {
		LERROR(fmt::format(
			"SceneGraphNode {} has no data with key '{}' in the centralized data storage",
			identifier,
			storage::getStorageKeyString(key)
		));
		return;
	}
	_storage.find(identifier)->second.find(key)->second.hasLoaded = true;
}

bool SyncableFloatDataStorage::hasLoaded(const Identifier& identifier, const storage::Key key) {
	if (!count(identifier, key)) {
		return false;
	}

	return _storage.find(identifier)->second.find(key)->second.hasLoaded;
}

void SyncableFloatDataStorage::store(
    const Identifier& identifier,
    const storage::Key key,
    const ValueData& data
) {
    LDEBUG(fmt::format("Storing data in float data storage: {}-{}", identifier, storage::getStorageKeyString(key)));
    std::lock_guard guard(_mutex);
    insertAssign(identifier, key, { data });
}

void SyncableFloatDataStorage::encodeStorage(SyncBuffer* syncBuffer, bool skipNonSynced) {
    std::lock_guard guard(_mutex);

    syncBuffer->encode(static_cast<uint16_t>(_storage.size()));

    for (auto& [identifier, sgnStorage] : _storage) {
        syncBuffer->encode(identifier);

        syncBuffer->encode(static_cast<uint16_t>(sgnStorage.size()));

        for (auto& [key, storageEntry] : sgnStorage) {
            if (skipNonSynced) {
                bool isSyncDirty = storageEntry.syncDirty;
                syncBuffer->encode(isSyncDirty);
                if (!isSyncDirty) continue;
            }

            syncBuffer->encode(static_cast<uint8_t>(key));

            syncBuffer->encode(storageEntry.data);

            // TODO: Maybe combine solution with syncDirty?
            storageEntry.hasEncoded = true;
        }
    }
}

void SyncableFloatDataStorage::decodeStorage(SyncBuffer* syncBuffer, bool skipNonSynced) {
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
            auto key = static_cast<storage::Key>(keyRaw);

            std::vector<float> dataEntry{};
            syncBuffer->decode(dataEntry);

            insertAssign(identifier, key, Value{ dataEntry });
        }
    }
}

void SyncableFloatDataStorage::store(const std::vector<std::byte>& storageDump) {
    ZoneScopedN("SyncableFloatDataStorage::store");
    auto syncBuffer = new SyncBuffer{ 0 };
    syncBuffer->setData(storageDump);
    decodeStorage(syncBuffer, false);
}

void SyncableFloatDataStorage::dump(std::vector<std::byte>& storageDump) {
    ZoneScopedN("SyncableFloatDataStorage::dump");

    auto syncBuffer = new SyncBuffer{ 0 };
    encodeStorage(syncBuffer, false);
    storageDump = syncBuffer->data();
}

/* =============== Utility functions ================ */
bool SyncableFloatDataStorage::erase(const Identifier& identifier, const storage::Key key) {
    if (count(identifier, key) == 0) return false;

    auto nErased = _storage.find(identifier)->second.erase(key);
    return nErased > 0;
}

void SyncableFloatDataStorage::insertAssign(const Identifier& identifier, const storage::Key key, const Value& value) {
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

size_t SyncableFloatDataStorage::count(const Identifier& identifier) {
    return _storage.count(identifier);
}

size_t SyncableFloatDataStorage::count(const Identifier& identifier, const storage::Key key) {
    auto sceneIt = _storage.find(identifier);
    if (sceneIt == _storage.end()) return 0;

    return sceneIt->second.count(key);
}

std::vector<SyncableFloatDataStorage::Identifier> SyncableFloatDataStorage::getAllIdentifiers() {
    std::vector<Identifier> identifiers;
    identifiers.reserve(_storage.size());
    for (auto [identifier, sceneStorage] : _storage) {
        identifiers.push_back(identifier);
    }
    return std::move(identifiers);
}

// Helper function for debugging
std::string SyncableFloatDataStorage::getStringOfAllKeysInStorage() {
    std::string keysString;

    for (auto [id, sceneStorage] : _storage) {
        keysString += '(' + id + ')' + ": ";
        for (auto [key, val] : sceneStorage) {
            keysString += storage::getStorageKeyString(key) + " ";
        }
        keysString += '\n';
    }
    return keysString;
}

/* ================================================== */

} // namespace openspace
