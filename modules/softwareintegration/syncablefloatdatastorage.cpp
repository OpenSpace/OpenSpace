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
	ZoneScopedN("SyncableFloatDataStorage::encode")

	std::lock_guard guard(_mutex);
	
	size_t nSGNs = _storage.size();
	syncBuffer->encode(nSGNs);

	for (auto& [identifier, sgnStorage] : _storage) {
		syncBuffer->encode(identifier.size());
		syncBuffer->encode(identifier);

		size_t nStorageEntries = sgnStorage.size();
		syncBuffer->encode(nStorageEntries);

		for (auto& [key, storageEntry] : sgnStorage) {
			bool isSyncDirty = storageEntry.syncDirty;
			syncBuffer->encode(isSyncDirty);
			if (!isSyncDirty) continue;

			syncBuffer->encode(static_cast<uint32_t>(key));

			size_t nValues = storageEntry.data.size();
			syncBuffer->encode(nValues);

			// Go trough all data in data entry. 
			// Sequentially structured as: x1, y1, z1, x2, y2, z2...
			for (auto val : storageEntry.data) {
				syncBuffer->encode(val);
			}

			// TODO: Maybe combine solution with syncDirty?
			storageEntry.hasEncoded = true;
		}
	}
}

void SyncableFloatDataStorage::decode(SyncBuffer* syncBuffer) {
	ZoneScopedN("SyncableFloatDataStorage::decode")

    std::lock_guard guard(_mutex);
	
	size_t nSGNs;
	syncBuffer->decode(nSGNs);

	for (size_t i = 0; i < nSGNs; ++i) {
		size_t identifierLength;
		syncBuffer->decode(identifierLength);

		std::string identifier;
		identifier.resize(identifierLength);
		syncBuffer->decode(identifier);

		size_t nStorageEntries;
		syncBuffer->decode(nStorageEntries);

		for (size_t j = 0; j < nStorageEntries; ++j) {
			bool isSyncDirty;
			syncBuffer->decode(isSyncDirty);
			if (!isSyncDirty) continue;

			uint32_t keyRaw;
			syncBuffer->decode(keyRaw);
			auto key = static_cast<storage::Key>(keyRaw);

			size_t nValues;
			syncBuffer->decode(nValues);

			std::vector<float> dataEntry{};
			dataEntry.reserve(nValues);
			for (size_t k = 0; k < nValues; ++k) {
				float value;
				syncBuffer->decode(value);
				dataEntry.push_back(std::move(value));
			}

			insertAssign(identifier, key, Value{ dataEntry });
		}
	}
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
	const Identifier& identifier, const storage::Key key
) {
	LDEBUG(fmt::format("Loading data from float data storage: {}-{}", identifier, storage::getStorageKeyString(key)));
	std::lock_guard guard(_mutex);
	if (!count(identifier)) {
		LERROR(fmt::format(
			"Could not find any data for SceneGraphNode '{}' in the centralized data storage", identifier
		));
		return ValueData{};
	}

	if (!count(identifier, key)) {
		LERROR(fmt::format(
			"SceneGraphNode {} has no data with key '{}' in the centralized data storage",
			identifier,
			storage::getStorageKeyString(key)
		));
		return ValueData{};
	}

	auto& value = _storage.find(identifier)->second.find(key)->second;

	value.dirty = false;

	return value.data;
}

bool SyncableFloatDataStorage::isDirty(
	const Identifier& identifier, const storage::Key key
) {
	if (!count(identifier, key)) {
		return false;
	}

	return _storage.find(identifier)->second.find(key)->second.dirty;
}

bool SyncableFloatDataStorage::isSyncDirty(
	const Identifier& identifier, const storage::Key key
) {
	if (!count(identifier, key)) {
		return true;
	}

	return _storage.find(identifier)->second.find(key)->second.syncDirty;
}

void SyncableFloatDataStorage::store(
	const Identifier& identifier, const storage::Key key, const ValueData& data
) {
	LDEBUG(fmt::format("Storing data in float data storage: {}-{}", identifier, storage::getStorageKeyString(key)));
	std::lock_guard guard(_mutex);
	insertAssign(identifier, key, { data });
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

// Helper function for debugging 
std::string SyncableFloatDataStorage::getStringOfAllKeysInStorage() {
	std::string keysString;
	
	for (auto [id, sceneStorage]: _storage) {
		keysString += '(' + id + ')' + ": ";
		for(auto [key, val]: sceneStorage) {
			keysString += storage::getStorageKeyString(key) + " ";
		}
		keysString += '\n';
	}
	return keysString;
}

/* ================================================== */

} // namespace openspace
