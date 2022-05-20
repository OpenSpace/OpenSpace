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
#include <openspace/util/syncbuffer.h>

#include <ghoul/logging/logmanager.h>


namespace {
	constexpr const char* _loggerCat = "SyncableFloatDataStorage";
} // namespace

namespace openspace {

/* ============== SyncEngine functions ============== */
void SyncableFloatDataStorage::encode(SyncBuffer* syncBuffer) {
	std::lock_guard guard(_mutex);
	
	size_t nDataEntries = _storage.size();
	syncBuffer->encode(nDataEntries);

	for (auto& [key, value] : _storage) {
		syncBuffer->encode(key);

		syncBuffer->encode(value.dirty);

		// Only encode data if it is dirty, to save bandwidth
		if (value.dirty) {
			// Go trough all data in data entry. 
			// Sequentially structured as: x1, y1, z1, x2, y2, z2...
			size_t nItemsInDataEntry = value.data.size();
			syncBuffer->encode(nItemsInDataEntry);
			for (auto val : value.data) {
				syncBuffer->encode(val);
			}
		}
	}
}

void SyncableFloatDataStorage::decode(SyncBuffer* syncBuffer) {
	std::lock_guard guard(_mutex);
	
	size_t nDataEntries;
	syncBuffer->decode(nDataEntries);

	for (size_t i = 0; i < nDataEntries; ++i) {
		std::string key;
		syncBuffer->decode(key);

		bool dirty;
		syncBuffer->decode(dirty);

		if (dirty) {
			size_t nItemsInDataEntry;
			syncBuffer->decode(nItemsInDataEntry);
			
			std::vector<float> dataEntry;
			dataEntry.reserve(nItemsInDataEntry);
			for (size_t j = 0; j < nItemsInDataEntry; ++j) {
				float value;
				syncBuffer->decode(value);
				dataEntry.push_back(value);
			}

			insertAssign(key, Value{ dataEntry, dirty });
		}
	}
}

void SyncableFloatDataStorage::postSync(bool isMaster) {
	if (isMaster) {
		for (auto& [key, value] : _storage) {
			if (value.dirty) {
				value.dirty = false;
			}
		}
	}
}
/* ================================================== */

const SyncableFloatDataStorage::ValueData& SyncableFloatDataStorage::fetch(const Key& key) {
	LDEBUG(fmt::format("Loading data from float data storage: {}", key));
	auto it = find(key);
    if (it == end()) {
        LERROR(fmt::format(
            "Could not find data with key '{}' in the centralized data storage", key
        ));
        return std::move(ValueData{});
    }

    it->second.localDirty = false;

    return it->second.data;
}

bool SyncableFloatDataStorage::isDirty(const Key& key) {
	auto it = find(key);
    if (it == end()) {
        return false;
    }

    return it->second.localDirty;
}

void SyncableFloatDataStorage::store(const Key& key, const ValueData& data) {
	LDEBUG(fmt::format("Storing data in float data storage: {}", key));
	Value value{ data, true, true };

	auto old = find(key);
	if (old != end()) {
		glm::vec3 firstValueOld{};
		for (glm::vec3::length_type i = 0; i < glm::vec3::length(); ++i) {
			firstValueOld[i] = old->second.data[i];
		}

		LDEBUG(fmt::format(
			"First data point: old: {}", firstValueOld
		));
	}

    insertAssign(key, std::move(value));

	auto newVal = find(key);
	if (newVal != end()) {
		glm::vec3 firstValueNew{};
		for (glm::vec3::length_type i = 0; i < glm::vec3::length(); ++i) {
			firstValueNew[i] = newVal->second.data[i];
		}

		LDEBUG(fmt::format(
			"First data point: new {}", firstValueNew
		));
	}
}

/* =============== Utility functions ================ */
size_t SyncableFloatDataStorage::erase(const SyncableFloatDataStorage::Key& key) {
	return _storage.erase(key);
}

void SyncableFloatDataStorage::insertAssign(Key key, const Value& value) {
	auto it = find(key);
	if (it == end()) {
		_storage.emplace(key, value);
	}
	else {
		it->second = value;
	}
}

SyncableFloatDataStorage::Value& SyncableFloatDataStorage::at(const Key& key) {
	return _storage.at(key);
}

SyncableFloatDataStorage::Iterator SyncableFloatDataStorage::find(const Key& key) {
	return _storage.find(key);
}
/* ================================================== */

/* =================== Iterators ==================== */
SyncableFloatDataStorage::Iterator SyncableFloatDataStorage::end() noexcept {
	return _storage.end();
}
		
SyncableFloatDataStorage::Iterator SyncableFloatDataStorage::begin() noexcept {
	return _storage.begin();
}
/* ================================================== */

} // namespace openspace
