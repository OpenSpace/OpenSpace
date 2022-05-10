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

	for (const auto& [key, dataEntry] : _storage) {
		syncBuffer->encode(key);

		// Go trough all data in data entry. 
		// Sequentially structured (ex: x1, y1, z1, x2, y2, z2...)
		size_t nItemsInDataEntry = dataEntry.size();
		syncBuffer->encode(nItemsInDataEntry);
		for (auto value : dataEntry) {
			syncBuffer->encode(value);
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

		size_t nItemsInDataEntry;
		syncBuffer->decode(nItemsInDataEntry);
		
		// TODO: Change to a glm::fvec3 so we can use an overload 
		// of decode(glm::fvec3) instead of using a for-loop over floats?
		std::vector<float> dataEntry;
        dataEntry.reserve(nItemsInDataEntry);
		for (size_t j = 0; j < nItemsInDataEntry; ++j) {
			float value;
			syncBuffer->decode(value);
            dataEntry.push_back(value);
		}

		_storage[key] = dataEntry;
	}
}

void SyncableFloatDataStorage::postSync(bool isMaster) {
	if (isMaster) {
		if (_storage.size() > 0) {
			// LWARNING(fmt::format("SyncableFloatDataStorage.size() (MASTER): {}", _storage.size()));
		}
	}
	else {
		if (_storage.size() > 0) {
			// LWARNING(fmt::format("SyncableFloatDataStorage.size() (CLIENT): {}", _storage.size()));
		}
	}
}
/* ================================================== */

/* =============== Utility functions ================ */
SyncableFloatDataStorage::Iterator SyncableFloatDataStorage::erase(SyncableFloatDataStorage::Iterator pos) {
	return _storage.erase(pos);
}
SyncableFloatDataStorage::Iterator SyncableFloatDataStorage::erase(const SyncableFloatDataStorage::Iterator first, const SyncableFloatDataStorage::Iterator last) {
	return _storage.erase(first, last);
}
size_t SyncableFloatDataStorage::erase(const SyncableFloatDataStorage::Key& key) {
	return _storage.erase(key);
}

std::pair<SyncableFloatDataStorage::Iterator, bool> SyncableFloatDataStorage::emplace(SyncableFloatDataStorage::Key key, SyncableFloatDataStorage::Value value) {
	return _storage.emplace(key, value);
}

SyncableFloatDataStorage::Value& SyncableFloatDataStorage::at(const SyncableFloatDataStorage::Key& key) {
	return _storage.at(key);
}
const SyncableFloatDataStorage::Value& SyncableFloatDataStorage::at(const SyncableFloatDataStorage::Key& key) const {
	return _storage.at(key);
}

SyncableFloatDataStorage::Iterator SyncableFloatDataStorage::find(const SyncableFloatDataStorage::Key& key) {
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

/* =============== Operator overloads =============== */
SyncableFloatDataStorage::Value& SyncableFloatDataStorage::operator[](SyncableFloatDataStorage::Key&& key) {
	return _storage[key];
}
/* ================================================== */

} // namespace openspace
