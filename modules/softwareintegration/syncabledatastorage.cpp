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

#include <modules/softwareintegration/syncabledatastorage.h>
#include <openspace/util/syncbuffer.h>

#include <ghoul/logging/logmanager.h>


namespace {
	constexpr const char* _loggerCat = "SoftwareIntegration_SyncableDataStorage";
} // namespace

namespace openspace {

/* ============== SyncEngine functions ============== */
void SyncableDataStorage::encode(SyncBuffer* syncBuffer) {
	std::lock_guard guard(_mutex);
	
	size_t nDatasets = _storage.size();
	syncBuffer->encode(nDatasets);

	for (const auto& [key, points] : _storage) {
		syncBuffer->encode(key);

		// Go trough all points. Structured as "x, y, z, x, y, z, x, y, ..."
		size_t nPoints = points.size();
		syncBuffer->encode(nPoints);
		for (auto value : points) {
			syncBuffer->encode(value);
		}
	}
}

void SyncableDataStorage::decode(SyncBuffer* syncBuffer) {
	std::lock_guard guard(_mutex);
	
	size_t nDatasets;
	syncBuffer->decode(nDatasets);

	for (size_t i = 0; i < nDatasets; ++i) {
		std::string key;
		syncBuffer->decode(key);

		size_t nPoints;
		syncBuffer->decode(nPoints);
		
		// TODO: Change to a glm::fvec3 so we can use an overload 
		// of decode(glm::fvec3) instead of using a for-loop over floats?
		std::vector<float> points;
		points.reserve(nPoints);
		for (size_t j = 0; j < nPoints; ++j) {
			float value;
			syncBuffer->decode(value);
			points.push_back(value);
		}

		_storage[key] = points;
	}
}

void SyncableDataStorage::postSync(bool isMaster) {
	if (isMaster) {
		if (_storage.size() > 0) {
			LWARNING(fmt::format("SyncableDataStorage.size() (MASTER): {}", _storage.size()));
		}
	}
	else {
		if (_storage.size() > 0) {
			LWARNING(fmt::format("SyncableDataStorage.size() (CLIENT): {}", _storage.size()));
		}
	}
}
/* ================================================== */

/* =============== Utility functions ================ */
SyncableDataStorage::Iterator SyncableDataStorage::erase(SyncableDataStorage::Iterator pos) {
	return _storage.erase(pos);
}
SyncableDataStorage::Iterator SyncableDataStorage::erase(const SyncableDataStorage::Iterator first, const SyncableDataStorage::Iterator last) {
	return _storage.erase(first, last);
}
size_t SyncableDataStorage::erase(const SyncableDataStorage::Key& key) {
	return _storage.erase(key);
}

std::pair<SyncableDataStorage::Iterator, bool> SyncableDataStorage::emplace(SyncableDataStorage::Key key, SyncableDataStorage::Value value) {
	return _storage.emplace(key, value);
}

SyncableDataStorage::Value& SyncableDataStorage::at(const SyncableDataStorage::Key& key) {
	return _storage.at(key);
}
const SyncableDataStorage::Value& SyncableDataStorage::at(const SyncableDataStorage::Key& key) const {
	return _storage.at(key);
}

SyncableDataStorage::Iterator SyncableDataStorage::find(const SyncableDataStorage::Key& key) {
	return _storage.find(key);
}
/* ================================================== */

/* =================== Iterators ==================== */
SyncableDataStorage::Iterator SyncableDataStorage::end() noexcept {
	return _storage.end();
}
		
SyncableDataStorage::Iterator SyncableDataStorage::begin() noexcept {
	return _storage.begin();
}
/* ================================================== */

/* =============== Operator overloads =============== */
SyncableDataStorage::Value& SyncableDataStorage::operator[](SyncableDataStorage::Key&& key) {
	return _storage[key];
}
/* ================================================== */

} // namespace openspace
