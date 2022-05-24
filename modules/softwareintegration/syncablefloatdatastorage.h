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

#ifndef __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SYNCABLEFLOATDATASTORAGE___H__
#define __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SYNCABLEFLOATDATASTORAGE___H__

#include <mutex>
#include <unordered_map>

#include <openspace/util/syncable.h>

namespace openspace {

class SyncableFloatDataStorage : public Syncable {
public:
	/* ====================== Types ===================== */
	struct Value {
		// a dataset stored like x1, y1, z1, x2, y2 ....
		std::vector<float> data;
		bool syncDirty = true;
		bool dirty = true;
	};
	using ValueData = decltype(Value::data);
	using Key = std::string;
	using Storage = std::unordered_map<Key, Value>;
	using Iterator = Storage::iterator;
	/* ================================================== */

	/* ============== SyncEngine functions ============== */
	virtual void encode(SyncBuffer* syncBuffer) override;
	virtual void decode(SyncBuffer* syncBuffer) override;
	virtual void postSync(bool isMaster) override;
	/* ================================================== */

	const ValueData& fetch(const Key& key);
	bool isDirty(const Key& key);
	bool isSyncDirty(const Key& key); 
	void store(const Key& key, const ValueData& data);

private:
	/* =============== Utility functions ================ */
	size_t erase(const Key& key);

	void insertAssign(Key key, const Value& value);

	Value& at(const Key& key);

	Iterator find(const Key& key);
	/* ================================================== */

	/* =================== Iterators ==================== */
	Iterator end() noexcept;
			
	Iterator begin() noexcept;
	/* ================================================== */

	std::mutex _mutex;
	Storage _storage;

	bool _storageDirty = true;

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SYNCABLEFLOATDATASTORAGE___H__
