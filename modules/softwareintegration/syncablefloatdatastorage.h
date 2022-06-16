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
#include <modules/softwareintegration/utils.h>

namespace openspace {

using namespace softwareintegration;

class SyncableFloatDataStorage : public Syncable {
public:
	/* ====================== Types ===================== */
	struct Value {
		// a dataset stored like x1, y1, z1, x2, y2 ....
		std::vector<float> data;
		bool hasEncoded = false;
		bool syncDirty = true;
		bool hasLoaded = false;
		bool dirty = true;
	};
	using ValueData = decltype(Value::data);
	using SceneStorage = std::unordered_map<storage::Key, Value>;
	using Identifier = std::string;
	using Storage = std::unordered_map<Identifier, SceneStorage>;
	using Iterator = Storage::iterator;
	using SceneIterator = SceneStorage::iterator;
	/* ================================================== */

	/* ============== SyncEngine functions ============== */
	virtual void encode(SyncBuffer* syncBuffer) override;
	virtual void decode(SyncBuffer* syncBuffer) override;
	virtual void postSync(bool isMaster) override;
	/* ================================================== */

	const ValueData& fetch(const Identifier& identifier, const storage::Key key);
	bool isDirty(const Identifier& identifier, const storage::Key key);
	void setLoaded(const Identifier& identifier, const storage::Key key); 
	bool hasLoaded(const Identifier& identifier, const storage::Key key); 
	void store(const Identifier& identifier, const storage::Key key, const ValueData& data);
	std::string getStringOfAllKeysInStorage();

private:
	/* =============== Utility functions ================ */
	bool erase(const Identifier& identifier, const storage::Key key);

	void insertAssign(const Identifier& identifier, const storage::Key key, const Value& value);

	size_t count(const Identifier& identifier, const storage::Key key);
	size_t count(const Identifier& identifier);
	/* ================================================== */

	std::mutex _mutex;
	Storage _storage;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SYNCABLEFLOATDATASTORAGE___H__
