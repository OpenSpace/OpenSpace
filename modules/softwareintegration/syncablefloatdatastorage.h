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

#include <openspace/util/syncable.h>
#include <mutex>

namespace openspace {

/**
 * A double buffered implementation of the Syncable interface.
 * Users are encouraged to used this class as a default way to synchronize different
 * C++ data types using the SyncEngine.
 *
 * This class aims to handle the synchronization parts and yet act like a regular
 * instance of T. Implicit casts are supported, however, when accessing member functions
 * or variables, user may have to do explicit casts.
 *
 * ((T&) t).method();
 *
 */
class SyncableFloatDataStorage : public Syncable {
public:
	/* ====================== Types ===================== */
	typedef std::string Key;
	typedef std::vector<float> Value;   // a dataset stored like x1, y1, z1, x2, y2 ....
	typedef std::map<Key, Value> Storage;
	typedef Storage::iterator Iterator;
	/* ================================================== */

	/* ============== SyncEngine functions ============== */
	// virtual void preSync(bool isMaster) override;
	virtual void encode(SyncBuffer* syncBuffer) override;
	virtual void decode(SyncBuffer* syncBuffer) override;
	virtual void postSync(bool isMaster) override;
	/* ================================================== */

	/* =============== Utility functions ================ */
	Iterator erase(Iterator pos);
	Iterator erase(const Iterator first, const Iterator last);
	size_t erase(const Key& key);

	std::pair<Iterator, bool> emplace(Key key, Value value);

	Value& at(const Key& key);
	const Value& at(const Key& key) const;

	Iterator find(const Key& key);
	/* ================================================== */

	/* =================== Iterators ==================== */
	Iterator end() noexcept;
			
	Iterator begin() noexcept;
	/* ================================================== */

	/* =============== Operator overloads =============== */
	Value& operator[](Key&& key);
	/* ================================================== */

private:
	std::mutex _mutex;
	Storage _storage;

	bool showMessageEncode = true;
	bool showMessageDecode = true;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SYNCABLEFLOATDATASTORAGE___H__
