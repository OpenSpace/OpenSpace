/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __LRU_CACHE__
#define __LRU_CACHE__


#include <ghoul/misc/assert.h>
//#include <modules/globebrowsing/datastructures/lrucache.h>


namespace openspace {

	
	template<typename KeyType, typename ValueType>
	LRUCache<KeyType, ValueType>::LRUCache(size_t size)
		: _cacheSize(size) { }

	template<typename KeyType, typename ValueType>
	LRUCache<KeyType, ValueType>::~LRUCache() {	
		// Clean up list and map!
	}


	//////////////////////////////
	//		PUBLIC INTERFACE	//
	//////////////////////////////

	template<typename KeyType, typename ValueType>
	void LRUCache<KeyType, ValueType>::put(const KeyType& key, const ValueType& value)
	{
		auto it = _itemMap.find(key);
		if (it != _itemMap.end()) {
			_itemList.erase(it->second);
			_itemMap.erase(it);
		}
		_itemList.push_front(std::make_pair(key, value));
		_itemMap.insert(std::make_pair(key, _itemList.begin()));
		clean();
	}


	template<typename KeyType, typename ValueType>
	bool LRUCache<KeyType, ValueType>::exist(const KeyType& key) const
	{
		return _itemMap.count(key) > 0;
	}


	template<typename KeyType, typename ValueType>
	ValueType LRUCache<KeyType, ValueType>::get(const KeyType& key)
	{
		ghoul_assert(exist(key), "Key " << key << " must exist");
		auto it = _itemMap.find(key);
		// Move list iterator pointing to value
		_itemList.splice(_itemList.begin(), _itemList, it->second);
		return it->second->second;
	}



	//////////////////////////////
	//		PRIVATE HELPERS		//
	//////////////////////////////
	template<typename KeyType, typename ValueType>
	void LRUCache<KeyType, ValueType>::clean()
	{
		while (_itemMap.size() > _cacheSize) {
			auto last_it = _itemList.end(); last_it--;
			_itemMap.erase(last_it->first);
			_itemList.pop_back();
		}
	}


} // namespace openspace

#endif // !__LRU_CACHE__