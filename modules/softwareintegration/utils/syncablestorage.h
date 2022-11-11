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

#ifndef __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SYNCABLESTORAGE___H__
#define __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SYNCABLESTORAGE___H__


#include <openspace/util/syncable.h>
#include <modules/softwareintegration/simp/simp.h>

#include <mutex>
#include <unordered_map>

namespace openspace {

namespace softwareintegration::storage {

enum class Key : uint8_t {
    DataPoints = 0,
    VelocityData,
    Colormap,
    ColormapAttrData,
    LinearSizeAttrData,
    Unknown
};

Key getStorageKey(const std::string& key);

std::string getStorageKeyString(const Key key);

bool hasStorageKey(const std::string& key);

} // namespace softwareintegration::storage

using namespace softwareintegration;

class SyncableStorage : public Syncable {
public:
	/* ====================== Types ===================== */
    struct Value {
        std::vector<std::byte> data;
        bool syncDirty = true; // Only used on master node
        bool hasLoaded = false;
        bool dirty = true;
    };
    using ValueData = decltype(Value::data);
    using SceneStorage = std::unordered_map<simp::DataKey, Value>;
    using Identifier = std::string;
    using Storage = std::unordered_map<Identifier, SceneStorage>;
    using Iterator = Storage::iterator;
    using SceneIterator = SceneStorage::iterator;
	/* ================================================== */

    /* ============== SyncEngine functions ============== */
    virtual void encode(SyncBuffer* syncBuffer) override;
    virtual void decode(SyncBuffer* syncBuffer) override;
    /* ================================================== */

    template<typename T>
    bool fetch(
        const Identifier& identifier,
        const storage::Key storageKey,
        T& resultingData
    );
    void setLoaded(const Identifier& identifier, const storage::Key storageKey); 
    bool hasLoaded(const Identifier& identifier, const storage::Key storageKey); 
    bool isDirty(const Identifier& identifier, const storage::Key storageKey);
    void store(const Identifier& identifier, const simp::DataKey key, const std::vector<std::byte>& data);

    void encodeStorage(SyncBuffer* syncBuffer, bool skipNonSynced = true);
    void decodeStorage(SyncBuffer* syncBuffer, bool skipNonSynced = true);
    void dump(std::vector<std::byte>& storageDump);
    void store(const std::vector<std::byte>& storageDump);
    std::vector<Identifier> getAllIdentifiers();

private:
    /* =============== Utility functions ================ */
    void insertAssign(const Identifier& identifier, const simp::DataKey key, const Value& value);
    size_t count(const Identifier& identifier);
    size_t count(const Identifier& identifier, const simp::DataKey key);
    std::vector<simp::DataKey> simpDataKeysFromStorageKey(const storage::Key key);
    bool fetchDimFloatData(
        const Identifier& identifier, 
        const std::vector<simp::DataKey> dimDataKeys,
        std::vector<float>& resultingData
    );
    /* ================================================== */

    std::mutex _mutex;
    Storage _storage;
};

} // namespace openspace

#include "syncablestorage.inl"

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SYNCABLESTORAGE___H__
