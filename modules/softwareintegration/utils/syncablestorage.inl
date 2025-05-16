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

#include <ghoul/logging/logmanager.h>

namespace openspace {

template<typename T>
bool SyncableStorage::fetch(
    const Identifier& identifier,
    const storage::Key storageKey,
    T& resultingData
) {
    LDEBUGC("SyncableStorage", std::format("Loading data from float data storage: {}-{}", identifier, storage::getStorageKeyString(storageKey)));
    std::lock_guard guard(_mutex);
    if (!count(identifier)) {
        LERRORC("SyncableStorage", std::format(
            "Could not find any data for SceneGraphNode '{}' in the centralized data storage",
            identifier
        ));
        return false;
    }

    switch (storageKey) {
        case storage::Key::DataPoints:
        case storage::Key::Colormap:
        case storage::Key::ColormapAttrData:
        case storage::Key::LinearSizeAttrData:
        case storage::Key::VelocityData: {
            if (!std::is_same<T, std::vector<float>>::value) {
                LERRORC("SyncableStorage", std::format(
                    "Can't put {} into a {}.",
                    storage::getStorageKeyString(storageKey), typeid(T).name()
                ));
                return false;
            }

            return fetchDimFloatData(identifier, simpDataKeysFromStorageKey(storageKey), resultingData);
        }
        default: {
            LERRORC("SyncableStorage", std::format(
                "Could not find data in storage for the key {}",
                storage::getStorageKeyString(storageKey)
            ));
            break;
        }
    }

    return false;
}

} // namespace openspace
