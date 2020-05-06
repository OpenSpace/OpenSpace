/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_CORE___SYNCENGINE___H__
#define __OPENSPACE_CORE___SYNCENGINE___H__

#include <openspace/util/syncbuffer.h>

#include <ghoul/misc/boolean.h>
#include <memory>
#include <vector>

namespace openspace {

class Syncable;

/**
 * Manages a collection of <code>Syncable</code>s and ensures they are synchronized
 * over SGCT nodes. Encoding/Decoding order is handles internally.
 */
class SyncEngine {
public:
    BooleanType(IsMaster);

    /**
     * Creates a new SyncEngine which a buffer size of \p syncBufferSize
     * \pre syncBufferSize must be bigger than 0
     */
    SyncEngine(unsigned int syncBufferSize);

    /**
     * Encodes all added Syncables in the injected <code>SyncBuffer</code>.
     * This method is only called on the SGCT master node
     */
    std::vector<std::byte> encodeSyncables();

    /**
     * Decodes the <code>SyncBuffer</code> into the added Syncables.
     * This method is only called on the SGCT slave nodes
     */
    void decodeSyncables(std::vector<std::byte> data);

    /**
     * Invokes the presync method of all added Syncables
     */
    void preSynchronization(IsMaster isMaster);

    /**
     * Invokes the postsync method of all added Syncables
     */
    void postSynchronization(IsMaster isMaster);

    /**
     * Add a Syncable to be synchronized over the SGCT cluster.
     * \pre syncable must not be nullptr
     */
    void addSyncable(Syncable* syncable);

    /**
     * Add multiple Syncables to be synchronized over the SGCT cluster
     * \pre syncables must not contain any nullptr
     */
    void addSyncables(const std::vector<Syncable*>& syncables);

    /**
     * Remove a Syncable from being synchronized over the SGCT cluster
     */
    void removeSyncable(Syncable* syncable);

    /**
    * Remove multiple Syncables from being synchronized over the SGCT cluster
    */
    void removeSyncables(const std::vector<Syncable*>& syncables);

private:
    /**
     * Vector of Syncables. The vectors ensures consistent encode/decode order
     */
    std::vector<Syncable*> _syncables;

    /**
     * Databuffer used in encoding/decoding
     */
    SyncBuffer _syncBuffer;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___SYNCENGINE___H__
