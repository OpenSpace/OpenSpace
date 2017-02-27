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

#ifndef __OPENSPACE_CORE___SYNCENGINE___H__
#define __OPENSPACE_CORE___SYNCENGINE___H__

#include <vector>
#include <memory>

namespace openspace {

class Syncable;
class SyncBuffer;

/**
* Manages a collection of <code>Syncable</code>s and ensures they are synchronized
* over SGCT nodes. Encoding/Decoding order is handles internally.
*/
class SyncEngine {
public:

    /**
    * Dependency injection: a SyncEngine relies on a SyncBuffer to encode the sync data.
    */
    SyncEngine(SyncBuffer* syncBuffer);


    /**
    * Encodes all added Syncables in the injected <code>SyncBuffer</code>. 
    * This method is only called on the SGCT master node
    */
    void encodeSyncables();

    /**
    * Decodes the <code>SyncBuffer</code> into the added Syncables.
    * This method is only called on the SGCT slave nodes
    */
    void decodeSyncables();

    /**
    * Invokes the presync method of all added Syncables
    */
    void presync(bool isMaster);

    /**
    * Invokes the postsync method of all added Syncables
    */
    void postsync(bool isMaster);
    


    /**
    * Add a Syncable to be synchronized over the SGCT cluster
    */
    void addSyncable(Syncable* syncable);

    /**
    * Add multiple Syncables to be synchronized over the SGCT cluster
    */
    void addSyncables(const std::vector<Syncable*>& syncables);

    /**
    * Remove a Syncable from being synchronized over the SGCT cluster
    */
    void removeSyncable(Syncable* syncable);

private:
    
    /** 
    * Vector of Syncables. The vectors ensures consistent encode/decode order
    */
    std::vector<Syncable*> _syncables;

    /**
    * Databuffer used in encoding/decoding
    */
    std::unique_ptr<SyncBuffer> _syncBuffer;
};


} // namespace openspace

#endif // __OPENSPACE_CORE___SYNCENGINE___H__
