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

#ifndef __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SYNCABLEMESSAGEQUEUE___H__
#define __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SYNCABLEMESSAGEQUEUE___H__

#include <openspace/util/syncable.h>
#include <modules/softwareintegration/network/softwareconnection.h>

namespace openspace {

struct PeerMessage {
	size_t peerId;
	SoftwareConnection::Message message;


};

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
class SyncableMessageQueue : public Syncable {
public:
	/* ============== SyncEngine functions ============== */
	// virtual void preSync(bool isMaster) override;
	virtual void encode(SyncBuffer* syncBuffer) override;
	virtual void decode(SyncBuffer* syncBuffer) override;
	virtual void postSync(bool isMaster) override;
	/* ================================================== */

	/* =============== Utility functions ================ */
	void push(PeerMessage &&item);
	void push(const PeerMessage& item);

	PeerMessage pop();

	size_t size() const;

	[[nodiscard]] bool empty() const;

	PeerMessage& front();
	const PeerMessage& front() const;

	PeerMessage& back();
	const PeerMessage& back() const;

	/* ================================================== */

private:
	std::mutex _mutex;
	std::list<PeerMessage> _queue;
	std::vector<PeerMessage> _messagesToSync;

	bool showMessageEncode = true;
	bool showMessageDecode = true;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SYNCABLEMESSAGEQUEUE___H__
