/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_CORE___RESOURCESYNCHRONIZATION___H__
#define __OPENSPACE_CORE___RESOURCESYNCHRONIZATION___H__

#include <openspace/documentation/documentation.h>
#include <openspace/util/concurrentjobmanager.h>

#include <ghoul/filesystem/directory.h>
#include <ghoul/misc/dictionary.h>

#include <unordered_map>

namespace openspace {

class ResourceSynchronization
    : public std::enable_shared_from_this<ResourceSynchronization>
{
public:
    enum class State : int {
        Unsynced,
        Syncing,
        Resolved,
        Rejected
    };

    using CallbackHandle = size_t;
    using StateChangeCallback = std::function<void(State)>;

    static documentation::Documentation Documentation();
    static std::unique_ptr<ResourceSynchronization> createFromDictionary(
        const ghoul::Dictionary& dictionary);

    ResourceSynchronization(const ghoul::Dictionary& dictionary);
    virtual ~ResourceSynchronization();
    virtual std::string directory() = 0;
    virtual void start() = 0;
    virtual void cancel() = 0;
    virtual void clear() = 0;

    virtual size_t nSynchronizedBytes() = 0;
    virtual size_t nTotalBytes() = 0;
    virtual bool nTotalBytesIsKnown() = 0;
    virtual float progress();

    State state() const;
    std::string name() const;
    bool isResolved();
    bool isRejected();
    bool isSyncing();
    CallbackHandle addStateChangeCallback(StateChangeCallback cb);
    void removeStateChangeCallback(CallbackHandle id);

protected:
    void resolve();
    void reject();
    void reset();
    void begin();

private:
    void setState(State state);

    std::string _name;
    std::atomic<State> _state;
    std::mutex _callbackMutex;
    CallbackHandle _nextCallbackId = 0;
    std::unordered_map<CallbackHandle, StateChangeCallback> _stateChangeCallbacks;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___RESOURCESYNCHRONIZATION___H__
