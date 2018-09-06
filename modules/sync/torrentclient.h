/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_SYNC___TORRENTCLIENT___H__
#define __OPENSPACE_MODULE_SYNC___TORRENTCLIENT___H__

#include <ghoul/misc/exception.h>
#include <atomic>
#include <condition_variable>
#include <functional>
#include <mutex>
#include <string>
#include <thread>
#include <unordered_map>

#ifdef SYNC_USE_LIBTORRENT
#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4265)
#pragma warning (disable : 4996)
#endif // _MSC_VER

// libtorrent defines a class with the name 'defer', which messes with out #define of the
// defer macro in ghoul/misc/defer.h
#undef defer

#include <libtorrent/torrent_handle.hpp>
#include <libtorrent/session.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER


#else // SYNC_USE_LIBTORRENT
// Dummy definition to make TorrentClient compile, these is not actually used if
// SYNC_USE_LIBTORRENT is FALSE
namespace libtorrent {
    using torrent_handle = void*;
    using session = void*;
} // namespace libtorrent

#endif // SYNC_USE_LIBTORRENT

namespace openspace {

struct TorrentError : public ghoul::RuntimeError {
    explicit TorrentError(std::string msg);
};

class TorrentClient {
public:
    struct TorrentProgress {
        bool finished = false;
        bool nTotalBytesKnown = false;
        size_t nTotalBytes = 0;
        size_t nDownloadedBytes = 0;
    };

    using TorrentProgressCallback = std::function<void(TorrentProgress)>;
    using TorrentId = int32_t;

    void initialize();
    void deinitialize();

    TorrentId addTorrentFile(const std::string& torrentFile,
        const std::string& destination, TorrentProgressCallback cb);

    TorrentId addMagnetLink(const std::string& magnetLink, const std::string& destination,
        TorrentProgressCallback cb);

    void removeTorrent(TorrentId id);

private:
    struct Torrent {
        TorrentId id;
        libtorrent::torrent_handle handle;
        TorrentProgressCallback callback;
    };

    void notify(TorrentId id);
    void pollAlerts();

    libtorrent::session _session;
    bool _isInitialized = false;

    std::atomic_bool _isActive = false;
    std::thread _torrentThread;
    std::condition_variable _abortNotifier;
    std::mutex _abortMutex;
    std::mutex _mutex;

    std::unordered_map<TorrentId, Torrent> _torrents;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SYNC___TORRENTCLIENT___H__
