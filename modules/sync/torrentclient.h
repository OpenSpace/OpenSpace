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

#ifndef __OPENSPACE_MODULE_SYNC___TORRENTCLIENT___H__
#define __OPENSPACE_MODULE_SYNC___TORRENTCLIENT___H__

#include <atomic>
#include <string>
#include <memory>
#include <thread>
#include <unordered_map>

#include "libtorrent/torrent_handle.hpp"

namespace libtorrent {
    class session;
}

namespace openspace {



class TorrentClient {
public:
    struct Torrent {
        size_t id;
        libtorrent::torrent_handle handle;
    };

    TorrentClient();
    ~TorrentClient();
    void initialize();
    size_t addTorrentFile(std::string torrentFile, std::string destination);
    size_t addMagnetLink(std::string magnetLink, std::string destination);
    void removeTorrent(size_t id);
    void pollAlerts();
private:
    size_t _nextId = 0;
    std::unordered_map<size_t, Torrent> _torrents;
    std::unique_ptr<libtorrent::session> _session;
    std::thread _torrentThread;
    std::atomic_bool _keepRunning = true;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SYNC___TORRENTCLIENT___H__
