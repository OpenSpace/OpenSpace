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

#include <modules/sync/torrentclient.h>

#include <openspace/openspace.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>

#ifdef SYNC_USE_LIBTORRENT
#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4265)
#pragma warning (disable : 4996)
#endif // _MSC_VER

#include <libtorrent/entry.hpp>
#include <libtorrent/bencode.hpp>
#include <libtorrent/session.hpp>
#include <libtorrent/alert_types.hpp>
#include <libtorrent/torrent_info.hpp>
#include <libtorrent/magnet_uri.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

#endif // SYNC_USE_LIBTORRENT

namespace {
    constexpr const char* _loggerCat = "TorrentClient";
    constexpr const std::chrono::milliseconds PollInterval(1000);
} // namespace

namespace openspace {

TorrentError::TorrentError(std::string msg)
    : RuntimeError(std::move(msg), "TorrentClient")
{}

void TorrentClient::initialize() {
#ifdef SYNC_USE_LIBTORRENT
    libtorrent::settings_pack settings;
    settings.set_str(
        libtorrent::settings_pack::user_agent,
        "OpenSpace/" + std::string(OPENSPACE_VERSION_NUMBER)
    );

    settings.set_str(
        libtorrent::settings_pack::listen_interfaces,
        "0.0.0.0:6881,0.0.0.0:20280,0.0.0.0:20285,0.0.0.0:20290"
    );
    settings.set_bool(libtorrent::settings_pack::allow_multiple_connections_per_ip, true);
    settings.set_bool(libtorrent::settings_pack::enable_upnp, true);
    //settings.set_bool(libtorrent::settings_pack::ignore_limits_on_local_network, true);
    settings.set_int(libtorrent::settings_pack::connection_speed, 20);
    settings.set_int(libtorrent::settings_pack::active_downloads, -1);
    settings.set_int(libtorrent::settings_pack::active_seeds, -1);
    settings.set_int(libtorrent::settings_pack::active_limit, 30);

    settings.set_str(
        libtorrent::settings_pack::dht_bootstrap_nodes,
        "router.utorrent.com,dht.transmissionbt.com,router.bittorrent.com,\
router.bitcomet.com"
    );
    settings.set_int(libtorrent::settings_pack::dht_announce_interval, 15);

    _session.apply_settings(settings);

    libtorrent::error_code ec;

    _isInitialized = true;
    _isActive = true;

    _torrentThread = std::thread([this]() {
        while (_isActive) {
            pollAlerts();
            std::unique_lock<std::mutex> lock(_abortMutex);
            _abortNotifier.wait_for(lock, PollInterval);
        }
    });
#endif // SYNC_USE_LIBTORRENT
}

void TorrentClient::deinitialize() {
#ifdef SYNC_USE_LIBTORRENT
    if (!_isActive) {
        return;
    }

    _isActive = false;
    _abortNotifier.notify_all();
    if (_torrentThread.joinable()) {
        _torrentThread.join();
    }

    const std::vector<lt::torrent_handle>& handles = _session.get_torrents();
    for (const lt::torrent_handle& h : handles) {
        _session.remove_torrent(h);
    }
    _torrents.clear();

    _session.abort();
    _isInitialized = false;
#endif // SYNC_USE_LIBTORRENT
}

void TorrentClient::pollAlerts() {
#ifdef SYNC_USE_LIBTORRENT
    // Libtorrent does not seem to reliably generate alerts for all added torrents.
    // To make sure that the program does not keep waiting for already finished
    // downsloads, we go through the whole list of torrents when polling.
    // However, in theory, the commented code below should be more efficient:
    /*
    std::vector<libtorrent::alert*> alerts;
    {
        std::lock_guard<std::mutex> guard(_mutex);
        _session->pop_alerts(&alerts);
    }
    for (lt::alert* a : alerts) {
        if (const lt::torrent_alert* alert =
           dynamic_cast<lt::torrent_alert*>(a))
        {
            notify(alert->handle.id());
        }
    }
    */
    std::vector<lt::torrent_handle> handles;
    {
        std::lock_guard<std::mutex> guard(_mutex);
        handles = _session.get_torrents();
    }
    for (const lt::torrent_handle& h : handles) {
        notify(h.id());
    }
#endif // SYNC_USE_LIBTORRENT
}

TorrentClient::TorrentId TorrentClient::addTorrentFile(
                                        [[ maybe_unused ]] const std::string& torrentFile,
                                          [[maybe_unused]] const std::string& destination,
                                              [[maybe_unused]] TorrentProgressCallback cb)
{
#ifdef SYNC_USE_LIBTORRENT
    std::lock_guard<std::mutex> guard(_mutex);

    if (!_isInitialized) {
        LERROR("Torrent session not initialized when adding torrent");
        return -1;
    }

    libtorrent::error_code ec;
    libtorrent::add_torrent_params p;
    p.save_path = destination;
    p.ti = std::make_shared<libtorrent::torrent_info>(torrentFile, ec);
    if (ec) {
        LERROR(fmt::format("{}: {}", torrentFile, ec.message()));
    }
    const libtorrent::torrent_handle h = _session.add_torrent(p, ec);
    if (ec) {
        LERROR(fmt::format("{}: {}", torrentFile, ec.message()));
    }

    TorrentId id = h.id();
    _torrents.emplace(id, Torrent{ id, h, std::move(cb) });
    return id;
#else // SYNC_USE_LIBTORRENT
    throw TorrentError("SyncModule is compiled without libtorrent support");
#endif // SYNC_USE_LIBTORRENT
}

TorrentClient::TorrentId TorrentClient::addMagnetLink(
                                           [[maybe_unused]] const std::string& magnetLink,
                                          [[maybe_unused]] const std::string& destination,
                                              [[maybe_unused]] TorrentProgressCallback cb)
{
#ifdef SYNC_USE_LIBTORRENT
    std::lock_guard<std::mutex> guard(_mutex);

    // TODO: register callback!
    if (!_isInitialized) {
        LERROR("Torrent session not initialized when adding torrent");
        return -1;
    }
    libtorrent::error_code ec;
    libtorrent::add_torrent_params p = libtorrent::parse_magnet_uri(magnetLink, ec);
    if (ec) {
        LERROR(fmt::format("{}: {}", magnetLink, ec.message()));
    }
    p.save_path = destination;
    p.storage_mode = libtorrent::storage_mode_allocate;
    const libtorrent::torrent_handle h = _session.add_torrent(p, ec);
    if (ec) {
        LERROR(fmt::format("{}: {}", magnetLink, ec.message()));
    }

    TorrentId id = h.id();
    _torrents.emplace(id, Torrent{ id, h, std::move(cb) });
    return id;
#else // SYNC_USE_LIBTORRENT
    throw TorrentError("SyncModule is compiled without libtorrent support");
#endif // SYNC_USE_LIBTORRENT
}

void TorrentClient::removeTorrent([[maybe_unused]] TorrentId id) {
#ifdef SYNC_USE_LIBTORRENT
    std::lock_guard<std::mutex> guard(_mutex);

    const auto it = _torrents.find(id);
    if (it == _torrents.end()) {
        return;
    }

    const libtorrent::torrent_handle h = it->second.handle;
    _session.remove_torrent(h);

    _torrents.erase(it);
#endif // SYNC_USE_LIBTORRENT
}

void TorrentClient::notify([[maybe_unused]] TorrentId id) {
#ifdef SYNC_USE_LIBTORRENT
    TorrentProgressCallback callback;
    TorrentProgress progress;

    {
        std::lock_guard<std::mutex> guard(_mutex);

        const auto it = _torrents.find(id);
        if (it == _torrents.end()) {
            return;
        }

        const libtorrent::torrent_handle h = it->second.handle;
        const libtorrent::torrent_status status = h.status();

        progress.finished = status.is_finished;
        progress.nTotalBytesKnown = status.total_wanted > 0;
        progress.nTotalBytes = status.total_wanted;
        progress.nDownloadedBytes = status.total_wanted_done;

        callback = it->second.callback;
    }

    callback(progress);
#endif // SYNC_USE_LIBTORRENT
}

} // namespace openspace
