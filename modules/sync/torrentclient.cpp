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

#include "torrentclient.h"

#include <libtorrent/entry.hpp>
#include <libtorrent/bencode.hpp>
#include <libtorrent/session.hpp>
#include <libtorrent/alert_types.hpp>
#include <libtorrent/torrent_info.hpp>
#include <libtorrent/magnet_uri.hpp>

#include <openspace/openspace.h>

#include <ghoul/logging/logmanager.h>

namespace {
    const char* _loggerCat = "TorrentClient";
    std::chrono::milliseconds PollInterval(200);
}

namespace openspace {

TorrentClient::TorrentClient() {}

TorrentClient::~TorrentClient() {
    _keepRunning = false;
    _torrentThread.join();
}

void TorrentClient::initialize() {
    libtorrent::settings_pack settings;

    _session = std::make_unique<libtorrent::session>();

    settings.set_str(libtorrent::settings_pack::user_agent, "OpenSpace/" +
        std::to_string(openspace::OPENSPACE_VERSION_MAJOR) + "." +
        std::to_string(openspace::OPENSPACE_VERSION_MINOR) + "." +
        std::to_string(openspace::OPENSPACE_VERSION_PATCH));

    settings.set_bool(libtorrent::settings_pack::allow_multiple_connections_per_ip, true);
    settings.set_bool(libtorrent::settings_pack::ignore_limits_on_local_network, true);
    settings.set_int(libtorrent::settings_pack::connection_speed, 20);
    settings.set_int(libtorrent::settings_pack::active_downloads, -1);
    settings.set_int(libtorrent::settings_pack::active_seeds, -1);
    settings.set_int(libtorrent::settings_pack::active_limit, 30);
    settings.set_int(libtorrent::settings_pack::dht_announce_interval, 60);
    _session->apply_settings(settings);

    _session->add_dht_router({ "router.utorrent.com", 6881 });
    _session->add_dht_router({ "dht.transmissionbt.com", 6881 });
    _session->add_dht_router({ "router.bittorrent.com", 6881 });
    _session->add_dht_router({ "router.bitcomet.com", 6881 });

    libtorrent::error_code ec;
    _session->listen_on(std::make_pair(20280, 20290), ec);
    _session->start_upnp();

    _torrentThread = std::thread([this]() {
        while (_keepRunning) {
            pollAlerts();
            std::this_thread::sleep_for(PollInterval);
        }
    });
}

void TorrentClient::pollAlerts() {
    std::vector<libtorrent::alert*> alerts;
    _session->pop_alerts(&alerts);

    for (lt::alert const* a : alerts) {
        //LINFO(a->message());
        // if we receive the finished alert or an error, we're done
        if (lt::alert_cast<lt::torrent_finished_alert>(a)) {
            //LINFO(a->message());
        }
        if (lt::alert_cast<lt::torrent_error_alert>(a)) {
            //LINFO(a->message());
        }
    }
}

size_t TorrentClient::addTorrentFile(std::string torrentFile, std::string destination) {
    if (!_session) {
        LERROR("Torrent session not initialized when adding torrent");
        return -1;
    }
    libtorrent::error_code ec;
    libtorrent::add_torrent_params p;

    p.save_path = destination;
    p.ti = std::make_shared<libtorrent::torrent_info>(torrentFile, ec, 0);

    libtorrent::torrent_handle h = _session->add_torrent(p, ec);
    if (ec) {
        LERROR(torrentFile << ": " << ec.message());
    }

    size_t id = _nextId++;
    _torrents.emplace(id, Torrent{id, h});
    return id;
}

size_t TorrentClient::addMagnetLink(std::string magnetLink, std::string destination) {
    if (!_session) {
        LERROR("Torrent session not initialized when adding torrent");
        return -1;
    }
    libtorrent::error_code ec;
    libtorrent::add_torrent_params p = libtorrent::parse_magnet_uri(magnetLink, ec);
    p.save_path = destination;
    p.storage_mode = libtorrent::storage_mode_allocate;

    libtorrent::torrent_handle h = _session->add_torrent(p, ec);
    if (ec) {
        LERROR(magnetLink << ": " << ec.message());
    }

    size_t id = _nextId++;
    _torrents.emplace(id, Torrent{id, h});
    return id;
}

void TorrentClient::removeTorrent(size_t id) {
}


}
