/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/base/dashboard/dashboarditemparallelconnection.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/network/parallelconnection.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/distanceconversion.h>
#include <ghoul/font/font.h>
#include <ghoul/misc/profiling.h>

namespace {
    // This `DashboardItem` displays information about the status of the parallel
    // connection, which is whether OpenSpace is directly connected to other OpenSpace
    // instances and can either control those instances or be controlled by the master of
    // the session. If OpenSpace is not connected, this `DashboardItem` will not display
    // anything.
    //
    // The information presented contains how many clients are connected to the same
    // session and whether this machine is currently the host of the session.
    struct [[codegen::Dictionary(DashboardItemParallelConnection)]] Parameters {};
#include "dashboarditemparallelconnection_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardItemParallelConnection::Documentation() {
    return codegen::doc<Parameters>(
        "base_dashboarditem_parallelconnection",
        DashboardTextItem::Documentation()
    );
}

DashboardItemParallelConnection::DashboardItemParallelConnection(
                                                      const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary)
{}

void DashboardItemParallelConnection::update() {
    ZoneScoped;

    const ParallelConnection::Status status = global::parallelPeer->status();
    const size_t nConnections = global::parallelPeer->nConnections();
    const std::string& hostName = global::parallelPeer->hostName();

    int nClients = static_cast<int>(nConnections);
    if (status == ParallelConnection::Status::Host) {
        nClients--;
        constexpr std::string_view Singular = "Hosting session with {} client";
        constexpr std::string_view Plural = "Hosting session with {} clients";

        _buffer =
            (nClients == 1) ?
            std::format(Singular, nClients) :
            std::format(Plural, nClients);
    }
    else if (status == ParallelConnection::Status::ClientWithHost) {
        nClients--;
        _buffer = "Session hosted by '" + hostName + "'";
    }
    else if (status == ParallelConnection::Status::ClientWithoutHost) {
        _buffer = "Host is disconnected";
    }

    if (status == ParallelConnection::Status::ClientWithHost ||
        status == ParallelConnection::Status::ClientWithoutHost)
    {
        _buffer += "\n";

        if (nClients > 2) {
            constexpr std::string_view Plural = "You and {} more clients are tuned in";
            _buffer += std::format(Plural, nClients - 1);
        }
        else if (nClients == 2) {
            constexpr std::string_view Singular = "You and {} more client are tuned in";
            _buffer += std::format(Singular, nClients - 1);
        }
        else if (nClients == 1) {
            _buffer += "You are the only client";
        }
    }
}

} // namespace openspace
