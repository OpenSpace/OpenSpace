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

#ifndef __OPENSPACE_MODULE_SYNC___TORRENTSYNCHRONIZATION___H__
#define __OPENSPACE_MODULE_SYNC___TORRENTSYNCHRONIZATION___H__

#include <openspace/util/resourcesynchronization.h>

#include <modules/sync/torrentclient.h>

namespace openspace {

class TorrentSynchronizationJob;

class TorrentSynchronization : public ResourceSynchronization {
public:
    TorrentSynchronization(const ghoul::Dictionary& dict,
        const std::string& synchronizationRoot, TorrentClient& client);

    virtual ~TorrentSynchronization();

    std::string directory() override;
    void start() override;
    void cancel() override;
    void clear() override;

    size_t nSynchronizedBytes() override;
    size_t nTotalBytes() override;
    bool nTotalBytesIsKnown() override;

    static documentation::Documentation Documentation();

private:
    void updateTorrentProgress(TorrentClient::TorrentProgress p);
    std::string uniformResourceName() const;
    bool hasSyncFile();
    void createSyncFile();

    std::atomic_bool _enabled = false;

    TorrentClient::TorrentId _torrentId;
    TorrentClient::TorrentProgress _progress;
    std::mutex _progressMutex;
    std::string _identifier;
    std::string _magnetLink;
    std::string _synchronizationRoot;
    TorrentClient& _torrentClient;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SYNC___TORRENTSYNCHRONIZATION___H__
