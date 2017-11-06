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

#include "torrentsynchronization.h"

#include <modules/sync/syncmodule.h>

#include <openspace/documentation/verifier.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/moduleengine.h>


#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>


namespace {
    const char* _loggerCat = "TorrentSynchronization";

    const char* KeyTorrentFile = "TorrentFile";
}

namespace openspace {

TorrentSynchronization::TorrentSynchronization(const ghoul::Dictionary& dict)
    : openspace::ResourceSynchronization()
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dict,
        "TorrentSynchroniztion"
    );

    _torrentFilePath = dict.value<std::string>(KeyTorrentFile);

    // Configure synchronization based on global settings in SyncModule 
    // TODO: For testability and decreaing deps, make it possible to inject this instead.
    // For example, allow this configuration to be done by the TemplateFactory.
    SyncModule* syncModule = OsEng.moduleEngine().module<SyncModule>();
    _synchronizationRoot = syncModule->synchronizationRoot();
    _torrentClient = syncModule->torrentClient();
}

documentation::Documentation TorrentSynchronization::Documentation() {
    using namespace openspace::documentation;
    return {
        "TorrentSynchronization",
        "torrent_synchronization",
        {
            {
                KeyTorrentFile,
                new StringVerifier,
                Optional::No,
                "An absolute path to a torrent file"
            },
        }
    };
}

std::string TorrentSynchronization::directory() {
    ghoul::filesystem::Directory d(
        _synchronizationRoot +
        ghoul::filesystem::FileSystem::PathSeparator +
        "torrent" +
        ghoul::filesystem::FileSystem::PathSeparator +
        "test"
    );

    return FileSys.absPath(d);
}

void TorrentSynchronization::synchronize() {
    _torrentClient->addTorrent(_torrentFilePath, directory());



    resolve();
    return;
}

} // namespace openspace
