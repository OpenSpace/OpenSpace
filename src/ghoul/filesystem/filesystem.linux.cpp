/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#ifndef WIN32

#include <ghoul/filesystem/filesystem.h>

#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <algorithm>
#include <cassert>
#include <cstdio>
#include <dirent.h>
#include <pwd.h>
#include <regex>
#include <unistd.h>
#include <sys/inotify.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/types.h>

namespace {
    constexpr std::string_view _loggerCat = "FileSystem";
    constexpr uint32_t Mask =
        IN_ALL_EVENTS | IN_IGNORED | IN_Q_OVERFLOW | IN_UNMOUNT | IN_ISDIR;
    constexpr int EventSize = sizeof(struct inotify_event);
    constexpr int BufferLength = 1024 * (EventSize + 16);
} // namespace

namespace ghoul::filesystem {

int FileSystem::FileChangeInfo::NextIdentifier = 0;

void FileSystem::initializeInternalLinux() {
    _inotifyHandle = inotify_init();
    _keepGoing = true;
    _t = std::thread(inotifyWatcher);
}

void FileSystem::deinitializeInternalLinux() {
    _keepGoing = false;
    if (_t.joinable()) {
        _t.join();
    }
    close(_inotifyHandle);
}

int FileSystem::addFileListener(std::filesystem::path path,
                                File::FileChangedCallback callback)
{

    const std::string filename = path.string();
    const int wd = inotify_add_watch(_inotifyHandle, filename.c_str(), Mask);

    const int idx = FileChangeInfo::NextIdentifier;

    FileChangeInfo info = {
        .identifier = idx,
        .inotifyHandle = wd,
        .path = std::move(path),
        .callback = std::move(callback)
    };
    _trackedFiles.push_back(std::move(info));

    FileChangeInfo::NextIdentifier += 1;
    return idx;
}

void FileSystem::removeFileListener(int callbackIdentifier) {
    for (size_t i = 0; i < _trackedFiles.size(); i += 1) {
        if (_trackedFiles[i].identifier == callbackIdentifier) {
            _trackedFiles.erase(_trackedFiles.begin() + i);
            return;
        }
    }

    LWARNING(std::format("Could not find callback identifier '{}'", callbackIdentifier));
}

void FileSystem::inotifyWatcher() {
    const int fd = FileSys._inotifyHandle;
    char buffer[BufferLength];
    struct timeval tv;
    tv.tv_sec = 1;
    tv.tv_usec = 0;
    fd_set rfds;
    while (FileSys._keepGoing) {
        FD_ZERO(&rfds);
        FD_SET(fd, &rfds);
        if (select(FD_SETSIZE, &rfds, nullptr, nullptr, &tv) < 1) {
            continue;
        }

        const ssize_t length = read(fd, buffer, BufferLength);
        if (length < 0) {
            continue;
        }

        long unsigned int offset = 0;
        while (offset < static_cast<long unsigned int>(length)) {
            struct inotify_event* e = reinterpret_cast<inotify_event*>(buffer + offset);
            switch (e->mask) {
                case IN_MODIFY:
                case IN_ATTRIB:
                {
                    const int wd = e->wd;
                    for (const FileChangeInfo& info : FileSys._trackedFiles) {
                        if (info.inotifyHandle == wd) {
                            info.callback();
                        }
                    }
                    break;
                }
                case IN_IGNORED:
                {
                    const int wd = e->wd;

                    // Remove tracking of the removed descriptor
                    inotify_rm_watch(fd, wd);

                    for (FileChangeInfo& info : FileSys._trackedFiles) {
                        if (info.inotifyHandle != wd) {
                            continue;
                        }

                        // Add new tracking
                        info.inotifyHandle = inotify_add_watch(
                            fd,
                            info.path.string().c_str(),
                            Mask
                        );
                    }
                    break;
                }
                default:
                    break;
            }
            offset += EventSize + e->len;
        }
    }
}

} // namespace ghoul::filesystem

#endif // WIN32
