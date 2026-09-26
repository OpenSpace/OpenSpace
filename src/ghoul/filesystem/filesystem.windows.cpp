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

#ifdef WIN32

#include <ghoul/format.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/exception.h>
#include <cstdlib>
#include <filesystem>
#include <string>
#include <string_view>
#include <vector>
#include <utility>
#include <Windows.h>

namespace {
    constexpr std::string_view _loggerCat = "FileSystem";
    constexpr unsigned int ChangeBufferSize = 16384u;
} // namespace

namespace ghoul::filesystem {

int FileSystem::FileChangeInfo::NextIdentifier = 0;

struct DirectoryHandle {
    HANDLE _handle = nullptr;
    unsigned char _activeBuffer = 0;
    std::vector<BYTE> _changeBuffer[2];
    OVERLAPPED _overlappedBuffer = { 0 };
};

void FileSystem::deinitializeInternalWindows() {
    using K = std::filesystem::path;
    using V = DirectoryHandle*;
    for (const std::pair<const K, V>& d : _directories) {
        DirectoryHandle* dh = d.second;
        if (dh && dh->_handle) {
            CancelIo(dh->_handle);
            CloseHandle(dh->_handle);
        }
        delete dh;
    }
}

int FileSystem::addFileListener(std::filesystem::path path,
                                File::FileChangedCallback callback)
{
    std::filesystem::path dir = path.parent_path();
    auto f = _directories.find(dir);
    if (f == _directories.end()) {
        LDEBUG(std::format("Started watching '{}'", dir));
        DirectoryHandle* handle = new DirectoryHandle;
        handle->_activeBuffer = 0;
        handle->_handle = nullptr;

        std::string d = dir.string();
        handle->_handle = CreateFile(
            d.c_str(),
            FILE_LIST_DIRECTORY,
            FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
            nullptr,
            OPEN_EXISTING,
            FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OVERLAPPED,
            nullptr
        );

        if (handle->_handle == INVALID_HANDLE_VALUE) {
            delete handle;
            throw RuntimeError(std::format(
                "Directory handle for '{}' could not be obtained", dir
            ));
        }

        _directories[dir] = handle;
        beginRead(handle);
    }

    int idx = FileChangeInfo::NextIdentifier;

    FileChangeInfo info = {
        .identifier = idx,
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

void FileSystem::callbackHandler(DirectoryHandle* directoryHandle,
                                 const std::string& filePath)
{
    std::filesystem::path fullPath;
    using K = std::filesystem::path;
    using V = DirectoryHandle*;
    for (const std::pair<const K, V>& d : FileSys._directories) {
        if (d.second == directoryHandle) {
            fullPath = d.first / filePath;
        }
    }

    for (const FileChangeInfo& info : FileSys._trackedFiles) {
        if (info.path == fullPath) {
            info.callback();
        }
    }
}

void callbackHandler(DirectoryHandle* directoryHandle, const std::string& filePath) {
    FileSys.callbackHandler(directoryHandle, filePath);
}

void readStarter(DirectoryHandle* directoryHandle) {
    FileSys.beginRead(directoryHandle);
}

static void CALLBACK completionHandler(DWORD, DWORD, LPOVERLAPPED lpOverlapped) {
    DirectoryHandle* handle = static_cast<DirectoryHandle*>(lpOverlapped->hEvent);

    const unsigned char currentBuffer = handle->_activeBuffer;

    // Change active buffer (ping-pong buffering)
    handle->_activeBuffer = (handle->_activeBuffer + 1) % 2;

    // Restart change listener as soon as possible
    readStarter(handle);

    BYTE* buf = handle->_changeBuffer[currentBuffer].data();

    // Data might have queued up, so we need to check all changes
    while (true) {
        // Extract the information which file has changed
        FILE_NOTIFY_INFORMATION& info = reinterpret_cast<FILE_NOTIFY_INFORMATION&>(*buf);

        if (info.Action == FILE_ACTION_MODIFIED) {
            std::vector<char> currentFilenameBuffer(info.FileNameLength);

            // Convert from DWORD to char*
            size_t i;
            wcstombs_s(
                &i,
                currentFilenameBuffer.data(),
                info.FileNameLength,
                info.FileName,
                info.FileNameLength
            );
            if (i > 0) {
                // Make sure the last char is string terminating
                currentFilenameBuffer[i - 1] = '\0';
                const std::string currentFilename(currentFilenameBuffer.data(), i - 1);
                callbackHandler(handle, currentFilename);
            }
        }
        if (!info.NextEntryOffset) {
            // We are done with all entries and didn't find our file
            break;
        }
        else {
            // Continue with the next entry
            buf += info.NextEntryOffset;
        }
    }
}

void FileSystem::beginRead(DirectoryHandle* directoryHandle) {
    HANDLE handle = directoryHandle->_handle;
    const unsigned char activeBuffer = directoryHandle->_activeBuffer;
    std::vector<BYTE>* changeBuffer = directoryHandle->_changeBuffer;
    OVERLAPPED* overlappedBuffer = &directoryHandle->_overlappedBuffer;

    ZeroMemory(overlappedBuffer, sizeof(OVERLAPPED));
    overlappedBuffer->hEvent = directoryHandle;

    changeBuffer[activeBuffer].resize(ChangeBufferSize);
    ZeroMemory(changeBuffer[activeBuffer].data(), ChangeBufferSize);

    DWORD returnedBytes;
    ReadDirectoryChangesW(
        handle,
        changeBuffer[activeBuffer].data(),
        static_cast<DWORD>(changeBuffer[activeBuffer].size()),
        false,
        FILE_NOTIFY_CHANGE_LAST_WRITE | FILE_NOTIFY_CHANGE_SIZE |
        FILE_NOTIFY_CHANGE_FILE_NAME | FILE_NOTIFY_CHANGE_CREATION,
        &returnedBytes,
        overlappedBuffer,
        &completionHandler
    );
}

} // namespace ghoul::filesystem

#endif // WIN32
