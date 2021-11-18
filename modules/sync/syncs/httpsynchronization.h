/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_MODULE_SYNC___HTTPSYNCHRONIZATION___H__
#define __OPENSPACE_MODULE_SYNC___HTTPSYNCHRONIZATION___H__

#include <openspace/util/resourcesynchronization.h>

#include <thread>
#include <vector>

namespace openspace {

/**
 * A concreate ResourceSynchronization that will request a list of files from a central
 * server (the server list is provided in the constructor) by asking for a specific
 * identifier and a file version and application version addition. The server is expected
 * to return a flat list of files that can be then directly downloaded into the #directory
 * of this synchronization. That list of files can have empty lines and commented out
 * lines (starting with a #).
 */
class HttpSynchronization : public ResourceSynchronization {
public:
    HttpSynchronization(const ghoul::Dictionary& dict,
        std::filesystem::path synchronizationRoot,
        std::vector<std::string> synchronizationRepositories);

    virtual ~HttpSynchronization();

    std::filesystem::path directory() const override;
    void start() override;
    void cancel() override;

    size_t nSynchronizedBytes() const override;
    size_t nTotalBytes() const override;
    bool nTotalBytesIsKnown() const override;

    static documentation::Documentation Documentation();

private:
    bool trySyncFromUrl(std::string url);

    std::atomic_bool _nTotalBytesKnown = false;
    std::atomic_size_t _nTotalBytes = 0;
    std::atomic_size_t _nSynchronizedBytes = 0;
    std::atomic_bool _shouldCancel = false;

    int _version = -1;
    const std::filesystem::path _syncRoot;
    const std::vector<std::string> _syncRepositories;

    std::thread _syncThread;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SYNC___HTTPSYNCHRONIZATION___H__
