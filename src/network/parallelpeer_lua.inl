/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

namespace {

[[codegen::luawrap]] void joinServer(std::string port, std::string address,
                                     std::string serverName, std::string password,
                                     std::string hostpassword = "",
                                     std::string name = "Anonymous") {
    using namespace openspace;
    if (global::windowDelegate->isMaster()) {
        ParallelPeer* peer = global::parallelPeer;
        peer->setPort(std::move(port));
        peer->setAddress(std::move(address));
        peer->setPassword(std::move(password));
        peer->setHostPassword(std::move(hostpassword));
        peer->setServerName(std::move(serverName));
        peer->setName(std::move(name));
        peer->connect();
    }
}

// Connect to parallel.
[[codegen::luawrap]] void connect() {
    using namespace openspace;
    if (global::windowDelegate->isMaster()) {
        global::parallelPeer->connect();
    }
}

// Disconnect from parallel.
[[codegen::luawrap]] void disconnect() {
    using namespace openspace;
    if (global::windowDelegate->isMaster()) {
        global::parallelPeer->disconnect();
    }
}

// Request to be the host for this session.
[[codegen::luawrap]] void requestHostship() {
    using namespace openspace;
    if (global::windowDelegate->isMaster()) {
        global::parallelPeer->requestHostship();
    }
}

// Resign hostship.
[[codegen::luawrap]] void resignHostship() {
    using namespace openspace;
    if (global::windowDelegate->isMaster()) {
        global::parallelPeer->resignHostship();
    }
}

#include "parallelpeer_lua_codegen.cpp"

} // namespace
