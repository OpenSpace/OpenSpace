/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <catch2/catch_test_macros.hpp>

#include <openspace/misc/dictionary.h>
#include <openspace/topic/serverinterface.h>
#include <memory>
#include <string>

using namespace openspace;

namespace {
    // Lists are stored as tables with consecutive integer keys starting at 1, as Lua does
    Dictionary list(const std::string& value) {
        Dictionary d;
        d.setValue("1", value);
        return d;
    }

    std::unique_ptr<ServerInterface> createInterface(const std::string& defaultAccess) {
        Dictionary d;
        d.setValue("Identifier", std::string("TestInterface"));
        d.setValue("Type", std::string("TcpSocket"));
        d.setValue("Port", 0);
        // The interface is not enabled so that no socket is opened by the tests
        d.setValue("Enabled", false);
        d.setValue("DefaultAccess", defaultAccess);
        d.setValue("AllowAddresses", list("10.0.0.1"));
        d.setValue("DenyAddresses", list("10.0.0.2"));
        d.setValue("RequirePasswordAddresses", list("10.0.0.3"));
        d.setValue("Password", std::string("secret"));
        return ServerInterface::createFromDictionary(d);
    }
} // namespace

TEST_CASE("ServerInterface: Default access deny", "[serverinterface]") {
    const std::unique_ptr<ServerInterface> si = createInterface("Deny");

    // Explicitly allowed addresses do not need a password
    CHECK_FALSE(si->clientIsBlocked("10.0.0.1"));
    CHECK(si->clientHasAccessWithoutPassword("10.0.0.1"));

    // Explicitly denied addresses are blocked
    CHECK(si->clientIsBlocked("10.0.0.2"));
    CHECK_FALSE(si->clientHasAccessWithoutPassword("10.0.0.2"));

    // Addresses requiring a password can connect, but have to provide it
    CHECK_FALSE(si->clientIsBlocked("10.0.0.3"));
    CHECK_FALSE(si->clientHasAccessWithoutPassword("10.0.0.3"));

    // All other addresses are blocked
    CHECK(si->clientIsBlocked("10.0.0.4"));
    CHECK_FALSE(si->clientHasAccessWithoutPassword("10.0.0.4"));
}

TEST_CASE("ServerInterface: Default access require password", "[serverinterface]") {
    const std::unique_ptr<ServerInterface> si = createInterface("RequirePassword");

    CHECK_FALSE(si->clientIsBlocked("10.0.0.1"));
    CHECK(si->clientHasAccessWithoutPassword("10.0.0.1"));

    CHECK(si->clientIsBlocked("10.0.0.2"));
    CHECK_FALSE(si->clientHasAccessWithoutPassword("10.0.0.2"));

    CHECK_FALSE(si->clientIsBlocked("10.0.0.3"));
    CHECK_FALSE(si->clientHasAccessWithoutPassword("10.0.0.3"));

    // All other addresses can connect, but have to provide the password
    CHECK_FALSE(si->clientIsBlocked("10.0.0.4"));
    CHECK_FALSE(si->clientHasAccessWithoutPassword("10.0.0.4"));
}

TEST_CASE("ServerInterface: Default access allow", "[serverinterface]") {
    const std::unique_ptr<ServerInterface> si = createInterface("Allow");

    CHECK_FALSE(si->clientIsBlocked("10.0.0.1"));
    CHECK(si->clientHasAccessWithoutPassword("10.0.0.1"));

    // Explicitly denied addresses stay blocked even if access is allowed by default
    CHECK(si->clientIsBlocked("10.0.0.2"));
    CHECK_FALSE(si->clientHasAccessWithoutPassword("10.0.0.2"));

    // All other addresses can connect without a password
    CHECK_FALSE(si->clientIsBlocked("10.0.0.4"));
    CHECK(si->clientHasAccessWithoutPassword("10.0.0.4"));
}

TEST_CASE("ServerInterface: Password", "[serverinterface]") {
    const std::unique_ptr<ServerInterface> si = createInterface("Deny");
    CHECK(si->password() == "secret");
}
