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

#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

#include <openspace/interaction/sessionrecording.h>
#include <ghoul/filesystem/filesystem.h>
#include <filesystem>

using namespace openspace::interaction;

namespace {
    std::filesystem::path test(std::string_view file) {
        return absPath(std::format("${{TESTDIR}}/sessionrecording/{}", file));
    }
} // namespace

TEST_CASE("SessionRecording: Version 01.00 Ascii Windows", "[sessionrecording]") {
    SessionRecording rec = loadSessionRecording(test("0100_ascii_windows.osrectxt"));
    REQUIRE(rec.entries.size() == 6);

    {
        const SessionRecording::Entry& e = rec.entries[0];
        CHECK(e.timestamp == 0.0);
        CHECK(e.simulationTime == 100.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Script>(e.value));
        const auto& script = std::get<SessionRecording::Entry::Script>(e.value);
        CHECK(script == "openspace.time.setPause(false)");
    }
    {
        const SessionRecording::Entry& e = rec.entries[1];
        CHECK(e.timestamp == 1.0);
        CHECK(e.simulationTime == 101.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(camera.position == glm::dvec3(101.123, 201.456, 301.789));
        CHECK(camera.rotation == glm::quat(0.82f, 0.06f, 0.26f, 0.49f));
        CHECK(camera.scale == 1.26e-03f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[2];
        CHECK(e.timestamp == 2.0);
        CHECK(e.simulationTime == 102.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(camera.position == glm::dvec3(102.123, 202.456, 302.789));
        CHECK(camera.rotation == glm::quat(0.84f, 0.12f, 0.19f, 0.48f));
        CHECK(camera.scale == 1.25e-03f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[3];
        CHECK(e.timestamp == 4.0);
        CHECK(e.simulationTime == 104.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Script>(e.value));
        const auto& script = std::get<SessionRecording::Entry::Script>(e.value);
        CHECK(
            script ==
            "openspace.setPropertyValueSingle(\"Scene.Earth.Renderable.Fade\", 1.0)"
        );
    }
    {
        const SessionRecording::Entry& e = rec.entries[4];
        CHECK(e.timestamp == 4.0);
        CHECK(e.simulationTime == 104.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(camera.position == glm::dvec3(104.123, 204.456, 304.789));
        CHECK(camera.rotation == glm::quat(0.85f, 0.30f, -0.01f, 0.41f));
        CHECK(camera.scale == 1.27e-03f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[5];
        CHECK(e.timestamp == 5.0);
        CHECK(e.simulationTime == 105.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(camera.position == glm::dvec3(105.123, 205.456, 305.789));
        CHECK(camera.rotation == glm::quat(0.85f, 0.31f, -0.02f, 0.41f));
        CHECK(camera.scale == 1.28e-03f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
}

TEST_CASE("SessionRecording: Version 01.00 Ascii Windows Roundtrip", "[sessionrecording]")
{
    SessionRecording rec = loadSessionRecording(test("0100_ascii_windows.osrectxt"));
    saveSessionRecording(absPath("${TEMPORARY}/ascii"), rec, DataMode::Ascii);
    saveSessionRecording(absPath("${TEMPORARY}/binary"), rec, DataMode::Binary);
    SessionRecording a = loadSessionRecording(absPath("${TEMPORARY}/ascii"));
    SessionRecording b = loadSessionRecording(absPath("${TEMPORARY}/binary"));

    CHECK(rec == a);
    CHECK(rec == b);
    CHECK(a == b);
}

TEST_CASE("SessionRecording: Version 02.00 Ascii Windows", "[sessionrecording]") {
    SessionRecording rec = loadSessionRecording(test("0200_ascii_windows.osrectxt"));
    REQUIRE(rec.entries.size() == 6);

    {
        const SessionRecording::Entry& e = rec.entries[0];
        CHECK(e.timestamp == 0.0);
        CHECK(e.simulationTime == 100.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Script>(e.value));
        const auto& script = std::get<SessionRecording::Entry::Script>(e.value);
        CHECK(script == "openspace.time.setPause(false)");
    }
    {
        const SessionRecording::Entry& e = rec.entries[1];
        CHECK(e.timestamp == 1.0);
        CHECK(e.simulationTime == 101.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(camera.position == glm::dvec3(101.123, 201.456, 301.789));
        CHECK(camera.rotation == glm::quat(0.82f, 0.06f, 0.26f, 0.49f));
        CHECK(camera.scale == 1.26e-03f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[2];
        CHECK(e.timestamp == 2.0);
        CHECK(e.simulationTime == 102.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(camera.position == glm::dvec3(102.123, 202.456, 302.789));
        CHECK(camera.rotation == glm::quat(0.84f, 0.12f, 0.19f, 0.48f));
        CHECK(camera.scale == 1.25e-03f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[3];
        CHECK(e.timestamp == 4.0);
        CHECK(e.simulationTime == 104.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Script>(e.value));
        const auto& script = std::get<SessionRecording::Entry::Script>(e.value);
        CHECK(
            script ==
            "openspace.setPropertyValueSingle(\"Scene.Earth.Renderable.Fade\", 1.0)"
        );
    }
    {
        const SessionRecording::Entry& e = rec.entries[4];
        CHECK(e.timestamp == 4.0);
        CHECK(e.simulationTime == 104.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(camera.position == glm::dvec3(104.123, 204.456, 304.789));
        CHECK(camera.rotation == glm::quat(0.85f, 0.30f, -0.01f, 0.41f));
        CHECK(camera.scale == 1.27e-03f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[5];
        CHECK(e.timestamp == 5.0);
        CHECK(e.simulationTime == 105.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(camera.position == glm::dvec3(105.123, 205.456, 305.789));
        CHECK(camera.rotation == glm::quat(0.85f, 0.31f, -0.02f, 0.41f));
        CHECK(camera.scale == 1.28e-03f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
}

TEST_CASE("SessionRecording: Version 02.00 Ascii Windows Roundtrip", "[sessionrecording]")
{
    SessionRecording rec = loadSessionRecording(test("0200_ascii_windows.osrectxt"));
    saveSessionRecording(absPath("${TEMPORARY}/ascii"), rec, DataMode::Ascii);
    saveSessionRecording(absPath("${TEMPORARY}/binary"), rec, DataMode::Binary);
    SessionRecording a = loadSessionRecording(absPath("${TEMPORARY}/ascii"));
    SessionRecording b = loadSessionRecording(absPath("${TEMPORARY}/binary"));

    CHECK(rec == a);
    CHECK(rec == b);
    CHECK(a == b);
}

TEST_CASE("SessionRecording: Version 01.00 <-> 02.00 Ascii Windows", "[sessionrecording]")
{
    SessionRecording v0100 = loadSessionRecording(test("0100_ascii_windows.osrectxt"));
    SessionRecording v0200 = loadSessionRecording(test("0200_ascii_windows.osrectxt"));
    CHECK(v0100 == v0200);
}

TEST_CASE("SessionRecording: Version 01.00 Binary Windows", "[sessionrecording]") {
    SessionRecording rec = loadSessionRecording(test("0100_binary_windows.osrec"));
    REQUIRE(rec.entries.size() == 6);

    {
        const SessionRecording::Entry& e = rec.entries[0];
        CHECK(e.timestamp == 0.0);
        CHECK(e.simulationTime == 100.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Script>(e.value));
        const auto& script = std::get<SessionRecording::Entry::Script>(e.value);
        CHECK(script == "openspace.time.setPause(false)");
    }
    {
        const SessionRecording::Entry& e = rec.entries[1];
        CHECK(e.timestamp == 1.0);
        CHECK(e.simulationTime == 101.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(camera.position == glm::dvec3(101.123, 201.456, 301.789));
        CHECK(camera.rotation == glm::quat(0.82f, 0.06f, 0.26f, 0.49f));
        CHECK(camera.scale == 1.26e-03f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[2];
        CHECK(e.timestamp == 2.0);
        CHECK(e.simulationTime == 102.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(camera.position == glm::dvec3(102.123, 202.456, 302.789));
        CHECK(camera.rotation == glm::quat(0.84f, 0.12f, 0.19f, 0.48f));
        CHECK(camera.scale == 1.25e-03f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[3];
        CHECK(e.timestamp == 4.0);
        CHECK(e.simulationTime == 104.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Script>(e.value));
        const auto& script = std::get<SessionRecording::Entry::Script>(e.value);
        CHECK(
            script ==
            "openspace.setPropertyValueSingle(\"Scene.Earth.Renderable.Fade\", 1.0)"
        );
    }
    {
        const SessionRecording::Entry& e = rec.entries[4];
        CHECK(e.timestamp == 4.0);
        CHECK(e.simulationTime == 104.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(camera.position == glm::dvec3(104.123, 204.456, 304.789));
        CHECK(camera.rotation == glm::quat(0.85f, 0.30f, -0.01f, 0.41f));
        CHECK(camera.scale == 1.27e-03f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[5];
        CHECK(e.timestamp == 5.0);
        CHECK(e.simulationTime == 105.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(camera.position == glm::dvec3(105.123, 205.456, 305.789));
        CHECK(camera.rotation == glm::quat(0.85f, 0.31f, -0.02f, 0.41f));
        CHECK(camera.scale == 1.28e-03f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
}

TEST_CASE(
    "SessionRecording: Version 01.00 Binary Windows Roundtrip",
    "[sessionrecording]"
)
{
    SessionRecording rec = loadSessionRecording(test("0100_binary_windows.osrec"));
    saveSessionRecording(absPath("${TEMPORARY}/ascii"), rec, DataMode::Ascii);
    saveSessionRecording(absPath("${TEMPORARY}/binary"), rec, DataMode::Binary);
    SessionRecording a = loadSessionRecording(absPath("${TEMPORARY}/ascii"));
    SessionRecording b = loadSessionRecording(absPath("${TEMPORARY}/binary"));

    CHECK(rec == a);
    CHECK(rec == b);
    CHECK(a == b);
}

TEST_CASE("SessionRecording: Version 02.00 Binary Windows", "[sessionrecording]") {
    SessionRecording rec = loadSessionRecording(test("0200_binary_windows.osrec"));
    REQUIRE(rec.entries.size() == 6);

    {
        const SessionRecording::Entry& e = rec.entries[0];
        CHECK(e.timestamp == 0.0);
        CHECK(e.simulationTime == 100.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Script>(e.value));
        const auto& script = std::get<SessionRecording::Entry::Script>(e.value);
        CHECK(script == "openspace.time.setPause(false)");
    }
    {
        const SessionRecording::Entry& e = rec.entries[1];
        CHECK(e.timestamp == 1.0);
        CHECK(e.simulationTime == 101.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(camera.position == glm::dvec3(101.123, 201.456, 301.789));
        CHECK(camera.rotation == glm::quat(0.82f, 0.06f, 0.26f, 0.49f));
        CHECK(camera.scale == 1.26e-03f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[2];
        CHECK(e.timestamp == 2.0);
        CHECK(e.simulationTime == 102.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(camera.position == glm::dvec3(102.123, 202.456, 302.789));
        CHECK(camera.rotation == glm::quat(0.84f, 0.12f, 0.19f, 0.48f));
        CHECK(camera.scale == 1.25e-03f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[3];
        CHECK(e.timestamp == 4.0);
        CHECK(e.simulationTime == 104.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Script>(e.value));
        const auto& script = std::get<SessionRecording::Entry::Script>(e.value);
        CHECK(
            script ==
            "openspace.setPropertyValueSingle(\"Scene.Earth.Renderable.Fade\", 1.0)"
        );
    }
    {
        const SessionRecording::Entry& e = rec.entries[4];
        CHECK(e.timestamp == 4.0);
        CHECK(e.simulationTime == 104.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(camera.position == glm::dvec3(104.123, 204.456, 304.789));
        CHECK(camera.rotation == glm::quat(0.85f, 0.30f, -0.01f, 0.41f));
        CHECK(camera.scale == 1.27e-03f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[5];
        CHECK(e.timestamp == 5.0);
        CHECK(e.simulationTime == 105.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(camera.position == glm::dvec3(105.123, 205.456, 305.789));
        CHECK(camera.rotation == glm::quat(0.85f, 0.31f, -0.02f, 0.41f));
        CHECK(camera.scale == 1.28e-03f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
}

TEST_CASE(
    "SessionRecording: Version 02.00 Binary Windows Roundtrip",
    "[sessionrecording]"
)
{
    SessionRecording rec = loadSessionRecording(test("0200_binary_windows.osrec"));
    saveSessionRecording(absPath("${TEMPORARY}/ascii"), rec, DataMode::Ascii);
    saveSessionRecording(absPath("${TEMPORARY}/binary"), rec, DataMode::Binary);
    SessionRecording a = loadSessionRecording(absPath("${TEMPORARY}/ascii"));
    SessionRecording b = loadSessionRecording(absPath("${TEMPORARY}/binary"));

    CHECK(rec == a);
    CHECK(rec == b);
    CHECK(a == b);
}

TEST_CASE(
    "SessionRecording: Version 01.00 <-> 02.00 Binary Windows",
    "[sessionrecording]"
)
{
    SessionRecording v0100 = loadSessionRecording(test("0100_binary_windows.osrec"));
    SessionRecording v0200 = loadSessionRecording(test("0200_binary_windows.osrec"));
    CHECK(v0100 == v0200);
}
