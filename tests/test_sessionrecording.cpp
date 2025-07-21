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

TEST_CASE("SessionRecording: 01.00 Ascii Windows", "[sessionrecording]") {
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

TEST_CASE("SessionRecording: 01.00 Ascii Windows Roundtrip", "[sessionrecording]") {
    SessionRecording rec = loadSessionRecording(test("0100_ascii_windows.osrectxt"));
    saveSessionRecording(absPath("${TEMPORARY}/ascii"), rec, DataMode::Ascii);
    saveSessionRecording(absPath("${TEMPORARY}/binary"), rec, DataMode::Binary);
    SessionRecording a = loadSessionRecording(absPath("${TEMPORARY}/ascii"));
    SessionRecording b = loadSessionRecording(absPath("${TEMPORARY}/binary"));

    CHECK(rec == a);
    CHECK(rec == b);
    CHECK(a == b);
}

TEST_CASE("SessionRecording: 02.00 Ascii Windows", "[sessionrecording]") {
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

TEST_CASE("SessionRecording: 02.00 Ascii Windows Roundtrip", "[sessionrecording]") {
    SessionRecording rec = loadSessionRecording(test("0200_ascii_windows.osrectxt"));
    saveSessionRecording(absPath("${TEMPORARY}/ascii"), rec, DataMode::Ascii);
    saveSessionRecording(absPath("${TEMPORARY}/binary"), rec, DataMode::Binary);
    SessionRecording a = loadSessionRecording(absPath("${TEMPORARY}/ascii"));
    SessionRecording b = loadSessionRecording(absPath("${TEMPORARY}/binary"));

    CHECK(rec == a);
    CHECK(rec == b);
    CHECK(a == b);
}

TEST_CASE("SessionRecording: 01.00 <-> 02.00 Ascii Windows", "[sessionrecording]") {
    SessionRecording v0100 = loadSessionRecording(test("0100_ascii_windows.osrectxt"));
    SessionRecording v0200 = loadSessionRecording(test("0200_ascii_windows.osrectxt"));
    CHECK(v0100 == v0200);
}

TEST_CASE("SessionRecording: 01.00 Binary Windows", "[sessionrecording]") {
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

TEST_CASE("SessionRecording: 01.00 Binary Windows Roundtrip", "[sessionrecording]") {
    SessionRecording rec = loadSessionRecording(test("0100_binary_windows.osrec"));
    saveSessionRecording(absPath("${TEMPORARY}/ascii"), rec, DataMode::Ascii);
    saveSessionRecording(absPath("${TEMPORARY}/binary"), rec, DataMode::Binary);
    SessionRecording a = loadSessionRecording(absPath("${TEMPORARY}/ascii"));
    SessionRecording b = loadSessionRecording(absPath("${TEMPORARY}/binary"));

    CHECK(rec == a);
    CHECK(rec == b);
    CHECK(a == b);
}

TEST_CASE("SessionRecording: 02.00 Binary Windows", "[sessionrecording]") {
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

TEST_CASE("SessionRecording: 02.00 Binary Windows Roundtrip", "[sessionrecording]") {
    SessionRecording rec = loadSessionRecording(test("0200_binary_windows.osrec"));
    saveSessionRecording(absPath("${TEMPORARY}/ascii"), rec, DataMode::Ascii);
    saveSessionRecording(absPath("${TEMPORARY}/binary"), rec, DataMode::Binary);
    SessionRecording a = loadSessionRecording(absPath("${TEMPORARY}/ascii"));
    SessionRecording b = loadSessionRecording(absPath("${TEMPORARY}/binary"));

    CHECK(rec == a);
    CHECK(rec == b);
    CHECK(a == b);
}

TEST_CASE("SessionRecording: 01.00 <-> 02.00 Binary Windows", "[sessionrecording]") {
    SessionRecording v0100 = loadSessionRecording(test("0100_binary_windows.osrec"));
    SessionRecording v0200 = loadSessionRecording(test("0200_binary_windows.osrec"));
    CHECK(v0100 == v0200);
}

TEST_CASE("SessionRecording: 02.00 Ascii <-> Binary Windows", "[sessionrecording]") {
    SessionRecording ascii = loadSessionRecording(test("0200_ascii_windows.osrectxt"));
    SessionRecording binary = loadSessionRecording(test("0200_binary_windows.osrec"));
    CHECK(ascii == binary);
}

TEST_CASE("SessionRecording: 01.00 Ascii Linux", "[sessionrecording]") {
    SessionRecording rec = loadSessionRecording(test("0100_ascii_linux.osrectxt"));
    REQUIRE(rec.entries.size() == 6);

    {
        const SessionRecording::Entry& e = rec.entries[0];
        CHECK(e.timestamp == 0.0);
        CHECK(e.simulationTime == 762933560.401);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Script>(e.value));
        const auto& script = std::get<SessionRecording::Entry::Script>(e.value);
        CHECK(script == "openspace.setPropertyValueSingle(\"Modules.CefWebGui.Visible\", true)");
    }
    {
        const SessionRecording::Entry& e = rec.entries[1];
        CHECK(e.timestamp == 0.0107105);
        CHECK(e.simulationTime == 762933560.412);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(
            camera.position ==
            glm::dvec3(-13521714.7579869, -18987604.3886074, -1780842.2573154)
        );
        CHECK(
            camera.rotation ==
            glm::quat(0.6503710f, 0.6953145f, -0.2337213f, -0.1973068f)
        );
        CHECK(camera.scale == 0.00126470590475947f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[2];
        CHECK(e.timestamp == 0.0210999);
        CHECK(e.simulationTime == 762933560.423);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(
            camera.position ==
            glm::dvec3(-13521714.7579868, -18987604.3885993, -1780842.2573175)
        );
        CHECK(
            camera.rotation ==
            glm::quat(0.6503710f, 0.6953145f, -0.2337212f, -0.1973068f)
        );
        CHECK(camera.scale == 0.00126470590475947f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[3];
        CHECK(e.timestamp == 2.48093);
        CHECK(e.simulationTime == 762933562.877);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Script>(e.value));
        const auto& script = std::get<SessionRecording::Entry::Script>(e.value);
        CHECK(script == "openspace.navigation.flyTo(\"Mars\")");
    }
    {
        const SessionRecording::Entry& e = rec.entries[4];
        CHECK(e.timestamp == 2.48222);
        CHECK(e.simulationTime == 762933562.886);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(
            camera.position ==
            glm::dvec3(-13521714.7579865, -18987604.3886450, -1780842.2572641)
        );
        CHECK(
            camera.rotation ==
            glm::quat(0.6503708f, 0.6953144f, -0.2337212f, -0.1973068f)
        );
        CHECK(camera.scale == 0.00126470590475947f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[5];
        CHECK(e.timestamp == 2.49067);
        CHECK(e.simulationTime == 762933562.886);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(
            camera.position ==
            glm::dvec3(-13523362.1802436, -18989917.8348593, -1781059.2290947)
        );
        CHECK(
            camera.rotation ==
            glm::quat(0.6503708f, 0.6953144f, -0.2337212f, -0.1973068f)
        );
        CHECK(camera.scale == 0.00126449402887374f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
}

TEST_CASE("SessionRecording: 01.00 Ascii Linux Roundtrip", "[sessionrecording]") {
    SessionRecording rec = loadSessionRecording(test("0100_ascii_linux.osrectxt"));
    saveSessionRecording(absPath("${TEMPORARY}/ascii"), rec, DataMode::Ascii);
    saveSessionRecording(absPath("${TEMPORARY}/binary"), rec, DataMode::Binary);
    SessionRecording a = loadSessionRecording(absPath("${TEMPORARY}/ascii"));
    SessionRecording b = loadSessionRecording(absPath("${TEMPORARY}/binary"));

    CHECK(rec == a);
    CHECK(rec == b);
    CHECK(a == b);
}

TEST_CASE("SessionRecording: 01.00 Binary Linux", "[sessionrecording]") {
    SessionRecording rec = loadSessionRecording(test("0100_binary_linux.osrec"));
    REQUIRE(rec.entries.size() == 6);

    {
        const SessionRecording::Entry& e = rec.entries[0];
        CHECK(e.timestamp == 0.0);
        CHECK(e.simulationTime == 763463598.23217);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Script>(e.value));
        const auto& script = std::get<SessionRecording::Entry::Script>(e.value);
        CHECK(script == "openspace.time.setPause(false)");
    }
    {
        const SessionRecording::Entry& e = rec.entries[1];
        CHECK(e.timestamp == 0.01045432000000801);
        CHECK(e.simulationTime == 763463598.2421138);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(
            camera.position ==
            glm::dvec3(-15942288.9063634, 8217794.03633486, -14994951.65751712)
        );
        CHECK(
            camera.rotation ==
            glm::quat(
                -0.05070944130420685f,
                0.8491553664207458f,
                -0.31565767526626587f,
                -0.42038553953170776f
            )
        );
        CHECK(camera.scale == 0.0012647059047594666f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[2];
        CHECK(e.timestamp == 0.21673793700000488);
        CHECK(e.simulationTime == 763463598.44845);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(
            camera.position ==
            glm::dvec3(-15942288.906364493, 8217794.036353504, -14994951.657487243)
        );
        CHECK(
            camera.rotation ==
            glm::quat(
                -0.05070945620536804f,
                0.8491553664207458f,
                -0.31565767526626587f,
                -0.420385479927063f
            )
        );
        CHECK(camera.scale == 0.0012647059047594666f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[3];
        CHECK(e.timestamp == 9.940045733000005);
        CHECK(e.simulationTime == 763463608.1701536);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Script>(e.value));
        const auto& script = std::get<SessionRecording::Entry::Script>(e.value);
        CHECK(script == "openspace.setPropertyValueSingle(\"Scene.Earth.Renderable.Fade\",0,1)");
    }
    {
        const SessionRecording::Entry& e = rec.entries[4];
        CHECK(e.timestamp == 9.94726788300001);
        CHECK(e.simulationTime == 763463608.1800178);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(
            camera.position ==
            glm::dvec3(-15741144.947559996, 14747679.124696942, -9014411.005969524)
        );
        CHECK(
            camera.rotation ==
            glm::quat(
                0.23194797337055206f,
                -0.7898141741752625f,
                0.2626582682132721f,
                0.5033928751945496f
            )
        );
        CHECK(camera.scale == 0.001264723134227097f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
    {
        const SessionRecording::Entry& e = rec.entries[5];
        CHECK(e.timestamp == 11.485186747);
        CHECK(e.simulationTime == 763463609.7179065);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(
            camera.position ==
            glm::dvec3(-15741006.774582125, 14747877.286725827, -9014328.087388767)
        );
        CHECK(
            camera.rotation ==
            glm::quat(
                0.2319525182247162f,
                -0.7898122668266296f,
                0.26266056299209595f,
                0.5033925771713257f
            )
        );
        CHECK(camera.scale == 0.001264723134227097f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Earth");
    }
}

TEST_CASE("SessionRecording: 01.00 Binary Linux Roundtrip", "[sessionrecording]") {
    SessionRecording rec = loadSessionRecording(test("0100_binary_linux.osrec"));
    saveSessionRecording(absPath("${TEMPORARY}/ascii"), rec, DataMode::Ascii);
    saveSessionRecording(absPath("${TEMPORARY}/binary"), rec, DataMode::Binary);
    SessionRecording a = loadSessionRecording(absPath("${TEMPORARY}/ascii"));
    SessionRecording b = loadSessionRecording(absPath("${TEMPORARY}/binary"));

    CHECK(rec == a);
    CHECK(rec == b);
    CHECK(a == b);
}

TEST_CASE("SessionRecording: 02.00 Ascii Linux", "[sessionrecording]") {
    SessionRecording rec = loadSessionRecording(test("0200_ascii_linux.osrectxt"));
    REQUIRE(rec.entries.size() == 6);

    {
        const SessionRecording::Entry& e = rec.entries[0];
        CHECK(e.timestamp == 0.0);
        CHECK(e.simulationTime == 0.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Script>(e.value));
        const auto& script = std::get<SessionRecording::Entry::Script>(e.value);
        CHECK(script == "openspace.time.setPause(false);openspace.time.setDeltaTime(1);");
    }
    {
        const SessionRecording::Entry& e = rec.entries[1];
        CHECK(e.timestamp == 0.0);
        CHECK(e.simulationTime == 0.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Script>(e.value));
        const auto& script = std::get<SessionRecording::Entry::Script>(e.value);
        CHECK(script == "openspace.setPropertyValueSingle(\"Scene.hoverCircle.Renderable.Fade\", 0)");
    }
    {
        const SessionRecording::Entry& e = rec.entries[2];
        CHECK(e.timestamp == 0.002065126085653901);
        CHECK(e.simulationTime == 779267322.4886187);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(
            camera.position ==
            glm::dvec3(123389190187.6973, -987938150497.865, 346357762351.6706)
        );
        CHECK(
            camera.rotation ==
            glm::quat(-0.4365736f, -0.36478543f, 0.4468942f, -0.6903772f)
        );
        CHECK(camera.scale == 2.0395897e-08f);
        CHECK(!camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Venus");
    }
    {
        const SessionRecording::Entry& e = rec.entries[3];
        CHECK(e.timestamp == 0.004236564040184021);
        CHECK(e.simulationTime == 779267322.4907901);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(
            camera.position ==
            glm::dvec3(123389190187.6973, -987938150497.865, 346357762351.6706)
        );
        CHECK(
            camera.rotation ==
            glm::quat(-0.4365736f, -0.36478543f, 0.4468942f, -0.6903772f)
        );
        CHECK(camera.scale == 2.0395897e-08f);
        CHECK(!camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Venus");
    }
    {
        const SessionRecording::Entry& e = rec.entries[4];
        CHECK(e.timestamp == 8.958355146809481);
        CHECK(e.simulationTime == 779267331.4449104);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Script>(e.value));
        const auto& script = std::get<SessionRecording::Entry::Script>(e.value);
        CHECK(script == "openspace.setPropertyValueSingle('Scene.hoverCircle.Renderable.Fade', 0.0);");
    }
    {
        const SessionRecording::Entry& e = rec.entries[5];
        CHECK(e.timestamp == 26.381116207921878);
        CHECK(e.simulationTime == 779267348.8676736);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(
            camera.position ==
            glm::dvec3(
                62165003943664156672.0,
                482784744565750824960.0,
                1117492338753629847552.0
            )
        );
        CHECK(
            camera.rotation ==
            glm::quat(0.7107617f, -0.16281216f, -0.117395274f, -0.6741872f)
        );
        CHECK(camera.scale == 1.7638751e-17f);
        CHECK(!camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Venus");
    }
}

TEST_CASE("SessionRecording: 02.00 Ascii Linux Roundtrip", "[sessionrecording]") {
    SessionRecording rec = loadSessionRecording(test("0200_ascii_linux.osrectxt"));
    saveSessionRecording(absPath("${TEMPORARY}/ascii"), rec, DataMode::Ascii);
    saveSessionRecording(absPath("${TEMPORARY}/binary"), rec, DataMode::Binary);
    SessionRecording a = loadSessionRecording(absPath("${TEMPORARY}/ascii"));
    SessionRecording b = loadSessionRecording(absPath("${TEMPORARY}/binary"));

    CHECK(rec == a);
    CHECK(rec == b);
    CHECK(a == b);
}

TEST_CASE("SessionRecording: 02.00 Binary Linux", "[sessionrecording]") {
    SessionRecording rec = loadSessionRecording(test("0200_binary_linux.osrec"));
    REQUIRE(rec.entries.size() == 6);

    {
        const SessionRecording::Entry& e = rec.entries[0];
        CHECK(e.timestamp == 0.0);
        CHECK(e.simulationTime == 0.0);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Script>(e.value));
        const auto& script = std::get<SessionRecording::Entry::Script>(e.value);
        CHECK(script == "openspace.time.setPause(false);openspace.time.setDeltaTime(1);");
    }
    {
        const SessionRecording::Entry& e = rec.entries[1];
        CHECK(e.timestamp == 0.0029818089678883553);
        CHECK(e.simulationTime == 779267268.772417);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(
            camera.position ==
            glm::dvec3(2560146.332327013, -5779694.5689156465, -852442.7158538934)
        );
        CHECK(
            camera.rotation ==
            glm::quat(
                0.6254600882530212f,
                0.5630295276641846f,
                0.502471387386322f,
                -0.19829261302947998f
            )
        );
        CHECK(camera.scale == 0.06643116474151611f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Venus");
    }
    {
        const SessionRecording::Entry& e = rec.entries[2];
        CHECK(e.timestamp == 0.005884706974029541);
        CHECK(e.simulationTime == 779267268.7753198);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(
            camera.position ==
            glm::dvec3(2560146.3323334977, -5779694.568918271, -852442.7158528116)
        );
        CHECK(
            camera.rotation ==
            glm::quat(
                0.6254600882530212f,
                0.5630295276641846f,
                0.502471387386322f,
                -0.19829261302947998f
            )
        );
        CHECK(camera.scale == 0.06643116474151611f);
        CHECK(camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Venus");
    }
    {
        const SessionRecording::Entry& e = rec.entries[3];
        CHECK(e.timestamp == 10.153814885416068);
        CHECK(e.simulationTime == 779267278.9232514);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Script>(e.value));
        const auto& script = std::get<SessionRecording::Entry::Script>(e.value);
        CHECK(script == "openspace.setPropertyValueSingle(\"Scene.Venus.Renderable.Layers.ColorLayers.Clouds_Magellan_Combo_Utah.Fade\",0)");
    }
    {
        const SessionRecording::Entry& e = rec.entries[4];
        CHECK(e.timestamp == 10.155818674364127);
        CHECK(e.simulationTime == 779267278.9252552);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(
            camera.position ==
            glm::dvec3(-57303121.02243042, 8346999.591819763, -128851858.36139679)
        );
        CHECK(
            camera.rotation ==
            glm::quat(
                0.1360674947500229f,
                0.658237636089325f,
                -0.7229072451591492f,
                -0.16004367172718048f
            )
        );
        CHECK(camera.scale == 0.0001590096508152783f);
        CHECK(!camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Venus");
    }
    {
        const SessionRecording::Entry& e = rec.entries[5];
        CHECK(e.timestamp == 27.533842067583464);
        CHECK(e.simulationTime == 779267296.303281);
        REQUIRE(std::holds_alternative<SessionRecording::Entry::Camera>(e.value));
        const auto& camera = std::get<SessionRecording::Entry::Camera>(e.value);
        CHECK(
            camera.position ==
            glm::dvec3(-4573226225.966583, 667900806.931778, -10309469556.229485)
        );
        CHECK(
            camera.rotation ==
            glm::quat(
                0.13572217524051666f,
                0.6582902073860168f,
                -0.7229912281036377f,
                -0.15974101424217224f
            )
        );
        CHECK(camera.scale == 0.0000019040056713492959f);
        CHECK(!camera.followFocusNodeRotation);
        CHECK(camera.focusNode == "Venus");
    }
}

TEST_CASE("SessionRecording: 02.00 Binary Linux Roundtrip", "[sessionrecording]") {
    SessionRecording rec = loadSessionRecording(test("0200_binary_linux.osrec"));
    saveSessionRecording(absPath("${TEMPORARY}/ascii"), rec, DataMode::Ascii);
    saveSessionRecording(absPath("${TEMPORARY}/binary"), rec, DataMode::Binary);
    SessionRecording a = loadSessionRecording(absPath("${TEMPORARY}/ascii"));
    SessionRecording b = loadSessionRecording(absPath("${TEMPORARY}/binary"));

    CHECK(rec == a);
    CHECK(rec == b);
    CHECK(a == b);
}
