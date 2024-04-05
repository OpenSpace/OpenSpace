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

#include <catch2/catch_test_macros.hpp>

#include <modules/volume/rawvolume.h>
#include <modules/volume/rawvolumereader.h>
#include <modules/volume/rawvolumewriter.h>
#include <openspace/util/time.h>
#include <openspace/util/timeline.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/filesystem.h>

TEST_CASE("RawVolumeIO: TinyInputOutput", "[rawvolumeio]") {
    using namespace openspace::volume;

    const glm::uvec3 dims = glm::uvec3(1, 1, 1);
    const float value = 0.5f;

    RawVolume<float> vol(dims);

    vol.set({ 0, 0, 0 }, value);
    CHECK(vol.get({ 0, 0, 0 }) == value);

    const std::filesystem::path volumePath = absPath("${TESTDIR}/tinyvolume.rawvolume");

    // Write the 1x1x1 volume to disk
    RawVolumeWriter<float> writer(volumePath.string());
    writer.write(vol);

    // Read the 1x1x1 volume and make sure the value is the same.
    RawVolumeReader<float> reader(volumePath.string(), dims);
    std::unique_ptr<RawVolume<float>> storedVolume = reader.read();
    CHECK(storedVolume->get({ 0, 0, 0 }) == value);
}

TEST_CASE("RawVolumeIO: BasicInputOutput", "[rawvolumeio]") {
    using namespace openspace::volume;

    const glm::uvec3 dims = glm::uvec3(2, 4, 8);
    auto value = [dims](const glm::uvec3& v) {
        return static_cast<float>(v.z * dims.z * dims.y + v.y * dims.y + v.x);
    };

    RawVolume<float> vol(dims);
    vol.forEachVoxel(
        [&vol, &value](const glm::uvec3& x, float) { vol.set(x, value(x)); }
    );
    vol.forEachVoxel([&value](const glm::uvec3& x, float v) { CHECK(v == value(x)); });

    const std::filesystem::path volumePath = absPath("${TESTDIR}/basicvolume.rawvolume");

    // Write the 2x4x8 volume to disk
    RawVolumeWriter<float> writer(volumePath.string());
    writer.write(vol);

    // Read the 2x4x8 volume and make sure the value is the same.
    RawVolumeReader<float> reader(volumePath.string(), dims);
    const std::unique_ptr<RawVolume<float>> storedVolume = reader.read();
    vol.forEachVoxel([&value](const glm::uvec3& x, float v) {
        CHECK(v == value(x));
    });
}
