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

#include "gtest/gtest.h"

#include <openspace/util/timeline.h>
#include <openspace/util/time.h>

#include <modules/volume/rawvolume.h>
#include <modules/volume/rawvolumereader.h>
#include <modules/volume/rawvolumewriter.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>

class RawVolumeIoTest : public testing::Test {};

TEST_F(RawVolumeIoTest, TinyInputOutput) {
    using namespace openspace::volume;

    glm::uvec3 dims{ 1, 1, 1 };
    float value = 0.5;

    RawVolume<float> vol(dims);

    vol.set({ 0, 0, 0 }, value);
    ASSERT_EQ(vol.get({ 0, 0, 0 }), value);

    std::string volumePath = absPath("${TESTDIR}/tinyvolume.rawvolume");

    // Write the 1x1x1 volume to disk
    RawVolumeWriter<float> writer(volumePath);
    writer.write(vol);

    // Read the 1x1x1 volume and make sure the value is the same.
    RawVolumeReader<float> reader(volumePath, dims);
    std::unique_ptr<RawVolume<float>> storedVolume = reader.read();
    ASSERT_EQ(storedVolume->get({ 0, 0, 0 }), value);
}

TEST_F(RawVolumeIoTest, BasicInputOutput) {
    using namespace openspace::volume;

    glm::uvec3 dims{ 2, 4, 8 };
    auto value = [dims](glm::uvec3 v) {
        return static_cast<float>(v.z * dims.z * dims.y + v.y * dims.y + v.x);
    };

    RawVolume<float> vol(dims);
    vol.forEachVoxel([&vol, &value](glm::uvec3 x, float) { vol.set(x, value(x)); });

    vol.forEachVoxel([&value](glm::uvec3 x, float v) { ASSERT_EQ(v, value(x)); });

    std::string volumePath = absPath("${TESTDIR}/basicvolume.rawvolume");

    // Write the 2x4x8 volume to disk
    RawVolumeWriter<float> writer(volumePath);
    writer.write(vol);

    // Read the 2x4x8 volume and make sure the value is the same.
    RawVolumeReader<float> reader(volumePath, dims);
    std::unique_ptr<RawVolume<float>> storedVolume = reader.read();
    vol.forEachVoxel([&value](glm::uvec3 x, float v) {
        ASSERT_EQ(v, value(x));
    });
}
