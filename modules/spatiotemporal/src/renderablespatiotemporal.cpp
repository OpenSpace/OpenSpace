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

#include <modules/spatiotemporal/include/renderablespatiotemporal.h>

#include <openspace/documentation/documentation.h>
#include <openspace/filesystem/filesystem.h>
#include <openspace/glm.h>
#include <openspace/io/texture/texturereader.h>
#include <openspace/logging/logmanager.h>
#include <openspace/util/spicemanager.h>

namespace {
    constexpr std::string_view _loggerCat = "RenderableSpatioTemporalTree";

    // Test of spatio temporal renderable
    struct [[codegen::Dictionary(RenderableSpatioTemporal)]] Parameters {
        // The directory where each file is an image of same size where each image is
        // a different time step.
        std::filesystem::path directory [[codegen::directory()]];
    };
} // namespace
#include "renderablespatiotemporal_codegen.cpp"

namespace openspace {

Documentation RenderableSpatioTemporal::Documentation() {
    return codegen::doc<Parameters>(
        "spatiotemporal_renderable_spatiotemporal",
        Renderable::Documentation()
    );
}

RenderableSpatioTemporal::RenderableSpatioTemporal(const Dictionary& dictionary)
    : Renderable(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    _directory = absPath(p.directory);
}

void RenderableSpatioTemporal::initialize()
{}

void RenderableSpatioTemporal::initializeGL() {
    glm::uvec3 prevDimensions = glm::uvec3(0);

    for (const auto& entry : std::filesystem::directory_iterator(_directory)) {
        if (entry.is_regular_file())
        {
            DataSlice slice;

            // Load the image file and create a texture
            std::unique_ptr<opengl::Texture> texture =
                io::texture::loadTexture(entry.path(), 2);

            // Check that the dimensions of the texture are consistent with previous textures
            glm::uvec3 dimensions = texture->dimensions();
            if (prevDimensions != glm::uvec3(0)) {
                if (dimensions != prevDimensions) {
                    throw std::runtime_error("Inconsistent texture dimensions");
                }
            }
            prevDimensions = texture->dimensions();

            slice.texture = std::move(texture);

            // Convert the filename to a time value
            slice.time = SpiceManager::ref().ephemerisTimeFromDate(
                entry.path().filename().string()
            );

            _dataSlices.push_back(std::move(slice));
        }
    }

    _tree = buildTreeFromDataSlices(_dataSlices);
}

void RenderableSpatioTemporal::deinitializeGL()
{}

void RenderableSpatioTemporal::render(const RenderData& data, RendererTasks& rendererTask)
{}

void RenderableSpatioTemporal::update(const UpdateData& data)
{}

SpatioTemporalTree RenderableSpatioTemporal::buildTreeFromDataSlices(
                                                     const std::vector<DataSlice>& slices)
{
    if (slices.empty()) {
        throw std::runtime_error("No data slices provided");
    }

    glm::uvec2 dimensions = slices.front().texture->dimensions();
    int nSlices = static_cast<int>(slices.size());
    return SpatioTemporalTree(dimensions, nSlices);
}

} // namespace openspace
