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
#include <openspace/logging/logmanager.h>

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

    _directory = p.directory;
}

void RenderableSpatioTemporal::initialize()
{}

void RenderableSpatioTemporal::initializeGL()
{}

void RenderableSpatioTemporal::deinitializeGL()
{}

void RenderableSpatioTemporal::render(const RenderData& data, RendererTasks& rendererTask)
{}

void RenderableSpatioTemporal::update(const UpdateData& data)
{}

} // namespace openspace
