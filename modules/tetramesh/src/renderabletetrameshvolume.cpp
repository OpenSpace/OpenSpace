/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/tetramesh/include/renderabletetrameshvolume.h>
#include <openspace/documentation/documentation.h>



namespace {
    constexpr std::string_view _loggerCat = "RenderableTetraMeshVolume";


    struct [[codegen::Dictionary(RenderableTetraMeshVolume)]] Parameters {
    };

#include "renderabletetrameshvolume_codegen.cpp"
} // namespace

namespace openspace {

    documentation::Documentation RenderableTetraMeshVolume::Documentation() {
        return codegen::doc<Parameters>("tetramesh_RenderableTetraMeshVolume");
    }


    RenderableTetraMeshVolume::RenderableTetraMeshVolume(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary)
    {

    }

    void RenderableTetraMeshVolume::initializeGL()
    {
    }
    void RenderableTetraMeshVolume::deinitializeGL()
    {
    }
    bool RenderableTetraMeshVolume::isReady() const
    {
        return false;
    }
    void RenderableTetraMeshVolume::render(const RenderData& data, RendererTasks& rendererTask)
    {
    }
    void RenderableTetraMeshVolume::update(const UpdateData& data)
    {
    }
}
