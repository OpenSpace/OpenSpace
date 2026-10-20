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

#ifndef __OPENSPACE_MODULE_SPATIOTEMPORAL___RENDERABLESPATIOTEMPORAL___H__
#define __OPENSPACE_MODULE_SPATIOTEMPORAL___RENDERABLESPATIOTEMPORAL___H__

#include <openspace/rendering/renderable.h>

#include <modules/spatiotemporal/include/spatiotemporaltree.h>
#include <openspace/opengl/texture.h>
#include <filesystem>
#include <vector>

namespace openspace {
    class RenderableSpatioTemporal : public Renderable {
    public:
        explicit RenderableSpatioTemporal(const Dictionary& dictionary);
        ~RenderableSpatioTemporal() override = default;

        void initialize() override;
        void initializeGL() override;
        void deinitializeGL() override;

        void render(const RenderData& data, RendererTasks& rendererTask) override;
        void update(const UpdateData& data) override;

        static openspace::Documentation Documentation();

    private:
        struct DataSlice{
            std::unique_ptr<opengl::Texture> texture;
            double time;
        };

        SpatioTemporalTree buildTreeFromDataSlices(const std::vector<DataSlice>& slices);

        std::filesystem::path _directory;
        glm::ivec2 _texturesDimensions = glm::ivec2(0);
        std::vector<DataSlice> _dataSlices;
        SpatioTemporalTree _tree;
    };

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPATIOTEMPORAL___RENDERABLESPATIOTEMPORAL___H__
