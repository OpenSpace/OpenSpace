/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#ifndef __GHOUL___MODELREADERLUA___H__
#define __GHOUL___MODELREADERLUA___H__

#include <ghoul/io/model/modelreaderbase.h>

namespace ghoul::io {

/**
 * This model reader reads a model that is specified as a Lua table. The Lua file must
 * return a single top-level table specifying `Vertices`, `Indices`,
 * `VertexAttribPointers`, and the `Mode`. The `Vertices` and `Indices` must be flat table
 * whereas the `VertexAttribPointers` dictionary must contain tables with the `Position`,
 * `Size`, `Stride`, `Offset`, and `Normalized` arguments found in OpenGL's
 * `VertexAttribPointer` method. The default values for the `VertexAttribPointer` are:
 * `Position = 0`, `Size = 0`, `Stride = 0`, `Offset = 0`, and `Normalized = false`. The
 * value for the `type` is always `GL_FLOAT`.
 *
 * The allowed values for the `Mode` are the strings: `GL_LINES`, `GL_POINTS`,
 * `GL_LINE_STRIP`, `GL_LINE_LOOP`, `GL_LINES`, `GL_LINE_STRIP_ADJACENCY`,
 * `GL_LINES_ADJACENCY`, `GL_TRIANGLE_STRIP`, `GL_TRIANGLE_FAN`, `GL_TRIANGLES`,
 * `GL_TRIANGLE_STRIP_ADJACENCY`, `GL_TRIANGLES_ADJACENCY`, or `GL_PATCHES`.
 *
 * \see https://www.opengl.org/sdk/docs/man/html/glVertexAttribPointer.xhtml
 */
class ModelReaderLua : public ModelReaderBase {
public:
    /**
     * Loads the model described as a Lua table and returns the initialized
     * VertexBufferObject.
     *
     * \param filename The Lua file that is to be read. The file must return a single Lua
     *        table
     * \return The initialized VertexBufferObject of the specified model
     *
     * \throw ModelReaderException If there was an exception loading the model
     * \pre \p filename must not be empty
     */
    virtual std::unique_ptr<opengl::VertexBufferObject> loadModel(
        const std::string& filename) const override;
};

} // namespace ghoul::io

#endif // __GHOUL___MODELREADERLUA___H__
