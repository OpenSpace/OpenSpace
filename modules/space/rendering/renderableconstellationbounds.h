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

#ifndef __OPENSPACE_MODULE_SPACE___RENDERABLECONSTELLATIONBOUNDS___H__
#define __OPENSPACE_MODULE_SPACE___RENDERABLECONSTELLATIONBOUNDS___H__

#include <modules/space/rendering/renderableconstellationsbase.h>

#include <ghoul/opengl/uniformcache.h>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

namespace documentation { struct Documentation; }

/**
 * This class renders the constellation bounds as defined in
 * http://cdsarc.u-strasbg.fr/viz-bin/Cat?cat=VI%2F49. It contains the bounds on the
 * celestial sky for the different constellations and is used to determine in which region
 * of the sky a specific object is located.
 *
 * The bounds are drawn as lines on a sphere with variable radius, set by the `_distance`
 * property. Currently, all constellation bounds are lines, which leads to artifacts if
 * the radius is very small.
 */
class RenderableConstellationBounds : public RenderableConstellationsBase {
public:
    explicit RenderableConstellationBounds(const ghoul::Dictionary& dictionary);

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& tasks) override;

    static documentation::Documentation Documentation();

private:
    /**
     * Stores the constellation bounds.
     */
    struct ConstellationBound {
        /// The abbreviation of the constellation
        std::string constellationAbbreviation;
        std::string constellationFullName;
        bool isEnabled = false;
        /// The index of the first vertex describing the bounds
        GLsizei startIndex;
        /// The number of vertices describing the bounds
        GLsizei nVertices;
    };

    /**
     * Loads the file specified in _vertexFilename and fills the _constellationBounds
     * variable, as well as the _vertexValues list. If this method fails, the content of
     * either destination is undefined.
     *
     * \return `true` if the loading succeeded, `false` otherwise
     */
    bool loadVertexFile();
    bool loadData();

    /**
     * Callback method that gets triggered when `_constellationSelection` changes.
     */
    void selectionPropertyHasChanged() override;

    /// The filename containing the constellation bounds
    properties::StringProperty _vertexFilename;

    /// Determines the color of the constellation lines
    properties::Vec3Property _color;

    /// The list of all loaded constellation bounds
    std::vector<ConstellationBound> _constellationBounds;

    std::unique_ptr<ghoul::opengl::ProgramObject> _program;

    struct Vertex {
        float x;
        float y;
        float z;
    };
    std::vector<Vertex> _vertexValues; ///< A list of all vertices of all bounds

    GLuint _vao = 0;
    GLuint _vbo = 0;
    UniformCache(campos, objpos, camrot, scaling, ViewProjection, ModelTransform, color,
        opacity) _uniformCache;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___RENDERABLECONSTELLATIONBOUNDS___H__
