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

#ifndef __OPENSPACE_MODULE_SPACE___RENDERABLECONSTELLATIONBOUNDS___H__
#define __OPENSPACE_MODULE_SPACE___RENDERABLECONSTELLATIONBOUNDS___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/selectionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <ghoul/opengl/ghoul_gl.h>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

namespace documentation { struct Documentation; }

/**
 * This class renders the constellation bounds as defined in
 * http://cdsarc.u-strasbg.fr/viz-bin/Cat?cat=VI%2F49. It contains the bounds on the
 * celestial sky for the different constellations and is used to determine in which region
 * of the sky a specific object is located.
 * The bounds are drawn as lines on a sphere with variable radius, set by the
 * <code>_distance</code> property. Currently, all constellation bounds are lines, which
 * leads to artifacts if the radius is very small.
 */
class RenderableConstellationBounds : public Renderable {
public:
    RenderableConstellationBounds(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;

    static documentation::Documentation Documentation();

private:
    /// Stores the constellation bounds
    struct ConstellationBound {
        std::string constellationAbbreviation; ///< The abbreviation of the constellation
        std::string constellationFullName;
        bool isEnabled;
        GLsizei startIndex; ///< The index of the first vertex describing the bounds
        GLsizei nVertices; ///< The number of vertices describing the bounds
    };

    /**
     * Loads the file specified in _vertexFilename and fills the _constellationBounds
     * variable, as well as the _vertexValues list. If this method fails, the content of
     * either destination is undefined.
     *
     * \return \c true if the loading succeeded, \c false otherwise
     */
    bool loadVertexFile();

    /**
     * Loads the file specified in _constellationFilename that contains the mapping
     * between abbreviations and full names of constellations.
     *
     * \return <code>true</code> if the loading succeeded, <code>false</code> otherwise
     */
    bool loadConstellationFile();

    /// Fills the <code>_constellationSelection</code> property with all constellations
    void fillSelectionProperty();

    /**
     * Callback method that gets triggered when <code>_constellationSelection</code>
     * changes.
     */
    void selectionPropertyHasChanged();

    /// The filename containing the constellation bounds
    properties::StringProperty _vertexFilename;

    /// The file containing constellation names
    properties::StringProperty _constellationFilename;

    /// Determines the color of the constellation lines
    properties::Vec3Property _color;

    std::unique_ptr<ghoul::opengl::ProgramObject> _program;

    /// The list of all loaded constellation bounds
    std::vector<ConstellationBound> _constellationBounds;

    struct Vertex {
        float x;
        float y;
        float z;
    };
    std::vector<Vertex> _vertexValues; ///< A list of all vertices of all bounds

    /// The property that stores all indices of constellations that should be drawn
    properties::SelectionProperty _constellationSelection;

    GLuint _vao = 0;
    GLuint _vbo = 0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___RENDERABLECONSTELLATIONBOUNDS___H__
