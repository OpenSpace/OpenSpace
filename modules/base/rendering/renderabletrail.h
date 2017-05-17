/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLETRAIL___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLETRAIL___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/vec3property.h>

#include <ghoul/opengl/ghoul_gl.h>

namespace ghoul {
namespace opengl {
    class ProgramObject;
    class Texture;
} // namespace opengl
} // namespace ghoul

namespace openspace {

namespace documentation { struct Documentation; }

class Translation;

/**
 * This is the base class for a trail that is drawn behind an arbitrary object. The two
 * concreate implementations are RenderableTrailOrbit, for objects that have a (roughly)
 * repeating orbit, and RenderableTrailTrajectory, for objects that are less orbit-like.
 * The main difference between two subclasses is that RenderableTrailOrbit updates itself
 * continously, whereas RenderableTrailTrajectory precomputes the entire trail in advance.
 *
 * This class is responsible for the rendering of the vertex buffer objects which are
 * filled by the subclasses. The buffers contain a list of TrailVBOLayout objects that is
 * the three dimensional position for each point along the line.
 *
 * Trails can be rendered either as lines, as points, or a combination of both with
 * varying colors, line thicknesses, or fading settings. If trails are rendered as points,
 * the RenderInformation's \c stride parameter determines the number of points between 
 * larger points. A potential use case for this is showing the passage of time along a
 * trail by using a point separation of one hour and a subsampling of 4, you would get a
 * point every 15 minutes with every hourly point being bigger.
 *
 * The positions for each point along the trail is provided through a Translation object,
 * the type of which is specified in the dictionary that is passed to the constructor. A
 * typical implementation of Translation used for the trail would be a SpiceTranslation.
 */
class RenderableTrail : public Renderable {
public:
    ~RenderableTrail() = default;

    bool initialize() override;
    bool deinitialize() override;

    bool isReady() const override;

    /**
     * The render method will set up the shader information and then render first the
     * information contained in the the \c _primaryRenderInformation, then the optional
     * \c _floatingRenderInformation using the provided \p data
     * \param data The data that is necessary to render this Renderable
     */
    void render(const RenderData& data) override;

protected:
    explicit RenderableTrail(const ghoul::Dictionary& dictionary);

    /// Returns the documentation entries that the con
    static documentation::Documentation Documentation();
   
    /// The layout of the VBOs
    struct TrailVBOLayout {
        float x, y, z;
    };

    /// The backend storage for the vertex buffer object containing all points for this
    /// trail.
    std::vector<TrailVBOLayout> _vertexArray;

    /// The index array that is potentially used in the draw call. If this is empty, no
    /// element draw call is used.
    std::vector<unsigned int> _indexArray;

    /// The Translation object that provides the position of the individual trail points
    std::unique_ptr<Translation> _translation;

    /// The RenderInformation contains information filled in by the concrete subclasses to
    /// be used by this class. 
    struct RenderInformation {
        enum class VertexSorting {
            NewestFirst = 0,    ///< Newer vertices have a lower index than older ones
            OldestFirst,        ///< Older vertices have a lower index than newer ones
            NoSorting           ///< No ordering in the vertices; no fading applied
        };
        /// The first element in the vertex buffer to be rendered
        GLint first = 0; 
        /// The number of values to be rendered
        GLsizei count = 0;
        /// The stride between 'major' points in the array
        int stride = 1;
        /// Sorting of the vertices; required for correct fading
        VertexSorting sorting = VertexSorting::NoSorting; 

        /// Local model matrix transformation, used for rendering in camera space
        glm::dmat4 _localTransform = glm::dmat4(1.0);

        /// The vertex array object for this RenderInformation
        GLuint _vaoID = 0;
        /// The main vertex buffer object
        GLuint _vBufferID = 0;
        /// The optional index buffer object
        GLuint _iBufferID = 0;
    };

    /// Primary set of information about the main rendering parts
    RenderInformation _primaryRenderInformation;
    /// Optional render information that contains information about the last, floating
    /// part of the trail
    RenderInformation _floatingRenderInformation;

private:
    /// Specifies the base color of the line before fading
    properties::Vec3Property _lineColor;
    /// Settings that enables or disables the line fading
    properties::BoolProperty _useLineFade;
    /// Specifies a multiplicative factor that fades out the line
    properties::FloatProperty _lineFade;
    /// Line width for the line rendering part
    properties::FloatProperty _lineWidth;
    /// Point size for the point rendering part
    properties::IntProperty _pointSize;
    /// The option determining which rendering method to use
    properties::OptionProperty _renderingModes;

    /// Program object used to render the data stored in RenderInformation 
    std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLETRAIL___H__
