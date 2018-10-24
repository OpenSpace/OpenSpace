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

#ifndef __OPENSPACE_MODULE_DSN___RENDERABLECOMMUNICATIONPACKAGE___H__
#define __OPENSPACE_MODULE_DSN___RENDERABLECOMMUNICATIONPACKAGE___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

namespace documentation { struct Documentation; }

class Translation;

/**
 * This is the base class for communication lines that are sent out from an emitter
 * (such as a station or spacecraft dish). This can be used by subclasses that either
 * has preloaded data or continuously update position data. This class is responsible
 * for the rendering of the vertex buffer objects which are filled by the subclasses.
 * The buffers contain a list of LineVBOLayout objects that contains pairs of three
 * dimensional position data that determine start and endpoint of a line segment.
 *
 * Communication lines can be rendered with varying colors and line thicknesses.
 *
 * The positions for each point along the communication is provided by checking for
 * ScenegraphNode positions in OpenSpace. These positions have different origins
 * depending on if they are representing a station position or a spacecraft position.
 * The reason for this is because of precision problems occurring due to using very 
 * large numbers representing the distances in space.
 **/
class RenderableCommunicationPackage : public Renderable {


public:
    ~RenderableCommunicationPackage() = default;

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    /// The VBO layout of the vertex position 
    struct PositionVBOLayout {
        float x, y, z;
    };
    /// The VBO layout of the color
    struct ColorVBOLayout {
        float r, g, b, a;
    };

    /**
     * The render method will set up the shader information and then render the
     * information contained in the the \c _lineRenderInformation, 
	 * using the provided \p data
     * \param data The data that is necessary to render this Renderable
     */
    void render(const RenderData& data, RendererTasks& rendererTask) override;

	/// The function deciding what color to use for a signal
	 ColorVBOLayout GetSiteColor(std::string dishIdentifier);

protected:
    explicit RenderableCommunicationPackage(const ghoul::Dictionary& dictionary);

    /// Returns the documentation entries
    static documentation::Documentation Documentation();

    /// The backend storage for the vertex buffer object containing all points for the
    /// lines to be rendered; position (3 floats) color (4 floats), 
    std::vector<float> _vertexArray;

    /// The index array that is potentially used in the draw call. If this is empty, no
    /// element draw call is used.
    std::vector<unsigned int> _indexArray;

    /// The Translation object that provides the position of the individual trail points
    std::unique_ptr<Translation> _translation;

    /// The RenderInformation contains information filled in by the concrete subclasses to
    /// be used by this class.
    struct RenderInformation {
        /// The first element in the vertex buffer to be rendered
        GLint first = 0;
        /// The number of values to be rendered
        GLsizei count = 0;
        /// Local model matrix, for spacecraft, dependant on focusnode, used for rendering in camera space
        glm::dmat4 _localTransformSpacecraft = glm::dmat4(1.0);
        /// The vertex array object for this RenderInformation
        GLuint _vaoID = 0;
        /// The main vertex buffer object
        GLuint _vBufferID = 0;
    };

    /// Set of information about the main rendering parts
    RenderInformation _lineRenderInformation;

	/// Specifies the base color of the site lines
	properties::Vec3Property _madridLineColor;
	properties::Vec3Property _goldstoneLineColor;
	properties::Vec3Property _canberraLineColor;

    /// The attribute location for vertex position
    const GLuint _locVer = 0;
    /// The attribute location for vertex color
    const GLuint _locCol = 1;
    /// Specifies the number of components per generic vertex attribute
    const GLuint _sizeColorVal = 4;
    const GLuint _sizePosVal = 3;

private:

    /// Line width for the line rendering part
    properties::FloatProperty _lineWidth;
    /// Program object used to render the data stored in RenderInformation
    ghoul::opengl::ProgramObject* _programObject = nullptr;

    UniformCache(modelViewStation, modelViewSpacecraft, projection) _uniformCache;

	enum SiteEnum {
		GoldStone = 0,
		Madrid,
		Canberra
	};

	// Key Value map of stations and their sites
	const std::map<std::string, SiteEnum> StationToSiteConversion = {
	{ "DSS14", GoldStone },
	{ "DSS24", GoldStone },
	{ "DSS25", GoldStone },
	{ "DSS26", GoldStone },
	{ "DSS43", Canberra },
	{ "DSS34", Canberra },
	{ "DSS35", Canberra },
	{ "DSS36", Canberra },
	{ "DSS63", Madrid },
	{ "DSS65", Madrid },
	{ "DSS54", Madrid },
	{ "DSS55", Madrid }
	};

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_DSN___RENDERABLECOMMUNICATIONPACKAGE___H__
