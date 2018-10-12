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
 * This is the base class for communication packages that are sent out from an emitter
 * (such as a station or spacecraft dish). This can be used by subclasses that either
 * has preloaded data or continuously update position data. This class is responsible
 * for the rendering of the vertex buffer objects which are filled by the subclasses.
 * The buffers contain a list of PackageVBOLayout objects that contains pairs of three
 * dimensional position data that determine start and endpoint of a package segment.
 *
 * Communication packages can be rendered as lines of varying colors and line thicknesses.
 *
 * The positions for each point along the trail is provided through a Translation object,
 * the type of which is specified in the dictionary that is passed to the constructor. A
 * typical implementation of Translation used for the trail would be a DsnTranslation.
 */
class RenderableCommunicationPackage : public Renderable {
public:
    ~RenderableCommunicationPackage() = default;

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;
    /// The layout of the VBOs
    struct PackageVBOLayout {
        float x, y, z;
    };

    /**
     * The render method will set up the shader information and then render first the
     * information contained in the the \c _mainRenderInformation, 
	 * using the provided \p data
     * \param data The data that is necessary to render this Renderable
     */
    void render(const RenderData& data, RendererTasks& rendererTask) override;

	/// The function deciding what color to use for a signal
	virtual glm::vec3 GetSiteColor(std::string dishIdentifier);

protected:
    explicit RenderableCommunicationPackage(const ghoul::Dictionary& dictionary);

    /// Returns the documentation entries
    static documentation::Documentation Documentation();

    /// The layout of the VBOs
   // struct PackageVBOLayout {
   //     float x, y, z;
   // };

    /// The backend storage for the vertex buffer object containing all points for the
    /// packages to be rendered
    std::vector<PackageVBOLayout> _vertexArray;

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
        /// Local model matrix transformation, used for rendering in camera space
        glm::dmat4 _localTransform = glm::dmat4(1.0);
        /// The vertex array object for this RenderInformation
        GLuint _vaoID = 0;
        /// The main vertex buffer object
        GLuint _vBufferID = 0;
        /// The optional index buffer object
        GLuint _iBufferID = 0;
    };

    /// Set of information about the main rendering parts
    RenderInformation _mainRenderInformation;

	/// Specifies the base color of the line
	glm::vec3 _lineColor;

	/// Specifies the base color of the site lines
	properties::Vec3Property _madridLineColor;
	properties::Vec3Property _goldstoneLineColor;
	properties::Vec3Property _canberraLineColor;

private:

    /// Line width for the line rendering part
    properties::FloatProperty _lineWidth;

    /// Program object used to render the data stored in RenderInformation
    ghoul::opengl::ProgramObject* _programObject = nullptr;

    UniformCache(modelView, projection, color) _uniformCache;

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
