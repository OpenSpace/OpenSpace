/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014                                                                    *
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

#ifndef __RENDERABLECONSTELLATIONBOUNDS_H__
#define __RENDERABLECONSTELLATIONBOUNDS_H__

#include <openspace/rendering/renderable.h>
#include <openspace/properties/scalarproperty.h>
#include <ghoul/opengl/programobject.h>
#include <array>

namespace openspace {

/**
 * This class renders the constellation bounds as defined in
 * http://cdsarc.u-strasbg.fr/viz-bin/Cat?cat=VI%2F49. It contains the bounds on the
 * celestial sky for the different constellations and is used to determine in which region
 * of the sky a specific object is located.
 * The bounds are drawn as lines on a sphere with variable radius, set by the
 * <code>_distance</code> property. Currently, all constellation bounds are lines, which
 * leads to artifacts if the radius is very small.
 * Renderable configuration attributes:
 * <code>File</code> [string] (required): The file that contains the bounds and the
 * abbreviations for the different constellations
 * <code>ReferenceFrame</code> [string]: The reference frame in which the points contained
 * in the <code>File</code> are stored in. Defaults to <code>J2000</code>
 *
 * @TODO Add a method to load (and access) a table translating from abbreviations to
 * full names ---abock
 * @TODO Make it possible to only show a subset of constellation bounds ---abock
 */
class RenderableConstellationBounds : public Renderable {
public:
	RenderableConstellationBounds(const ghoul::Dictionary& dictionary);

	bool initialize() override;
	bool deinitialize() override;

	bool isReady() const override;

	void render(const RenderData& data) override;
	void update(const UpdateData& data) override;

private:
	/// Stores the constellation bounds
	struct ConstellationBound {
		std::string constellation; ///< The abbreviation of the constellation
		size_t startIndex; ///< The index of the first vertex describing the bounds
		size_t nVertices; ///< The number of vertices describing the bounds
	};

	/**
	 * Loads the file specified in <code>_filename</code> and fills the
	 * <code>_constellationBounds</code> variable, as well as the
	 * <code>_vertexValues</code> list. If this method fails, the content of either
	 * destination is undefined.
	 * \return <code>true</code> if the loading succeeded, <code>false</code> otherwise
	 */
	bool loadFile();

	std::string _filename; ///< The filename containing the constellation bounds

	ghoul::opengl::ProgramObject* _program;
	bool _programIsDirty;

	/// The list of all loaded constellation bounds
	std::vector<ConstellationBound> _constellationBounds;
	
	typedef std::array<float, 3> Vertex;
	std::vector<Vertex> _vertexValues; ///< A list of all vertices of all bounds

	/// The radius of the celestial sphere onto which the bounds are drawn
	properties::FloatProperty _distance;

	std::string _originReferenceFrame; ///< Reference frame in which bounds are defined
	
	/// Used to translate between the origin reference frame and the target frame
	glm::dmat3 _stateMatrix;

	GLuint _vao;
	GLuint _vbo;
};

} // namespace openspace

#endif // __RENDERABLECONSTELLATIONBOUNDS_H__
