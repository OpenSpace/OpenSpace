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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GRIDGEOMETRY___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GRIDGEOMETRY___H__

#include <modules/globebrowsing/meshes/trianglesoup.h>

#include <ghoul/glm.h>

#include <memory>
#include <vector>

namespace openspace {
namespace globebrowsing {

/**
 * Abstract class defining an interface used for geometries with grid structures.
 * The class <code>Grid</code> should be extended for use of geometries with a 2D
 * structure where the number of segments in x and y direction represents the number
 * of vertices + 1 in each direction.
 */
class Grid {
public:
    Grid(int xSegments, int ySegments,
        TriangleSoup::Positions usePositions = TriangleSoup::Positions::No,
        TriangleSoup::TextureCoordinates useTextures =
         TriangleSoup::TextureCoordinates::No,
        TriangleSoup::Normals useNormals = TriangleSoup::Normals::No);
    
    virtual ~Grid() = default;

    TriangleSoup& geometry();

    /**
     * Returns the number of grid cells in the x direction. Hence the number of vertices
     * in the x direction is xResolution + 1.
     */
    virtual int xSegments() const = 0;
    
    /**
     * Returns the number of grid cells in the y direction. Hence the number of vertices
     * in the y direction is xResolution + 1.
     */
    virtual int ySegments() const = 0;

protected:
    /**
     * Should return the indices of vertices for a grid with size <code>xSegments</code> * 
     * <code>ySegments</code>. Where the number of vertices in each direction is the number
     * of segments + 1.
     */
    virtual std::vector<GLuint> CreateElements(int xSegments, int ySegments) = 0;
    
    /**
     * Should return the positions of vertices for a grid with size <code>xSegments</code>
     * * <code>ySegments</code>. Where the number of vertices in each direction is the
     * number of segments + 1.
     */
    virtual std::vector<glm::vec4> CreatePositions(int xSegments, int ySegments) = 0;
    
    /**
     * Should return the texture coordinates of vertices for a grid with size
     * <code>xSegments</code> * <code>ySegments</code>. Where the number of vertices in
     * each direction is the number of segments + 1.
     */
    virtual std::vector<glm::vec2>
        CreateTextureCoordinates(int xSegments, int ySegments) = 0;
    
    /**
     * Should return the normals of vertices for a grid with size <code>xSegments</code> *
     * <code>ySegments</code>. Where the number of vertices in each direction is the number
     * of segments + 1.
     */
    virtual std::vector<glm::vec3> CreateNormals(int xSegments, int ySegments) = 0;

    std::unique_ptr<TriangleSoup> _geometry;

    const int _xSegments;
    const int _ySegments;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GRIDGEOMETRY___H__
