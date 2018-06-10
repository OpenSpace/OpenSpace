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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___BASICGRIDGEOMETRY___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___BASICGRIDGEOMETRY___H__

#include <modules/globebrowsing/meshes/grid.h>

#include <modules/globebrowsing/meshes/trianglesoup.h>
#include <vector>

namespace openspace::globebrowsing {

class BasicGrid : public Grid {
public:
    /**
     * \param xSegments is the number of grid cells in the x direction.
     * \param ySegments is the number of grid cells in the y direction.
     * \param usePositions determines whether or not to upload any vertex position data
     *        to the GPU.
     * \param useTextureCoordinates determines whether or not to upload any vertex texture
     *        coordinate data to the GPU.
     * \param useNormals determines whether or not to upload any vertex normal data
     *        to the GPU.
    */
    BasicGrid(unsigned int xSegments, unsigned int ySegments,
        TriangleSoup::Positions usePositions,
        TriangleSoup::TextureCoordinates useTextureCoordinates,
        TriangleSoup::Normals useNormals);

    virtual int xSegments() const override;
    virtual int ySegments() const override;

private:
    std::vector<GLuint> createElements(int xSegments, int ySegments) override;
    std::vector<glm::vec4> createPositions(int xSegments, int ySegments) override;
    std::vector<glm::vec2> createTextureCoordinates(int xSegments,
        int ySegments) override;
    std::vector<glm::vec3> createNormals(int xSegments, int ySegments) override;

    void validate(int xSegments, int ySegments);

    inline size_t numElements(int xSegments, int ySegments);
    inline size_t numVertices(int xSegments, int ySegments);
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___BASICGRIDGEOMETRY___H__
