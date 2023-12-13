/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#ifndef __OPENSPACE_MODULE_TETRAMESH___VOLUMETETRAMESH___H__
#define __OPENSPACE_MODULE_TETRAMESH___VOLUMETETRAMESH___H__

#include <modules/volume/rawvolume.h>
#include <modules/gaia/tasks/generateGaiaVolumeTask.h>
#include <glm/glm.hpp>
#include <vector>

namespace openspace {

// \ingroup datastructures
// \brief Data required for rendering an Inviwo Volume as tetrahedral mesh
//
// Provides an interface between a Volume and the data structures required for rendering a
// tetrahedral mesh. Six tetrahedra are created in between each four voxels of the volume to convert
// the cell-centered data of the uniform grid of the Volume to node-centered values.
//
// The extent of the TetraMesh will be smaller than the extent of the Volume by half a voxel in each
// dimension.
//
// Data structures for tetrahedra indexing and face enumeration based on
// M. Lage, T. Lewiner, H. Lopes, and L. Velho.
// CHF: A scalable topological data structure for tetrahedral meshes.
// In Brazilian Symposium on Computer Graphics and Image Processing
// (SIBGRAPI'05), pp. 349-356, 2005, doi: 10.1109/SIBGRAPI.2005.18
//
// 
class VolumeTetraMesh {
public:
    VolumeTetraMesh() = default;
    VolumeTetraMesh(const std::shared_ptr<const volume::RawVolume<gaiavolume::GaiaVolumeDataLayout>>& volume, int channel = 0);

    /**
     * Use \p volume as source for the tetrahedralization into a TetraMesh.
     *
     * @param volume    input volume to be converted
     * @param channel   volume channel used as scalar values
     * @throws Exception if one of the \p volume dimensions is less than 2
     */
    void setData(const std::shared_ptr<const volume::RawVolume<gaiavolume::GaiaVolumeDataLayout>>& volume, int channel = 0);

    int getNumberOfCells() const;
    int getNumberOfPoints() const;

    // Fill the \p nodes vector with the 3D coordinates of each node along with its scalar value
    // (vec4). The scalar is stored in the w component. The \p nodeIds vector is filled with the
    // node/vertex IDs for each tetrahedron (ivec4). The faces opposite of each node are implicitly
    // encoded.
    // The coordinates are given in Data space and can be transformed to Model space using the
    // Model matrixSpatialCoordinateTransformer::getDataToModelMatrix(). or 
    void get(std::vector<glm::vec4>& nodes, std::vector<glm::ivec4>& nodeIds) const;

     // Return the bounding box of all nodes of the tetrahedral mesh in world space. The bounding
     // box is represented using a mat4, where all positions are between `bbox * (x,y,z,1)` where x,
     // y, and z are between 0 and 1.
    glm::mat4 getBoundingBox() const;

    // Return the data range of the scalar values
    // @return scalar value range
    glm::vec2 getDataRange() const;


    glm::mat4 tetraBoundingBox() const;

private:
    std::shared_ptr<const volume::RawVolume<gaiavolume::GaiaVolumeDataLayout>> _volume;
    int _channel = 0;

    //glm::mat4 _modelMatrix;
    //glm::mat4 _worldMatrix;
};

}  // namespace openspace

#endif // __OPENSPACE_MODULE_TETRAMESH___VOLUMETETRAMESH___H__
