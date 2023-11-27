/*********************************************************************************
 *
 * Inviwo - Interactive Visualization Workshop
 *
 * Copyright (c) 2023 Inviwo Foundation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *********************************************************************************/
#pragma once

#include <inviwo/tetramesh/tetrameshmoduledefine.h>

#include <inviwo/core/metadata/metadataowner.h>
#include <inviwo/core/datastructures/datatraits.h>
#include <inviwo/core/datastructures/spatialdata.h>
#include <inviwo/core/util/document.h>

#include <fmt/format.h>

namespace openspace {

/**
 * \ingroup datastructures
 * \brief Data required to render tetrahedral meshes
 *
 * Provides and interface for the data structures required for rendering a tetrahedral mesh with
 * OpenGL.
 *
 * Data structures for tetrahedra indexing and face enumeration based on
 *    M. Lage, T. Lewiner, H. Lopes, and L. Velho.
 *    CHF: A scalable topological data structure for tetrahedral meshes.
 *    In Brazilian Symposium on Computer Graphics and Image Processing
 *    (SIBGRAPI'05), pp. 349-356, 2005, doi: 10.1109/SIBGRAPI.2005.18
 *
 * \see TetraMeshBuffers
 */
class IVW_MODULE_TETRAMESH_API TetraMesh : public SpatialEntity<3> {
public:
    TetraMesh() = default;
    virtual TetraMesh* clone() const = 0;
    virtual ~TetraMesh() = default;

    virtual int getNumberOfCells() const = 0;
    virtual int getNumberOfPoints() const = 0;

    /**
     * Fill the \p nodes vector with the 3D coordinates of each node along with its scalar value
     * (vec4). The scalar is stored in the w component. The \p nodeIds vector is filled with the
     * node/vertex IDs for each tetrahedron (ivec4). The faces opposite of each node are implicitly
     * encoded.
     * The coordinates are given in Data space and can be transformed to Model space using the
     * Model matrix or SpatialCoordinateTransformer::getDataToModelMatrix().
     */
    virtual void get(std::vector<vec4>& nodes, std::vector<ivec4>& nodeIds) const = 0;

    /**
     * Return the bounding box of all nodes of the tetrahedral mesh in world space. The bounding
     * box is represented using a mat4, where all positions are between `bbox * (x,y,z,1)` where x,
     * y, and z are between 0 and 1.
     */
    virtual mat4 getBoundingBox() const = 0;

    /**
     * Return the data range of the scalar values
     *
     * @return scalar value range
     */
    virtual dvec2 getDataRange() const = 0;
};

template <>
struct DataTraits<TetraMesh> {
    static std::string classIdentifier() { return "org.inviwo.tetra.TetraMesh"; }
    static std::string dataName() { return "TetraMesh"; }
    static uvec3 colorCode() { return uvec3{50, 161, 234}; }

    static Document info(const TetraMesh& data) {
        using P = Document::PathComponent;
        using H = utildoc::TableBuilder::Header;
        Document doc;
        doc.append("b", "TetraMesh Data", {{"style", "color:white;"}});
        utildoc::TableBuilder tb(doc.handle(), P::end());

        tb(H("Tetras"), data.getNumberOfCells());
        tb(H("Points"), data.getNumberOfPoints());

        tb(H("Scalar Value Range"), data.getDataRange());

        vec3 worldMin(std::numeric_limits<float>::max());
        vec3 worldMax(std::numeric_limits<float>::lowest());

        const mat4 bbox{data.getBoundingBox()};
        const std::array<vec3, 8> corners = {vec3{0, 0, 0}, vec3{1, 0, 0}, vec3{1, 1, 0},
                                             vec3{0, 1, 0}, vec3{0, 0, 1}, vec3{1, 0, 1},
                                             vec3{1, 1, 1}, vec3{0, 1, 1}};
        for (const auto& corner : corners) {
            const auto point = vec3(bbox * vec4(corner, 1.f));
            worldMin = glm::min(worldMin, point);
            worldMax = glm::max(worldMax, point);
        }

        tb(H("Bounding Box"), fmt::format("{} - {}", worldMin, worldMax));

        return doc;
    }
};

}  // namespace openspace
