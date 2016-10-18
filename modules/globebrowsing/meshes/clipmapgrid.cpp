/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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

#include <modules/globebrowsing/meshes/clipmapgrid.h>

#include <ghoul/opengl/ghoul_gl.h>

namespace {
    const std::string _loggerCat = "ClipMapGrid";
}

namespace openspace {
namespace globebrowsing {

//////////////////////////////////////////////////////////////////////////////////////////
//                            CLIPMAP GRID  (Abstract class)                            //
//////////////////////////////////////////////////////////////////////////////////////////

ClipMapGrid::ClipMapGrid(unsigned int segments)
    : Grid(
        segments,
        segments,
        TriangleSoup::Positions::No,
        TriangleSoup::TextureCoordinates::Yes,
        TriangleSoup::Normals::No)
{}

ClipMapGrid::~ClipMapGrid()
{}

int ClipMapGrid::xSegments() const {
    return segments();
}

int ClipMapGrid::ySegments() const {
    return segments();
}

int ClipMapGrid::segments() const {
    return _xSegments;
}

//////////////////////////////////////////////////////////////////////////////////////////
//                                    OUTER CLIPMAP GRID                                //
//////////////////////////////////////////////////////////////////////////////////////////

OuterClipMapGrid::OuterClipMapGrid(unsigned int segments)
: ClipMapGrid(segments)
{
    _geometry = std::unique_ptr<TriangleSoup>(new TriangleSoup(
        CreateElements(segments, segments),
        TriangleSoup::Positions::No,
        TriangleSoup::TextureCoordinates::Yes,
        TriangleSoup::Normals::No));

    _geometry->setVertexTextureCoordinates(CreateTextureCoordinates(segments, segments));
}

OuterClipMapGrid::~OuterClipMapGrid()
{

}

size_t OuterClipMapGrid::numElements(int segments)
{
    int numElementsInTotalSquare = 6 * (segments + 1) * (segments + 1);
    int numElementsInHole = 6 * (segments / 4 * segments / 4);
    return numElementsInTotalSquare - numElementsInHole;
}

size_t OuterClipMapGrid::numVerticesBottom(int segments)
{
    return (segments + 1 + 2) * (segments / 4 + 1 + 1);
}

size_t OuterClipMapGrid::numVerticesLeft(int segments)
{
    return (segments / 4 + 1 + 1) * (segments / 2 + 1);
}

size_t OuterClipMapGrid::numVerticesRight(int segments)
{
    return (segments / 4 + 1 + 1) * (segments / 2 + 1);
}

size_t OuterClipMapGrid::numVerticesTop(int segments)
{
    return (segments + 1 + 2) * (segments / 4 + 1 + 1);
}

size_t OuterClipMapGrid::numVertices(int segments)
{
    return    numVerticesBottom(segments) +
            numVerticesLeft(segments) +
            numVerticesRight(segments) +
            numVerticesTop(segments);
}

void OuterClipMapGrid::validate(int xRes, int yRes) {
    
    ghoul_assert(xRes == yRes,
        "segments must be equal in x and in y. ");
    int segments = xRes;
    ghoul_assert(segments >= 8,
        "segments must be at least 8. (" << segments << ")");
    ghoul_assert(segments == pow(2, int(log2(segments))),
        "segments must be a power of 2. (" << segments << ")");
}

std::vector<GLuint> OuterClipMapGrid::CreateElements(int xRes, int yRes) {
    validate(xRes, yRes);
    int segments = xRes;
    std::vector<GLuint> elements;
    elements.reserve(numElements(segments));
    
    // The clipmap geometry is built up by four parts as follows:
    // 0 = Bottom part
    // 1 = Left part
    // 2 = Right part
    // 3 = Top part
    //
    // 33333333
    // 33333333
    // 11    22
    // 11    22
    // 00000000
    // 00000000


    // x    v01---v11   x ..
    //       |  /  |
    // x    v00---v10   x ..
    //
    // x    x     x     x ..
    // :    :     :     :

    unsigned int previousVerts[4];
    previousVerts[0] = 0;
    previousVerts[1] = previousVerts[0] + numVerticesBottom(segments);
    previousVerts[2] = previousVerts[1] + numVerticesLeft(segments);
    previousVerts[3] = previousVerts[2] + numVerticesRight(segments);

    // Build the bottom part of the clipmap geometry
    for (unsigned int y = 0; y < segments / 4 + 1; y++) {
        for (unsigned int x = 0; x < segments + 2; x++) {
            GLuint v00 = previousVerts[0] + (y + 0) * (segments + 3) + x + 0;
            GLuint v10 = previousVerts[0] + (y + 0) * (segments + 3) + x + 1;
            GLuint v01 = previousVerts[0] + (y + 1) * (segments + 3) + x + 0;
            GLuint v11 = previousVerts[0] + (y + 1) * (segments + 3) + x + 1;

            elements.push_back(v00);
            elements.push_back(v10);
            elements.push_back(v11);

            elements.push_back(v00);
            elements.push_back(v11);
            elements.push_back(v01);
        }
    }

    // Build the left part of the clipmap geometry
    for (unsigned int y = 0; y < segments / 2; y++) {
        for (unsigned int x = 0; x < segments / 4 + 1; x++) {
            GLuint v00 = previousVerts[1] + (y + 0) * (segments / 4 + 2) + x + 0;
            GLuint v10 = previousVerts[1] + (y + 0) * (segments / 4 + 2) + x + 1;
            GLuint v01 = previousVerts[1] + (y + 1) * (segments / 4 + 2) + x + 0;
            GLuint v11 = previousVerts[1] + (y + 1) * (segments / 4 + 2) + x + 1;

            elements.push_back(v00);
            elements.push_back(v10);
            elements.push_back(v11);

            elements.push_back(v00);
            elements.push_back(v11);
            elements.push_back(v01);
        }
    }

    // Build the left part of the clipmap geometry
    for (unsigned int y = 0; y < segments / 2; y++) {
        for (unsigned int x = 0; x < segments / 4 + 1; x++) {
            GLuint v00 = previousVerts[2] + (y + 0) * (segments / 4 + 2) + x + 0;
            GLuint v10 = previousVerts[2] + (y + 0) * (segments / 4 + 2) + x + 1;
            GLuint v01 = previousVerts[2] + (y + 1) * (segments / 4 + 2) + x + 0;
            GLuint v11 = previousVerts[2] + (y + 1) * (segments / 4 + 2) + x + 1;

            elements.push_back(v00);
            elements.push_back(v10);
            elements.push_back(v11);

            elements.push_back(v00);
            elements.push_back(v11);
            elements.push_back(v01);
        }
    }

    // Build the left part of the clipmap geometry
    for (unsigned int y = 0; y < segments / 4 + 1; y++) {
        for (unsigned int x = 0; x < segments + 2; x++) {
            GLuint v00 = previousVerts[3] + (y + 0) * (segments + 3) + x + 0;
            GLuint v10 = previousVerts[3] + (y + 0) * (segments + 3) + x + 1;
            GLuint v01 = previousVerts[3] + (y + 1) * (segments + 3) + x + 0;
            GLuint v11 = previousVerts[3] + (y + 1) * (segments + 3) + x + 1;

            elements.push_back(v00);
            elements.push_back(v10);
            elements.push_back(v11);

            elements.push_back(v00);
            elements.push_back(v11);
            elements.push_back(v01);
        }
    }
    return elements;
}

std::vector<glm::vec4> OuterClipMapGrid::CreatePositions(int xRes, int yRes)
{
    validate(xRes, yRes);
    int segments = xRes;
    std::vector<glm::vec4> positions;
    positions.reserve(numVertices(segments));
    std::vector<glm::vec2> templateTextureCoords = CreateTextureCoordinates(xRes, yRes);

    // Copy from 2d texture coordinates and use as template to create positions
    for (unsigned int i = 0; i < templateTextureCoords.size(); i++) {
        positions.push_back(
            glm::vec4(
                templateTextureCoords[i].x,
                templateTextureCoords[i].y,
                0,
                1));
    }

    return positions;
}

std::vector<glm::vec2> OuterClipMapGrid::CreateTextureCoordinates(int xRes, int yRes){
    validate(xRes, yRes);
    int segments = xRes;
    std::vector<glm::vec2> textureCoordinates;
    textureCoordinates.reserve(numVertices(segments));

    // Build the bottom part of the clipmap geometry
    for (int y = -1; y < segments / 4 + 1; y++) {
        for (int x = -1; x < segments + 2; x++) {
            textureCoordinates.push_back(glm::vec2(
                static_cast<float>(x) / segments,
                static_cast<float>(y) / segments));
        }
    }
    
    // Build the left part of the clipmap geometry
    for (int y = segments / 4; y < 3 * segments / 4 + 1; y++) {
        for (int x = -1; x < segments / 4 + 1; x++) {
            textureCoordinates.push_back(glm::vec2(
                static_cast<float>(x) / segments,
                static_cast<float>(y) / segments));
        }
    }
    
    // Build the right part of the clipmap geometry
    for (int y = segments / 4; y < 3 * segments / 4 + 1; y++) {
        for (int x = 3 * segments / 4; x < segments + 2; x++) {
            float u = static_cast<float>(x) / segments;
            float v = static_cast<float>(y) / segments;
            textureCoordinates.push_back(glm::vec2(u, v));
        }
    }
    
    // Build the top part of the clipmap geometry
    for (int y = 3 * segments / 4; y < segments + 2; y++) {
        for (int x = -1; x < segments + 2; x++) {
            textureCoordinates.push_back(glm::vec2(
                static_cast<float>(x) / segments,
                static_cast<float>(y) / segments));
        }
    }
    
    return textureCoordinates;
}

std::vector<glm::vec3> OuterClipMapGrid::CreateNormals(int xRes, int yRes) {
    validate(xRes, yRes);
    int segments = xRes;
    std::vector<glm::vec3> normals;
    normals.reserve(numVertices(segments));

    for (int y = -1; y < segments + 2; y++) {
        for (int x = -1; x < segments + 2; x++) {
            normals.push_back(glm::vec3(0, 0, 1));
        }
    }

    return normals;
}

//////////////////////////////////////////////////////////////////////////////////////////
//                                    INNER CLIPMAP GRID                                //
//////////////////////////////////////////////////////////////////////////////////////////

InnerClipMapGrid::InnerClipMapGrid(unsigned int segments)
    : ClipMapGrid(segments)
{
    _geometry = std::unique_ptr<TriangleSoup>(new TriangleSoup(
        CreateElements(segments, segments),
        TriangleSoup::Positions::No,
        TriangleSoup::TextureCoordinates::Yes,
        TriangleSoup::Normals::No));

    _geometry->setVertexTextureCoordinates(CreateTextureCoordinates(segments, segments));
}

InnerClipMapGrid::~InnerClipMapGrid()
{

}

size_t InnerClipMapGrid::numElements(int segments)
{
    return segments * segments * 6;
}

size_t InnerClipMapGrid::numVertices(int segments)
{
    return (segments + 1) * (segments + 1);
}

void InnerClipMapGrid::validate(int xRes, int yRes) {

    ghoul_assert(xRes == yRes,
        "segments must be equal in x and in y. ");
    int segments = xRes;
    ghoul_assert(segments >= 1,
        "segments must be at least 1. (" << segments << ")");
}

std::vector<GLuint> InnerClipMapGrid::CreateElements(int xRes, int yRes) {
    validate(xRes, yRes);
    int segments = xRes;
    std::vector<GLuint> elements;
    elements.reserve(numElements(segments));

    // x    v01---v11   x ..
    //       |  /  |
    // x    v00---v10   x ..
    //
    // x    x     x     x ..
    // :    :     :     :

    for (unsigned int y = 0; y < segments + 2; y++) {
        for (unsigned int x = 0; x < segments + 2; x++) {
            GLuint v00 = (y + 0) * (segments + 3) + x + 0;
            GLuint v10 = (y + 0) * (segments + 3) + x + 1;
            GLuint v01 = (y + 1) * (segments + 3) + x + 0;
            GLuint v11 = (y + 1) * (segments + 3) + x + 1;

            elements.push_back(v00);
            elements.push_back(v10);
            elements.push_back(v11);

            elements.push_back(v00);
            elements.push_back(v11);
            elements.push_back(v01);
        }
    }

    return elements;
}

std::vector<glm::vec4> InnerClipMapGrid::CreatePositions(int xRes, int yRes)
{
    validate(xRes, yRes);
    int segments = xRes;
    std::vector<glm::vec4> positions;
    positions.reserve(numVertices(segments));
    std::vector<glm::vec2> templateTextureCoords = CreateTextureCoordinates(xRes, yRes);

    // Copy from 2d texture coordinates and use as template to create positions
    for (unsigned int i = 0; i < templateTextureCoords.size(); i++) {
        positions.push_back(
            glm::vec4(
                templateTextureCoords[i].x,
                templateTextureCoords[i].y,
                0,
                1));
    }

    return positions;
}

std::vector<glm::vec2> InnerClipMapGrid::CreateTextureCoordinates(int xRes, int yRes) {
    validate(xRes, yRes);
    int segments = xRes;
    std::vector<glm::vec2> textureCoordinates;
    textureCoordinates.reserve(numVertices(segments));

    // Build the bottom part of the clipmap geometry
    for (int y = -1; y < segments + 2; y++) {
        for (int x = -1; x < segments + 2; x++) {
            textureCoordinates.push_back(glm::vec2(
                static_cast<float>(x) / segments,
                static_cast<float>(y) / segments));
        }
    }

    return textureCoordinates;
}

std::vector<glm::vec3> InnerClipMapGrid::CreateNormals(int xRes, int yRes) {
    validate(xRes, yRes);
    int segments = xRes;
    std::vector<glm::vec3> normals;
    normals.reserve(numVertices(segments));

    for (int y = -1; y < segments + 2; y++) {
        for (int x = -1; x < segments + 2; x++) {
            normals.push_back(glm::vec3(0, 0, 1));
        }
    }

    return normals;
}

} // namespace globebrowsing
} // namespace openspace