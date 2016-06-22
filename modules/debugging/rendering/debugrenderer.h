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

#ifndef __DEBUG_RENDERER_H__
#define __DEBUG_RENDERER_H__

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>

#include <openspace/util/updatestructures.h>
#include <modules/globebrowsing/geometry/aabb.h>


#include <glm/glm.hpp>
#include <memory>
#include <vector>



#include <ghoul/misc/assert.h>

namespace openspace {
    using namespace ghoul::opengl;

    /**
        A helper class for quick rendering of vertices clipping space
    */
    class DebugRenderer {
    public:
        DebugRenderer();

        static std::shared_ptr<DebugRenderer> ref();

        typedef std::vector<glm::vec4> Vertices;
        typedef glm::vec4 RGBA;

        
        void renderVertices(const Vertices& clippingSpacePoints, GLenum mode, RGBA = {1, 0, 0, 1}) const;
        void renderBoxFaces(const Vertices& clippingSpacePoints, RGBA rgba = { 1, 0, 0, 1 }) const;
        void renderBoxEdges(const Vertices& clippingSpacePoints, RGBA rgba = { 1, 0, 0, 1 }) const;
        void renderNiceBox(const Vertices& clippingSpacePoints, RGBA rgba = { 1, 0, 0, 0.3 }) const;

        void renderCameraFrustum(const RenderData& data, const Camera& otherCamera) const;
        void renderAABB2(const AABB2& screenSpaceAABB, RGBA rgba = { 1, 1, 1, 0.3 }) const;

        
        const Vertices verticesFor(const AABB3& screenSpaceAABB) const;
        

    private:


        
        std::shared_ptr<ProgramObject> _programObject;


        static std::shared_ptr<DebugRenderer> _singleton;

    };

} // namespace openspace



#endif // __DEBUG_RENDERER_H__

