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
        A helper class for quick rendering of vertices IN clipping space. 

        The class is practically stateless. It only stores a ghoul::opengl::ProgramObject
        which can be reused despite the fact that rendering calls are invoked from different callers.
        Therefor a static reference is provided for convenience which is accessed through ref(). Note:
        That constructors are still public and the class is not a strict singleton.
    */
    class DebugRenderer {
    public:
        typedef std::vector<glm::vec4> Vertices;
        typedef glm::vec4 RGBA;

        /**
         *  Consider using ref() before creating a new default instance!
         */
        DebugRenderer();

        /**
         *  Instantiate a new DebugRenderer with a custom shader program
         */
        DebugRenderer(std::unique_ptr<ProgramObject> programObject);
        ~DebugRenderer();

        /**
         *  Access the static reference
         */
        static const DebugRenderer& ref();

        /**
         *  Render the vector of clipping space points in the specified mode and color.
         */
        void renderVertices(const Vertices& clippingSpacePoints, GLenum mode, RGBA = {1, 0, 0, 1}) const;

        /**
         *  Takes a vector of exactly 8 vertices, i.e. corner points in a box.
         *  The box corners should be ordered from smaller to larger,
         *  first by x, the, y and finally z. 
         *
         *  6-------7
         *  |\      |\
         *  | 2-------3
         *  4 | - - 5 |
         *   \|      \|
         *    0-------1
         *
         */
        void renderBoxFaces(const Vertices& clippingSpaceBoxCorners, RGBA rgba = { 1, 0, 0, 1 }) const;
        
         /**
         *  Takes a vector of exactly 8 vertices, i.e. corner points in a box.
         *  The box corners should be ordered from smaller to larger,
         *  first by x, the, y and finally z. 
         *
         *  6-------7
         *  |\      |\
         *  | 2-------3
         *  4 | - - 5 |
         *   \|      \|
         *    0-------1
         *
         */
        void renderBoxEdges(const Vertices& clippingSpaceBoxCorners, RGBA rgba = { 1, 0, 0, 1 }) const;

        /**
        *  Takes a vector of exactly 8 vertices, i.e. corner points in a box.
        *  The box corners should be ordered from smaller to larger,
        *  first by x, the, y and finally z.
        *
        *  6-------7
        *  |\      |\
        *  | 2-------3
        *  4 | - - 5 |
        *   \|      \|
        *    0-------1
        *
        */
        void renderNiceBox(const Vertices& clippingSpaceBoxCorners, RGBA rgba = { 1, 0, 0, 0.3 }) const;

        /**
         *  Input arguments:
         *  1. const RenderData& data:     defines position and camera that we will see the 
         *                                 other cameras view frustum from
         *  2. const Camera& otherCamera:  The camera who's view frustum is to be rendered
         *  3. RGBA rgba                   Color to draw the view frustum with
         */
        void renderCameraFrustum(const RenderData& data, const Camera& otherCamera, RGBA rgba = { 1, 1, 1, 0.3 }) const;

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
        /**
         *  Renders a screen space AABB2 to the screen with the provided color
         */
        void renderAABB2(const globebrowsing::AABB2& screenSpaceAABB, RGBA rgba = { 1, 1, 1, 0.3 }) const;
#endif // OPENSPACE_MODULE_GLOBEBROWSING_ENABLED

        
#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
        /**
        *  Takes a AABB3 in screen space and returns vertices representing the corner points 
        *  of the AABB. The ordering of the corner points is compatible with the box rendering 
        *  methods in this class.
        */
        const Vertices verticesFor(const globebrowsing::AABB3& screenSpaceAABB) const;
#endif // OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
        
    protected:
        std::unique_ptr<ProgramObject> _programObject;


        // A raw pointer for the reason that it should not be deleted by the static
        // destructor and the normal destructor. This class has ownership
        static DebugRenderer* _reference;
    };
} // namespace openspace
#endif // __DEBUG_RENDERER_H__

