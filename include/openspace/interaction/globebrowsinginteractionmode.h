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

#ifndef __OPENSPACE_CORE___GLOBEBROWSINGINTERACTIONMODE___H__
#define __OPENSPACE_CORE___GLOBEBROWSINGINTERACTIONMODE___H__

#include <openspace/interaction/orbitalinteractionmode.h>

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodetic3.h>
#endif

namespace openspace {

class Camera;
class SceneGraphNode;

namespace globebrowsing {
    class RenderableGlobe;
}

namespace interaction {

class GlobeBrowsingInteractionMode : public OrbitalInteractionMode
{
public:
    GlobeBrowsingInteractionMode(std::shared_ptr<MouseStates> mouseStates);
    ~GlobeBrowsingInteractionMode();

    virtual void setFocusNode(SceneGraphNode* focusNode);
    virtual void updateCameraStateFromMouseStates(Camera& camera, double deltaTime);
    bool followingNodeRotation() const override;
#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
    void goToChunk(Camera& camera, globebrowsing::TileIndex ti, glm::vec2 uv,
                   bool resetCameraDirection);
    void goToGeodetic2(Camera& camera, globebrowsing::Geodetic2 geo2,
                   bool resetCameraDirection);
    
    void goToGeodetic3(Camera& camera,  globebrowsing::Geodetic3 geo3);
    void resetCameraDirection(Camera& camera,  globebrowsing::Geodetic2 geo2);
#endif
private:

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
    globebrowsing::RenderableGlobe* _globe;
#endif
};

} // namespace interaction
} // namespace openspace

#endif // __OPENSPACE_CORE___GLOBEBROWSINGINTERACTIONMODE___H__
