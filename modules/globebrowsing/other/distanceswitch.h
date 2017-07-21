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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___DISTANCESWITCH___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___DISTANCESWITCH___H__

#include <memory>
#include <vector>

namespace openspace {

class Renderable;
struct RenderData;
struct UpdateData;

namespace globebrowsing {

/**
 * Selects a specific Renderable to be used for rendering, based on distance to the 
 * camera
 */
class DistanceSwitch {
public:
    ~DistanceSwitch();

    bool initialize();
    bool deinitialize();

    /**
     * Picks the first Renderable with the associated maxDistance greater than the 
     * current distance to the camera
     */
    void render(const RenderData& data);
    void update(const UpdateData& data);

    /**
     * Adds a new renderable
     */
    void addSwitchValue(std::shared_ptr<Renderable> renderable);

private:
    std::vector<std::shared_ptr<Renderable>> _renderables;
};

} // namespace globebrowsing
} // openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___DISTANCESWITCH___H__
