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

#ifndef __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___LOD_MODEL_SWITCH___H__
#define __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___LOD_MODEL_SWITCH___H__

#include <ghoul/ghoul.h>
#include <modules/globebrowsing/src/renderableglobe.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>

namespace openspace {

using namespace globebrowsing;

class LodModelSwitch {
public:
    enum class Mode {
        Far,
        High,
        Low,
        Close,
    };

    LodModelSwitch();

    void initialize(RenderableGlobe* owner);

    Mode getLevel(const RenderData& data);

private:
    RenderableGlobe* _owner;
    SceneGraphNode* _parent;

    double _ellipsoidShrinkTerm;

    bool _firstLow;
    bool _firstHigh;
    bool _firstClose;

    glm::dvec3 _prevCam;
};

} // namespace openspace

#endif //__OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___LOD_MODEL_SWITCH___H__
