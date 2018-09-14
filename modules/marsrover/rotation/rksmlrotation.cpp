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

#include <modules/marsrover/rotation/rksmlrotation.h>
#include <modules/marsrover/surfaceprojection/projectionprovider.h>
#include <modules/marsrover/surfaceprojection/wheeldataprovider.h>
#include <modules/roverterrainrenderer/model/modelprovider.h>


#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/engine/moduleengine.h>
#include <modules/marsrover/marsrovermodule.h>

#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/timeline.h>

#include <functional>

#include <fstream>
#include <iostream>
#include <string>

namespace {
    const char* DefaultReferenceFrame = "GALACTIC";
    
    static const openspace::properties::Property::PropertyInfo ObjectPartInfo = {
        "Object",
        "Reference Frame",
        "This value specifies the angle of correction around staticAxisRotation2."
        "Valid strings are expressed in radians"
    };

    static const openspace::properties::Property::PropertyInfo AxisInfo = {
        "RotationAxis",
        "Rotation Axis",
        "This value specifies the angle of correction around staticAxisRotation2."
        "Valid strings are expressed in radians"
    };

} // namespace

using namespace std::placeholders;

namespace openspace {
    
constexpr const char* _loggerCat = "RksmlRotation";

RksmlRotation::RksmlRotation(const ghoul::Dictionary& dictionary)
    : _frame(ObjectPartInfo, DefaultReferenceFrame)
    , _rotationAxis(AxisInfo, 1)
{
    //Fix: move down to matrix?
    double now = OsEng.windowWrapper().applicationTime();

    _frame = dictionary.value<std::string>(ObjectPartInfo.identifier);
    _rotationAxis = static_cast<int>(dictionary.value<double>(AxisInfo.identifier));

    ghoul::Dictionary modelDic;

    //calls the projectionprovider
    std::unique_ptr<ProjectionProvider> _projectionProvider;

    addProperty(_frame);
    addProperty(_rotationAxis);
    
    //openFile();

    auto update = [this]() {
        requireUpdate();
    };

    //Fix: needed?
    _frame.onChange(update);
    _rotationAxis.onChange(update);

}

RksmlRotation::~RksmlRotation() {}

glm::dmat3 RksmlRotation::matrix(const UpdateData& data) const {
    
    double currentTime = data.time.j2000Seconds();// * pow(10.0, 8.0);

    // Get timeline object
    LERROR(fmt::format("_Frame: '{}'", _frame));
    Timeline<WheelDataProvider::Node> Object_Timeline = OsEng.moduleEngine().module<MarsroverModule>()->getFrameData(_frame);

    const Keyframe<WheelDataProvider::Node>* nextKeyframe = Object_Timeline.firstKeyframeAfter(currentTime);
    const Keyframe<WheelDataProvider::Node>* prevKeyframe = Object_Timeline.lastKeyframeBefore(currentTime);

    double radiansResult = 0.0;
    double radians1 = 0.0;
    double radians2 = 0.0;

    double time1 = 0.0;
    double time2 = 0.0;

    if (prevKeyframe != nullptr) 
    {
        if (nextKeyframe != nullptr) 
        {
            time1 = prevKeyframe->data.frameTime; // x0
            radians1 = prevKeyframe->data.rotValue; // y0
            //if exact time
            if (nextKeyframe->timestamp == prevKeyframe->timestamp)
                radiansResult = nextKeyframe->data.rotValue;
            //If between two timestamps - compute interpolation between times
            else 
            {
                time2 = nextKeyframe->data.frameTime; // x1
                radians2 = nextKeyframe->data.rotValue; // y1
                //(y0 * (x1 - value) + y1 * (value - x0)) / (x1 - x0);
                radiansResult = (radians1 * (time2 - currentTime) + radians2 * (currentTime - time1)) / (time2 - time1); 
            }
        }
        else 
            radiansResult = prevKeyframe->data.rotValue;
    }
    else if (nextKeyframe != nullptr)
        radiansResult = nextKeyframe->data.rotValue; 

    double sin = glm::sin(radiansResult);
    double cos = glm::cos(radiansResult);

    LERROR(fmt::format("sin: '{}'", sin));
    LERROR(fmt::format("cos: '{}'", cos));

    glm::dmat3 rotMatrix = glm::dmat3(1.0);

    
    switch(_rotationAxis)
    {   //currently opposite direction for all matrises
        case 1:
            rotMatrix = glm::dmat3( 1.0,  0.0, 0.0,
                                    0.0,  cos, sin, 
                                    0.0, -sin, cos );
            break;
        case 2:
            //opposite direction = correct
            rotMatrix = glm::dmat3( cos, 0.0, -sin, 
                                    0.0, 1.0,  0.0, 
                                    sin, 0.0,  cos );
            break;
        case 3:    
            rotMatrix = glm::dmat3( cos, sin, 0.0, 
                                   -sin, cos, 0.0, 
                                    0.0, 0.0, 1.0 );  
            break;
    }

    return rotMatrix;
}

} // namespace openspace
