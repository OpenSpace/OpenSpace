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

//FIX: 
//Everything should be saved once in another file
//No need to declare all tags for all objects, for every object!!

#include <modules/marsrover/rotation/rksmlrotation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>

#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>

#include <functional>

#include <fstream>
#include <iostream>
#include <string>
//#define GET_VARIABLE_NAME(Variable) (#Variable)

namespace {
    const char* DefaultReferenceFrame = "GALACTIC";

    static const openspace::properties::Property::PropertyInfo DataPathInfo = {
        "DataPath",
        "Path",
        "This value specifies the angle of correction around staticAxisRotation2."
        "Valid strings are expressed in radians"
    };
    
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
    : _dataPath(DataPathInfo)
    , _objectPart(ObjectPartInfo, DefaultReferenceFrame)
    , _rotationAxis(AxisInfo, 1)
{
    //Fix: move down to matrix?
    double now = OsEng.windowWrapper().applicationTime();

    _dataPath = dictionary.value<std::string>(DataPathInfo.identifier);
    _objectPart = dictionary.value<std::string>(ObjectPartInfo.identifier);
    _rotationAxis = static_cast<int>(dictionary.value<double>(AxisInfo.identifier));

    //LERROR(fmt::format("Rotation axis: '{}'", _rotationAxis));

    addProperty(_dataPath);
    addProperty(_objectPart);
    addProperty(_rotationAxis);
    
    openFile();

    auto update = [this]() {
        requireUpdate();
    };

    //Fix: needed?
    _dataPath.onChange(update);
    _objectPart.onChange(update);
    _rotationAxis.onChange(update);

}
/*
Timeline<RksmlRotation::Node>& RksmlRotation::timeline() {
    //LERROR("inside of timeline");
    return LF_DRIVE_Timeline;   //FIX add all timelines
}

void RksmlRotation::addKeyframe(double timestamp, RksmlRotation::Node data) {
    //LERROR("inside of addKeyframe");
    //LF_DRIVE_Timeline.addKeyframe(timestamp, pose);
    timeline().addKeyframe(timestamp, data);
}
*/

glm::dmat3 RksmlRotation::matrix(const Time& time) const {
    
    double currentTime = time.j2000Seconds();// * pow(10.0, 8.0);

    LERROR(fmt::format("current Time: '{}'", std::to_string(currentTime)));

    //double tt = 402555992.017; //00057 (middle of two frames)
    const Keyframe<RksmlRotation::Node>* nextKeyframe = Object_Timeline.firstKeyframeAfter(currentTime);
    const Keyframe<RksmlRotation::Node>* prevKeyframe = Object_Timeline.lastKeyframeBefore(currentTime);

    double radiansResult = 0.0;
    double radians1 = 0.0;
    double radians2 = 0.0;

    double time1 = 0.0;
    double time2 = 0.0;


    //If both timeframes are found
    if (nextKeyframe != nullptr && prevKeyframe != nullptr) {

        LERROR(fmt::format("--------- Gar in i nextKeyframe != nullptr && prevKeyframe != nullptr"));

       
        //if exact time
        if (nextKeyframe->timestamp == prevKeyframe->timestamp)
            radiansResult = nextKeyframe->data.rotValue;
        //If between two timestamps
        else {
            time1 = prevKeyframe->data.frameTime; // x0
            radians1 = prevKeyframe->data.rotValue; // y0
            time2 = nextKeyframe->data.frameTime; // x1
            radians2 = nextKeyframe->data.rotValue; // y1
            
            //compute interpolation between times
            //(y0 * (x1 - value) + y1 * (value - x0)) / (x1 - x0);
            radiansResult = (radians1 * (time2 - currentTime) + radians1 * (currentTime - time1)) / (time1 - time2); 
        }
    }
    //If only one timeframe is found
    else {

        if (prevKeyframe) 
            radiansResult = prevKeyframe->data.rotValue;
        
        else if (nextKeyframe) 
            radiansResult = nextKeyframe->data.rotValue;
    }

    double sin = glm::sin(radiansResult);
    double cos = glm::cos(radiansResult);

    glm::dmat3 rotMatrix = glm::dmat3(1.0);
    
    switch(_rotationAxis)
    {
        case 1:
            rotMatrix = glm::dmat3( 1.0, 0.0,  0.0,
                                    0.0, cos, -sin, 
                                    0.0, sin,  cos );
            break;
        case 2:
            //opposite direction
            rotMatrix = glm::dmat3( cos, 0.0, -sin, 
                                    0.0, 1.0,  0.0, 
                                    sin, 0.0,  cos );
            break;
        case 3:    
            rotMatrix = glm::dmat3( cos, -sin, 0.0, 
                                    sin,  cos, 0.0, 
                                    0.0,  0.0, 1.0 );  
            break;

        LERROR(fmt::format("rotMatrix: '{}'", rotMatrix));
    }

    return rotMatrix;
}


void RksmlRotation::openFile() {

    std::string path = _dataPath;// + "/*/rksml_playback_filt_eha.rksml";
    std::string fileName;

    const int start = 28;
    const int end = 2072;

    for (int i = 0; i < (end - start); i++) {
        if (i < (100 - start)) 
            fileName = path + "000" + std::to_string(i + start); 
        
        else if (i < (1000 - start)) 
            fileName = path + "00" + std::to_string(i + start); 
        
        else  
            fileName = path + "0" + std::to_string(i + start); 

        fileName = fileName + "/rksml_playback_filt_eha.rksml";
        std::replace(fileName.begin(), fileName.end(), '\\', '/');
    
        //parse file
        parseFile(fileName);
    }

    //LERROR(fmt::format("nummer av objekt: '{}'", Object_Timeline.nKeyframes()));
}

void RksmlRotation::parseFile(std::string path) {
	//call file that will parse the data
    double val;
    double e;
    std::ifstream myfile (path);
    std::istringstream iss;
    
    std::string theline,
                trash,
                time,
                unit,
                name,
                value,
                tag,
                raised,
                line;

    if (myfile.is_open())
    {
        while (getline (myfile, line) )
        {
            iss.clear();
            iss = std::istringstream(line);
            
            //get first node tag
            std::getline(iss, trash, '<');
            std::getline(iss, tag, ' ');

            if (tag == "Node")
            {
                std::getline(iss, trash, '"');
                std::getline(iss, time,  '"');
            }

            else if (tag == "Knot")
            {
                std::getline(iss, trash, '"');
                std::getline(iss, name,  '"');
                std::getline(iss, trash, '"');
                std::getline(iss, unit,  '"');
                std::getline(iss, trash, '>');
                std::getline(iss, value, '<');

                //Send to new object with time to 
                //If moved to another file, if statement is not neccessary
                if (name == std::to_string(_objectPart)) {     
                    Node nodeObject;
                    nodeObject.frameName = name;
                    //double te = 399958865.0;
                    nodeObject.frameTime = atof(time.c_str());
                    //nodeObject.frameTime = te;

                    //if *10^x is found
                    if (value.find('E') != std::string::npos)
                    {
                        raised = value.substr(value.find('E') + 1, value.find('E') + 3 );
                        value.erase(value.find('E'), value.length());
                        val = atof(value.c_str());
                        e = atof(raised.c_str());

                        nodeObject.rotValue = val * pow(10.0, e); 
                    }
                    else 
                        nodeObject.rotValue = atof(value.c_str());

                    Object_Timeline.addKeyframe(nodeObject.frameTime, nodeObject);
                }
            }
            iss.clear();
            //trash = "";
        }
        myfile.close();
    }
    else LERROR(fmt::format("never opened file")); 
    
    //LERROR(fmt::format("nummer av objekt: '{}'", Object_Timeline.nKeyframes()));
}


} // namespace openspace
