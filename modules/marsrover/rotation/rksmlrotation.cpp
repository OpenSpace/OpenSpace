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
#define GET_VARIABLE_NAME(Variable) (#Variable)

namespace {
    const char* DefaultReferenceFrame = "GALACTIC";

    static const openspace::properties::Property::PropertyInfo DataPathInfo = {
        "DataPath",
        "Path",
        "This value specifies the source frame that is used as the basis for the "
        "coordinate transformation. This has to be a valid SPICE name."
    };
    static const openspace::properties::Property::PropertyInfo FrameInfo = {
        "Frame",
        "Reference Frame",
        "This value specifies the source frame that is used as the basis for the "
        "coordinate transformation. This has to be a valid SPICE name."
    };

} // namespace

using namespace std::placeholders;

namespace openspace {
    
    constexpr const char* _loggerCat = "RksmlRotation";

    //Test Node
    struct myNode
    {
        std::string na;
        double ti;
        double va;
    };


RksmlRotation::RksmlRotation(const ghoul::Dictionary& dictionary)
    : _dataPath(DataPathInfo) // @TODO Missing documentation
    , _frame(FrameInfo, DefaultReferenceFrame)
{
    double now = OsEng.windowWrapper().applicationTime();

    _dataPath = dictionary.value<std::string>(DataPathInfo.identifier);
    //datapath will be: rksml/00028/rksml_playback_filt_eha.rksml

    if (dictionary.hasKey(FrameInfo.identifier)) {
        _frame = dictionary.value<std::string>(FrameInfo.identifier);
    }

    addProperty(_dataPath);
    addProperty(_frame);
    
    openFile();
    //create a pointer to a new node and assign it some data
    //Node caroline;
    //caroline.time = 3.0;
    //caroline.rotValue = 2.0;
    //Node *ptrToCaroline;
    //ptrToCaroline = &caroline;
    //LERROR(fmt::format("Caroline    '{}'", ptrToCaroline->time));//
//
    //addKeyframe(5000.555, caroline);
    //create a new node for every wheelpart
    // and then assign it the data from what we read in


    //_dataTimeline.firstKeyframeAfter(ptrToCaroline->time);

    //creates a keyframe object of type node 
    //const Keyframe<Node>* nextKeyframe = _dataTimeline.firstKeyframeAfter(now);
    //const Keyframe<Node>* prevKeyframe = _dataTimeline.lastKeyframeBefore(now);
    //if (_dataTimeline.nKeyframes() == 0) {
    //    return;
    //}


    auto update = [this]() {
        requireUpdate();
    };


    //Fix: needed?
    _dataPath.onChange(update);
    _frame.onChange(update);

}
    
//for interpolation
//const Keyframe<CameraPose>* nextKeyframe = _cameraPoseTimeline.firstKeyframeAfter(now);
//const Keyframe<CameraPose>* prevKeyframe = _cameraPoseTimeline.lastKeyframeBefore(now);


Timeline<RksmlRotation::Node>& RksmlRotation::timeline() {
    //LERROR("inside of timeline");
    return LF_DRIVE_Timeline;   //FIX add all timelines
}

void RksmlRotation::addKeyframe(double timestamp, RksmlRotation::Node data) {
    //LERROR("inside of addKeyframe");
    //LF_DRIVE_Timeline.addKeyframe(timestamp, pose);
    timeline().addKeyframe(timestamp, data);
}


glm::dmat3 RksmlRotation::matrix(const Time& time) const {
    
    return glm::dmat3(1.0);
}

void RksmlRotation::openFile() {

    std::string path = _dataPath;// + "/*/rksml_playback_filt_eha.rksml";
    //fileName = rksml/
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
        //parseFile(fileName);
    }
    std::string testPath = path + "00048/rksml_playback_filt_eha.rksml";
    parseFile(fileName);
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
            std::getline(iss, trash, ' ');

            if (trash == "Node")
            {
                std::getline(iss, trash, '"');
                std::getline(iss, time,  '"');
            }

            else if (trash == "Knot")
            {
                std::getline(iss, trash, '"');
                std::getline(iss, name,  '"');
                std::getline(iss, trash, '"');
                std::getline(iss, unit,  '"');
                std::getline(iss, trash, '>');
                std::getline(iss, value, '<');

                //Send to new object with time to 
                Node nodeObject;
                nodeObject.frameName = name;
                nodeObject.frameTime = atof(time.c_str());

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

                std::string timelineObject = name + "_Timeline";
                addTimelineObject(timelineObject, nodeObject);
            }
            iss.clear();
            //trash = "";
        }
        myfile.close();
    }
    else LERROR(fmt::format("never opened file")); 
}

void RksmlRotation::addTimelineObject(std::string s, RksmlRotation::Node n)
{

    Timeline<Node> const *ptr;
    
    //ptr = &LF_DRIVE_Timeline;
    //change to case

    if ( "LF_DRIVE_Timeline" == s) 
        ptr = &LF_DRIVE_Timeline;

    else if ("LF_STEER_Timeline" == s) 
        ptr = &LF_STEER_Timeline;
    
    else if ("LM_DRIVE_Timeline" == s)     
        ptr = &LM_DRIVE_Timeline;
    
    else if ("LR_DRIVE_Timeline" == s) 
        ptr = &LR_DRIVE_Timeline;
    
    else if ("LR_STEER_Timeline" == s) 
        ptr = &LR_STEER_Timeline;
    
    else if ("RF_DRIVE_Timeline" == s) 
        ptr = &RF_DRIVE_Timeline;
    
    else if ("RF_STEER_Timeline" == s) 
        ptr = &RF_STEER_Timeline;
    
    else if ("RM_DRIVE_Timeline" == s) 
        ptr = &RM_DRIVE_Timeline;
    
    else if ("RR_DRIVE_Timeline" == s) 
        ptr = &RR_DRIVE_Timeline;
    
    else if ("RR_STEER_Timeline" == s) 
        ptr = &RR_STEER_Timeline;
    
    else if ("LEFT_BOGIE_Timeline" == s) 
        ptr = &LEFT_BOGIE_Timeline;
    
    else if ("LEFT_DIFFERENTIAL_Timeline" == s) 
        ptr = &LEFT_DIFFERENTIAL_Timeline;
    
    else if ("RIGHT_BOGIE_Timeline" == s) 
        ptr = &RIGHT_BOGIE_Timeline;
    
    else if ("RIGHT_DIFFERENTIAL_Timeline" == s) 
        ptr = &RIGHT_DIFFERENTIAL_Timeline;
    
    else if ("QUAT_C_Timeline" == s) 
        ptr = &QUAT_C_Timeline;
    
    else if ("QUAT_X_Timeline" == s) 
        ptr = &QUAT_X_Timeline;
    
    else if ("QUAT_Y_Timeline" == s) 
        ptr = &QUAT_Y_Timeline;
    
    else if ("QUAT_Z_Timeline" == s) 
        ptr = &QUAT_Z_Timeline;
        
    else
        ptr = &QUAT_Z_Timeline; //solve case
    

    //addKeyframe(2000.222, n);
    QUAT_Z_Timeline.addKeyframe(n.frameTime, n);
    //ptr.addKeyframe(n.frameTime, n);
    //ptr->addKeyframe(n.frameTime, n);
    //ptr->addKeyframe(n.frameTime, n);
}

} // namespace openspace
