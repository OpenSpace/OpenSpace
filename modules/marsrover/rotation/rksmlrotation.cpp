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

#include <openspace/util/timemanager.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>

#include <functional>

#include <fstream>
#include <iostream>
#include <string>

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

    ghoul::Dictionary modelDic;

    //calls the projectionprovider
    std::unique_ptr<ProjectionProvider> _projectionProvider;


    addProperty(_dataPath);
    addProperty(_frame);
    
    openFile();

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

RksmlRotation::~RksmlRotation() {}


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
    parseFile(testPath);

    //working
    LERROR(fmt::format("LF DRIVE number of keyframes '{}'", std::to_string(LF_DRIVE_Timeline.nKeyframes())));
    //LERROR(fmt::format("LF DRIVE number of keyframes '{}'", std::to_string(LF_STEER_Timeline.nKeyframes())));
    //LERROR(fmt::format("LF DRIVE number of keyframes '{}'", std::to_string(LM_DRIVE_Timeline.nKeyframes())));
    //LERROR(fmt::format("LF DRIVE number of keyframes '{}'", std::to_string(LR_DRIVE_Timeline.nKeyframes())));
    //LERROR(fmt::format("LF DRIVE number of keyframes '{}'", std::to_string(LR_STEER_Timeline.nKeyframes())));
    //LERROR(fmt::format("LF DRIVE number of keyframes '{}'", std::to_string(RF_DRIVE_Timeline.nKeyframes())));
    //LERROR(fmt::format("LF DRIVE number of keyframes '{}'", std::to_string(RF_STEER_Timeline.nKeyframes())));
    //LERROR(fmt::format("LF DRIVE number of keyframes '{}'", std::to_string(RM_DRIVE_Timeline.nKeyframes())));
    //LERROR(fmt::format("LF DRIVE number of keyframes '{}'", std::to_string(RR_DRIVE_Timeline.nKeyframes())));
    
    double testTime = 401720200.625000;
    const Keyframe<RksmlRotation::Node>* testNode0 = LF_DRIVE_Timeline.lastKeyframeBefore(testTime);
    const Keyframe<RksmlRotation::Node>* testNode1 = LF_STEER_Timeline.lastKeyframeBefore(testTime);
    const Keyframe<RksmlRotation::Node>* testNode2 = LM_DRIVE_Timeline.lastKeyframeBefore(testTime);
    const Keyframe<RksmlRotation::Node>* testNode3 = LR_DRIVE_Timeline.lastKeyframeBefore(testTime);
    const Keyframe<RksmlRotation::Node>* testNode4 = LR_STEER_Timeline.lastKeyframeBefore(testTime);
    const Keyframe<RksmlRotation::Node>* testNode5 = RF_DRIVE_Timeline.lastKeyframeBefore(testTime);
    const Keyframe<RksmlRotation::Node>* testNode6 = RF_STEER_Timeline.lastKeyframeBefore(testTime);
    const Keyframe<RksmlRotation::Node>* testNode7 = RM_DRIVE_Timeline.lastKeyframeBefore(testTime);
    const Keyframe<RksmlRotation::Node>* testNode8 = RR_DRIVE_Timeline.lastKeyframeBefore(testTime);
    const Keyframe<RksmlRotation::Node>* testNode9 = RR_STEER_Timeline.lastKeyframeBefore(testTime);
    const Keyframe<RksmlRotation::Node>* testNode10 = LEFT_BOGIE_Timeline.lastKeyframeBefore(testTime);
    const Keyframe<RksmlRotation::Node>* testNode11 = LEFT_DIFFERENTIAL_Timeline.lastKeyframeBefore(testTime);
    const Keyframe<RksmlRotation::Node>* testNode12 = RIGHT_BOGIE_Timeline.lastKeyframeBefore(testTime);
    const Keyframe<RksmlRotation::Node>* testNode13 = RIGHT_DIFFERENTIAL_Timeline.lastKeyframeBefore(testTime);
    const Keyframe<RksmlRotation::Node>* testNode14 = QUAT_C_Timeline.lastKeyframeBefore(testTime);
    const Keyframe<RksmlRotation::Node>* testNode15 = QUAT_X_Timeline.lastKeyframeBefore(testTime);
    const Keyframe<RksmlRotation::Node>* testNode16 = QUAT_Y_Timeline.lastKeyframeBefore(testTime);
    const Keyframe<RksmlRotation::Node>* testNode17 = QUAT_Z_Timeline.lastKeyframeBefore(testTime);


    double nextTime = 0;

    //if (testNode) {
    //    nextTime = testNode->timestamp;
    //    LERROR(fmt::format("nextTime: '{}'", nextTime));
    //}
    //else 
    //    LERROR(fmt::format("ingen testNode!!!!"));

    LERROR(fmt::format("QUAT_C_Timeline name: '{}'", std::to_string(testNode14->data.frameName)));
    LERROR(fmt::format("QUAT_C_Timeline rotvalue: '{}'", std::to_string(testNode14->data.rotValue)));
    double angle = testNode14->data.rotValue;
    glm::dmat3 rotationMatrix = glm::dmat3( cos(angle), -sin(angle), 0.0,
                                            sin(angle), cos(angle), 0.0,
                                            0.0,        0.0,        1.0);

    LERROR(fmt::format("rotationMatrix     '{}'", rotationMatrix));


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

    //Timeline<Node> const *ptr;
    
    //ptr = &LF_DRIVE_Timeline;
    //change to case

    if ( "LF_DRIVE_Timeline" == s) 
        LF_DRIVE_Timeline.addKeyframe(n.frameTime, n);

    else if ("LF_STEER_Timeline" == s) 
        LF_STEER_Timeline.addKeyframe(n.frameTime, n);
    
    else if ("LM_DRIVE_Timeline" == s)     
        LM_DRIVE_Timeline.addKeyframe(n.frameTime, n);
    
    else if ("LR_DRIVE_Timeline" == s) 
        LR_DRIVE_Timeline.addKeyframe(n.frameTime, n);
    
    else if ("LR_STEER_Timeline" == s) 
        LR_STEER_Timeline.addKeyframe(n.frameTime, n);
    
    else if ("RF_DRIVE_Timeline" == s) 
        RF_DRIVE_Timeline.addKeyframe(n.frameTime, n);
    
    else if ("RF_STEER_Timeline" == s) 
        RF_STEER_Timeline.addKeyframe(n.frameTime, n);
    
    else if ("RM_DRIVE_Timeline" == s) 
        RM_DRIVE_Timeline.addKeyframe(n.frameTime, n);
    
    else if ("RR_DRIVE_Timeline" == s) 
        RR_DRIVE_Timeline.addKeyframe(n.frameTime, n);
    
    else if ("RR_STEER_Timeline" == s) 
        RR_STEER_Timeline.addKeyframe(n.frameTime, n);
    
    else if ("LEFT_BOGIE_Timeline" == s) 
        LEFT_BOGIE_Timeline.addKeyframe(n.frameTime, n);
    
    else if ("LEFT_DIFFERENTIAL_Timeline" == s) 
        LEFT_DIFFERENTIAL_Timeline.addKeyframe(n.frameTime, n);
    
    else if ("RIGHT_BOGIE_Timeline" == s) 
        RIGHT_BOGIE_Timeline.addKeyframe(n.frameTime, n);
    
    else if ("RIGHT_DIFFERENTIAL_Timeline" == s) 
        RIGHT_DIFFERENTIAL_Timeline.addKeyframe(n.frameTime, n);
    
    else if ("QUAT_C_Timeline" == s) 
        QUAT_C_Timeline.addKeyframe(n.frameTime, n);
    
    else if ("QUAT_X_Timeline" == s) 
        QUAT_X_Timeline.addKeyframe(n.frameTime, n);
    
    else if ("QUAT_Y_Timeline" == s) 
        QUAT_Y_Timeline.addKeyframe(n.frameTime, n);
    
    else if("QUAT_Z_Timeline" == s) 
        QUAT_Z_Timeline.addKeyframe(n.frameTime, n); 

    else 
        QUAT_Z_Timeline.addKeyframe(n.frameTime, n); //solve case!!!!!!!!!!!!!!!!
        
}

} // namespace openspace
