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


#ifndef __OPENSPACE_MODULE_SPACE___RKSMLROTATION___H__
#define __OPENSPACE_MODULE_SPACE___RKSMLROTATION___H__

#include <openspace/scene/rotation.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/util/timeline.h>

#include <string>

#include <openspace/network/parallelpeer.h>

//include projectionprovider tooo!
//#include <modules/marsrover/surfaceprojection/projectionprovider.h>

namespace openspace {

namespace documentation { struct Documentation; }

class RksmlRotation : public Rotation { //change to : public ProjectionProvider 
public:

    struct Node {
        std::string frameName;
        double frameTime;
        double rotValue;    //radians 
    };


    RksmlRotation(const ghoul::Dictionary& dictionary);
    ~RksmlRotation();
    glm::dmat3 matrix(const Time& time) const override;
    

private:
    void openFile();
    void parseFile(std::string path);
    void addTimelineObject(std::string s, Node n);

    Timeline<Node> LF_DRIVE_Timeline;    //creates a Timeline object of the structure type Node 
    Timeline<Node> LF_STEER_Timeline;
    Timeline<Node> LM_DRIVE_Timeline;
    Timeline<Node> LR_DRIVE_Timeline;
    Timeline<Node> LR_STEER_Timeline;
    Timeline<Node> RF_DRIVE_Timeline;
    Timeline<Node> RF_STEER_Timeline;
    Timeline<Node> RM_DRIVE_Timeline;
    Timeline<Node> RR_DRIVE_Timeline;
    Timeline<Node> RR_STEER_Timeline;
    Timeline<Node> LEFT_BOGIE_Timeline;
    Timeline<Node> LEFT_DIFFERENTIAL_Timeline;
    Timeline<Node> RIGHT_BOGIE_Timeline;
    Timeline<Node> RIGHT_DIFFERENTIAL_Timeline;
    Timeline<Node> QUAT_C_Timeline;
    Timeline<Node> QUAT_X_Timeline;
    Timeline<Node> QUAT_Y_Timeline;
    Timeline<Node> QUAT_Z_Timeline;


    
    Node *_LFdrive;    
    Node *_LFsteer;
    Node *_LMdrive;
    Node *_LRdrive;
    Node *_LRsteer;
    Node *_RFdrive;
    Node *_RFsteer;
    Node *_RMdrive;
    Node *_RRdrive;
    Node *_RRsteer;
    Node *_leftBogie;
    Node *_leftDifferential;
    Node *_rightBogie;
    Node *_rightDifferential;


    properties::StringProperty _frame;

    properties::StringProperty _dataPath;


    //properties::IntProperty    _rotationAxisCorrection;
    //properties::FloatProperty  _rotationAngle;
    //properties::BoolProperty   _rotationDir2;
};

} // namespace openspace



#endif // __OPENSPACE_MODULE_SPACE___RKSMLROTATION___H__
