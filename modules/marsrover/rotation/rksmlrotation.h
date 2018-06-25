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



namespace openspace {

namespace documentation { struct Documentation; }

class RksmlRotation : public Rotation {
public:

    struct Node {
        double time;
        double rotValue;    //radians 
    };

    RksmlRotation(const ghoul::Dictionary& dictionary);
    Timeline<Node>& timeline();
    void addKeyframe(double timestamp, RksmlRotation::Node data);
    Timeline<Node> getValue();
        
    glm::dmat3 matrix(const Time& time) const override;
    

private:
    void openFile() const;

    Timeline<Node> _dataTimeline;
    
    Node *_LFdrive;    //creates a Timeline object of the structure type Node 
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

/*
    Timeline<Node> _quatC;
    Timeline<Node> _quatX;
    Timeline<Node> _quatY;
    Timeline<Node> _quatZ;
*/

    properties::StringProperty _frame;
    void parseFile(std::string path) const;

    properties::StringProperty _dataPath;


    //properties::IntProperty    _rotationAxisCorrection;
    //properties::FloatProperty  _rotationAngle;
    //properties::BoolProperty   _rotationDir2;
};

} // namespace openspace



#endif // __OPENSPACE_MODULE_SPACE___RKSMLROTATION___H__
