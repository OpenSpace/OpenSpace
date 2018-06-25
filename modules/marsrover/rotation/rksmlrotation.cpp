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
    Node caroline;
    caroline.time = 3.0;
    caroline.rotValue = 2.0;
    Node *ptrToCaroline;
    ptrToCaroline = &caroline;
    LERROR(fmt::format("Caroline    '{}'", ptrToCaroline->time));//

    addKeyframe(5000.555, caroline);
    //create a new node for every wheelpart
    // and then assign it the data from what we read in


    _dataTimeline.firstKeyframeAfter(ptrToCaroline->time);

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
//Timeline<Node> RksmlRotation::getValue() {
//    return _dataTimeline;
//}

Timeline<RksmlRotation::Node>& RksmlRotation::timeline() {
    LERROR("inside of timeline");
    return _dataTimeline;
}
void RksmlRotation::addKeyframe(double timestamp, RksmlRotation::Node data) {
    LERROR("inside of addKeyframe");
    timeline().addKeyframe(timestamp, data);
}


glm::dmat3 RksmlRotation::matrix(const Time& time) const {
    glm::dmat3 hej = glm::dmat3(1.0);
    return hej;
}

void RksmlRotation::openFile() const {

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
        parseFile(fileName);
    }
}

//troligtvis need to have the functionality of parsing in another file, since we need to use namespace libtorrent
void RksmlRotation::parseFile(std::string path) const {
	//call file that will parse the data
    std::string theline;
    std::string trash;
    std::string time;
    std::string unit;
    std::string name;
    std::string value;
    std::string tag;
    std::string raised;
    double val;
    double e;

    //std::string testPath = path + "00048/rksml_playback_filt_eha.rksml";

    std::string line;
    std::ifstream myfile (path);
    std::istringstream iss;

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

                //TEST
                //Send to new object with time to 
                myNode n;
                n.na = name;
                n.ti = atof(time.c_str());

                //if *10^x is found
                if (value.find('E') != std::string::npos)
                {
                    raised = value.substr(value.find('E') + 1, value.find('E') + 3 );
                    value.erase(value.find('E'), value.length());
                    val = atof(value.c_str());
                    e = atof(raised.c_str());

                    n.va = val * pow(10.0, e); //fix
                }
                else 
                    n.va = atof(value.c_str());
            }
            iss.clear();
            //trash = "";
        }

        myfile.close();
    }
    else LERROR(fmt::format("never opened file")); //which files are not opened?

}
} // namespace openspace
