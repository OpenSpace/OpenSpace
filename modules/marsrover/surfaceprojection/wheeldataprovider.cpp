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

#include <modules/marsrover/surfaceprojection/wheeldataprovider.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <ghoul/fmt.h>

#include <fstream>
#include <iostream>
#include <string>

#include <modules/globebrowsing/src/renderableglobe.h>

#include <ghoul/logging/logmanager.h>

namespace {
 const std::string _loggerCat = "WheelDataProvider";
}

namespace openspace {
WheelDataProvider::WheelDataProvider(const ghoul::Dictionary& dictionary)
 : ProjectionProvider(dictionary) {
}

void WheelDataProvider::loadData(std::string path) {
    std::string fileName;

    const int start = 28;
    const int end = 2072;

    LERROR(fmt::format("hej"));

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
    std::string testFile = path + "/00048/rksml_playback_filt_eha.rksml";
    parseFile(testFile);
}

void WheelDataProvider::parseFile(std::string path) 
{
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
    /*
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
                //if (name == std::to_string(_objectPart)) {     
                
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
                else {
                    nodeObject.rotValue = atof(value.c_str());
                    //If not empty, fix: make correct
                    if (getNode(std::to_string(nodeObject.name + "_Timeline")->data.name =! "")
                        getNode(std::to_string(nodeObject.name + "_Timeline").addKeyframe(nodeObject.frameTime, nodeObject)));
                }
            } 
            iss.clear();
            //trash = "";
        }
        myfile.close();
    }
    else LERROR(fmt::format("never opened file")); */
}

Timeline<WheelDataProvider::Node>& WheelDataProvider::getNode(std::string s)
{
    if ( "LF_DRIVE_Timeline" == s) 
        return LF_DRIVE_Timeline;

    else if ("LF_STEER_Timeline" == s) 
        return LF_STEER_Timeline;
    
    else if ("LM_DRIVE_Timeline" == s)     
        return LM_DRIVE_Timeline;
    
    else if ("LR_DRIVE_Timeline" == s) 
        return LR_DRIVE_Timeline;
    
    else if ("LR_STEER_Timeline" == s) 
        return LR_STEER_Timeline;
    
    else if ("RF_DRIVE_Timeline" == s) 
        return RF_DRIVE_Timeline;
    
    else if ("RF_STEER_Timeline" == s) 
        return RF_STEER_Timeline;
    
    else if ("RM_DRIVE_Timeline" == s) 
        return RM_DRIVE_Timeline;
    
    else if ("RR_DRIVE_Timeline" == s) 
        return RR_DRIVE_Timeline;
    
    else if ("RR_STEER_Timeline" == s) 
        return RR_STEER_Timeline;
    
    else if ("LEFT_BOGIE_Timeline" == s) 
        return LEFT_BOGIE_Timeline;
    
    else if ("LEFT_DIFFERENTIAL_Timeline" == s) 
        return LEFT_DIFFERENTIAL_Timeline;
    
    else if ("RIGHT_BOGIE_Timeline" == s) 
        return RIGHT_BOGIE_Timeline;
    
    else if ("RIGHT_DIFFERENTIAL_Timeline" == s) 
        return RIGHT_DIFFERENTIAL_Timeline;
    
    else if ("QUAT_C_Timeline" == s) 
        return QUAT_C_Timeline;
    
    else if ("QUAT_X_Timeline" == s) 
        return QUAT_X_Timeline;
    
    else if ("QUAT_Y_Timeline" == s) 
        return QUAT_Y_Timeline;
    
    else if("QUAT_Z_Timeline" == s) 
        return QUAT_Z_Timeline; 

    //fix
    //return empty object
    return QUAT_Z_Timeline;
}

void WheelDataProvider::initialize() {
    return;
}

} // namespace openspace
