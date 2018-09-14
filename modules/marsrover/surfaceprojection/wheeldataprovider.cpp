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
#include <vector>

#include <modules/globebrowsing/globes/renderableglobe.h>

#include <ghoul/logging/logmanager.h>

namespace {
 const std::string _loggerCat = "WheelDataProvider";
}

namespace openspace {
WheelDataProvider::WheelDataProvider() {}

void WheelDataProvider::loadData(const std::string path) {
    //LERROR(fmt::format("load data"));
    //LERROR(fmt::format("filepath1 '{}'", path));

    std::string fileName;

    const int start = 28;
    const int end = 2072;

    for (int i = 0; i < (end - start); i++) {
        if (i < (100 - start)) 
            fileName = path + "/000" + std::to_string(i + start); 
        
        else if (i < (1000 - start)) 
            fileName = path + "/00" + std::to_string(i + start); 
        
        else  
            fileName = path + "/0" + std::to_string(i + start); 

        fileName = fileName + "/rksml_playback_filt_eha.rksml";
        std::replace(fileName.begin(), fileName.end(), '\\', '/');
    
        //parse file
        parseFile(fileName);
    }
    //testfile
    //std::string testFile = path + "/00048/rksml_playback_filt_eha.rksml";
    //parseFile(testFile);

    //for (auto x : Timeline_Objects)
        //LERROR(fmt::format(x.first));
}

void WheelDataProvider::parseFile(const std::string path) 
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
                    
                if (nodeObject.frameName != "")
                    addKeyframe(nodeObject);    

            } 
            iss.clear();
            //trash = "";
        }
        myfile.close();
    }
    else LERROR(fmt::format("never opened file")); 

}

void WheelDataProvider::addKeyframe(const Node &node)
{
    bool found = false;

    // If Frame timeline object already exist, add new time
    if (Timeline_Objects.size() > 0)
    {
    
        for (auto obj : Timeline_Objects)
        {
            if (node.frameName == obj.first) // if already in vector -> add time 
            {
                obj.second.addKeyframe(node.frameTime, node);
                found = true;
                break;
            }
        }
    }

    // Add new timeline object to vector
    if (!found)
    {
        Timeline<WheelDataProvider::Node> timeline_obj;
        timeline_obj.addKeyframe(node.frameTime, node);
        Timeline_Objects.push_back(std::make_pair(node.frameName, timeline_obj));
    }
}


Timeline<WheelDataProvider::Node> &WheelDataProvider::getNode(const std::string s) const
{    
    LERROR(fmt::format("(getNode) s: '{}'", s));

    for (auto obj : Timeline_Objects)
    {
        LERROR(fmt::format("(getNode) obj.first '{}'", obj.first));

        if (s == obj.first)
        {
            //LERROR(fmt::format("init projection provider1: '{}'", obj.second.firstKeyframeAfter(100000.0)->data.rotValue));
            
            return obj.second;
        }
    }



    // No data for object
    LERROR(fmt::format("No data for frame"));
    Timeline<WheelDataProvider::Node> empty;
    return empty;
}

void WheelDataProvider::initialize() {
    std::string path = "../../../sync/http/marscuriosity_wheeldata/1/rksml";
    loadData(path);
    return;
}

} // namespace openspace
