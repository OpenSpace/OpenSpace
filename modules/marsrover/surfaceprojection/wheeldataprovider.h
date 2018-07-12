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

#ifndef __OPENSPACE_MODULE_MARSROVER___WHEEL_DATA_PROVIDER___H__
#define __OPENSPACE_MODULE_MARSROVER___WHEEL_DATA_PROVIDER___H__

#include <modules/marsrover/surfaceprojection/projectionprovider.h>
#include <openspace/util/timeline.h>
#include <string>

namespace openspace {
class WheelDataProvider : public ProjectionProvider {
public:

	struct Node {
        std::string frameName;
        double frameTime;
        double rotValue; //radians
        int axis; 
    };


	WheelDataProvider(const ghoul::Dictionary& dictionary);

	//virtual std::vector<std::shared_ptr<Subsite>> calculate(const std::vector<std::vector<std::shared_ptr<Subsite>>> subsites,
	// const RenderData& data, const SceneGraphNode* parent);
	static void loadData(std::string path);
	static void parseFile(std::string path); 
	Timeline<WheelDataProvider::Node>& getNode(std::string s);
	virtual void initialize();

private:
	Timeline<WheelDataProvider::Node> Object_Timeline;

	Timeline<WheelDataProvider::Node> LF_DRIVE_Timeline;    
    Timeline<WheelDataProvider::Node> LF_STEER_Timeline;
    Timeline<WheelDataProvider::Node> LM_DRIVE_Timeline;
    Timeline<WheelDataProvider::Node> LR_DRIVE_Timeline;
    Timeline<WheelDataProvider::Node> LR_STEER_Timeline;
    Timeline<WheelDataProvider::Node> RF_DRIVE_Timeline;
    Timeline<WheelDataProvider::Node> RF_STEER_Timeline;
    Timeline<WheelDataProvider::Node> RM_DRIVE_Timeline;
    Timeline<WheelDataProvider::Node> RR_DRIVE_Timeline;
    Timeline<WheelDataProvider::Node> RR_STEER_Timeline;
    Timeline<WheelDataProvider::Node> LEFT_BOGIE_Timeline;
    Timeline<WheelDataProvider::Node> LEFT_DIFFERENTIAL_Timeline;
    Timeline<WheelDataProvider::Node> RIGHT_BOGIE_Timeline;
    Timeline<WheelDataProvider::Node> RIGHT_DIFFERENTIAL_Timeline;
    Timeline<WheelDataProvider::Node> QUAT_C_Timeline;
    Timeline<WheelDataProvider::Node> QUAT_X_Timeline;
    Timeline<WheelDataProvider::Node> QUAT_Y_Timeline;
    Timeline<WheelDataProvider::Node> QUAT_Z_Timeline;

    //const int timelinesNr = 18;
};


} // namespace openspace

#endif //__OPENSPACE_MODULE_MARSROVER___WHEEL_DATA_PROVIDER___H__
