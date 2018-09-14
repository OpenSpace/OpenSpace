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

//#include <modules/marsrover/surfaceprojection/projectionprovider.h>
#include <openspace/util/timeline.h>
#include <string>
#include <vector>

namespace openspace {
class WheelDataProvider  { // : public ProjectionProvider
public:

	struct Node {
        std::string frameName = "";
        double frameTime = 0;
        double rotValue = 0; //radians
        int axis = -1; 
    };

	WheelDataProvider(); //const ghoul::Dictionary& dictionary

	//virtual std::vector<std::shared_ptr<Subsite>> calculate(const std::vector<std::vector<std::shared_ptr<Subsite>>> subsites,
	// const RenderData& data, const SceneGraphNode* parent);
	void loadData(const std::string path);
	void parseFile(const std::string path); 
    void addKeyframe(const WheelDataProvider::Node &node);
	Timeline<WheelDataProvider::Node> &getNode(const std::string s) const;
	void initialize();

private:
    std::vector<std::pair<std::string, Timeline<WheelDataProvider::Node>>> Timeline_Objects;

};


} // namespace openspace

#endif //__OPENSPACE_MODULE_MARSROVER___WHEEL_DATA_PROVIDER___H__
