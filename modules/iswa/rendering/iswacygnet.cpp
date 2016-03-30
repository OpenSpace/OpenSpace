/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2015                                                               *
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
#include <modules/iswa/rendering/iswacygnet.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>


namespace openspace{

ISWACygnet::ISWACygnet(std::string path)
	:_enabled("enabled", "Is Enabled", true)
	,_cygnetId("cygnetId", "CygnetID",7, 0, 10)
	,_path("path", "Path", path)
	,_updateInterval("updateInterval", "Update Interval", 3, 1, 10)
	,_shader(nullptr)
{
	addProperty(_enabled);
	addProperty(_cygnetId);
	addProperty(_path);
	addProperty(_updateInterval);
}

ISWACygnet::~ISWACygnet(){}

bool ISWACygnet::initialize(){
	setParent();

	_month["JAN"] = "01";
   	_month["FEB"] = "02";
   	_month["MAR"] = "03";
   	_month["APR"] = "04";
   	_month["MAY"] = "05";
   	_month["JUN"] = "06";
   	_month["JUL"] = "07";
   	_month["AUG"] = "08";
   	_month["SEP"] = "09";
   	_month["OCT"] = "10";
   	_month["NOV"] = "11";
   	_month["DEC"] = "12";

	return true;
}

bool ISWACygnet::deinitialize(){
	_parent = nullptr;
	return true;
}

void ISWACygnet::render(){

}

void ISWACygnet::update(){}

void ISWACygnet::setPscUniforms(
	ghoul::opengl::ProgramObject* program, 
	const Camera* camera,
	const PowerScaledCoordinate& position) 
{
	program->setUniform("campos", camera->position().vec4());
	program->setUniform("objpos", position.vec4());
	program->setUniform("camrot", camera->viewRotationMatrix());
	program->setUniform("scaling", camera->scaling());
}

std::string ISWACygnet::getiSWAurl(int id){
	std::string url = "http://iswa2.ccmc.gsfc.nasa.gov/IswaSystemWebApp/iSWACygnetStreamer?timestamp=";
	std::string t = Time::ref().currentTimeUTC(); 

	std::stringstream ss(t);
	std::string token;

	std::getline(ss, token, ' ');
	url += token + "-"; 
	std::getline(ss, token, ' ');
	url += _month[token] + "-";
	std::getline(ss, token, 'T');
	url += token + "%20";
	std::getline(ss, token, '.');
	url += token;

	url += "&window=-1&cygnetId=";
	url += std::to_string(id);

	std::cout << url <<  std::endl;

	// puts(buffer);
	return url;
}
}//namespace openspac