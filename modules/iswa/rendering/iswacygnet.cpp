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

ISWACygnet::ISWACygnet(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _updateInterval("updateInterval", "Update Interval", 0.35, 0.1 , 1.0)
    , _delete("delete", "Delete")
    , _shader(nullptr)
    , _texture(nullptr)
    , _memorybuffer("")
{
    _data = std::make_shared<Metadata>();

	// dict.getValue can only set strings in _data directly
	float renderableId;
	float updateTime;
	glm::vec3 min, max;
	glm::vec2 spatialScale;

	dictionary.getValue("Id", renderableId);
	dictionary.getValue("UpdateTime", updateTime);
	dictionary.getValue("SpatialScale", spatialScale);
	dictionary.getValue("Min", min);
	dictionary.getValue("Max", max);
	dictionary.getValue("Frame",_data->frame);

	
	_data->id = (int) renderableId;
	_data->updateTime = (int) updateTime;
	_data->spatialScale = spatialScale;
	_data->min = min;
	_data->max = max;

    _data->scale = glm::vec3(
    		(max.x - min.x),
    		(max.y - min.y),
    		(max.z - min.z)
    );

	_data->offset = glm::vec3(
			(min.x + (std::abs(min.x)+std::abs(max.x))/2.0f),
        	(min.y + (std::abs(min.y)+std::abs(max.y))/2.0f),
        	(min.z + (std::abs(min.z)+std::abs(max.z))/2.0f)
	);


	// dictionary.getValue("Path",_data->path);
	// dictionary.getValue("Parent",_data->parent);

    // addProperty(_enabled);
    addProperty(_updateInterval);
    addProperty(_delete);

	// std::cout << _data->id << std::endl;
	// std::cout << std::to_string(_data->offset) << std::endl;
	// std::cout << std::to_string(_data->scale) << std::endl;
	// std::cout << std::to_string(_data->max) << std::endl;
    // std::cout << std::to_string(_data->min) << std::endl;
	// std::cout << _data->path << std::endl;
	// std::cout << _data->parent << std::endl;
	// std::cout << _data->frame << std::endl;

    _delete.onChange([this](){ISWAManager::ref().deleteISWACygnet(name());});
}

ISWACygnet::~ISWACygnet(){}

void ISWACygnet::registerProperties(){
    OsEng.gui()._iSWAproperty.registerProperty(&_enabled);
    OsEng.gui()._iSWAproperty.registerProperty(&_updateInterval);
    OsEng.gui()._iSWAproperty.registerProperty(&_delete);
}

void ISWACygnet::unregisterProperties(){
    OsEng.gui()._iSWAproperty.unregisterProperties(name());
}

}//namespace openspac