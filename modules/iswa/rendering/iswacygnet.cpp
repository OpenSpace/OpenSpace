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
	, _updateInterval("updateInterval", "Update Interval", 3, 1, 10)
	, _delete("delete", "Delete")
	, _shader(nullptr)
	, _texture(nullptr)
	, _memorybuffer("")
{
	_data = std::make_shared<Metadata>();

	dictionary.getValue("Id",_data->id);
	dictionary.getValue("Path",_data->path);
	dictionary.getValue("Parent",_data->parent);
	dictionary.getValue("Frame",_data->frame);
	dictionary.getValue("Offset",_data->offset);
	dictionary.getValue("Scale",_data->scale);

	// addProperty(_enabled);
	addProperty(_updateInterval);
	addProperty(_delete);

	_delete.onChange([this](){ISWAManager::ref().deleteISWACygnet(name());});
}

ISWACygnet::~ISWACygnet(){}

void ISWACygnet::registerProperties(){
	OsEng.gui()._property.registerProperty(&_updateInterval);
	OsEng.gui()._property.registerProperty(&_delete);
}

void ISWACygnet::unregisterProperties(){
	OsEng.gui()._property.unregisterProperties(name());
}

}//namespace openspac