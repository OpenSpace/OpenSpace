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
#include <modules/datasurface/rendering/datasurfacecontainer.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <ghoul/filesystem/filesystem>

#include <modules/datasurface/rendering/datasurface.h>
#include <modules/datasurface/rendering/dataplane.h>

namespace openspace{
	DataSurfaceContainer::DataSurfaceContainer(const ghoul::Dictionary& dictionary)
		:Renderable(dictionary)
	{
		std::cout << "Created datasurface container" << std::endl;
	}

	DataSurfaceContainer::~DataSurfaceContainer(){}

	bool DataSurfaceContainer::initialize(){
		std::cout << "Initialized datasurface container" << std::endl;

		addDataSurface("${OPENSPACE_DATA}/BATSRUS.cdf");
	}
	bool DataSurfaceContainer::deinitialize(){}
	bool DataSurfaceContainer::isReady() const {}

	void DataSurfaceContainer::render(const RenderData& data){
		for(dataSurface : _dataSurfaces)
			dataSurface->render();
	} 

	void DataSurfaceContainer::update(const UpdateData& data){}

	void DataSurfaceContainer::addDataSurface(std::string path){
		if(FileSys.fileExists(absPath(path))) {
			std::shared_ptr<DataSurface> ds;
			std::shared_ptr<KameleonWrapper> kw = std::make_shared<KameleonWrapper>(absPath(path));

			//find out what class to create
			ds = std::make_shared<DataPlane>(kw, path);


			ds->initialize();
			_dataSurfaces.push_back(ds);
		}else{
			std::cout << "file does not exist";
		}
	}
}