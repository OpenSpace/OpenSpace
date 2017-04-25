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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING__SURFACEMODELLOADJOB__H_
#define __OPENSPACE_MODULE_GLOBEBROWSING__SURFACEMODELLOADJOB__H_

#include <modules/globebrowsing/tile/loadjob/loadjob2.h>
#include <modules/globebrowsing/other/concurrentjobmanager.h>
#include <modules/globebrowsing/models/model.h>
#include <ghoul/misc/dictionary.h>

#include <modules/base/rendering/modelgeometry.h>

namespace openspace {
namespace globebrowsing {

	class modelgeometry::ModelGeometry;
	
struct SurfaceModelLoadJob : LoadJob2 {
	SurfaceModelLoadJob(const ghoul::Dictionary& dictionary, std::shared_ptr<Model> model)
		: _dictionary(dictionary)
		, _model(model)
	{}
	
	virtual ~SurfaceModelLoadJob() = default;
 	
	virtual void execute() override;

	virtual std::shared_ptr<Model> product() const override;

protected:
	ghoul::Dictionary _dictionary;
	std::shared_ptr<Model> _model;
};

} // globebrowsing
} // openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING__SURFACEMODELLOADJOB__H_