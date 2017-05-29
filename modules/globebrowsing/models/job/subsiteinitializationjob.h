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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING__SUBSITEINITIALIZATIONJOB__H_
#define __OPENSPACE_MODULE_GLOBEBROWSING__SUBSITEINITIALIZATIONJOB__H_

#include <modules/globebrowsing/tile/loadjob/loadjob2.h>
#include <modules/globebrowsing/other/concurrentjobmanager.h>
#include <modules/globebrowsing/models/subsitemodels.h>

#include <modules/base/rendering/asyncmultimodelgeometry.h>

namespace openspace {
namespace globebrowsing {

struct SubsiteInitializationJob : LoadJob2 {
	SubsiteInitializationJob(const std::shared_ptr<SubsiteModels> subsiteModels)
		: _subsiteModels(subsiteModels)
	{}

	virtual ~SubsiteInitializationJob() = default;

	virtual void execute() override;

	virtual std::shared_ptr<SubsiteModels> product() const override;

protected:
	std::shared_ptr<SubsiteModels> _subsiteModels;
};

} // globebrowsing
} // openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING__SUBSITEINITIALIZATIONJOB__H_