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

#ifndef __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER__SUBSITEINITIALIZATIONJOB__H_
#define __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER__SUBSITEINITIALIZATIONJOB__H_

#include <modules/roverterrainrenderer/loadjob/loadjob.h>
#include <openspace/util/concurrentjobmanager.h>
#include <modules/roverterrainrenderer/renderable/subsitemodels.h>

#include <modules/base/rendering/asyncmultimodelgeometry.h>

namespace openspace {

struct SubsiteInitializationJob : LoadJob {
    SubsiteInitializationJob(const std::shared_ptr<SubsiteModels> subsiteModels)
    : _subsiteModels(subsiteModels)
    {}

    virtual ~SubsiteInitializationJob() = default;

    virtual void execute() override;

    virtual SubsiteModels product() override;

protected:
    std::shared_ptr<SubsiteModels> _subsiteModels;
};

} // openspace

#endif // __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER__SUBSITEINITIALIZATIONJOB__H_
