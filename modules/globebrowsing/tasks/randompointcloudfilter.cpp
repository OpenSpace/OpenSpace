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

#include <modules/globebrowsing/tasks/randompointcloudfilter.h>

#include <ghoul/logging/logmanager.h>

namespace {
  const std::string _loggerCat = "RandomPointCloudFilter";
  const std::string WindowSize = "WindowSize";
  const std::string MaxAllowedMovement = "MaxAllowedMovement";
}

namespace openspace {
namespace globebrowsing {
  RandomPointCloudFilter::RandomPointCloudFilter(const ghoul::Dictionary& dictionary)
    : PointCloudFilter(dictionary) {
	  _rFilter.setKeepOrganized(true);
  }

  bool RandomPointCloudFilter::initialize() {
	return true;
  }

  bool RandomPointCloudFilter::filter(pcl::PointCloud<pcl::PointXYZ>::Ptr outputCloud) {
	_rFilter.setSample(_inputCloud->size() / 2);
	_rFilter.setInputCloud(_inputCloud);
	_rFilter.filter(*outputCloud);
	return true;
  }
}
}
