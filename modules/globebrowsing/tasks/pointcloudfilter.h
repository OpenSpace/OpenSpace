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

#ifndef __OPENSPACE___POINTCLOUD_FILTERING___H__
#define __OPENSPACE___POINTCLOUD_FILTERING___H__

#include <ghoul/misc/dictionary.h>

#include <pcl/point_types.h>
#include <pcl/point_cloud.h>

namespace openspace {
namespace globebrowsing {
class PointCloudFilter  {
public:
	static std::unique_ptr<PointCloudFilter> createFromDictionary(const ghoul::Dictionary &dictionary);
	
	PointCloudFilter(const ghoul::Dictionary dictionary);

	virtual bool initialize() = 0;

	virtual bool filter(pcl::PointCloud<pcl::PointXYZ>::Ptr outputCloud) = 0;

	void setInputCloud(pcl::PointCloud<pcl::PointXYZ>::Ptr inputCloud);

protected:
	pcl::PointCloud<pcl::PointXYZ>::Ptr _inputCloud;
};
}
}

#endif //__OPENSPACE___POINTCLOUD_FILTERING___H__
