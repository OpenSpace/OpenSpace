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

#ifndef __OPENSPACE___MEDIAN_POINTCLOUD_FILTERING___H__
#define __OPENSPACE___MEDIAN_POINTCLOUD_FILTERING___H__

#include <modules/globebrowsing/tasks/pointcloudfilter.h>

#include <pcl/filters/median_filter.h>

namespace openspace {
namespace globebrowsing {
class MedianPointCloudFilter : public PointCloudFilter {
public:
  MedianPointCloudFilter(const ghoul::Dictionary& dictionary);

  virtual bool initialize();

  virtual bool filter(pcl::PointCloud<pcl::PointXYZ>::Ptr outputCloud);

private:
  int _windowSize = 7;
  float _maxAllowedMovement = 0.02;

  pcl::MedianFilter<pcl::PointXYZ> _mFilter;
};
}
}

#endif //__OPENSPACE___MEDIAN_POINTCLOUD_FILTERING___H__
