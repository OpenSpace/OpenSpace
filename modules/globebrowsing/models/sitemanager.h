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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___SITE_MANAGER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___SITE_MANAGER___H__

#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalar/boolproperty.h>

#include <modules/globebrowsing/models/subsite.h>

struct SiteWithDrives {
	std::string _site;
	std::vector<std::string> _drives;
	std::vector<std::string> sols;
	std::vector<glm::dvec2> _driveCoords;
};

namespace openspace {
namespace globebrowsing {
class SitePropertyOwner;

class SiteManager : public properties::PropertyOwner {
public: 
	SiteManager(std::string name, std::vector<std::shared_ptr<Subsite>> ss);
	
	std::vector<std::shared_ptr<SitePropertyOwner>> siteProperties() { return _sitePropertyOwner; };

	Subsite driveEnabled();
private:
	std::vector<std::shared_ptr<SitePropertyOwner>> _sitePropertyOwner;

	//std::vector<std::shared_ptr<Subsite>> _subsites;

	properties::BoolProperty _isEnabledProperty;
};
}
}

#endif //__OPENSPACE_MODULE_GLOBEBROWSING___SITE_MANAGER___H__
