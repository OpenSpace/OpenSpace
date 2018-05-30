/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2018                                                               *
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

#include <modules/exoplanets/discoverymethods/discoverymethods.h>
#include <modules/exoplanets/exoplanetsmodule.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scripting/scriptengine.h>

namespace {
	constexpr const char* _loggerCat = "DiscoveryMethods";

	static const openspace::properties::Property::PropertyInfo TransitMethodInfo = {
		"TransitMethod",
		"Show transit method",
		"Change the view so that the transit method can be presented."
	};
	
	static const openspace::properties::Property::PropertyInfo DopplerMethodInfo = {
		"DopplerMethod",
		"Show doppler method",
		"Change the view so that the doppler method can be presented."
	};
} // namespace

namespace openspace::exoplanets{

	DiscoveryMethods::DiscoveryMethods()
		: PropertyOwner({ "DiscoveryMethods" })
		, _showTransit(TransitMethodInfo, false)
		, _showDoppler(DopplerMethodInfo, false)
	{
		addProperty(_showTransit);
		addProperty(_showDoppler);
        _showTransit.onChange([&]() { LINFO("transit"); });
        _showDoppler.onChange([&]() { LINFO("doppler"); });
	}

} // namespce

