/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014                                                                    *
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

#ifndef __SIMPLESPHEREGEOMETRYPROJECTION_H__
#define __SIMPLESPHEREGEOMETRYPROJECTION_H__

#include <openspace/rendering/planets/planetgeometryprojection.h>
#include <openspace/properties/vectorproperty.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/util/powerscaledsphere.h>

namespace openspace {

class RenderablePlanet;

namespace planetgeometryprojection {

class SimpleSphereGeometryProjection : public PlanetGeometryProjection {
public:
	SimpleSphereGeometryProjection(const ghoul::Dictionary& dictionary);
	~SimpleSphereGeometryProjection();


	bool initialize(RenderablePlanetProjection* parent) override;
    void deinitialize() override;
    void render() override;

private:
    void createSphere();

    properties::Vec2Property _radius;
    properties::IntProperty _segments;

    PowerScaledSphere* _planet;
};

}  // namespace planetgeometry
}  // namespace openspace

#endif // __SIMPLESPHEREGEOMETRY_H__
