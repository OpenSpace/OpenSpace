/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <modules/iswa/rendering/cygnetsphere.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <modules/base/rendering/planetgeometry.h>
#include <modules/base/rendering/simplespheregeometry.h>
#include <openspace/util/powerscaledsphere.h>
#include <openspace/util/powerscaledscalar.h>

namespace openspace{

CygnetSphere::CygnetSphere(const ghoul::Dictionary& dictionary)
    :IswaCygnet(dictionary)
    ,_sphere(nullptr)
{
    float radius;
    dictionary.getValue("Radius", radius);
    _radius = radius;

}

CygnetSphere::~CygnetSphere(){}

bool CygnetSphere::createGeometry(){
    PowerScaledScalar radius =  PowerScaledScalar(6.371f*_radius, 6.0);
    int segments = 100;
    _sphere = std::make_shared<PowerScaledSphere>(radius, segments);
    _sphere->initialize();
    return true;
}

bool CygnetSphere::destroyGeometry(){
    _sphere = nullptr;
    return true;
}

void CygnetSphere::renderGeometry() const {
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    _sphere->render();
}



} //namespace openspace