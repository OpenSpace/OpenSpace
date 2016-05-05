//  * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
//  * software and associated documentation files (the "Software"), to deal in the Software *
//  * without restriction, including without limitation the rights to use, copy, modify,    *
//  * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
//  * permit persons to whom the Software is furnished to do so, subject to the following   *
//  * conditions:                                                                           *
//  *                                                                                       *
//  * The above copyright notice and this permission notice shall be included in all copies *
//  * or substantial portions of the Software.                                              *
//  *                                                                                       *
//  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
//  * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
//  * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
//  * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
//  * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
//  * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
//  ****************************************************************************************/

#include <modules/iswa/rendering/cygnetsphere.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <modules/base/rendering/planetgeometry.h>
#include <modules/base/rendering/simplespheregeometry.h>

namespace openspace{

CygnetSphere::CygnetSphere(const ghoul::Dictionary& dictionary)
    :ISWACygnet(dictionary)
    ,_geometry(nullptr)
{
	//read segments from dictronary
	_segments = 20;
}

CygnetSphere::~CygnetSphere(){}

bool CygnetSphere::createGeometry(){
    glm::vec4 radius(6.371f, 6.371f, 6.371f, 6); //use scale in _data

    ghoul::Dictionary geometryDictionary;
    geometryDictionary.setValue(SceneGraphNode::KeyName, name());
    geometryDictionary.setValue("Radius", radius);
    geometryDictionary.setValue("Segments", _segments);

    glm::vec4 radiuss;
    bool success = geometryDictionary.getValue("Radius", radiuss);
    if(success){
        std::cout << "It exists" << std::endl;
    }

    _geometry = std::make_shared<planetgeometry::SimpleSphereGeometry>(geometryDictionary);
    _geometry->initialize(this);
}

bool CygnetSphere::destroyGeometry(){
    if(_geometry)
        _geometry->deinitialize();

    _geometry = nullptr;
}

bool CygnetSphere::renderGeometry(){
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    _geometry->render();
}



} //namespace openspace