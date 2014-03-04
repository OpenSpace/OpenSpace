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
 
// open space includes
#include "scenegraph/scenegraphnode.h"
#include "util/spice.h"

// ghoul includes
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/consolelog.h>

namespace {
	std::string _loggerCat = "SceneGraphNode";
}

namespace openspace {
	
SceneGraphNode::SceneGraphNode(): _parent(nullptr), _nodeName("Unnamed OpenSpace SceneGraphNode"),
                                  _renderable(nullptr), _renderableVisible(false),
                                  _boundingSphereVisible(false), _spiceName("") {}

SceneGraphNode::~SceneGraphNode() {
	LDEBUG("Deallocating: " << _nodeName);

	// deallocate the renderable
	if(_renderable != nullptr)
		delete _renderable;

	// deallocate the child nodes and delete them, iterate c++11 style
	for( auto child: _children)
		delete child;
}


bool SceneGraphNode::initialize() {
    return true;
}

bool SceneGraphNode::initializeWithDictionary(ghoul::Dictionary* dictionary) {
    
    if( ! initialize()) {
        return false;
    }
    
    // set the _nodeName if available
    dictionary->getValue("Name", _nodeName);
    
    // get the renderable
    if(dictionary->hasKey("Renderable")) {
        ghoul::Dictionary* renderableDictionary;
        if(dictionary->getValue("Renderable", renderableDictionary)) {
            
        }
    }
    
    // get the position
    if(dictionary->hasKey("Position")) {
        ghoul::Dictionary* positionDictionary;
        if(dictionary->getValue("Position", positionDictionary)) {
            
        }
    }

    return true;
}

// essential
void SceneGraphNode::update() {
    /*
	if(_spiceID > 0) {
		double state[3];
		//double orientation[3][3];
		Spice::ref().spk_getPosition(_spiceID, _parentSpiceID, state);

		// multiply with factor 1000, spice uses km as standard and Open Space uses m
		_position = psc::CreatePSC(state[0]*1000.0,state[1]*1000.0,state[2]*1000.0);
		
		// update rotation
		//if(Spice::ref().spk_getOrientation(spiceName_,orientation)) {
		//	printf("%s\n",spiceName_);
		//	printf("%.5f %.5f %.5f \n", orientation[0][0], orientation[0][1], orientation[0][2]);
		//	printf("%.5f %.5f %.5f \n", orientation[1][0], orientation[1][1], orientation[1][2]);
		//	printf("%.5f %.5f %.5f \n", orientation[2][0], orientation[2][1], orientation[2][2]);
		//}
	}

	if(_renderable) {
		_renderable->update();
	}
    */
}

void SceneGraphNode::evaluate(const Camera *camera, const psc & parentPosition) {

	const psc thisPosition = parentPosition + _position;
	const psc camPos = camera->getPosition();
	const psc toCamera = thisPosition - camPos;
	
	// init as not visible
	_boundingSphereVisible = true;
	_renderableVisible = false;

	// check if camera is outside the node boundingsphere
	if(toCamera.length() > _boundingSphereVisible) {
		
		// check if the boudningsphere is visible before avaluating children
		if( ! sphereInsideFrustum(thisPosition, _boundingSphere, camera)) {
			// the node is completely outside of the camera view, stop evaluating this node
			return;
		}
		
	} 
	_boundingSphereVisible = true;
	// inside boudningsphere or parts of the sphere is visible, individual children needs to be evaluated
	
	// this node has an renderable
	if(_renderable) {

		//  check if the renderable boundingsphere is visible
		_renderableVisible = sphereInsideFrustum(thisPosition, _renderable->getBoundingSphere(), camera);
	}

	// evaluate all the children, tail-recursive function(?)
	for(auto &child: _children) {
		child->evaluate(camera,thisPosition);
	}

}

void SceneGraphNode::render(const Camera *camera, const psc & parentPosition) {

	const psc thisPosition = parentPosition + _position;
	
	// check if camera is outside the node boundingsphere
	if( ! _boundingSphereVisible) {
		return;
	} 
	if(_renderableVisible) {
        if (_nodeName == "earth")
            _nodeName = _nodeName;
		_renderable->render(camera,thisPosition);
	}

	// evaluate all the children, tail-recursive function(?)
	for(auto &child: _children) {
		child->render(camera,thisPosition);
	}

}

// set & get
void SceneGraphNode::addNode(SceneGraphNode *child) {
	// add a child node and set this node to be the parent

    child->setParent(this);
    _children.push_back(child);
}

void SceneGraphNode::setName(const std::string &name) {
	_nodeName = name;
}

void SceneGraphNode::setParent(SceneGraphNode *parent) {
	_parent = parent;
}


void SceneGraphNode::setPosition(const psc &position) {
	_position = position;
}

void SceneGraphNode::setSpiceID(const int spiceID, const int parentSpiceID) {
	
    _spiceID = spiceID;
	_parentSpiceID = parentSpiceID;
	update();
 
}

void SceneGraphNode::setSpiceName(const std::string &name) {
	_spiceName = name;
}

const int SceneGraphNode::getSpiceID() const {
	return 0;//spiceID_;
}

const std::string & SceneGraphNode::getSpiceName() {
	return _spiceName;
}

const psc &SceneGraphNode::getPosition() const {
	return _position;
}

psc SceneGraphNode::getWorldPosition() const {

	// recursive up the hierarchy if there are parents available
	if(_parent) {
		return _position + _parent->getWorldPosition();
	} else {
		return _position;
	}
}

// bounding sphere
pss SceneGraphNode::calculateBoundingSphere() {
	
	// set the vounding sphere to 0.0
	_boundingSphere = 0.0;

	if(_children.size() > 0) { // node
		pss maxChild;

		// loop though all children and find the one furthest away/with the largest bounding sphere
		for(size_t i = 0; i < _children.size(); ++i) {

			// when positions is dynamix, change this part to fins the most distant position
			pss child = _children.at(i)->getPosition().length() + _children.at(i)->calculateBoundingSphere();
			if(child > maxChild) {
				maxChild = child;
			}
		}
		_boundingSphere += maxChild;
	} else { // leaf

		// if has a renderable, use that boundingsphere
		if(_renderable)
			_boundingSphere += _renderable->getBoundingSphere();
	}

	return _boundingSphere;
}

// renderable
void SceneGraphNode::setRenderable(Renderable *renderable) {
	_renderable = renderable;
	update();
}

const Renderable *  SceneGraphNode::getRenderable() const{
	return _renderable;
}

// private helper methods
bool SceneGraphNode::sphereInsideFrustum(const psc s_pos, const pss & s_rad, const Camera *camera) {

	// direction the camera is looking at in power scale
	psc psc_camdir = psc(camera->getViewDirection());

	// the position of the camera, moved backwards in the view direction to encapsulate the sphere radius
	psc U = camera->getPosition() - psc_camdir * s_rad * ( 1.0 / camera->getSinMaxFov());

	// the vector to the object from the new position
	psc D = s_pos - U;
	
	// check if outside the maximum angle
    if (_nodeName == "earth") {
        //psc tmp = s_pos - camera->getPosition();
        
        //LINFOC("", "Angle: " << psc_camdir.angle(D));
        //LINFOC("", "Pos: " << tmp.getVec4f()[0] << " " << tmp.getVec4f()[1] << " " << tmp.getVec4f()[2] << " " << tmp.getVec4f()[3]);
    }
    const double a = psc_camdir.angle(D);
	if ( a < camera->getMaxFov())
	{
		// center is inside K''
		D = s_pos - camera->getPosition();
		if ( D.length()* psc_camdir.length() *camera->getSinMaxFov() <= -psc_camdir.dot(D)) {
			// center is inside K'' and inside K'
			return D.length() <= s_rad;
		} else {
			// center is inside K'' and outside K'
			return true;
		}
	} else {
		// outside the maximum angle
		return false;
	}
	
}
	
} // namespace openspace