
// open space includes
#include "scenegraph/scenegraphnode.h"
#include "util/spice.h"

// ghoul includes
#include "ghoul/logging/logmanager.h"
#include "ghoul/logging/consolelog.h"

namespace openspace {
	
SceneGraphNode::SceneGraphNode() {

	nodeName_ = "";

	// init pointers with nullptr
	renderable_ = nullptr;
	parent_ = nullptr;
	boundingSphereVisible_ = false;
	renderableVisible_ = false;

	// spice not used when spiceID_ is 0, 0 is the sun barycenter
	spiceID_ = 0;
	parentSpiceID_ = 0;
}

SceneGraphNode::~SceneGraphNode() {
	// logger string
	std::string _loggerCat = "SceneGraphNode::~SceneGraphNode()";
	LDEBUG("Deallocating: " << nodeName_);

	// deallocate the renderable
	if(renderable_)
		delete renderable_;

	// deallocate the child nodes and delete them, iterate c++11 style
	for( auto &child: children_) {
		delete child;
	}

	// empty the vector
	children_.erase (children_.begin(),children_.end());
}

// essential
void SceneGraphNode::update() {

	if(spiceID_ > 0) {
		double state[3];
		//double orientation[3][3];
		Spice::ref().spk_getPosition(spiceID_, parentSpiceID_, state);

		// multiply with factor 1000, spice uses km as standard and Open Space uses m
		position_ = psc::CreatePSC(state[0]*1000.0,state[1]*1000.0,state[2]*1000.0);
		
		// update rotation
		//if(Spice::ref().spk_getOrientation(spiceName_,orientation)) {
		//	printf("%s\n",spiceName_);
		//	printf("%.5f %.5f %.5f \n", orientation[0][0], orientation[0][1], orientation[0][2]);
		//	printf("%.5f %.5f %.5f \n", orientation[1][0], orientation[1][1], orientation[1][2]);
		//	printf("%.5f %.5f %.5f \n", orientation[2][0], orientation[2][1], orientation[2][2]);
		//}
	}

	if(renderable_) {
		renderable_->update();
	}
}

void SceneGraphNode::evaluate(const Camera *camera, const psc & parentPosition) {

	const psc thisPosition = parentPosition + position_;
	const psc toCamera = thisPosition - camera->getPosition();
	
	// init as not visible
	boundingSphereVisible_ = true;
	renderableVisible_ = false;

	// check if camera is outside the node boundingsphere
	if(toCamera.length() > boundingSphere_) {
		
		// check if the boudningsphere is visible before avaluating children
		if( ! sphereInsideFrustum(thisPosition, boundingSphere_, camera)) {
			// the node is completely outside of the camera view, stop evaluating this node
			return;
		}
		
	} 
	boundingSphereVisible_ = true;
	// inside boudningsphere or parts of the sphere is visible, individual children needs to be evaluated
	
	// this node has an renderable
	if(renderable_) {

		//  check if the renderable boundingsphere is visible
		renderableVisible_ = sphereInsideFrustum(thisPosition, renderable_->getBoundingSphere(), camera);
	}

	// evaluate all the children, tail-recursive function(?)
	for(auto &child: children_) {
		child->evaluate(camera,thisPosition);
	}

}

void SceneGraphNode::render(const Camera *camera, const psc & parentPosition) {

	const psc thisPosition = parentPosition + position_;
	
	// check if camera is outside the node boundingsphere
	if( ! boundingSphereVisible_) {
		return;
	} 
	if(renderableVisible_) {
        if (nodeName_ == "earth")
            nodeName_ = nodeName_;
		renderable_->render(camera,thisPosition);
	}

	// evaluate all the children, tail-recursive function(?)
	for(auto &child: children_) {
		child->render(camera,thisPosition);
	}

}

// set & get
void SceneGraphNode::addNode(SceneGraphNode *child) {

	// add a child node and set this node to be the parent
	child->setParent(this);
	children_.push_back(child);
}

void SceneGraphNode::setName(const std::string &name) {
	nodeName_ = name;
}

void SceneGraphNode::setParent(SceneGraphNode *parent) {
	parent_ = parent;
}

void SceneGraphNode::setPosition(const psc &position) {
	position_ = position;
}

void SceneGraphNode::setSpiceID(const int spiceID, const int parentSpiceID) {
	spiceID_ = spiceID;
	parentSpiceID_ = parentSpiceID;
	update();
}

void SceneGraphNode::setSpiceName(const std::string &name) {
	spiceName_ = name;
}

const int SceneGraphNode::getSpiceID() const {
	return spiceID_;
}

const std::string & SceneGraphNode::getSpiceName() {
	return spiceName_;
}

const psc &SceneGraphNode::getPosition() const {
	return position_;
}

psc SceneGraphNode::getWorldPosition() const {

	// recursive up the hierarchy if there are parents available
	if(parent_) {
		return position_ + parent_->getWorldPosition();
	} else {
		return position_;
	}
}

// bounding sphere
pss SceneGraphNode::calculateBoundingSphere() {
	
	// set the vounding sphere to 0.0
	boundingSphere_ = 0.0;

	if(children_.size() > 0) { // node
		pss maxChild;

		// loop though all children and find the one furthest away/with the largest bounding sphere
		for(size_t i = 0; i < children_.size(); ++i) {

			// when positions is dynamix, change this part to fins the most distant position
			pss child = children_.at(i)->getPosition().length() + children_.at(i)->calculateBoundingSphere();
			if(child > maxChild) {
				maxChild = child;
			}
		}
		boundingSphere_ += maxChild;
	} else { // leaf

		// if has a renderable, use that boundingsphere
		if(renderable_)
			boundingSphere_ += renderable_->getBoundingSphere();
	}

	return boundingSphere_;
}

// renderable
void SceneGraphNode::setRenderable(Renderable *renderable) {
	renderable_ = renderable;
	update();
}

const Renderable *  SceneGraphNode::getRenderable() const{
	return renderable_;
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
    if (nodeName_ == "earth") {
        psc tmp = s_pos - camera->getPosition();
        
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