#ifndef SCENEGRAPHNODE_H
#define SCENEGRAPHNODE_H

// open space includes
#include "rendering/renderable.h"

// std includes
#include <iostream>
#include <vector>
#include <string>

namespace openspace {

class SceneGraphNode {
public:

	// constructors & destructor
	SceneGraphNode();
	~SceneGraphNode();

	// essential
	void update();
	void evaluate(const Camera *camera, const psc &parentPosition = psc());
	void render(const Camera *camera, const psc &parentPosition = psc());

	// set & get
	void addNode(SceneGraphNode *child);
	void setName(const std::string &name);
	void setParent(SceneGraphNode *parent);
	void setPosition(const psc &position);
	void setSpiceID(const int spiceID, const int parentSpiceID);
	void setSpiceName(const std::string &name);
	const int getSpiceID() const;
	const std::string & getSpiceName();
	const psc& getPosition() const;
	psc getWorldPosition() const;
    std::string nodeName() const { return nodeName_; }

    SceneGraphNode* parent() const { return parent_; }
    const std::vector<SceneGraphNode*>& children() const { return children_; }

	// bounding sphere
	pss calculateBoundingSphere();
	
    SceneGraphNode* get(const std::string& name) {
        if (nodeName_ == name)
            return this;
        else
            for (auto it : children_)
                return it->get(name);
    }

    void print() const {
        std::cout << nodeName_ << std::endl;
        for (auto it : children_) {
            it->print();
        }
    }

	// renderable
	void setRenderable(Renderable *renderable);
	const Renderable * getRenderable() const;
	
private:

	// essential
	std::vector<SceneGraphNode*> children_;
	SceneGraphNode *parent_;
	psc position_;
	std::string nodeName_;

	// renderable
	Renderable *renderable_;
	bool renderableVisible_;
	
	// bounding sphere
	bool boundingSphereVisible_;
	pss boundingSphere_;
	
	// spice
	std::string spiceName_;
	int spiceID_;
	int parentSpiceID_;
	
	// private helper methods
	bool sphereInsideFrustum(const psc s_pos, const pss & s_rad, const Camera *camera);

};

} // namespace openspace

#endif