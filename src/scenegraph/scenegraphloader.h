#ifndef SCENEGRAPHLOADER_H
#define SCENEGRAPHLOADER_H

// open space includes
#include "object.h"
#include "scenegraph/scenegraphnode.h"

// std includes
#include <vector>
#include <string>
#include <map>

// ghoul includes
#include "ghoul/opengl/programobject.h"
#include "ghoul/opengl/texture.h"

// sgct includes
#include "external/tinyxml2.h"

namespace openspace {

class SceneGraphLoader: public Object {
public:

	// constructors & destructor
	SceneGraphLoader(std::vector<SceneGraphNode*> *nodes, std::map<std::string, ghoul::opengl::ProgramObject*> *commonShaders);
	~SceneGraphLoader();

	SceneGraphNode *loadSceneGraph(const std::string &path);

private:

	SceneGraphNode *root_;
	std::map<std::string, ghoul::opengl::ProgramObject*> *commonShaders_;
	std::vector<SceneGraphNode*> *nodes_;

	// private methods
	void loadSceneGraphTree(tinyxml2::XMLElement* node, SceneGraphNode *current, const std::string &path);
	SceneGraphNode * loadSceneGraphNodeFromFile(std::string name, SceneGraphNode *parent, const std::string &path);
	SceneGraphNode * loadSceneGraphNode(tinyxml2::XMLElement *xmlnode, std::string name, SceneGraphNode *parent, const std::string &path);
	
	bool getSpice(SceneGraphNode *node,SceneGraphNode *parent, tinyxml2::XMLElement *xmlnode);
	bool getRadii(pss *radii, tinyxml2::XMLElement *xmlnode);
	bool getTexture(ghoul::opengl::Texture **texture, const std::string &path, tinyxml2::XMLElement *xmlnode);
	bool getShader(ghoul::opengl::ProgramObject **program, const std::string &path, tinyxml2::XMLElement *xmlnode);

};

} // namespace openspace

#endif