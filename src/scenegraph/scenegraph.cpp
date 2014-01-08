
// open space includes
#include "scenegraph/scenegraph.h"
#include "scenegraph/scenegraphloader.h"
#include "rendering/renderablebody.h"
#include "interaction/interactionhandler.h"
#include "util/spice.h"

// ghoul includes
#include "ghoul/opengl/programobject.h"
#include "ghoul/logging/logmanager.h"
#include "ghoul/logging/consolelog.h"
#include "ghoul/opengl/texturereader.h"
#include "ghoul/opengl/texture.h"
#include <ghoul/filesystem/filesystem.h>

namespace openspace {
	
SceneGraph::SceneGraph() {
	root_ = nullptr;
}

SceneGraph::~SceneGraph() {

	// deallocate the scene graph
	if(root_)
		delete root_;
	
	// deallocate shaders, iterate c++11 style
	for (auto& shaderTuple: shaders_) {

		// the shader is in the maps second position
		delete shaderTuple.second;
	}
		
}

void SceneGraph::init() {
	// logger string
	std::string _loggerCat = "SceneGraph::init";
	
	SceneGraphLoader *loader = new SceneGraphLoader(&nodes_, &shaders_);
    root_ = loader->loadSceneGraph(p("${BASE_PATH}/modules"));
	update();
	pss bs = root_->calculateBoundingSphere();
}

void SceneGraph::update() {
	for(int i = 0; i < nodes_.size(); ++i) {
		nodes_[i]->update();
	}
}

void SceneGraph::evaluate(Camera *camera) {
	root_->evaluate(camera);
}

void SceneGraph::render(Camera *camera) {
	root_->render(camera);
}

	
} // namespace openspace