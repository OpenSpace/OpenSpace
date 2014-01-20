
#include "openspaceengine.h"
// open space includes
#include "scenegraph/scenegraphloader.h"
#include "rendering/renderablebody.h"
#include "rendering/renderableplanet.h"
#include "interaction/interactionhandler.h"
#include "util/spice.h"

// ghoul includes
#include "ghoul/logging/logmanager.h"
#include "ghoul/logging/consolelog.h"
#include "ghoul/opengl/texturereader.h"
#include "ghoul/opengl/texture.h"

// std includes
#include <utility> 

namespace openspace {


SceneGraph* loadSceneGraph(const std::string& sceneGraphPath) {
    SceneGraph* result = new SceneGraph;
    //result->setRo

    return result;
}


	
SceneGraphLoader::SceneGraphLoader(std::vector<SceneGraphNode*> *nodes, std::map<std::string, ghoul::opengl::ProgramObject*> *commonShaders) {
	root_ = nullptr;
	nodes_ = nodes;
	commonShaders_ = commonShaders;
}

SceneGraphLoader::~SceneGraphLoader() {
}

SceneGraphNode* SceneGraphLoader::loadSceneGraph(const std::string &path) {
	
	assert(commonShaders_);

	std::string _loggerCat = "SceneGraphLoader::loadSceneGraph";

	// initializes the scene graph
	root_ = new SceneGraphNode();

	// loading all common stuff
	tinyxml2::XMLDocument commonXML;
	std::string commonPath = path + "/common/common.xml";
	commonXML.LoadFile(commonPath.c_str());

	// loading the shaders
	tinyxml2::XMLElement* shaders = commonXML.FirstChildElement( "shaders" );
	if(shaders) {
		tinyxml2::XMLElement* shader = shaders->FirstChildElement( "shader" );
		for(;shader; shader = shader->NextSiblingElement( "shader" )) {
			if(shader->Attribute("identifier")) {
				std::string identifier = shader->Attribute("identifier");
				ghoul::opengl::ProgramObject *programObject = nullptr;
				if(getShader(&programObject,path + "/common/", shader ))
					commonShaders_->insert( std::make_pair(identifier, programObject) );
			}
		}
	}
	
	tinyxml2::XMLDocument scenegraphXML;
	std::string scenegraphPath = path + "/scenegraph.xml";
	scenegraphXML.LoadFile(scenegraphPath.c_str());

	// loading the scenegraph
	tinyxml2::XMLElement* root = scenegraphXML.FirstChildElement( "root" );
	if(root) {
		tinyxml2::XMLElement* node = root->FirstChildElement( "node" );
		loadSceneGraphTree(node, root_, path);
	}
		
	return root_;
}

// ugly
void SceneGraphLoader::loadSceneGraphTree(tinyxml2::XMLElement* node, SceneGraphNode *current, const std::string &path) {
	// ghoul logger
	std::string _loggerCat = "SceneGraphLoader::loadSceneGraphTree";

	for(;node; node = node->NextSiblingElement( "node" )) {
		SceneGraphNode *thisNode = nullptr;
		std::string name = "";
		if(node->Attribute("module")) {
			name = node->Attribute("module");
			thisNode = loadSceneGraphNodeFromFile(name, current, path);
		} else {
			thisNode = loadSceneGraphNode(node, "", current, path);
		}

		if(thisNode) {
			tinyxml2::XMLElement* children = node->FirstChildElement( "node" );
			if(children) {
				loadSceneGraphTree(children, thisNode, path);
			}
			nodes_->push_back(thisNode);

			// camera stuff
			tinyxml2::XMLElement* camera = node->FirstChildElement( "camera" );
			if(camera && camera->Attribute("setting")) {
				std::string setting = camera->Attribute("setting");
				if(setting == "focus") {
                    // HACK
                    OsEng.interactionHandler().setFocusNode(thisNode->parent());
					LINFO("Setting camera focus: " << name);
				} else if(setting == "position") {
					OsEng.interactionHandler().getCamera()->setPosition(thisNode->getWorldPosition());
					LINFO("Setting camera position: " << name);
				}
			}

					
		} else {
			LERROR("Could not load SceneGraphNode [ " << name << " ], ignoring all children!");
		}
		
	}
}

SceneGraphNode * SceneGraphLoader::loadSceneGraphNodeFromFile(std::string name, SceneGraphNode *parent, const std::string &path) {
	std::string _loggerCat = "SceneGraphLoader::loadSceneGraphNode";
	
	// path and name
	std::string nodePath = path + "/" + name + "/" + name + ".xml";
	tinyxml2::XMLDocument nodeXML;
	nodeXML.LoadFile(nodePath.c_str());

	return loadSceneGraphNode(nodeXML.FirstChildElement( "module" ), name, parent, path + "/" + name);
}

SceneGraphNode * SceneGraphLoader::loadSceneGraphNode(tinyxml2::XMLElement *xmlnode, std::string name, SceneGraphNode *parent, const std::string &path) {

	assert(commonShaders_);

	std::string _loggerCat = "SceneGraphLoader::loadSceneGraphNode";
	
	// the node
	SceneGraphNode *thisNode = nullptr;

	// load the properties
	tinyxml2::XMLElement* moduleElement = xmlnode;
	if(moduleElement) {

		thisNode = new SceneGraphNode();

		// load spice
		getSpice(thisNode, parent, moduleElement->FirstChildElement( "spice" ));
		
		tinyxml2::XMLElement* renderableElement = moduleElement->FirstChildElement( "renderable" );
		if(renderableElement && renderableElement->Attribute("type")) {
			std::string type = renderableElement->Attribute("type");
			if(type == "RenderableBody") {

				RenderableBody *renderable = nullptr;

				// load radius
				pss radius;

				// the radii element takes priority when setting the radius
				if( getRadii(&radius, renderableElement->FirstChildElement( "radii" )) || thisNode->getSpiceID() > 0) {
					double radii[3];
					int n;
					if(Spice::ref().getRadii(thisNode->getSpiceName(),radii,&n)) {
						
						// multiply with factor 1000, spice uses km as standard and Open Space uses m
						radius = pss::CreatePSS(radii[0]*1000.0);
					} else {
						LERROR("Tried to use spice raddi but failed for: " << name);
						delete thisNode;
						return nullptr;
					}
				} else {
					LERROR("Could not find radiiElement or spice id for: " << name << " " << thisNode->getSpiceID() );
					delete thisNode;
					return nullptr;
				}
				

				LINFO("Adding renderable: "<< name);
				// load texture and shader
				ghoul::opengl::Texture *texture = nullptr;
				ghoul::opengl::ProgramObject *program = nullptr;

				tinyxml2::XMLElement* textureElement = renderableElement->FirstChildElement( "texture" );
				if(textureElement) {
					if( ! getTexture(&texture, path, textureElement->FirstChildElement( "file" ))) {
						LERROR("Could not load texture " << name);
					}
				} else {
					LERROR("Could not find texture for: " << name);
				}

				// load shader
				if( ! getShader(&program, path, renderableElement->FirstChildElement( "shader" ))) {
					LERROR("Could not find shader for: " << name);
				}

				if( ! texture || ! program) {
					delete thisNode;
					return nullptr;
				}
				
				// create the renderable
				renderable = new RenderableBody(radius);
				renderable->setTexture(texture);
				renderable->setProgramObject(program);
				thisNode->setRenderable(renderable);

			} else if(type == "RenderablePlanet") {

				RenderablePlanet *renderable = nullptr;

				// load radius
				pss radius;

				// the radii element takes priority when setting the radius
				if( getRadii(&radius, renderableElement->FirstChildElement( "radii" )) || thisNode->getSpiceID() > 0) {
					double radii[3];
					int n;
					if(Spice::ref().getRadii(thisNode->getSpiceName(),radii,&n)) {
						
						// multiply with factor 1000, spice uses km as standard and Open Space uses m
						radius = pss::CreatePSS(radii[0]*1000.0);
					} else {
						LERROR("Tried to use spice raddi but failed for: " << name);
						delete thisNode;
						return nullptr;
					}
				} else {
					LERROR("Could not find radiiElement or spice id for: " << name << " " << thisNode->getSpiceID() );
					delete thisNode;
					return nullptr;
				}
				

				LINFO("Adding renderable: "<< name);
				// load texture and shader
				ghoul::opengl::Texture *texture = nullptr;
				ghoul::opengl::ProgramObject *program = nullptr;

				tinyxml2::XMLElement* textureElement = renderableElement->FirstChildElement( "texture" );
				if(textureElement) {
					if( ! getTexture(&texture, path, textureElement->FirstChildElement( "file" ))) {
						LERROR("Could not load texture " << name);
					}
				} else {
					LERROR("Could not find texture for: " << name);
				}

				// load shader
				if( ! getShader(&program, path, renderableElement->FirstChildElement( "shader" ))) {
					LERROR("Could not find shader for: " << name);
				}

				if( ! texture || ! program) {
					delete thisNode;
					return nullptr;
				}
				
				// create the renderable
				renderable = new RenderablePlanet(radius);
				renderable->setTexture(texture);
				renderable->setProgramObject(program);
				thisNode->setRenderable(renderable);
				LINFO("Adding renderablePlanet");
			}
		}

		// set identifier and add to parent
		thisNode->setName(name);
		parent->addNode(thisNode);
		return thisNode;

	} else {
		LERROR("Unable to open properties element");
		return nullptr;
	}
}


bool SceneGraphLoader::getSpice(SceneGraphNode *node, SceneGraphNode *parent, tinyxml2::XMLElement *xmlnode) {
	std::string _loggerCat = "SceneGraphLoader::getSpice";
	if(xmlnode && node) {
		tinyxml2::XMLElement* identifierElement = xmlnode->FirstChildElement( "identifier" );
		if(identifierElement && identifierElement->Attribute("string")) {
			std::string identifier = identifierElement->Attribute("string");
						
			int spiceID;
			int spiceIDFound;
			Spice::ref().bod_NameToInt(identifierElement->Attribute("string"), &spiceID, &spiceIDFound);
			if(spiceIDFound) {

				// The spice id exists, save the identifier
				node->setSpiceName(identifier);

				int parentSpice = 0; // sun barycenter
				if(parent) {
					if( parent->getSpiceID() != 0)
						parentSpice = parent->getSpiceID();
				}
				node->setSpiceID(spiceID,parentSpice);
				return true;
			} else {
				LERROR("Could not find spice ID ");
			}
		} else {
			LERROR("Could not find spice identifier element in xml ");
		}
	}
	return false;
}

bool SceneGraphLoader::getRadii(pss *radii, tinyxml2::XMLElement *xmlnode) {
	std::string _loggerCat = "SceneGraphLoader::getRadii";
	if(xmlnode ) {
		double value = 0.0;
		double power = 0.0;
		if(xmlnode->Attribute("value")) {
			value = xmlnode->DoubleAttribute("value");
			if(xmlnode->Attribute("power"))
				power = xmlnode->DoubleAttribute("power");
			*radii = pss(value, power);
			return true;
		}
	}
	return false;
}

bool SceneGraphLoader::getTexture(ghoul::opengl::Texture **texture, const std::string &path, tinyxml2::XMLElement *xmlnode) {
	std::string _loggerCat = "SceneGraphLoader::getTexture";
	if(xmlnode && xmlnode->Attribute("path")) {
		std::string texturePath = path + "/" + xmlnode->Attribute("path");
		*texture = ghoul::opengl::loadTexture(texturePath);

		// if textures where accessed, upload them to the graphics card. This check needs to be done to avoid crash.
		if(texture) {
			LINFO("Uploading tetxure: "<< texturePath);
			(*texture)->uploadTexture();
			//ghoul::opengl::Texture *tmp = new ghoul::opengl::Texture(*texture);
			//texture = tmp;
			return true;
		} else {
			LERROR("Could not load texture: "<< texturePath);
		}
	} else {
		LERROR("Could not find file element");
	}
	return false;
}

bool SceneGraphLoader::getShader(ghoul::opengl::ProgramObject **program, const std::string &path, tinyxml2::XMLElement *xmlnode) {
	std::string _loggerCat = "SceneGraphLoader::getShader";
	if( ! xmlnode)
		return false;

	// use a common shader
	if(xmlnode->IntAttribute("common") && xmlnode->Attribute( "identifier" )) {
		std::string identifier  = xmlnode->Attribute( "identifier" );
		std::map<std::string, ghoul::opengl::ProgramObject*>::iterator it;
		it = commonShaders_->find(identifier);
		if(it != commonShaders_->end()) {
			*program = it->second;
			return true;
		} 

		LERROR("Could not find common shader: " << identifier);
		return false;

	// load and return the shader
	} else {
		tinyxml2::XMLElement* glsl_vs = xmlnode->FirstChildElement( "vertex" );
		tinyxml2::XMLElement* glsl_fs = xmlnode->FirstChildElement( "fragment" );
		if(glsl_vs && glsl_fs) {
			if(glsl_vs->Attribute("path") && glsl_fs->Attribute("path")) {
				std::string vs_path = glsl_vs->Attribute("path");
				std::string fs_path = glsl_fs->Attribute("path");
							
				vs_path = path + vs_path;
				fs_path = path + fs_path;
				ghoul::opengl::ProgramObject *programObject = new ghoul::opengl::ProgramObject();
				ghoul::opengl::ShaderObject *vs = new ghoul::opengl::ShaderObject(ghoul::opengl::ShaderObject::ShaderType::ShaderTypeVertex, vs_path);
				ghoul::opengl::ShaderObject *fs = new ghoul::opengl::ShaderObject(ghoul::opengl::ShaderObject::ShaderType::ShaderTypeFragment, fs_path);
				programObject->attachObject(vs);
				programObject->attachObject(fs);
						
				// check for bindings
				tinyxml2::XMLElement* bindings = xmlnode->FirstChildElement( "bindings" );
				if(bindings) {

					tinyxml2::XMLElement* attributes = bindings->FirstChildElement( "attribute" );
					for(;attributes; attributes = attributes->NextSiblingElement( "attribute" )) {
						if(attributes->Attribute("name") && attributes->Attribute("position")) {
							std::string name = attributes->Attribute("name");
							int position = attributes->IntAttribute("position");
							programObject->bindAttributeLocation(name,position);
						}
					}
					tinyxml2::XMLElement* fragdata = bindings->FirstChildElement( "fragdata" );
					for(;fragdata; fragdata = fragdata->NextSiblingElement( "fragdata" )) {
						if(fragdata->Attribute("name") && fragdata->Attribute("position")) {
							std::string name = fragdata->Attribute("name");
							int position = fragdata->IntAttribute("position");
							programObject->bindFragDataLocation(name,position);
						}
					}
				}

				if(programObject->compileShaderObjects()) {
					if(programObject->linkProgramObject()) {
						*program = programObject;
						LINFO("Common shader successfully loaded!");
						return true;
					} else {
						LERROR("Common shader could not be linked!");
					}
				} else {
					LERROR("Common shader  could not be compiled!");
				}

				if(programObject)
					delete programObject;

				return false;
			}
		}
	}
	
	LERROR("Could not load shader");
	return false;
}

} // namespace openspace