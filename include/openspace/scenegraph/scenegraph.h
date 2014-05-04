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
 
#ifndef SCENEGRAPH_H
#define SCENEGRAPH_H

// std includes
#include <vector>
#include <map>

#include <openspace/util/camera.h>

// ghoul includes
#include <ghoul/opengl/programobject.h>
#include <ghoul/misc/dictionary.h>

namespace openspace {

class SceneGraphNode;

class SceneGraph {
public:
    // constructors & destructor
    SceneGraph();
    ~SceneGraph();

    /**
     * Initalizes the SceneGraph by loading modules from the ${SCENEPATH} directory
     */
    bool initialize();

    /*
     * Clean up everything
     */
    bool deinitialize();

    /*
     * Load the scenegraph from the provided folder
     */
    bool loadScene(const std::string& sceneDescriptionFilePath,
                   const std::string& defaultModulePath);

    void loadModule(const std::string& modulePath);

    /*
     * Updates all SceneGraphNodes relative positions
     */
    void update();

    /*
     * Evaluates if the SceneGraphNodes are visible to the provided camera
     */
    void evaluate(Camera* camera);

    /*
     * Render visible SceneGraphNodes using the provided camera
     */
    void render(Camera* camera);

    /*
     * Prints the SceneGraph tree. For debugging purposes
     */
    void printChildren() const;

    /*
     * Returns the root SceneGraphNode
     */
    SceneGraphNode* root() const;

private:
    std::string _focus, _position;

    // actual scenegraph
    SceneGraphNode* _root;
    std::vector<SceneGraphNode*> _nodes;
    std::map<std::string, SceneGraphNode*> _allNodes;
};

} // namespace openspace

#endif