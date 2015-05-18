/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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
 
#ifndef __SCENEGRAPH_H__
#define __SCENEGRAPH_H__

#include <ghoul/misc/dictionary.h>

#include <unordered_map>
#include <vector>

namespace openspace {

class SceneGraphNode;

class SceneGraph {
public:
    SceneGraph();

    void clear();
    bool loadFromFile(const std::string& sceneDescription);

    // Returns if addition was successful
    bool addSceneGraphNode(SceneGraphNode* node);
    bool removeSceneGraphNode(SceneGraphNode* node); 

    const std::vector<SceneGraphNode*>& nodes();

    SceneGraphNode* rootNode() const;
    SceneGraphNode* sceneGraphNode(const std::string& name) const;

private:
    struct SceneGraphNodeInternal {
        SceneGraphNode* node;
        // From nodes that are dependent on this one
        std::vector<SceneGraphNodeInternal*> incomingEdges;
        // To nodes that this node depends on
        std::vector<SceneGraphNodeInternal*> outgoingEdges;
    };

    bool nodeIsDependentOnRoot(SceneGraphNodeInternal* node);
    bool sortTopologically();

    SceneGraphNodeInternal* nodeByName(const std::string& name);

    SceneGraphNode* _rootNode;
    std::vector<SceneGraphNodeInternal*> _nodes;
    std::vector<SceneGraphNode*> _topologicalSortedNodes;
};

} // namespace openspace

#endif // __SCENEGRAPH_H__
