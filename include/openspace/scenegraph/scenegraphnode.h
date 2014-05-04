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
 
#ifndef SCENEGRAPHNODE_H
#define SCENEGRAPHNODE_H

// open space includes
#include <openspace/rendering/renderable.h>
#include <openspace/scenegraph/positioninformation.h>

#include <openspace/scenegraph/scenegraph.h>
#include <ghoul/misc/dictionary.h>

// std includes
#include <iostream>
#include <memory>
#include <string>
#include <vector>

namespace openspace {

class SceneGraphNode {
public:
    // constructors & destructor
    SceneGraphNode();
    ~SceneGraphNode();

    static SceneGraphNode* createFromDictionary(const ghoul::Dictionary& dictionary);

    bool initialize();
    bool deinitialize();

    // essential
    void update();
    void evaluate(const Camera* camera, const psc& parentPosition = psc());
    void render(const Camera* camera, const psc& parentPosition = psc());

    // set & get
    void addNode(SceneGraphNode* child);

    void setName(const std::string& name);
    void setParent(SceneGraphNode* parent);
    const psc& getPosition() const;
    psc getWorldPosition() const;
    std::string nodeName() const;

    SceneGraphNode* parent() const;
    const std::vector<SceneGraphNode*>& children() const;

    // bounding sphere
    pss calculateBoundingSphere();

    SceneGraphNode* get(const std::string& name);

    void print() const;

    // renderable
    void setRenderable(Renderable* renderable);
    const Renderable* getRenderable() const;

private:
    // essential
    std::vector<SceneGraphNode*> _children;
    SceneGraphNode* _parent;
    std::string _nodeName;
    PositionInformation* _position;

    // renderable
    Renderable* _renderable;
    bool _renderableVisible;

    // bounding sphere
    bool _boundingSphereVisible;
    pss _boundingSphere;

    // private helper methods
    bool sphereInsideFrustum(const psc s_pos, const pss& s_rad, const Camera* camera);
};

} // namespace openspace

#endif
