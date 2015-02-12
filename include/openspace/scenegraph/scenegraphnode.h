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
 
#ifndef __SCENEGRAPHNODE_H__
#define __SCENEGRAPHNODE_H__

// open space includes
#include <openspace/rendering/renderable.h>
#include <openspace/scenegraph/ephemeris.h>
#include <openspace/properties/propertyowner.h>

#include <openspace/scenegraph/scenegraph.h>
#include <ghoul/misc/dictionary.h>
#include <openspace/util/updatestructures.h>

// std includes
#include <iostream>
#include <memory>
#include <string>
#include <vector>

namespace openspace {

class SceneGraphNode : public properties::PropertyOwner {
public:
	struct PerformanceRecord {
		long long renderTime;  // time in ns
		long long updateTimeRenderable;  // time in ns
		long long updateTimeEphemeris;  // time in ns
	};

    static std::string RootNodeName;
    
    SceneGraphNode();
    ~SceneGraphNode();

    static SceneGraphNode* createFromDictionary(const ghoul::Dictionary& dictionary);

	bool initialize();
    bool deinitialize();

    void update(const UpdateData& data);
    void evaluate(const Camera* camera, const psc& parentPosition = psc());
    void render(const RenderData& data);
	void updateCamera(Camera* camera) const;

    void addNode(SceneGraphNode* child);

    void setParent(SceneGraphNode* parent);
    const psc& position() const;
    psc worldPosition() const;

    SceneGraphNode* parent() const;
    const std::vector<SceneGraphNode*>& children() const;

    PowerScaledScalar calculateBoundingSphere();
    PowerScaledScalar boundingSphere() const;

    SceneGraphNode* childNode(const std::string& name);

	const PerformanceRecord& performanceRecord() const { return _performanceRecord; }

    void setRenderable(Renderable* renderable);
    const Renderable* renderable() const;
    Renderable* renderable();

private:
    bool sphereInsideFrustum(const psc& s_pos, const PowerScaledScalar& s_rad, const Camera* camera);

	std::vector<SceneGraphNode*> _children;
    SceneGraphNode* _parent;
    Ephemeris* _ephemeris;

	PerformanceRecord _performanceRecord;

    Renderable* _renderable;
    bool _renderableVisible;

    bool _boundingSphereVisible;
    PowerScaledScalar _boundingSphere;
};

} // namespace openspace

#endif // __SCENEGRAPHNODE_H__
