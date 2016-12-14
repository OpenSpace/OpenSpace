/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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
 
#ifndef __OPENSPACE_CORE___SCENEGRAPHNODE___H__
#define __OPENSPACE_CORE___SCENEGRAPHNODE___H__

// open space includes
#include <openspace/documentation/documentation.h>

#include <openspace/rendering/renderable.h>
#include <openspace/scene/translation.h>
#include <openspace/scene/rotation.h>
#include <openspace/scene/scale.h>
#include <openspace/properties/propertyowner.h>

#include <openspace/scene/scene.h>
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
        long long updateTimeTranslation; // time in ns
        long long updateTimeRotation;  // time in ns
        long long updateTimeScaling;  // time in ns
    };

    static const std::string RootNodeName;
    static const std::string KeyName;
    static const std::string KeyParentName;
    static const std::string KeyDependencies;
    
    SceneGraphNode();
    ~SceneGraphNode();

    static SceneGraphNode* createFromDictionary(const ghoul::Dictionary& dictionary);

    bool initialize();
    bool deinitialize();

    void update(const UpdateData& data);
    void evaluate(const Camera* camera, const psc& parentPosition = psc());
    void render(const RenderData& data, RendererTasks& tasks);
    void updateCamera(Camera* camera) const;

    //void addNode(SceneGraphNode* child);

    void addChild(SceneGraphNode* child);
    void setParent(SceneGraphNode* parent);
    //bool abandonChild(SceneGraphNode* child);

    glm::dvec3 position() const;
    const glm::dmat3& rotationMatrix() const;
    double scale() const;

    glm::dvec3 worldPosition() const;
    const glm::dmat3& worldRotationMatrix() const;
    double worldScale() const;

    SceneGraphNode* parent() const;
    const std::vector<SceneGraphNode*>& children() const;

    PowerScaledScalar calculateBoundingSphere();
    PowerScaledScalar boundingSphere() const;

    SceneGraphNode* childNode(const std::string& name);

    const PerformanceRecord& performanceRecord() const { return _performanceRecord; }

    void setRenderable(Renderable* renderable);
    const Renderable* renderable() const;
    Renderable* renderable();

    static documentation::Documentation Documentation();

private:
    bool sphereInsideFrustum(const psc& s_pos, const PowerScaledScalar& s_rad, const Camera* camera);

    glm::dvec3 calculateWorldPosition() const;
    glm::dmat3 calculateWorldRotation() const;
    double calculateWorldScale() const;

    std::vector<SceneGraphNode*> _children;
    SceneGraphNode* _parent;

    PerformanceRecord _performanceRecord;

    Renderable* _renderable;
    bool _renderableVisible;

    bool _boundingSphereVisible;
    PowerScaledScalar _boundingSphere;

    // Transformation defined by ephemeris, rotation and scale
    struct {
        std::unique_ptr<Translation> translation;
        std::unique_ptr<Rotation> rotation;
        std::unique_ptr<Scale> scale;
    } _transform;

    // Cached transform data
    glm::dvec3 _worldPositionCached;
    glm::dmat3 _worldRotationCached;
    double _worldScaleCached;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___SCENEGRAPHNODE___H__
