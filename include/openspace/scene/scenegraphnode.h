/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/properties/propertyowner.h>

#include <ghoul/glm.h>
#include <ghoul/misc/boolean.h>

#include <functional>
#include <memory>
#include <string>
#include <vector>

namespace ghoul { class Dictionary; }

namespace openspace {

class Camera;
struct RenderData;
class Renderable;
struct RendererTasks;
class Rotation;
class Translation;
class Scale;
class Scene;
struct UpdateData;
struct SurfacePositionHandle;

namespace documentation { struct Documentation; }

class SceneGraphNode : public properties::PropertyOwner {
public:
    using UpdateScene = ghoul::Boolean;

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
    static const std::string KeyTag;
    
    SceneGraphNode();
    ~SceneGraphNode();

    static std::unique_ptr<SceneGraphNode> createFromDictionary(const ghoul::Dictionary& dictionary);

    void initialize();
    void deinitialize();

    void traversePreOrder(std::function<void(SceneGraphNode*)> fn);
    void traversePostOrder(std::function<void(SceneGraphNode*)> fn);
    void update(const UpdateData& data);
    void render(const RenderData& data, RendererTasks& tasks);
    void updateCamera(Camera* camera) const;

    void attachChild(std::unique_ptr<SceneGraphNode> child);
    std::unique_ptr<SceneGraphNode> detachChild(SceneGraphNode& child);
    void clearChildren();
    void setParent(SceneGraphNode& parent);

    void addDependency(SceneGraphNode& dependency);
    void removeDependency(SceneGraphNode& dependency);
    void clearDependencies();
    void setDependencies(const std::vector<SceneGraphNode*>& dependencies);

    SurfacePositionHandle calculateSurfacePositionHandle(
                                                      const glm::dvec3& targetModelSpace);

    const std::vector<SceneGraphNode*>& dependencies() const;
    const std::vector<SceneGraphNode*>& dependentNodes() const;

    Scene* scene();
    void setScene(Scene* scene);

    glm::dvec3 position() const;
    const glm::dmat3& rotationMatrix() const;
    double scale() const;

    glm::dvec3 worldPosition() const;
    const glm::dmat3& worldRotationMatrix() const;
    glm::dmat4 modelTransform() const;
    glm::dmat4 inverseModelTransform() const;
    double worldScale() const;

    SceneGraphNode* parent() const;
    std::vector<SceneGraphNode*> children() const;

    float boundingSphere() const;

    SceneGraphNode* childNode(const std::string& name);

    const PerformanceRecord& performanceRecord() const { return _performanceRecord; }

    void setRenderable(std::unique_ptr<Renderable> renderable);
    const Renderable* renderable() const;
    Renderable* renderable();

    static documentation::Documentation Documentation();

private:
    glm::dvec3 calculateWorldPosition() const;
    glm::dmat3 calculateWorldRotation() const;
    double calculateWorldScale() const;

    std::vector<std::unique_ptr<SceneGraphNode>> _children;
    SceneGraphNode* _parent;
    std::vector<SceneGraphNode*> _dependencies;
    std::vector<SceneGraphNode*> _dependentNodes;
    Scene* _scene;

    PerformanceRecord _performanceRecord;

    std::unique_ptr<Renderable> _renderable;

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

    glm::dmat4 _modelTransformCached;
    glm::dmat4 _inverseModelTransformCached;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___SCENEGRAPHNODE___H__
