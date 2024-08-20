/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/ivec2property.h>
#include <ghoul/glm.h>
#include <ghoul/misc/boolean.h>
#include <ghoul/misc/managedmemoryuniqueptr.h>
#include <atomic>
#include <chrono>
#include <functional>
#include <memory>
#include <optional>
#include <vector>

 //#define Debugging_Core_SceneGraphNode_Indices

namespace ghoul { class Dictionary; }
namespace ghoul::opengl { class ProgramObject; }

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
class TimeFrame;
class Time;

namespace documentation { struct Documentation; }

class SceneGraphNode : public properties::PropertyOwner {
public:
    enum class State : int {
        Loaded,
        Initialized,
        GLInitialized
    };

    BooleanType(UpdateScene);

    static constexpr std::string_view KeyIdentifier = "Identifier";
    static constexpr std::string_view KeyParentName = "Parent";
    static constexpr std::string_view KeyDependencies = "Dependencies";
    static constexpr std::string_view KeyTag = "Tag";

    SceneGraphNode();
    virtual ~SceneGraphNode() override;

    static ghoul::mm_unique_ptr<SceneGraphNode> createFromDictionary(
        const ghoul::Dictionary& dictionary);

    void initialize();
    void initializeGL();
    void deinitialize();
    void deinitializeGL();

    void traversePreOrder(const std::function<void(SceneGraphNode*)>& fn);
    void traversePostOrder(const std::function<void(SceneGraphNode*)>& fn);
    void update(const UpdateData& data);
    void render(const RenderData& data, RendererTasks& tasks);

    void attachChild(ghoul::mm_unique_ptr<SceneGraphNode> child);
    ghoul::mm_unique_ptr<SceneGraphNode> detachChild(SceneGraphNode& child);
    void clearChildren();
    void setParent(SceneGraphNode& parent);

    void addDependency(SceneGraphNode& dependency);
    void removeDependency(SceneGraphNode& dependency);
    void clearDependencies();
    void setDependencies(const std::vector<SceneGraphNode*>& dependencies);

    SurfacePositionHandle calculateSurfacePositionHandle(
        const glm::dvec3& targetModelSpace) const;

    const std::vector<SceneGraphNode*>& dependencies() const;
    const std::vector<SceneGraphNode*>& dependentNodes() const;

    Scene* scene();
    void setScene(Scene* scene);

    glm::dvec3 position() const;
    const glm::dmat3& rotationMatrix() const;
    glm::dvec3 scale() const;

    glm::dvec3 worldPosition() const;
    const glm::dmat3& worldRotationMatrix() const;
    glm::dmat4 modelTransform() const;
    glm::dvec3 worldScale() const;
    bool isTimeFrameActive(const Time& time) const;

    SceneGraphNode* parent() const;
    std::vector<SceneGraphNode*> children() const;

    const std::vector<std::string>& onApproachAction() const;
    const std::vector<std::string>& onReachAction() const;
    const std::vector<std::string>& onRecedeAction() const;
    const std::vector<std::string>& onExitAction() const;

    double boundingSphere() const;
    double interactionSphere() const;

    double reachFactor() const;
    double approachFactor() const;

    bool supportsDirectInteraction() const;

    SceneGraphNode* childNode(const std::string& id);

    const Renderable* renderable() const;
    Renderable* renderable();

    std::string guiPath() const;
    bool hasGuiHintHidden() const;
    void setGuiHintHidden(bool value);

    static documentation::Documentation Documentation();

private:
    glm::dvec3 calculateWorldPosition() const;
    glm::dmat3 calculateWorldRotation() const;
    glm::dvec3 calculateWorldScale() const;
    void computeScreenSpaceData(RenderData& newData);
    void renderDebugSphere(const Camera& camera, double size, const glm::vec4& color);

    std::atomic<State> _state = State::Loaded;
    std::vector<ghoul::mm_unique_ptr<SceneGraphNode>> _children;
    SceneGraphNode* _parent = nullptr;
    std::vector<SceneGraphNode*> _dependencies;
    std::vector<SceneGraphNode*> _dependentNodes;
    Scene* _scene = nullptr;

    std::vector<std::string> _onApproachAction;
    std::vector<std::string> _onReachAction;
    std::vector<std::string> _onRecedeAction;
    std::vector<std::string> _onExitAction;

    ghoul::mm_unique_ptr<Renderable> _renderable;

    // If this value is 'true' GUIs are asked to hide this node from collections, as it
    // might be a node that is not very interesting (for example barycenters)
    properties::BoolProperty _guiHidden;

    properties::StringProperty _guiPath;
    properties::StringProperty _guiDisplayName;
    properties::StringProperty _guiDescription;
    properties::FloatProperty _guiOrderingNumber;
    properties::BoolProperty _useGuiOrdering;

    // Transformation defined by translation, rotation and scale
    struct {
        ghoul::mm_unique_ptr<Translation> translation;
        ghoul::mm_unique_ptr<Rotation> rotation;
        ghoul::mm_unique_ptr<Scale> scale;
    } _transform;

    ghoul::mm_unique_ptr<TimeFrame> _timeFrame;

    // Cached transform data
    glm::dvec3 _worldPositionCached = glm::dvec3(0.0);
    glm::dmat3 _worldRotationCached = glm::dmat3(1.0);
    glm::dvec3 _worldScaleCached = glm::dvec3(1.0);

    glm::dmat4 _modelTransformCached = glm::dmat4(1.0);

    properties::DoubleProperty _boundingSphere;
    properties::DoubleProperty _evaluatedBoundingSphere;
    properties::DoubleProperty _interactionSphere;
    properties::DoubleProperty _evaluatedInteractionSphere;
    properties::DoubleProperty _approachFactor;
    properties::DoubleProperty _reachFactor;
    properties::BoolProperty _computeScreenSpaceValues;
    properties::IVec2Property _screenSpacePosition;
    properties::BoolProperty _screenVisibility;
    properties::DoubleProperty _distFromCamToNode;
    properties::DoubleProperty _screenSizeRadius;
    properties::FloatProperty _visibilityDistance;
    properties::BoolProperty _supportsDirectInteraction;

    // This variable is used for the rate-limiting of the screenspace positions (if they
    // are calculated when _computeScreenSpaceValues is true)
    std::chrono::high_resolution_clock::time_point _lastScreenSpaceUpdateTime;

    properties::BoolProperty _showDebugSphere;
    static ghoul::opengl::ProgramObject* _debugSphereProgram;

    std::optional<double> _overrideBoundingSphere;
    std::optional<double> _overrideInteractionSphere;

#ifdef Debugging_Core_SceneGraphNode_Indices
    int index = 0;
    static int nextIndex;
#endif // Debugging_Core_SceneGraphNode_Indices
};

} // namespace openspace

#endif // __OPENSPACE_CORE___SCENEGRAPHNODE___H__
