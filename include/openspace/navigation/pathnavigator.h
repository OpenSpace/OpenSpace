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

#ifndef __OPENSPACE_CORE___PATHNAVIGATOR___H__
#define __OPENSPACE_CORE___PATHNAVIGATOR___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/navigation/path.h>
#include <openspace/properties/list/stringlistproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <ghoul/glm.h>
#include <memory>

namespace openspace {
    class Camera;
    struct CameraPose;
    class SceneGraphNode;
} // namespace openspace

namespace openspace::scripting { struct LuaLibrary; }

namespace openspace::interaction {

class Path;

class PathNavigator : public properties::PropertyOwner {
public:
    PathNavigator();
    virtual ~PathNavigator() override;

    // Accessors
    Camera* camera() const;
    const SceneGraphNode* anchor() const;
    const Path* currentPath() const;
    double speedScale() const;
    double arrivalDistanceFactor() const;
    float linearRotationSpeedFactor() const;

    bool hasCurrentPath() const;
    bool hasFinished() const;
    bool isPlayingPath() const;
    bool isPaused() const;

    float estimatedRemainingTimeInPath() const;

    void updateCamera(double deltaTime);
    void createPath(const ghoul::Dictionary& dictionary);
    void clearPath();
    void startPath();
    void abortPath();
    void pausePath();
    void continuePath();
    void skipToEnd();

    Path::Type defaultPathType() const;
    double minValidBoundingSphere() const;
    double findValidBoundingSphere(const SceneGraphNode* node) const;

    double defaultArrivalHeight(const std::string& sgnIdentifier) const;

    const std::vector<SceneGraphNode*>& relevantNodes();

    /**
     * Find a node close to the given node. Closeness is determined by a factor times
     * the bounding sphere of the object.
     *
     * \return Pointer to the SGN if one was found, nullptr otherwise
     */
    static SceneGraphNode* findNodeNearTarget(const SceneGraphNode* node);

    /**
     * \return The Lua library that contains all Lua functions available to affect the
     *         path navigation
     */
    static scripting::LuaLibrary luaLibrary();

private:
    void handlePathEnd();

    /**
     * Populate list of nodes that are relevant for collision checks, etc.
     */
    void findRelevantNodes();

    void removeRollRotation(CameraPose& pose) const;

    std::unique_ptr<Path> _currentPath = nullptr;
    bool _isPlaying = false;
    bool _startSimulationTimeOnFinish = false;

    bool _setCameraToEndNextFrame = false;

    properties::OptionProperty _defaultPathType;
    properties::BoolProperty _includeRoll;
    properties::FloatProperty _speedScale;
    properties::BoolProperty _applyIdleBehaviorOnFinish;
    properties::DoubleProperty _arrivalDistanceFactor;
    properties::FloatProperty _linearRotationSpeedFactor;
    properties::DoubleProperty _minValidBoundingSphere;
    properties::StringListProperty _relevantNodeTags;

    std::vector<SceneGraphNode*> _relevantNodes;
    bool _hasInitializedRelevantNodes = false;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___PATHNAVIGATOR___H__
