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

#ifndef __OPENSPACE_CORE___NAVIGATIONHANDLER___H__
#define __OPENSPACE_CORE___NAVIGATIONHANDLER___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/interaction/keyframenavigator.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/util/mouse.h>
#include <openspace/util/keys.h>

#include <ghoul/misc/boolean.h>

namespace openspace {
    class Camera;
    class SceneGraphNode;
} // namespace openspace

namespace openspace::interaction {

class NavigationHandler : public properties::PropertyOwner {
public:
    NavigationHandler();
    ~NavigationHandler();

    void initialize();
    void deinitialize();

    // Mutators
    void setFocusNode(SceneGraphNode* node);
    void setCamera(Camera* camera);
    void resetCameraDirection();

    void setCameraStateFromDictionary(const ghoul::Dictionary& cameraDict);    
    
    void updateCamera(double deltaTime);

    // Accessors
    ghoul::Dictionary getCameraStateDictionary();
    SceneGraphNode* focusNode() const;
    glm::dvec3 focusNodeToCameraVector() const;
    glm::quat focusNodeToCameraRotation() const;
    Camera* camera() const;
    const InputState& inputState() const;
    const OrbitalNavigator& orbitalNavigator() const;
    KeyframeNavigator& keyframeNavigator() const; 

    // Callback functions 
    void keyboardCallback(Key key, KeyModifier modifier, KeyAction action);
    void mouseButtonCallback(MouseButton button, MouseAction action);
    void mousePositionCallback(double x, double y);
    void mouseScrollWheelCallback(double pos);

    void saveCameraStateToFile(const std::string& filepath);
    void restoreCameraStateFromFile(const std::string& filepath);

    /**
    * \return The Lua library that contains all Lua functions available to affect the
    * interaction
    */
    static scripting::LuaLibrary luaLibrary();

private:
    bool _cameraUpdatedFromScript = false;

    std::unique_ptr<InputState> _inputState;
    Camera* _camera;

    std::unique_ptr<OrbitalNavigator> _orbitalNavigator;
    std::unique_ptr<KeyframeNavigator> _keyframeNavigator;

    // Properties
    properties::StringProperty _origin;
    properties::BoolProperty _useKeyFrameInteraction;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___NAVIGATIONHANDLER___H__
