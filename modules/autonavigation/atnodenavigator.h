/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#ifndef __OPENSPACE_MODULE___ATNODENAVIGATOR___H__
#define __OPENSPACE_MODULE___ATNODENAVIGATOR___H__

#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalar/doubleproperty.h>

namespace openspace {
    class Camera;
    class SceneGraphNode;
} // namespace openspace

namespace openspace::autonavigation {

class AtNodeNavigator : public properties::PropertyOwner {
public:
    enum Behavior {
        None = 0,
        Orbit,
    };

    AtNodeNavigator();
    ~AtNodeNavigator();

    const Behavior behavior() const;
    void setBehavior(Behavior behavior);

    void updateCamera(double deltaTime);

private:
    Camera* camera() const;
    const SceneGraphNode* anchor() const;

    //Behaviors
    void orbitNode(double deltaTime);

    Behavior _behavior;
    properties::DoubleProperty _orbitSpeedFactor;
};

} // namespace openspace::autonavigation

#endif // __OPENSPACE_MODULE___ATNODENAVIGATOR___H__
