/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_CORE___INTERACTIONMONITOR___H__
#define __OPENSPACE_CORE___INTERACTIONMONITOR___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>

namespace openspace::interaction {

/**
 * The class InteractionMonitor keeps track of user interactions during an OpenSpace
 * session. It keeps track of when the latest interaction was made and of when the state
 * changes to idle.
 */
class InteractionMonitor : public properties::PropertyOwner {
public:
    InteractionMonitor();

    void setActivityState(bool isActive);
    void setIdleTime(float time);

    /*
     * Called every frame from OpenSpaceEngine and calculates the activity state depending
     * on the last registered interaction.
     */
    void updateActivityState();

    /*
     * Called from all places we want to mark activity from. Updates the last registered
     * interaction time.
     */
    void markInteraction();

private:
    double _lastInteractionTime = 0;
    properties::BoolProperty _isInActiveState;
    properties::FloatProperty _idleTime; // in seconds

    // @TODO (lovisa) make a list of interactions to listen for
    // and only allow registering updates from those interactions
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___INTERACTIONMONITOR___H__
