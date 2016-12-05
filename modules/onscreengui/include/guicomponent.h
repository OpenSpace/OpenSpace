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

#ifndef __GUICOMPONENT_H__
#define __GUICOMPONENT_H__

#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalar/boolproperty.h>

namespace openspace {
namespace gui {

class GUI;

/**
 * The base class for a GUI component that can be rendered to the screen.
 */
class GuiComponent : public properties::PropertyOwner {
public:
    /// Constructor that initializes this components member variables
    GuiComponent(std::string name);

    /**
     * Returns if this component is enabled, that is, if it is currently active and
     * visible on the screen.
     * \return <code>true</code> if this component is enabled, <code>false</code>
     * otherwise
     */
    bool isEnabled() const;

    /**
     * Sets if this component is enabled, that is, if it is currently active and visible
     * on the screen.
     * \param enabled The new enabled status of this component
     */
    void setEnabled(bool enabled);

    /// Initializes the component with everything that does not require an OpenGL context
    virtual void initialize();
    /// Initializes the component with everything that requires an OpenGL context
    virtual void initializeGL();

    /// Deinitializes the component with things that do not require an OpenGL context
    virtual void deinitialize();
    
    /// Deinitializes the component with things that require an OpenGL context
    virtual void deinitializeGL();

    /// Renders the individual subcomponents to the screen
    virtual void render() = 0;

protected:
    /// <code>true</code> if this component is enabled and visible on the screen
    properties::BoolProperty _isEnabled;
};

} // namespace gui
} // namespace openspace

#endif // __GUICOMPONENT_H__
