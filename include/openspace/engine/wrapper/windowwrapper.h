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

#ifndef __WINDOWWRAPPER_H__
#define __WINDOWWRAPPER_H__

#include <ghoul/glm.h>
#include <ghoul/misc/exception.h>

#include <cstdint>
#include <functional>
#include <vector>

namespace openspace {

/**
 * A WindowWrapper is a class that handles the abstraction between OpenSpace and a
 * specific window creation framework.<br>
 * Every new windowing framework needs to have its own WindowWrapper subclass exposing the
 * required features.
 */
class WindowWrapper {
public:
    /**
     * This method closes the application by calling the necessary terminate function of
     * the window management system
     */
    virtual void terminate();

    /**
     * This method enables or disables a framelock barrier. If the specific windowing
     * framework does not provide a framelock, this method defaults to a no-op.
     * \param enabled If <code>true</code> the framelock is enabled, <code>false</code>
     * disables it
     */
    virtual void setBarrier(bool enabled);

    /**
    * This method enables or disables a framelock barrier. If the specific windowing
    * framework does not provide a framelock, this method defaults to a no-op.
    * \param enabled If <code>true</code> the framelock is enabled, <code>false</code>
    * disables it
    */
    virtual void setSynchronization(bool enabled);
    
    /**
     * This method clears all the rendering windows with the specified \p clearColor. In
     * most OpenGL cases, this will end up with one or mode <code>glClear</code> calls.
     * This method defaults to a no-op.
     * \param clearColor The color with which to clear all windows
     */
    virtual void clearAllWindows(const glm::vec4& clearColor);
    
    /**
     * Returns whether the current window has been resized recently. On default, this
     * method always returns <code>false</code>.
     * \return <code>true</code> if the current window has been resized recently,
     * <code>false</code> otherwise
     */
    virtual bool windowHasResized() const;

    /**
     * Returns the average frametime in seconds. On default, this method returns
     * <code>0.0</code>.
     * \return The average frametime in seconds
     */
    virtual double averageDeltaTime() const;

    /**
    * Returns the frametime in seconds. On default, this method returns <code>0.0</code>.
    * \return The frametime in seconds
    */
    virtual double deltaTime() const;

    /**
     * Returns the location of the mouse cursor in pixel screen coordinates. On default,
     * this method returns <code>0,0</code>.
     * \return The location of the mouse cursor in pixel screen coordinates
     */
    virtual glm::vec2 mousePosition() const;
    
    /**
     * Returns a bitmask of the status of all available mouse buttons. Bit <code>i</code>
     * is <code>1</code> if mouse button <code>i</code> is pressed down;
     * <code>false</code> otherwise. On default, this method returns that none of the
     * buttons is pressed.
     * \param maxNumber The maximum number of mouse buttons that should be queried 
     * \return A bitmask showing the status of all mouse buttons (up to \p maxNumber)
     */
    virtual uint32_t mouseButtons(int maxNumber = 8) const;
    
    /**
     * Returns the size of the currently active window in pixel coordinates. On default,
     * this method returns a window size of <code>0,0</code>.
     * \return The size of the currently active window in pixel coordinates
     */
    virtual glm::ivec2 currentWindowSize() const;
    
    /**
     * Returns the resolution of the currently active window in pixel coordinates. On
     * default, this method returns the same size as #currentWindowSize.
     * \return The resolution of the currently active window in pixel coordinates
     */
    virtual glm::ivec2 currentWindowResolution() const;
    
    /**
     * Returns the resolution of the currently active framebuffer in pixel coordinates.
     * On default, this method returns the same size as #currentWindowSize.
     * \return The resolution of the currently active window in pixel coordinates
     */
    virtual glm::ivec2 currentDrawBufferResolution() const;

    /**
     * Returns the number of anti-aliasing samples used in the current window.
     */
    virtual int currentNumberOfAaSamples() const;

    /**
     * Returns <code>true</code> if the current rendering method is regular, i.e., it is
     * a flat projection without non-linear distortions. Returns <code>false</code> in
     * other cases, for example fisheye projections. On default, this method will return
     * <code>true</code>.
     * \return Whether the current rendering method is a regular method
     */
    virtual bool isRegularRendering() const;

    /**
     * Returns <code>true</code> if there is a dedicated GUI window. In these cases, the
     * GUI elements will not be drawn on the non-GUI windows but only on the dedicated
     * windows.
     * \return Whether there are dedicated GUI windows
     */
    virtual bool hasGuiWindow() const;

    /**
     * Returns <code>true</code> if the current rendering window is a GUI-only window. In
     * these cases, the regular rendering is suppressed, but input is still possible in
     * the window
     * \return Whether the current rendering window is GUI-only
     */
    virtual bool isGuiWindow() const;

    /**
    * Returns <code>true</code> if the current rendering window is using swap groups.
    */
    virtual bool isUsingSwapGroups() const;

    /**
    * Returns <code>true</code> if the current rendering window is master of the swap its group. 
    */
    virtual bool isSwapGroupMaster() const;

    /**
     * Returns the currently employed view-projection matrix. On default, this method will
     * return the identity matrix.
     * \return The currently employed view-projection matrix
     */
    virtual glm::mat4 viewProjectionMatrix() const;

    /**
     * Returns the currently employed model matrix. On default, this method will return
     * the identity matrix.
     * \return The currently employed model matrix
     */
    virtual glm::mat4 modelMatrix() const;
    
    /**
     * Sets the near and far clipping planes of the rendering window. This method defaults
     * to a no-op.
     * \param near The near clipping plane
     * \param far The far clipping plane
     */
    virtual void setNearFarClippingPlane(float near, float far);
    
    /**
     * Sets the stereo eye separation distance for the render engine.
     * \param distance The distance between eyes for stereo rendering.
     */
    virtual void setEyeSeparationDistance(float distance);

    /**
     * Returns the location and size of the current viewport (<code>x</code>,
     * <code>width</code>, <code>y</code>, and <code>height</code>). If there is only a
     * single viewport, <code>x</code> and <code>y</code> are <code>0</code> whereas
     * <code>width</code> and <code>height</code> are equal to #currentWindowResolution.
     * \return The location and size of the current viewport
     */
    virtual glm::ivec4 viewportPixelCoordinates() const;
    
    /**
     * Returns <code>true</code> if there is an external control connected, i.e., an
     * application that can receive control commands. On default, this method will return
     * <code>false</code>.
     * \return If there is an external control connected
     */
    virtual bool isExternalControlConnected() const;
    
    /**
     * Sends a \p message to an external control. This method defaults to a no-op.
     * \param message The message to be sent
     */
    virtual void sendMessageToExternalControl(const std::vector<char>& message) const;
    
    /**
     * Returns <code>true</code> if the rendering is a single viewport with an single
     * window; <code>false</code> otherwise. On default, this method returns
     * <code>true</code>
     * \returns <code>true</code> if the rendering is a single viewport with an single
     * widnow; <code>false</code> otherwise
     */
    virtual bool isSimpleRendering() const;

    /**
     * Advises the windowing system to take a screenshot. This method defaults to a no-op.
     */
    virtual void takeScreenshot() const;

    struct WindowWrapperException : public ghoul::RuntimeError {
        explicit WindowWrapperException(const std::string& msg);
    };
};

} // namespace openspace

#endif // _WINDOW_H__
