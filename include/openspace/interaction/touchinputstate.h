/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_CORE___TOUCHINPUTSTATE___H__
#define __OPENSPACE_CORE___TOUCHINPUTSTATE___H__

#include <openspace/util/touch.h>
#include <chrono>
#include <vector>

namespace openspace::interaction {

/**
 * This class represents the global input state of touch interaction.
 */
class TouchInputState {
public:
    void initialize();

    bool touchDetectedCallback(TouchInput i);
    bool touchUpdatedCallback(TouchInput i);
    void touchExitCallback(TouchInput i);

    bool touchHappened() const;

    bool isTap() const;
    bool isDoubleTap() const;
    void setMaxDoubleTapTime(unsigned int milliseconds);

    const std::vector<TouchInputHolder>& touchPoints() const;
    const std::vector<TouchInput>& lastProcessedInputs() const;

    void processTouchInput(const std::vector<TouchInput>& inputs,
        const std::vector<TouchInput>& removals);

    void clearInputs();

    void updateLastTouchPoints();

private:
    void addTouchInput(TouchInput input);
    void updateOrAddTouchInput(TouchInput input);
    void removeTouchInput(TouchInput input);

    std::vector<TouchInputHolder> _touchPoints;
    std::vector<TouchInput> _deferredRemovals;
    std::vector<TouchInput> _lastTouchInputs;

    bool _tap = false;
    bool _doubleTap = false;

    std::chrono::milliseconds _time;
    unsigned int _maxDoubleTapTimeInterval = 300; // milliseconds
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___TOUCHINPUTSTATE___H__
