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

#ifndef __OPENSPACE_MODULE_TOUCH___TUIO_EAR___H__
#define __OPENSPACE_MODULE_TOUCH___TUIO_EAR___H__

// -Wold-style-cast
#if (defined(__GNUC__) && !defined(__clang__))
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wold-style-cast"
#endif // defined(__GNUC__) && !defined(__clang__)

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wold-style-cast"
#endif // __clang__

#include <modules/touch/ext/libTUIO11/TUIO/TuioListener.h>
#include <modules/touch/ext/libTUIO11/TUIO/TuioClient.h>

#if (defined(__GNUC__) && !defined(__clang__))
#pragma GCC diagnostic pop
#endif // defined(__GNUC__) && !defined(__clang__)
#ifdef __clang__
#pragma clang diagnostic pop
#endif // __clang__

#include <openspace/util/touch.h>
#include <ghoul/glm.h>
#include <algorithm>
#include <math.h>
#include <mutex>
#include <numeric>
#include <vector>

namespace openspace {

class TuioEar : public TUIO::TuioListener {
public:
    TuioEar();
    ~TuioEar();

    /**
    * Callback functions, listens to the TUIO server
    */
    void addTuioObject(TUIO::TuioObject *tobj);
    void updateTuioObject(TUIO::TuioObject *tobj);
    void removeTuioObject(TUIO::TuioObject *tobj);

    void addTuioCursor(TUIO::TuioCursor *tcur);
    void updateTuioCursor(TUIO::TuioCursor *tcur);
    void removeTuioCursor(TUIO::TuioCursor *tcur);

    void addTuioBlob(TUIO::TuioBlob *tblb);
    void updateTuioBlob(TUIO::TuioBlob *tblb);
    void removeTuioBlob(TUIO::TuioBlob *tblb);

    void refresh(TUIO::TuioTime frameTime);

    /**
        * Lock-swap the containers of this listener
        */
    std::vector<TouchInput> takeInput();
    std::vector<TouchInput> takeRemovals();

private:
    TUIO::TuioClient _tuioClient;

    std::vector<TouchInput> _inputList;
    std::vector<TouchInput> _removalList;
    std::mutex _mx;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_TOUCH___TUIO_EAR___H__
