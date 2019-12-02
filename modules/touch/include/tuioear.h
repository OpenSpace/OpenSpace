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
#include <modules/touch/ext/libTUIO11/TUIO/UdpReceiver.h>
#include <modules/touch/ext/libTUIO11/TUIO/TcpReceiver.h>

#if (defined(__GNUC__) && !defined(__clang__))
#pragma GCC diagnostic pop
#endif // defined(__GNUC__) && !defined(__clang__)
#ifdef __clang__
#pragma clang diagnostic pop
#endif // __clang__

#include <ghoul/glm.h>

#include <math.h>
#include <vector>
#include <mutex>
#include <numeric>
#include <algorithm>


class TuioEar : public TUIO::TuioListener {
    public:
        TuioEar();
        ~TuioEar() {
            _tuioClient.disconnect();
        }

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
        * Returns a list of all touch history that happened since the last frame
        */
        std::vector<TUIO::TuioCursor> getInput();

        /**
        * Returns true if a tap occured since the last frame
        */
        bool tap();

        /**
        * Returns tap's cursor coordinates and time information
        */
        TUIO::TuioCursor getTap();

        /**
        * Clears the input list, function called after getInput() each frame
        */
        void clearInput();

    private:
        bool _tap = false;
        TUIO::TuioCursor _tapCo = TUIO::TuioCursor(-1, -1, -1.0f, -1.0f);
        std::mutex _mx;

        TUIO::TuioClient _tuioClient;

        std::vector<TUIO::TuioCursor> _list;

        /**
        * A list that tracks all of the cursor ID's that got removed since last frame
        */
        std::vector<long> _removeList;
};

#endif // __OPENSPACE_MODULE_TOUCH___TUIO_EAR___H__
