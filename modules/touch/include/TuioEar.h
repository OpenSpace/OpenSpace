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

#ifndef __OPENSPACE_MODULE_TOUCH___TOUCHEAR___H__
#define __OPENSPACE_MODULE_TOUCH___TOUCHEAR___H__

#include <modules/touch/ext/libTUIO/TUIO/TuioListener.h>
#include <modules/touch/ext/libTUIO/TUIO/TuioClient.h>
#include <modules/touch/ext/libTUIO/TUIO/UdpReceiver.h>
#include <modules/touch/ext/libTUIO/TUIO/TcpReceiver.h>
#include "glm/glm.hpp"
#include <math.h>
#include <vector>
#include <mutex>

using namespace TUIO;

class TuioEar : public TuioListener {
	
	public:
		TuioEar();
		~TuioEar() {
			_tuioClient->disconnect();
			delete _tuioClient;
			delete _oscReceiver;
		}
	
		void addTuioObject(TuioObject *tobj);
		void updateTuioObject(TuioObject *tobj);
		void removeTuioObject(TuioObject *tobj);

		void addTuioCursor(TuioCursor *tcur);
		void updateTuioCursor(TuioCursor *tcur);
		void removeTuioCursor(TuioCursor *tcur);

		void addTuioBlob(TuioBlob *tblb);
		void updateTuioBlob(TuioBlob *tblb);
		void removeTuioBlob(TuioBlob *tblb);

		void refresh(TuioTime frameTime);

		std::vector<TuioCursor> getInput();
		void clearInput();
		
	private:
		TuioClient *_tuioClient;
		OscReceiver *_oscReceiver;

		std::vector<TuioCursor> _list;
		std::vector<int> _removeList;
		std::mutex _mx;
};

#endif // __OPENSPACE_MODULE_TOUCH___TOUCHWRAPPER___H__
