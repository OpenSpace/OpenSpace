/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <openspace/interaction/remotecontroller.h>

#include <openspace/engine/openspaceengine.h>

#include <openspace/interaction/interactionhandler.h>

#include <openspace/util/time.h>

namespace openspace {
namespace interaction {

	RemoteController::RemoteController()
		: _isBroadcasting(false),
		_lastTimeStap(0.0)
	{
		//ff.open("path.txt");
	}

	RemoteController::~RemoteController(){ 
	
	}

	void RemoteController::update(const double& dt){
		ControllerKeyFrame kf;
		kf._position = _handler->camera()->position();
		kf._viewRotationMatrix = _handler->camera()->viewRotationMatrix();
		kf._timeStamp = Time::ref().currentTime();

		std::string write = std::to_string(kf._position.vec4().x) + "\t" + std::to_string(kf._position.vec4().y) + "\t" + std::to_string(kf._position.vec4().z) + "\t" + std::to_string(kf._position.vec4().w) + "\n";
		//write += 
		//printf("%s", write.c_str());
	}

	void RemoteController::sendKeyFrame(){
	
	}

	void RemoteController::keyFrameReceived(const ControllerKeyFrame& keyframe){
	
	}
	

} // namespace interaction
} // namespace openspace
