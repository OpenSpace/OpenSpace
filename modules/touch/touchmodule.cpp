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

#include <modules/touch/touchmodule.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/settingsengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>

#include <ghoul/logging/logmanager.h>
#include <glm/ext.hpp>

#include <sstream>
#include <string>
#include <iostream>
#include <thread>         // std::this_thread::sleep_for
#include <chrono>         // std::chrono::seconds

using namespace TUIO;

namespace {
	const std::string _loggerCat = "TouchModule";
}

namespace openspace {

	TuioEar TouchModule::*ear;
	TouchInteraction *touch;

bool TouchModule::gotNewInput() {
	// Get new input from listener
	list = ear->getInput();
	ear->clearInput();

	// Erase old input id's that no longer exists
	lastProcessed.erase(
		std::remove_if(
			lastProcessed.begin(),
			lastProcessed.end(),
			[this](const Point& point) {
		return std::find_if(
			list.begin(),
			list.end(),
			[&point](const TuioCursor& c) {
			return point.first == c.getSessionID();
		}
		) == list.end(); }),
		lastProcessed.end());

	// Return true if we got new input
	
	if (list.size() == lastProcessed.size() && list.size() > 0) {
		bool newInput = true;
		for_each(lastProcessed.begin(), lastProcessed.end(), [this, &newInput](Point& p) {
			if (p.second.getTuioTime().getTotalMilliseconds() == find_if(list.begin(), list.end(), [&p](const TuioCursor& c) { return c.getSessionID() == p.first; })->getPath().back().getTuioTime().getTotalMilliseconds())
				newInput = false;
		});
		return newInput;
	}		
	else
		return false;
}

TouchModule::TouchModule()
    : OpenSpaceModule("Touch")
{

	OsEng.registerModuleCallback(
		OpenSpaceEngine::CallbackOption::Initialize,
		[&]() {
		LDEBUGC("TouchModule", "Initializing TuioEar");
		ear = new TuioEar();
		touch = new TouchInteraction();
		addPropertySubOwner(touch);
	}
	);

	OsEng.registerModuleCallback(
		OpenSpaceEngine::CallbackOption::Deinitialize,
		[&]() {
		LDEBUGC("TouchModule", "Deinitialize TuioEar");
		delete ear;
		delete touch;
	}
	);
	
	OsEng.registerModuleCallback(
		OpenSpaceEngine::CallbackOption::PreSync,
		[&]() {
		if (gotNewInput() && OsEng.windowWrapper().isMaster()) {
			touch->update(list, lastProcessed);

			// for debugging
			//std::this_thread::sleep_for(std::chrono::seconds(1));
			std::ostringstream os; 
			for (const TuioCursor &j : list) { // go through each item
				std::list<TuioPoint> path = j.getPath();
				
				TuioTime lastTime = find_if(
				lastProcessed.begin(),
				lastProcessed.end(),
				[&j](const Point& p) { return p.first == j.getSessionID(); }
				)->second.getTuioTime();
				
				std::list<TuioPoint>::iterator lastPoint = find_if(
				path.begin(),
				path.end(),
				[&lastTime](const TuioPoint& c) { return lastTime == c.getTuioTime();  });
				
				int count = -1;
				for (; lastPoint != path.end(); ++lastPoint) // here we can access all elements that are to be processed
					count++;

				os << ", Id: " << j.getCursorID() << ", path size: " << j.getPath().size() << ", To Process: " << count;
			}
			LINFO("List size: " << list.size() << os.str() << "\n");
			os.clear();

		}

		// update lastProcessed
		lastProcessed.clear();
		for (const TuioCursor& c : list) {
			lastProcessed.push_back(std::make_pair(c.getSessionID(), c.getPath().back()));
		}
		touch->step(OsEng.windowWrapper().deltaTime());
	}
	);
	
}

} // namespace openspace
