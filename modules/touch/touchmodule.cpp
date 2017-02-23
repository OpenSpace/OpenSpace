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
#include <modules/touch/include/TuioEar.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/settingsengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>

#include <ghoul/logging/logmanager.h>
#include <vector>
#include <glm/glm.hpp>

#include <sstream>
#include <string>
#include <iostream>
#include <thread>         // std::this_thread::sleep_for
#include <chrono>         // std::chrono::seconds

namespace {
	const std::string _loggerCat = "TouchModule";
}

namespace openspace {

	TuioEar TouchModule::*ear;

TouchModule::TouchModule()
    : OpenSpaceModule("Touch")
{

	OsEng.registerModuleCallback(
		OpenSpaceEngine::CallbackOption::Initialize,
		[&]() {
		LDEBUGC("TouchModule", "Initializing TuioEar");
		ear = new TuioEar();
	}
	);
	
	OsEng.registerModuleCallback(
		OpenSpaceEngine::CallbackOption::Deinitialize,
		[&]() {
		LDEBUGC("TouchModule", "Deinitialize TuioEar");
		delete ear;
	}
	);
	
	OsEng.registerModuleCallback( // maybe call ear->clearInput() here rather than postdraw
		OpenSpaceEngine::CallbackOption::PreSync,
		[&]() {
		//std::this_thread::sleep_for(std::chrono::seconds(1));
		list = ear->getInput();
		ear->clearInput();
		glm::vec2 centroid;
		
		
		//print list for debugging
		std::string s = "";
		std::ostringstream os;
		for (const TuioCursor &j : list) {
			
			int count = 0;
			std::list<TuioPoint> path = j.getPath();
			if (lastList.size() > 0 && list.size() > 0) { // sanity check
				std::vector<TuioCursor>::iterator last = find_if(
					lastList.begin(),
					lastList.end(),
					[&j](const TuioCursor& c) { return c.getSessionID() == j.getSessionID(); }
				);
				if (last != lastList.end())
					(path.size() < 128) ? count = path.size() - last->getPath().size() : count = 1; // guess how many? all?
			}
			
			os << ", Id: " << j.getCursorID() << ", path size: " << j.getPath().size() << ", (" << j.getX() << "," << j.getY() << "), To Process: " << count;
		}
		if (list.size() > 0)
			LINFO("List size: " << list.size()  << os.str() << "\n");
		os.clear();
		/*
		// calculate centroid if multipleID
		if (list.size() > 1) {
			centroid = glm::vec2(0.0f, 0.0f);
			for (auto &&i : list) {
				centroid.x += i->getX();
				centroid.y += i->getY();
			}
			centroid.x /= list.size();
			centroid.y /= list.size();

			//LINFO("List size: " << list.size() << ", Centroid: (" << centroid.x << ", " << centroid.y << ")" << "\n");
		}
		*/
		
		glm::mat4 t;
		
		//OsEng.interactionHandler().camera()->rotate();

		lastList = list;
	}
	);

	OsEng.registerModuleCallback(
		OpenSpaceEngine::CallbackOption::PostDraw,
		[&]() {
		WindowWrapper& wrapper = OsEng.windowWrapper();
		if (OsEng.isMaster() && wrapper.isRegularRendering()) {
			
		}
	}
	);
	
}

} // namespace openspace
